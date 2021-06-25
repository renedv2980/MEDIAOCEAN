*          DATA SET ACCRD00    AT LEVEL 035 AS OF 05/01/02                      
*PHASE T60D00A,*                                                                
*INCLUDE GETBROAD                                                               
*INCLUDE PUBVAL                                                                 
*INCLUDE GETDAY                                                                 
*&&UK                                                                           
*INCLUDE ADDAY                                                                  
*&&                                                                             
         TITLE 'T60D00 - CREDIT TRANSACTION MARKER ROOT.'                       
T60D00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (T60DEND-T60DD),*T60D00*,RR=R5,CLEAR=YES                         
         LA    R9,T60D00+2048                                                   
         LA    R9,2048(R9)                                                      
         USING T60D00+4096,R9                                                   
         USING T60DD,RC                                                         
         L     RA,4(R1)                                                         
         USING T60DFFD,RA                                                       
         ST    R5,RELO                                                          
         ST    RB,ABASE                                                         
         ST    R9,ABASE2                                                        
         ST    RD,SAVED                                                         
         ST    RA,ATWA0                                                         
         MVC   ATIA,12(R1)                                                      
         L     R8,ATIA                                                          
         USING CRDSAVD,R8                                                       
         L     RF,8(R1)                                                         
         USING ACCFACSD,RF                                                      
         MVC   FACLIST,ADATAMGR                                                 
         MVC   ADDAY,AADDAY                                                     
         MVC   GETDAY,AGETDAY                                                   
         L     RF,16(R1)                                                        
         USING COMFACSD,RF                                                      
         MVC   SCANNER,CSCANNER                                                 
         MVC   CASHVAL,CCASHVAL                                                 
         MVC   GETFACT,CGETFACT                                                 
         MVC   GETPROF,CGETPROF                                                 
         XC    SQUASHER,SQUASHER                                                
         MVC   COMPANY,0(R1)                                                    
         MVC   TERMINAL,0(RA)      TASK NO.                                     
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     RE,0(R1)            RE = A(GETFACT RETURN BLOCK).                
         USING FACTSD,RE                                                        
         MVC   TODAYB,FADATEB      EXTRACT TODAY IN BINARY YMD.                 
         DROP  RE                                                               
         GOTO1 DATCON,DMCB,(3,TODAYB),(2,TODAYC)                                
         XC    WORK(20),WORK       READ SYSTEM PROFILE                          
         MVC   WORK(4),=C'A000'                                                 
         MVC   WORK+4(1),COMPANY                                                
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         MVC   WORK+12(2),TWAAGY                                                
         GOTO1 GETPROF,DMCB,WORK,SYSTPROF,DATAMGR                               
*                                                                               
         LA    RF,VTYPES                                                        
         LA    RE,ADDRESS                                                       
CRD000   CLI   0(RF),X'FF'                                                      
         BE    CRD0010                                                          
         L     R1,0(RF)                                                         
         A     R1,RELO                                                          
         ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         B     CRD000                                                           
*                                                                               
CRD0010  MVI   DMINBTS,X'C0'       DON'T WANT DELETES                           
         MVI   DMOUTBTS,X'FD'                                                   
         MVI   SPACES,X'40'                                                     
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   SAVE50,SPACES                                                    
         ZAP   TRANSCT,=P'28'      28 ENTRIES PER SCREEN.                       
         OC    OLDKEY,OLDKEY                                                    
         BNZ   *+12                NOT FIRST CALL.                              
         MVI   CRDSTMP1,0          FIRST CALL AVOIDS TEMPSTOR READ              
         B     CRD0012             AND FORCES CLEARING OF TIA.                  
         ZICM  R2,2(RA),2                                                       
         GOTO1 DATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',(1,(R2)),ATIA                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CRD0012  DS    0H                                                               
         MVC   KEY,SPACES                                                       
         CLC   CRDSTMP1,=C'**T60D00Q**'                                         
         BNE   *+14                                                             
         CLC   CRDSTMP2,OLDKEY                                                  
         BE    CRD0016                                                          
         L     RE,ATIA                                                          
         LA    RF,CRDSVLNQ                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR SAVE STORAGE.                          
         MVC   CRDSTMP1,=C'**T60D00Q**'                                         
         MVC   FILTERC,SPACES                                                   
         MVC   CRDTHED(14),=C'**TOTALS FOR**'                                   
         MVC   CRDTHED+22(5),=C'TOTAL'                                          
         MVC   CRDTHED+54(7),=C'BALANCE'                                        
*                                                                               
CRD0016  LA    RF,CRDTYPH          GET FUNCTION.                                
         ST    RF,FADR                                                          
         BAS   RE,ANY                                                           
         MVI   ERRNUM,INVALID                                                   
         LR    RF,RA                                                            
         USING TWAD,RF                                                          
         CLI   TWAOFFC,C'*'        DDS ONLY                                     
         BNE   ERROR                                                            
         LA    RE,TYPTAB                                                        
         ZIC   R1,CRDTYPH+5                                                     
         BCTR  R1,0                                                             
         USING TYPTABD,RE                                                       
*                                                                               
CRD0020  CLI   TYPTYP,X'FF'                                                     
         BE    ERROR                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TYPTYP(0),CRDTYP                                                 
         BE    CRD0040                                                          
         LA    RE,TYPTBLNQ(RE)                                                  
         B     CRD0020                                                          
*                                                                               
CRD0040  MVC   CRDOLA,TYPOLA                                                    
         MVC   CRDBIT,TYPBIT       SAVE THE BIT FOR TRNSSTAT.                   
         MVC   CRDAUTH,TYPAUTH     SAVE U/D AUTHORIZATION FOR THIS TYPE         
         CLC   CRDOLA,OLDOLA                                                    
         BE    CRD0060                                                          
         MVC   OLDOLA,CRDOLA       NEW FUNCTION FORCES                          
         BAS   RE,NEWKEY           CLEAN START.                                 
CRD0060  GOTO1 CALLOV,PARM,(CRDOLA,0),0                                         
         CLI   PARA2,X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,PARA1                                                         
         ST    RF,APPLIC                                                        
         EJECT                                                                  
*              SAVE SOME COMPANY VALUES                                         
         SPACE 2                                                                
*                                                                               
CRD0014  MVC   KEY(1),COMPANY                                                   
         CLC   COMPANY,COMPSAVE                                                 
         BE    CRD010                                                           
         BAS   RE,NEWKEY                                                        
         MVC   COMPSAVE,COMPANY                                                 
         GOTO1 AIORTNS,PARM,AREAD                                               
         LA    R2,IOAREA                                                        
CRD002   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         SR    R1,R1                                                            
         CLI   0(R2),X'10'                                                      
         BE    CRD004                                                           
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     CRD002                                                           
         SPACE 1                                                                
CRD004   MVC   MYCOMPEL,0(R2)                                                   
         EJECT                                                                  
*              VALIDATE LEDGER                                                  
*                                                                               
CRD010   LA    RF,CRDLEGH                                                       
         ST    RF,FADR                                                          
         MVI   ERRNUM,INVALID                                                   
         MVI   KEY+1,C'S'                                                       
         CLI   CRDOLA,RECON        ALL APPLICS BUT BANK RECON (02) TAKE         
         BNE   CRD014              LEDGER FROM SCREEN.                          
         LA    RF,MYCOMPEL         RECON GETS BANK ACC'T U/L FROM               
         USING ACCOMPD,RF          COMPANY RECORD.                              
         MVC   KEY+1(2),ACMPBANK                                                
         MVC   CRDLEG,KEY+2        DISPLAY BANK ACC'T LEDGER.                   
         OI    CRDLEGH+6,X'81'                                                  
         B     CRD030                                                           
*                                                                               
CRD014   BAS   RE,ANY                                                           
         LA    RE,LEDGTAB                                                       
CRD020   CLI   0(RE),X'FF'                                                      
         BE    ERROR                                                            
         CLC   CRDLEG,0(RE)                                                     
         BE    CRD030              PROVISIONALLY O.K.                           
         LA    RE,L'LEDGTAB(RE)                                                 
         B     CRD020                                                           
CRD030   DS    0H                                                               
*                                                                               
         MVC   KEY+2(1),CRDLEG                                                  
         CLI   CRDLEG,C'E'         EXPENSE DEFAULT.                             
         BNE   *+14                NO.                                          
         USING ACCOMPD,RF                                                       
         LA    RF,MYCOMPEL                                                      
         MVC   KEY+2(1),ACMPSUPX                                                
         CLC   ULSAVE,KEY+1                                                     
         BE    CRD080                                                           
         MVC   KEY+3(L'KEY-3),SPACES                                            
         BAS   RE,NEWKEY                                                        
         MVI   ERRNUM,INVLEG                                                    
         GOTO1 AIORTNS,PARM,AHIGH                                               
         LA    RF,CRDLEGH                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    RE,IOAREA                                                        
         SR    RF,RF                                                            
*                                                                               
CRD040   CLI   0(RE),0             LOOK FOR STATUS ELEMENT.                     
         BE    CRD080              NONE.                                        
         CLI   0(RE),X'30'                                                      
         BE    CRD060              GOT HIM.                                     
         CLI   0(RE),X'20'                                                      
         BE    CRD074              NAME ELEMENT.                                
CRD044   ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     CRD040                                                           
*                                                                               
         USING ACSTATD,RE                                                       
         USING TWAD,RF                                                          
CRD060   LR    RF,RA                                                            
         CLC   TWAAUTH+1(1),ACSTSECY+1                                          
         BNL   CRD044                                                           
         LA    RF,CRDLEGH                                                       
         MVC   CRDLNM,SPACES                                                    
         OI    CRDLNMH+6,X'80'                                                  
CRD070   MVI   ERRNUM,SECREJ                                                    
         OI    DMCB+8,X'04'                                                     
         B     ERROR                                                            
*                                                                               
         USING ACNAMED,RE                                                       
CRD074   ZIC   RF,ACNMLEN                                                       
         MVC   CRDLNM,SPACES                                                    
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CRDLNM(0),ACNMNAME                                               
         OI    CRDLNMH+6,X'80'                                                  
         B     CRD044                                                           
CRD080   DS    0H                                                               
         MVC   ULSAVE,KEY+1        SAVE NEW UNIT/LEDGER.                        
         DROP  RE,RF                                                            
         EJECT                                                                  
*              PAYEE VALIDATION                                                 
*                                                                               
         LA    RF,CRDACCH                                                       
         ST    RF,FADR                                                          
         BAS   RE,ANY                                                           
         MVI   ERRNUM,BADACC                                                    
         ZIC   RE,5(RF)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),CRDACC                                                  
         CLI   KEY+2,C'P'          DEAL WITH PUB NUMBERS                        
         BNE   CRD100                                                           
         CLI   CRDACCH+5,6         ASSUME PRINT REPS ARE LESS                   
         BL    CRD100              THAN 6 LONG                                  
         CLI   CRDACC+9,C'*'       ACCOUNTS CREATED AS OFFICE OVERRIDES         
         BE    CRD100                                                           
         LR    R2,RE                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'ACCOUNT',KEY,KEY                    
         CLI   DMCB+8,0                                                         
         BE    CRD100              VALID ACCOUNT                                
         MVC   KEY,KEYSAVE                                                      
         MVC   KEY+4(20),SPACES    NOT VALID SO SEE IF VALID PUBVAL             
         GOTO1 =V(PUBVAL),DMCB,((R2),CRDACC+1),(1,KEY+4),RR=RELO                
         CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
CRD100   CLC   ACCSAVE,KEY+3                                                    
         BE    CRD170                                                           
         MVC   KEYCON,SPACES                                                    
         BAS   RE,NEWKEY                                                        
*                                                                               
         GOTO1 AIORTNS,PARM,AHIGH                                               
         LA    RF,CRDACCH                                                       
         CLC   KEY(15),KEYSAVE                                                  
         BNE   ERROR                                                            
         LA    RF,IOAREA                                                        
CRD120   CLI   0(RF),0                                                          
         BE    CRD170                                                           
         CLI   0(RF),X'30'                                                      
         BE    CRD140              STATUS ELEMENT.                              
         CLI   0(RF),X'20'                                                      
         BE    CRD160              NAME ELEMENT.                                
CRD130   ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     CRD120                                                           
*                                                                               
         USING ACSTATD,RF                                                       
CRD140   LR    RE,RA                                                            
         USING TWAD,RE                                                          
         CLC   TWAAUTH+1(1),ACSTSECY+1                                          
         BNL   CRD130                                                           
         LA    RF,CRDACCH                                                       
         FOUT  CRDANMH,SPACES,36                                                
         B     CRD070              HANDLES DETAILS OF SECURITY ERROR.           
*                                                                               
         USING ACNAMED,RF                                                       
CRD160   ZIC   RE,ACNMLEN                                                       
         MVC   CRDANM,SPACES                                                    
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CRDANM(0),ACNMNAME                                               
         OI    CRDANMH+6,X'80'                                                  
         B     CRD130                                                           
*                                                                               
CRD170   DS    0H                                                               
         MVC   ACCSAVE,KEY+3       SAVE NEW ACCOUNT CODE.                       
         EJECT                                                                  
*              VALIDATE OPTIONS                                                 
*                                                                               
         LA    RF,CRDOPTH                                                       
         ST    RF,FADR                                                          
         MVC   OPTIONS,SPACES                                                   
         CLI   CRDOPTH+5,0                                                      
         MVI   FNDX,0                                                           
         BE    CRD400                                                           
         GOTO1 SCANNER,DMCB,CRDOPTH,SCANTAB                                     
         ZICM  R3,DMCB+4           NUMBER OF LINES                              
         BNZ   CRD180                                                           
         MVI   ERRNUM,INVALID                                                   
         B     ERROR                                                            
*                                                                               
CRD180   MVI   FNDX,1                                                           
         LA    R4,SCANTAB                                                       
*                                                                               
         USING OPTABD,R2                                                        
CRD200   LA    R2,OPTAB                                                         
CRD210   CLI   OPTNAM,X'FF'                                                     
         BNE   CRD216                                                           
CRD214   MVI   ERRNUM,INVOPT                                                    
         B     ERROR                                                            
CRD216   CLC   12(4,R4),=C'HELP'                                                
         BE    HELP                                                             
         ZIC   RE,0(R4)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   OPTNAM(0),12(R4)                                                 
         BE    CRD220                                                           
         LA    R2,OPTABLNQ(R2)                                                  
         B     CRD210                                                           
*                                                                               
CRD220   ZICM  RF,OPTRTN,3                                                      
         A     RF,RELO             RF = A(OPTION VALIDATION ROUTINE).           
         BAS   RE,CRD240                                                        
         BNE   CRD214              CC OF NEQ FAILS VALIDATION.                  
         LA    R4,32(R4)                                                        
         ZIC   RF,FNDX                                                          
         LA    RF,1(RF)                                                         
         STC   RF,FNDX                                                          
         BCT   R3,CRD200                                                        
         B     CRD400                                                           
*                                                                               
CRD240   NTR1                                                                   
         BR    RF                                                               
*                                                                               
*              OPTION VALIDATIONS AND SET-UPS HERE                              
*                                                                               
CRD260   MVC   ALL,22(R4)                                                       
         CLI   ALL,C'Y'                                                         
         BE    EXIT                                                             
         CLI   ALL,C'N'                                                         
         B     EXIT                                                             
*                                                                               
CRD280   MVI   SWITCH,1                                                         
         CR    RB,RB               FORCE CC OF EQUAL.                           
         B     EXIT                                                             
*                                                                               
CRD400   DS    0H                                                               
         EJECT                                                                  
*                                  VALIDATE DATES AND FILTERS.                  
*                                                                               
         LA    RF,CRDFILH                                                       
         ST    RF,FADR                                                          
         MVC   WFILTERC,SPACES                                                  
         XC    WFILTERB,WFILTERB                                                
         MVI   FNDX,0                                                           
         MVI   CHANGE,C'N'                                                      
         OI    CRDFILH+6,X'81'                                                  
         CLI   CRDFILH+5,0                                                      
         BNE   CRD402                                                           
         CLC   FILTERC,SPACES                                                   
         BNE   *+14                                                             
         OC    FILTERB,FILTERB                                                  
         BZ    CRD448                                                           
         MVI   CHANGE,C'Y'                                                      
         MVC   FILTERC,SPACES                                                   
         XC    FILTERB,FILTERB                                                  
         B     CRD448                                                           
*                                                                               
CRD402   CLC   FILTERC,SPACES                                                   
         BNE   CRD403                                                           
         OC    FILTERB,FILTERB                                                  
         BNZ   CRD403                                                           
         MVI   CHANGE,C'Y'                                                      
CRD403   GOTO1 SCANNER,DMCB,CRDFILH,SCANTAB                                     
         ZICM  R3,DMCB+4                                                        
         BNZ   CRD410                                                           
CRD405   MVI   ERRNUM,INVALID                                                   
CRD406   B     ERROR                                                            
*                                                                               
CRD410   MVI   FNDX,1                                                           
         LA    R4,SCANTAB                                                       
*                                                                               
         USING FILTABD,R2                                                       
CRD420   LA    R2,FILTAB                                                        
CRD430   CLI   FILTNAM,X'FF'                                                    
         BE    CRD405                                                           
         CLC   12(4,R4),=C'HELP'                                                
         BE    HELP                                                             
         ZIC   RE,0(R4)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FILTNAM(0),12(R4)                                                
         BE    CRD440                                                           
         LA    R2,FILTBLNQ(R2)                                                  
         B     CRD430                                                           
*                                                                               
CRD440   ZICM  RF,FILRTN,3         GET A(VALIDATION ROUTINE).                   
         BZ    CRD442              ZERO = NO ROUTINE.                           
         A     RF,RELO             RF = A(ROUTINE).                             
         BAS   RE,CRD460           SAVE REGISTERS.                              
         BNE   CRD405              RETURN WITH CC OF NOT EQUAL FAILS.           
         B     CRD444              SKIP THE NORMAL SET-UPS.                     
CRD442   ZICM  RF,FILNAD,2                                                      
         LA    RF,WKSTRT(RF)       RF = A(FILTER DATA FIELD)                    
         ZIC   RE,FILNLN           RE = L'FILTER DATA FIELD.                    
         EX    RE,FILNMVC          MOVE I/P TO FILTER DATA FIELD.               
         ZICM  RF,FILLAD,2         GET A(FILTER DATA LENGTH FIELD).             
         BZ    CRD444              IF ZERO, IT'S NOT NEEDED.                    
         LA    RF,WKSTRT(RF)       RF = A(LENGTH FIELD)                         
         LA    RE,1(RE)                                                         
         CLM   RE,1,1(R4)          ENSURE I/P LENGTH IS OK FOR FIELD.           
         BL    CRD405                                                           
         MVC   0(1,RF),1(R4)                                                    
CRD444   LA    R4,32(R4)           GET NEXT SCANTAB LINE.                       
         ZIC   RE,FNDX                                                          
         LA    RE,1(RE)                                                         
         STC   RE,FNDX                                                          
         BCT   R3,CRD420                                                        
         CLC   FILTERS,WFILTERS                                                 
         BE    CRD448                                                           
         MVI   CHANGE,C'Y'                                                      
         MVC   FILTERS,WFILTERS                                                 
CRD448   CLI   CRDOLA,REVERSE      FOR REVERSE (03), WE MUST HAVE AN            
         BNE   CRD449              AMOUNT TO FILTER ON.                         
         CLC   FILTAMNT,SPACES                                                  
         BE    CRD590              NO AMOUNT, SAY SO.                           
*&&UK                                                                           
         CLC   MOS,SPACES          MONTH OF SERVICE                             
         BE    CRD590              FILTER TOO, PLEASE                           
*&&                                                                             
CRD449   CLI   CRDOLA,RECON        FOR RECON (02), IF NO START CHEK NO          
         BNE   CRD450              WAS INPUT, FORCE IT TO 1.                    
         CLC   FRSTCHEK,SPACES                                                  
         BNE   *+8                                                              
         MVI   FRSTCHEK+5,C'1'                                                  
CRD450   CLI   CHANGE,C'N'                                                      
         BE    CRD600                                                           
         MVC   KEYCON,SPACES                                                    
         BAS   RE,NEWKEY                                                        
         B     CRD600                                                           
*                                                                               
FILNMVC  MVC   0(0,RF),22(R4)                                                   
*                                                                               
CRD460   NTR1                                                                   
         BR    RF                  GOTO THE VALIDATION ROUTINE.                 
*                                                                               
*              ANY EXTRA VALIDATION CODE GOES HERE.                             
*                                                                               
CRD480   MVI   BYTE,0                                                           
         CLI   1(R4),5                                                          
         BH    *+8                                                              
         MVI   BYTE,2                                                           
         GOTO1 DATVAL,DMCB,(BYTE,22(R4)),WORK                                   
         MVI   ERRNUM,BADATE                                                    
         OC    DMCB(4),DMCB                                                     
         BZ    FAILIT                                                           
         CLI   BYTE,0                                                           
         BNE   EXIT                                                             
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         ZIC   R1,0(R4)            DISTINGUISH SIMPLE DATE FROM                 
         BCTR  R1,0                START OR END DATE.                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),=C'DATE'                                                
         BNE   CRD490                                                           
         MVC   SDATEP,WORK+6                                                    
         MVC   EDATEP,SDATEP                                                    
         B     PASSIT                                                           
*                                                                               
CRD490   ZICM  RF,FILNAD,2                                                      
         LA    RF,WKSTRT(RF)                                                    
         MVC   0(3,RF),WORK+6                                                   
         B     PASSIT                                                           
*                                                                               
CRD500   GOTO1 DATVAL,PARM,(2,22(R4)),WORK                                      
         MVI   ERRNUM,BADATE                                                    
         OC    PARA1,PARA1                                                      
         BZ    FAILIT                                                           
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,PARM,(0,WORK),(1,WORK+6)                                  
         OI    WORK+6,X'F0'                                                     
         TM    WORK+7,X'10'                                                     
         BO    *+12                                                             
         OI    WORK+7,X'F0'                                                     
         B     CRD510                                                           
         ZIC   RF,WORK+7                                                        
         LA    RF,X'B1'(RF)                                                     
         STC   RF,WORK+7                                                        
CRD510   MVC   MOS,WORK+6                                                       
         B     PASSIT                                                           
*                                                                               
CRD540   MVC   MARKED,22(R4)                                                    
         CLI   MARKED,C'Y'                                                      
         BE    EXIT                                                             
         CLI   MARKED,C'N'                                                      
         BE    EXIT                                                             
         CLI   MARKED,C'A'                                                      
         BNE   EXIT                                                             
         MVI   MARKED,C' '                                                      
         B     EXIT                                                             
*                                                                               
CRD560   ZIC   R2,1(R4)            R2 = L'INPUT AMOUNT.                         
         GOTO1 CASHVAL,PARM,22(R4),(R2)                                         
         CLI   PARA1,0                                                          
         BNE   EXIT                INVALID AMOUNT.                              
         L     R2,PARA2                                                         
         CVD   R2,DUB                                                           
         MVC   FILTAMNT,DUB+2                                                   
         B     PASSIT                                                           
*                                                                               
*&&US                                                                           
CRD580   LA    RE,CLILEGS          GET CLIENT LEDGER TABLE.                     
CRD580A  CLI   0(RE),X'FF'                                                      
         BE    FAILIT              NO CLIENT IN THIS LEDGER.                    
         CLC   0(1,RE),ULSAVE+1                                                 
         BNE   *+14                                                             
         MVC   CLIENT,22(R4)       SET CLIENT FILTER.                           
         B     PASSIT              GOOD EXIT                                    
         LA    RE,2(RE)                                                         
         B     CRD580A                                                          
*&&                                                                             
CRD590   MVI   ERRNUM,NOAMTMOS     INDICATE MONTH OF SERVICE                    
*&&US*&& MVI   ERRNUM,INFILAMT                                                  
         B     CRD980              AS WELL AS AMOUNT REQUIRED                   
*                                                                               
CRDTYPF  ZIC   R2,1(R4)                                                         
         BCTR  R2,0                R2 = L'INPUT - 1.                            
         LA    RE,CRTYPES          LOOK FOR I/P IN LIST OF CREDIT               
         MVI   TYPEFILT,C'C'       SYNONYMS.                                    
CRTYP020 CLI   0(RE),X'FF'                                                      
         BE    CRTYP040            NOT A CREDIT SYNONYM.                        
         EX    R2,CRTYPCLC         CLC   22(0,R4),0(RE)                         
         BE    PASSIT              IT'S A CREDIT TYPE.                          
         LA    RE,8(RE)                                                         
         B     CRTYP020                                                         
*                                                                               
CRTYP040 MVI   TYPEFILT,C'D'       NOW DEBIT SYNONYMS.                          
         LA    RE,DRTYPES                                                       
CRTYP050 CLI   0(RE),X'FF'                                                      
         BE    FAILIT              COMPLETELY ASYNONYMOUS.                      
         EX    R2,CRTYPCLC         CLC   22(0,R4),0(RE)                         
         BE    PASSIT              IT'S A DEBIT.                                
         LA    RE,8(RE)                                                         
         B     CRTYP050                                                         
*                                                                               
CRTYPES  DC    CL8'PAYMENT'                                                     
         DC    AL1(255)                                                         
DRTYPES  DC    CL8'RECEIPTS'                                                    
         DC    AL1(255)                                                         
*                                                                               
CRTYPCLC CLC   22(0,R4),0(RE)                                                   
*                                                                               
CRDAUTHF CLI   SYSTPRF2,C'Y'                                                    
         BNE   FAILIT              NOT USING AUTHORIZATION SYSTEM               
         CLC   ULSAVE,=C'SV'                                                    
         BNE   FAILIT              PRODUCTION ONLY                              
         CLI   22(R4),C'Y'                                                      
         BNE   FAILIT                                                           
         MVI   UNAUTH,C'Y'                                                      
         B     PASSIT                                                           
         EJECT                                                                  
*        VALIDATE & CONSOLIDATE US PUB, ED, ZONE HERE.                          
CRD600   DS    0H                                                               
*&&US                                                                           
         CLC   PUBLCN,SPACES                                                    
         BE    CRD600X                                                          
         LA    R4,PUBLCN           BUILD AN EXPRESSION FOR PUBVAL               
         IC    R3,PUBLEN                                                        
         AR    R4,R3                                                            
         CLC   ZONE,SPACES                                                      
         BE    CRD6002                                                          
         MVI   PUBLEN,X'FF'                                                     
         CLC   ZONE,=C'ALL'                                                     
         BNE   *+14                                                             
         MVC   PUBLCN+8(3),=C'ZZZ'                                              
         B     CRD600X                                                          
         MVI   0(R4),C','                                                       
         IC    R3,ZLEN                                                          
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),ZONE                                                     
         LA    R4,2(R3,R4)         READY FOR NEXT COMMA                         
*                                                                               
CRD6002  CLC   EDITION,SPACES                                                   
         BE    CRD6004                                                          
         MVI   0(R4),C','                                                       
         IC    R3,EDLEN                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),EDITION                                                  
         MVI   PUBLEN,X'FF'                                                     
*                                                                               
CRD6004  LA    R4,PUBLCN+14                                                     
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         LA    R3,PUBLCN                                                        
         SR    R4,R3                                                            
         LA    R4,1(R4)                                                         
         GOTO1 =V(PUBVAL),DMCB,((R4),PUBLCN),(1,WORK),RR=RELO                   
         CLI   DMCB,X'FF'                                                       
         BE    CRD6006                                                          
         MVC   PUBLCN,SPACES                                                    
         MVC   PUBLCN(11),WORK                                                  
         B     CRD600X                                                          
*                                                                               
CRD6006  MVC   CRDMSG,SPACES                                                    
         MVC   CRDMSG(39),=C'ERROR *** INVALID PUB, EDITION OR ZONE.'           
         MVI   FNDX,0                                                           
         MVI   ERRNUM,0                                                         
         OI    CRDMSGH+6,X'80'                                                  
         LA    RF,CRDFILH                                                       
         ST    RF,FADR                                                          
         B     CRD980                                                           
*&&                                                                             
*                                                                               
CRD600X  DS    0H                                                               
         EJECT                                                                  
*              START OF MAIN LOOP                                               
*                                                                               
CRD610   MVI   ERRNUM,0                                                         
         LA    RF,CRDMRKH                                                       
         ST    RF,FADR                                                          
         CLI   MODE,C'M'           IF MODE = M                                  
         BE    CRD800              MARK RECORDS FROM PREVIOUS DISPLAY.          
*                                                                               
*              READ AND DISPLAY TRANSACTIONS.                                   
*                                                                               
         TWAXC CRDMRKH,CRDTHEDH-1,PROT=Y                                        
         MVI   DMINBTS,X'00'       DON'T READ DELETES, NO UPDATE.               
         LA    RE,KEYTAB           CLEAR LIST OF DISPLAYED KEYS                 
         LA    RF,KEYTBLNQ                                                      
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   KEY,OLDKEY                                                       
         CLC   KEYCON,SPACES                                                    
         BNE   CRD614                                                           
         CLI   CRDOLA,RECON        FOR RECON (02), 1ST READ SKIPS VOIDS         
         BNE   *+8                                                              
         MVI   KEYCON+3,X'5D'      C'*' + 1 = X'5D'                             
         GOTO1 AIORTNS,PARM,AHIGH                                               
         B     CRD640                                                           
CRD614   GOTO1 AIORTNS,PARM,AREAD                                               
         B     CRD640                                                           
*                                                                               
CRD620   GOTO1 AIORTNS,PARM,ASEQ                                                
CRD640   CLC   KEYACC,KEYSAVE                                                   
         BE    *+12                ACCOUNT CONTINUES.                           
         MVI   ACCEND,C'Y'         ACCOUNT ENDS                                 
         B     CRD650                                                           
         LA    RF,IOAREA                                                        
         CLI   0(RF),X'44'                                                      
         BNE   CRD620                                                           
*                                                                               
         GOTO1 AFILTER                                                          
         BNE   CRD620              TRANSACTION DOESN'T QUALIFY.                 
         CP    TRANSCT,=P'1'                                                    
         BNL   CRD660              STILL ROOM ON SCREEN.                        
CRD650   MVC   OLDKEY,KEY                                                       
         GOTO1 APPLIC,PARM,(RC)                                                 
         BNE   ERROR                                                            
CRD654   MVI   MODE,C'M'                                                        
         B     CRD920              EXIT SETTING TOTALS.                         
*                                                                               
CRD660   XC    TABREC,TABREC                                                    
         GOTO1 APPLIC,PARM,(RC)    O'LAY DOES DISPLAY AND SAVES KEYS.           
         BNE   ERROR                                                            
         B     CRD620                                                           
         EJECT                                                                  
CRD800   MVI   CHANGE,C'N'                                                      
         MVI   DMINBTS,X'88'       READ DELETES AND FOR UPDATE.                 
         GOTO1 APPLIC,PARM,(RC)    O'LAY CHECKS MARKS AND MARKS KEYS            
         BNE   ERROR               FOR RE-WRITE. CC OF NEQ IS ERROR.            
*                                                                               
CRD880   CLI   CHANGE,C'Y'                                                      
         BNE   CRD896              NO CHANGES, TEST FOR ACC'T END.              
         L     RE,ATWA0            CHECK THAT TERMINAL IS AUTHORIZED            
         USING TWAD,RE             FOR UPDATES.                                 
         IC    R1,CRDAUTH          R1 = AUTH BIT FOR THIS FUNCTION.             
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TWAAUTH,0                                                        
         BNZ   CRD882              IT IS.                                       
         MVC   CRDMSG,SPACES                                                    
         MVC   CRDMSG(35),=C'TERMINAL NOT AUTHORIZED FOR MARKING.'              
         OI    CRDMSGH+6,X'80'                                                  
         TWAXC CRDTYPH,CRDFILH                                                  
         TWAXC CRDMRKH,CRDTHEDH-1,PROT=Y                                        
         XC    CRDTOT,CRDTOT                                                    
         OI    CRDTOTH+6,X'80'                                                  
         BAS   RE,NEWKEY           FORCE A RESTART.                             
         OI    CRDTHEDH+6,X'80'                                                 
         LA    RF,CRDTYPH                                                       
         ST    RF,FADR                                                          
         B     CRD990                                                           
         DROP  RE                                                               
*                                                                               
CRD882   CLI   CRDBIT,0                                                         
         BE CRD896                 NO BIT, OLAY MARKS FILE.                     
*                                                                               
         USING KEYTABD,R2                                                       
         LA    R2,KEYTAB           SEARCH LIST OF DISPLAYED KEYS FOR            
         LA    R3,28               NON-ZERO ENTRIES.                            
CRD884   OC    KTBKEY,KTBKEY                                                    
         BNZ   CRD886                                                           
CRD885   LA    R2,KTBLNQ(R2)                                                    
         BCT   R3,CRD884                                                        
         B     CRD896                                                           
*                                                                               
CRD886   MVC   KEY(3),COMPSAVE     COMPANY/UNIT/LEDGER                          
         MVC   KEY+3(12),ACCSAVE   ACCOUNT CODE                                 
         MVC   KEY+L'ACKEYACC(L'KTBKEY),KTBKEY                                  
         GOTO1 AIORTNS,PARM,AREAD  GET THE RECORD                               
         LA    RF,IOAREA                                                        
         USING TRANSD,RF                                                        
         CLC   TRNSSTAT,KTBDSTAT   IF INCOMING STATUS DIFFERS FROM              
         BNE   CRD960              SAVED STATUS, THERE'S PROBLEMS.              
         CLI   KTBCHNG,X'FF'                                                    
         BNE   CRD885              ENTRY IS NOT BEING CHANGED.                  
         ZIC   RE,CRDBIT                                                        
         EX    RE,*+8              TEST THE APP'T TRNSSTAT BIT.                 
         B     *+8                                                              
         TM    TRNSSTAT,0                                                       
         BO    CRD887              YES.                                         
         AP    ACCMRKD,TRNSAMNT    NO.                                          
         SP    ACCBAL,TRNSAMNT                                                  
         B     CRD888                                                           
CRD887   SP    ACCMRKD,TRNSAMNT                                                 
         AP    ACCBAL,TRNSAMNT                                                  
CRD888   ZIC   RE,CRDBIT                                                        
         EX    RE,*+8                                                           
         B     *+8                                                              
         XI    TRNSSTAT,0          FLIP THE STATUS BIT FOR THIS PHASE.          
         MVC   KTBDSTAT,TRNSSTAT   RESET THE SAVED STATUS BYTE.                 
         MVI   KTBCHNG,0           RESET THE CHANGE MARKER.                     
         CLI   CRDOLA,REVERSE      FOR REVERSALS, MARK TRANSACTIONS AS          
         BNE   CRD890              USED/UNUSED IF REVERSING/UNREVERSING         
         LA    RE,KEY                                                           
         USING ACKEYD,RE                                                        
         TM    TRNSSTAT,X'20'                                                   
         BZ    *+14                UNREVERSING.                                 
         MVC   ACDTUSED,TODAYC     REVERSING, MARK AS USED.                     
         B     CRD890                                                           
         XC    ACDTUSED,ACDTUSED   MARK AS UNUSED.                              
         DROP  RE                                                               
*                                                                               
CRD890   GOTO1 AIORTNS,PARM,AWRITE PUT THE RECORD BACK.                         
         B     CRD885              TRY THE NEXT ONE.                            
         DROP  R2                                                               
*                                                                               
CRD896   CLI   ACCEND,C'Y'         NO MARKERS CHANGED                           
         BNE   *+12                ACC'T CONTINUES.                             
         BAS   RE,TOTOUT           ACC'T ENDS, DISPLAY TOTALS.                  
         B     CRD900                                                           
         ZAP   TRANSCT,=P'28'                                                   
         MVI   MODE,C'D'           ACC'T CONTINUES SO JUST DISPLAY              
         B     CRD610              NEXT SCREEN.                                 
*                                                                               
CRD900   MVC   CRDMSG,SPACES                                                    
         MVC   CRDMSG(37),=C'END OF TRANSACTIONS FOR THIS ACCOUNT.'             
         OI    CRDMSGH+6,X'80'                                                  
         LA    RF,CRDLEGH                                                       
         ST    RF,FADR                                                          
         B     CRD980                                                           
*                                                                               
CRD920   BAS   RE,TOTOUT           DISPLAY TOTALS.                              
         MVC   CRDMSG,SPACES                                                    
         CP    TRANSCT,=P'28'                                                   
         BE    CRD950              NO ITEMS QUALIFIED.                          
         MVC   CRDMSG(47),=C'MARK TRANSACTIONS OR HIT ENTER FOR NEXT SC*        
               REEN.'                                                           
         OI    CRDMSGH+6,X'80'                                                  
         CLI   ACCEND,C'Y'                                                      
         BNE   CRD930                                                           
         MVC   CRDMSG+31(16),=C'TO END ACCOUNT. '                               
         USING TWAD,RE                                                          
CRD930   L     RE,ATWA0            IF TERMINAL IS NOT AUTHORIZED FOR            
         IC    R1,CRDAUTH          MARKING, ALTER MESSAGE.                      
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    TWAAUTH,0                                                        
         BNZ   CRD990              IT IS AUTHORIZED.                            
         MVC   CRDMSG(26),CRDMSG+21                                             
         MVC   CRDMSG+26(L'CRDMSG-26),SPACES                                    
         B     CRD990                                                           
         DROP  RE                                                               
*                                                                               
CRD950   MVC   CRDMSG,SPACES                                                    
         MVC   CRDMSG(36),=C'NO ITEMS DISPLAYED FOR THIS ACCOUNT.'              
         OI    CRDMSGH+6,X'80'                                                  
         LA    RF,CRDLEGH                                                       
         ST    RF,FADR                                                          
         B     CRD980                                                           
*                                                                               
CRD960   MVI   ERRNUM,DUBLUPDT                                                  
         LA    RF,CRDLEGH                                                       
         ST    RF,FADR                                                          
         B     CRD980                                                           
*                                                                               
CRD980   XC    ULSAVE,ULSAVE       RESET UNIT/LEDGER                            
         XC    ACCSAVE,ACCSAVE     AND ACC'T SAVED CODES.                       
         XC    OLDKEY,OLDKEY                                                    
         XC    KEYSAVE,KEYSAVE                                                  
         MVI   MODE,C'D'                                                        
         CLI   ERRNUM,0                                                         
         BNE   ERROR1                                                           
CRD990   BAS   RE,TWARYT                                                        
         L     RF,FADR                                                          
         OI    6(RF),X'40'                                                      
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*              SET VALUES FOR CHANGE OF KEY.                                    
*                                                                               
NEWKEY   MVC   OLDKEY,KEY                                                       
         MVI   MODE,C'D'                                                        
         MVI   ACCEND,C'N'                                                      
         ZAP   ACCMRKD,=P'0'       RESET TOTAL ACCUMULATORS.                    
         ZAP   ACCBAL,=P'0'                                                     
         ZAP   ACCTOT,=P'0'                                                     
         XC    HICHEK,HICHEK                                                    
         BR    RE                                                               
*                                                                               
*              TEST TRANSACTIONS FOR COMPLIANCE WITH PROGRAM                    
*              AND FILTER REQUIREMENTS                                          
*              EXIT WITH CC OF EQUAL IF IT QUALIFIES.                           
*              EXIT WITH CC OF NOT EQUAL IF IT FAILS.                           
*                                                                               
FILTERIT NTR1  BASE=ABASE                                                       
         L     R9,ABASE2                                                        
         LA    R2,IOAREA                                                        
         USING TRANSD,R2                                                        
         LA    RF,KEY                                                           
         USING ACKEYD,RF                                                        
*                                                                               
         CLC   ULSAVE,=C'SV'                                                    
         BE    *+14                SV AND SX MAY USE AUTHORISATION              
         CLC   ULSAVE,=C'SX'                                                    
         BNE   FIL005                                                           
         CLI   CRDOLA,DISCOUNT     FOR ALL OLAYS BUT DISCOUNT                   
         BE    FIL005                                                           
         CLI   CRDOLA,REVERSE      AND REVERSAL/MATCH,                          
         BE    FIL005              AND FOR ALL AGENCIES USING AUTHOR-           
         CLI   SYSTPRF2,C'Y'       IZATION SYSTEM, FILTER AUTHED/               
         BNE   FIL005              UNAUTHED                                     
         CLI   UNAUTH,C'Y'                                                      
         BE    FIL005              TAKING AUTHED AND UNAUTHED                   
         TM    TRNSSTAT,X'08'                                                   
         BZ    FAILIT              AUTHED ONLY                                  
FIL005   CLI   CRDOLA,REVERSE      IF REVERSING/MATCHING                        
         BNE   FIL008                                                           
         CLI   MARKED,C' '         DEFAULT TO SHOW ONLY UN-MATCHED              
         BNE   FIL008                                                           
         TM    TRNSSTAT,X'20'                                                   
         BNZ   FAILIT              SO MATCHED ITEMS FAIL                        
FIL008   CLI   TYPEFILT,C'D'                                                    
         BE    FIL010              ACCEPTING DEBITS ONLY.                       
         TM    TRNSSTAT,X'80'                                                   
         BNZ   FAILIT              ACCEPTING CREDITS ONLY.                      
         B     *+12                                                             
FIL010   TM    TRNSSTAT,X'80'                                                   
         BZ    FAILIT                                                           
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   FAILIT              IT'S BEEN PEELED.                            
         CLI   CRDOLA,RECON                                                     
         BNE   FIL014                                                           
         CP    TRNSAMNT,=P'0'                                                   
         BE    FAILIT              AVOID A ZERO CHECK.                          
*                                                                               
FIL014   CLI   CRDOLA,REVERSE      EXCLUDE APPROVED ITEMS FROM                  
         BE    FIL014A             MATCH/REVERSAL AND VICE VERSA.               
         OC    ACDTUSED,ACDTUSED                                                
         BNZ   FAILIT              IT'S BEEN THROUGH A CHEQUE RUN.              
         CLI   CRDOLA,RECON                                                     
         BE    FIL015              RECONCILE DOESN'T DISCRIMINATE.              
         TM    TRNSSTAT,X'20'                                                   
         BO    FAILIT              MARKED FOR REVERSAL.                         
         B     FIL015                                                           
FIL014A  TM    TRNSSTAT,X'02'                                                   
         BO    FAILIT              APPROVED.                                    
         OC    ACDTUSED,ACDTUSED                                                
         BZ    FIL015             NOT REVERSED OR BEEN THRU CHECK RUN.          
         CLC   ACDTUSED,TODAYC                                                  
         BNE   FAILIT             CAN ONLY UNREVERSE TODAY'S ACTIVITY.          
*                                                                               
         DROP  RF                                                               
FIL015   BAS   RE,ELEMDIG                                                       
         CLI   CRDOLA,DISCOUNT                                                  
         BNE   FIL016                                                           
         CLC   SAVE50,SPACES       DISCOUNT REQUIRES X'50' EL WITH              
         BE    FAILIT              TYPE OF C'D'                                 
         LA    RF,SAVE50                                                        
         USING TRCASHD,RF                                                       
          CLI   TRCSTYPE,C'D'                                                   
         BE    *+12                                                             
         CLI   TRCSTYPE,C'X'                                                    
         BNE   FAILIT                                                           
          DROP  RF                                                              
*                                                                               
FIL016   LA    R3,FILTAB                                                        
         USING FILTABD,R3                                                       
*                                                                               
FIL020   CLI   FILTNAM,X'FF'                                                    
         BE    PASSIT              END OF TABLE, PASSED ALL FILTERS.            
         ZICM  RF,FILNAD,2                                                      
         BZ    FIL060              KEYWORD HAS NO UNIQUE FILTER FIELD.          
         LA    RF,WKSTRT(RF)       RF = A(FILTER FIELD)                         
         ZIC   RE,FILNLN           GET L'FILTER FIELD.                          
         LA    R1,FILCNUL          IF FILFTYP = 0, A NULL FILTER WILL           
         CLI   FILFTYP,1           BE X'00'S, IF IT'S 1, A NULL WILL            
         BE    *+8                 BE SPACES.                                   
         LA    R1,FILBNUL                                                       
         EX    RE,0(R1)            CHECK IT OUT                                 
         BC    8,FIL060            NO FILTER TO APPLY.                          
         ZICM  RF,FILFRTN,3        GET A(SPECIAL FILTER ROUTINE).               
         BZ    FIL040              THERE IS NONE.                               
         A     RF,RELO             RF = A(ROUTINE).                             
         BAS   RE,FIL100                                                        
         BNE   FAILIT                                                           
         B     FIL060                                                           
FIL040   ZICM  RF,FILNAD,2         GET A(FILTER FIELD).                         
         LA    RF,WKSTRT(RF)       RF = A(FILTER FIELD)                         
         ZIC   R1,FILNLN           R1 = ABSOLUTE L'FIELD-1.                     
         LA    R1,1(R1)                                                         
         ZICM  RE,FILLAD,2         GET A(FILTER I/P LENGTH).                    
         BZ    FIL050              NO I/P LENGTH SO USE ABSOLUTE.               
         LA    RE,WKSTRT(RE)                                                    
         ZIC   R1,0(RE)            R1 = L'FILTER I/P.                           
FIL050   ZICM  RE,FILFAD,2         GET A(RECORD FIELD FOR FILTER).              
         BZ    FIL060              DUMMY OR PASSIVE FILTER.                     
         LA    RE,KEY(RE)                                                       
         BCTR  R1,R0                                                            
         EX    R1,FILFCLC          TEST IT.                                     
         BNE   FAILIT                                                           
*                                                                               
FIL060   LA    R3,FILTBLNQ(R3)                                                  
         B     FIL020                                                           
*                                                                               
FIL100   NTR1  SAVE REGS BEFORE SPECIAL ROUTINE.                                
         BR    RF                  DO IT                                        
*                                                                               
FAILIT   LTR   RB,RB               FORCE CC OF NOT EQUAL.                       
         B     EXIT                                                             
*                                                                               
PASSIT   CR    RB,RB               FORCE CC OF EQUAL.                           
         B     EXIT                                                             
*                                                                               
FILBNUL  OC    0(0,RF),0(RF)                                                    
FILCNUL  CLC   0(0,RF),SPACES                                                   
FILFCLC  CLC   0(0,RF),0(RE)                                                    
*                                                                               
*              NON-STANDARD FILTERING CODE HERE.                                
*                                                                               
*&&US                                                                           
FILINV   LA    RF,IOAREA                                                        
         USING TRANSD,RF                                                        
         LA    RF,TRNSREF                                                       
         MVC   PAYTYPE,TRNSTYPE                                                 
*                                                                               
         CLI   ULSAVE+1,C'T'       FOR LEDGERS P, Q, S & T, USE INV NO          
         BH    FILINV2             IN X'46' ELEMENT.                            
         CLI   ULSAVE+1,C'P'                                                    
         BL    FILINV2                                                          
         LA    RF,SAVE46                                                        
         USING TRPAYD,RF                                                        
         LA    RF,TRPYINV                                                       
         B     FILINV4                                                          
         DROP  RF                                                               
*                                                                               
FILINV2  LA    RF,IOAREA                                                        
         USING TRANSD,RF                                                        
         LA    RF,TRNSREF                                                       
*                                                                               
FILINV4  ZIC   RE,INVLEN                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     EXIT                                                             
         CLC   0(0,RF),INVNO                                                    
         DROP  RF                                                               
*                                                                               
*                                                                               
*&&                                                                             
FILSDAT  LA    RF,IOAREA                                                        
         USING TRANSD,RF                                                        
         CLC   TRNSDATE,SDATEP     TEST TRANS DATE VS. START DATE .             
         BNL   PASSIT                                                           
         B     EXIT                                                             
*                                                                               
FILEDAT  LA    RF,IOAREA                                                        
         CLC   TRNSDATE,EDATEP     TEST TRANS DATE VS. END DATE.                
         BNH   PASSIT                                                           
         B     EXIT                                                             
*                                                                               
FILAMNT  LA    RF,IOAREA                                                        
         CP    FILTAMNT,TRNSAMNT                                                
         BE    PASSIT              AMOUNT = FILTER.                             
         MP    FILTAMNT,=P'-1'                                                  
         CP    FILTAMNT,TRNSAMNT                                                
         BE    PASSIT              EQUAL TO INVERSE (OF INVERSE).               
         B     FAILIT              NOT EQUAL TO ANYTHING.                       
*                                                                               
FILCHEK  LA    RF,IOAREA                                                        
         CLC   TRNSREF,FRSTCHEK                                                 
         BL    FAILIT              BELOW THRESHHOLD.                            
         B     PASSIT                                                           
*                                                                               
FILMRK   LA    RF,IOAREA                             NZ.                        
         CLI   CRDOLA,DISCOUNT                                                  
         BE    FMK020                                                           
         LA    RE,X'70'            SET FOR EXECUTED BNZ.                        
         CLI   MARKED,C'Y'         MARKED ONLY NEEDS BNZ.                       
         BE    *+8                                                              
         LA    RE,16(RE)           CHANGE TO BZ.                                
         ZIC   R1,CRDBIT           GET BIT VALUE TO TEST FOR.                   
         EX    R1,BITTM            TM    TRNSSTAT,0                             
         EX    RE,BRANCH           BC    0,PASSIT                               
         B     FAILIT                                                           
BITTM    TM    TRNSSTAT,0                                                       
BRANCH   BC    0,PASSIT                                                         
         USING TRCASHD,RF                                                       
FMK020   LA    RF,SAVE50           ADDRESS THE CASH ELEMENT.                    
         LA    RE,X'80'            SET UP FOR EXECUTED BE                       
         CLI   MARKED,C'Y'         MARKED ONLY NEEDS BE.                        
         BE    *+8                                                              
         SH    RE,=H'16'           UNMARKED ONLY NEEDS BNE.                     
         CLI   TRCSTYPE,C'D'                                                    
         EX    RE,BRANCH                                                        
         B     FAILIT                                                           
         USING TRANSD,RF                                                        
*                                                                               
*                                                                               
*&&US                                                                           
FILCLI   LA    RE,KEY                                                           
         USING ACKEYD,RE                                                        
         LA    RF,CLILEGS                                                       
FILC020  CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                LOST A LEDGER SOMEWHERE.                     
         CLC   0(1,RF),ACKEYACC+2                                               
         BE    FILC040                                                          
         LA    RF,2(RF)                                                         
         B     FILC020             TRY AGAIN.                                   
*                                                                               
FILC040  ZIC   R1,1(RF)            GET DISP INTO CONTRA FOR CLIENT.             
         LA    RF,ACKEYCON(R1)     RF = A(CLIENT CODE)                          
         CLC   CLIENT,0(RF)                                                     
         B     EXIT                EXIT WITH APT CC.                            
*                                                                               
FILSUP   DS    0H                                                               
         LA    RE,KEY                                                           
         CLI   PAYTYPE,X'22'       SPOT REP                                     
         BE    FILS020                                                          
         CLI   PAYTYPE,X'32'       PRINT REP                                    
         BE    FILS040                                                          
FILS020  CLC   STATN,SPACES                                                     
         BE    EXIT                                                             
         CLC   STATN,ACKEYCON+4                                                 
         BE    EXIT                                                             
         B     FAILIT                                                           
         SPACE 1                                                                
FILS040  CLC   PUBLCN,SPACES                                                    
         BE    EXIT                                                             
         CLI   PUBLEN,X'FF'        STRAIGHT PUB,OR DOES IT HAVE                 
         BE    FILS060             EDITION & ZONE                               
         SR    R6,R6                                                            
         IC    R6,PUBLEN                                                        
         BCTR  R6,R0                                                            
         EX    R6,*+8                                                           
         B     *+10                                                             
         CLC   PUBLCN(0),ACKEYCON+1                                             
         B     EXIT                                                             
         SPACE 1                                                                
FILS060  CLC   PUBLCN(11),ACKEYCON+1    THIS ONE HAS ZONE ETC                   
         B     EXIT                                                             
*&&                                                                             
*                                                                               
FILMOS   LA    RF,IOAREA                                                        
         CLC   MOS,TRNSBTCH                                                     
         B     EXIT                                                             
*                                                                               
FILOFF   LA    RF,IOAREA                                                        
         CLC   OFFICE,TRNSANAL                                                  
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
*                   COMMUNICATION WITH DATA MANAGER(DIRECTORY)                  
IORTNS   NTR1  BASE=ABASE                                                       
         L     R9,ABASE2                                                        
         L     RF,0(R1)            GET TYPE OF IO.                              
         BR    RF                                                               
         SPACE 2                                                                
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRECTRY                                                         
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         B     DIRECTRY                                                         
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRECTRY                                                         
ADD      MVC   COMMAND,=C'DMADD'                                                
         B     DIRECTRY                                                         
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRECTRY                                                         
         SPACE 2                                                                
DIRECTRY GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'ACCOUNT',KEY,         X        
               KEY,(TERMINAL,0)                                                 
         CLC   COMMAND,=C'DMWRT '                                               
         BE    *+14                                                             
         CLC   COMMAND,=C'DMREAD'                                               
         BNE   DMCHECK                                                          
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                                                             
         SPACE 2                                                                
DMCHECK  MVC   HALF(1),DMCB+8                                                   
         NC    HALF(1),DMOUTBTS                                                 
         BZ    EXIT                                                             
         MVI   ERRNUM,0                                                         
         B     ERROR1                                                           
*                                                                               
ERROR    L     RB,ABASE                                                         
         L     R9,ABASE2                                                        
*                                                                               
         SPACE 2                                                                
ERROR1   L     R2,ATWA0                 MESSAGE ALWAYS IN LINE 1                
         LA    R2,64(R2)                                                        
         L     RF,FADR                                                          
         OI    6(RF),X'40'         POSITION CURSOR.                             
         GOTO1 GETMSG,DMCB+12,(ERRNUM,8(R2)),(FNDX,DMCB),              X        
               (TERMINAL,DATAMGR)                                               
         FOUT  (R2)                                                             
         BAS   RE,TWARYT                                                        
         L     RD,SAVED                                                         
*                                                                               
EXIT     XMOD1 1                                                                
*                                                                               
*                                                                               
*                                                                               
ANY      CLI   5(RF),0                                                          
         BNER  RE                                                               
         MVI   ERRNUM,MISSING                                                   
         B     ERROR                                                            
*                                                                               
*                                                                               
ELEMDIG  NTR1                                                                   
         LA    RF,IOAREA                                                        
         MVC   SAVE23,SPACES                                                    
         MVC   SAVE46,SPACES                                                    
         MVC   SAVE50,SPACES                                                    
ED2      CLI   0(RF),0                                                          
         BE    EXIT                                                             
         CLI   0(RF),X'23'                                                      
         BE    ED6                                                              
         CLI   0(RF),X'46'                                                      
         BE    ED8                                                              
         CLI   0(RF),X'50'                                                      
         BE    ED10                                                             
ED4      ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     ED2                                                              
*                                                                               
ED6      MVC   SAVE23,0(RF)                                                     
         B     ED4                                                              
ED8      MVC   SAVE46,0(RF)                                                     
         B     ED4                                                              
ED10     MVC   SAVE50,0(RF)                                                     
         B     ED4                                                              
*                                                                               
TWARYT   NTR1                                                                   
         ZICM  R2,2(RA),2                                                       
         MVC   CRDSTMP2,OLDKEY     UPDATE TEMPSTOR STAMP.                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT ',=C'TEMPSTR',(1,(R2)),ATIA                
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
*                                                                               
HELP     TWAXC CRDMRKH,CRDTHEDH-1,PROT=Y                                        
         MVI   ERRNUM,0                                                         
         BAS   RE,NEWKEY                                                        
         BAS   RE,TOTOUT                                                        
         LA    RF,CRDMRKH                                                       
         USING CRDLYND,RF                                                       
         LA    R1,28               MAX OF 28 ITEMS.                             
         LA    RE,CRDFILH                                                       
         C     RE,FADR                                                          
         BE    HELP020             CAME FROM FILTERS.                           
         LA    RE,CRDOPTH                                                       
         C     RE,FADR                                                          
         BE    HELP040             CAME FROM OPTIONS.                           
         DC    H'0'                                                             
*                                                                               
         USING FILTABD,RE                                                       
HELP020  LA    RE,FILTAB                                                        
*                                                                               
HELP024  CLI   FILTNAM,X'FF'                                                    
         BE    CRD980              NO MORE FILTERS.,                            
         CLC   FILTNAM(6),=C'UNAUTH'                                            
         BNE   HELP025                                                          
         CLC   ULSAVE,=C'SV'                                                    
         BNE   HELP026             PRODUCTION ONLY                              
         CLI   SYSTPRF2,C'Y'                                                    
         BNE   HELP026                                                          
HELP025  MVC   CRLNCON(8),FILTNAM  TYPE.                                        
         MVI   CRLNCON+8,C'='                                                   
         MVC   CRLNCON+9(18),FILHELP                                            
         OI    CRLNDISH+6,X'80'                                                 
         LA    RF,CRLNLNQ(RF)                                                   
HELP026  LA    RE,FILTBLNQ(RE)                                                  
         BCT   R1,HELP024                                                       
         DC    H'0'                                                             
*                                                                               
         USING OPTABD,RE                                                        
HELP040  LA    RE,OPTAB                                                         
*                                                                               
HELP044  CLI   OPTNAM,X'FF'                                                     
         BE    CRD980              NO MORE OPTIONS.                             
         MVC   CRLNCON(8),OPTNAM                                                
         MVI   CRLNCON+8,C'='                                                   
         MVC   CRLNCON+9(6),=C'Y OR N'                                          
         OI    CRLNDISH+6,X'80'                                                 
         LA    RF,CRLNLNQ(RF)                                                   
         LA    RE,OPTABLNQ(RE)                                                  
         BCT   R1,HELP044                                                       
         DC    H'0'                                                             
         DROP  RE,RF                                                            
         DC    H'0'                                                             
*                                                                               
TOTOUT   NTR1                                                                   
         CLI   CRDOLA,DISCOUNT     DISCOUNT (04) DOESN'T DISPLAY TOTALS         
         BE    EXIT                                                             
         LA    RE,ACCTOT                                                        
         LA    RF,CRDTOT+14                                                     
         LA    R3,3                                                             
CRD940   EDIT  (P6,(RE)),(15,(RF)),2,MINUS=YES                                  
         LA    RE,L'ACCTOT(RE)                                                  
         LA    RF,17(RF)                                                        
         BCT   R3,CRD940                                                        
         MVC   CRDTOT+2(10),=C' DISPLAYS '                                      
         CLI   ACCEND,C'Y'                                                      
         BNE   *+10                                                             
         MVC   CRDTOT+2(10),=C'  ACCOUNT '                                      
         OI    CRDTOTH+6,X'80'                                                  
         B     EXIT                                                             
         SPACE 1                                                                
RELO     DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              TABLE OF FILTERS                                                 
*                                                                               
FILTAB   DS    0F                                                               
*&&UK                                                                           
INVFIL   DC    CL8'INV',AL2(INVNO-WKSTRT),AL2(INVLEN-WKSTRT),AL3(0)             
         DC    AL1(L'INVNO-1)                                                   
         DC    AL2(ACKEYREF-ACKEYD),2AL1(255),AL1(255),AL1(1),AL3(0)            
         DC    CL18'6 CHAR INV OR REF'                                          
STAFIL   DC    CL8'STA',AL2(STANM-WKSTRT),AL2(0),AL3(0),AL1(L'STANM-1)          
         DC    AL2(ACKEYCON+3-ACKEYD),CL2'F',XL1'52',AL1(1),AL3(0)              
         DC    CL18'STA SHORT NAME'                                             
PUBFIL   DC    CL8'PUB',AL2(PUBLCN-WKSTRT),AL2(PUBLEN-WKSTRT),AL3(0)            
         DC    AL1(5)                                                           
         DC    AL2(ACKEYCON+3-ACKEYD),CL2'F',XL1'62',AL1(1),AL3(0)              
         DC    CL18'PUBLICATION NUMBER'                                         
*&&                                                                             
*&&US                                                                           
INVFIL   DC    CL8'INV',AL2(INVNO-WKSTRT),AL2(INVLEN-WKSTRT),AL3(0)             
         DC    AL1(L'INVNO-1)                                                   
         DC    AL2(0),2AL1(255),AL1(255),AL1(1),AL3(FILINV)                     
         DC    CL18'INV OR REF NO'                                              
STAFIL   DC    CL8'STA',AL2(STATN-WKSTRT),AL2(0),AL3(0),AL1(L'STATN-1)          
         DC    AL2(0),3AL1(255),AL1(1),AL3(FILSUP)                              
         DC    CL18'STATION'                                                    
PUBFIL   DC    CL8'PUB',AL2(PUBLCN-WKSTRT),AL2(PUBLEN-WKSTRT),AL3(0)            
         DC    AL1(L'PUBLCN-1)                                                  
         DC    AL2(0),3AL1(255),AL1(1),AL3(FILSUP)                              
         DC    CL18'PUBLICATION NUM'                                            
ZONEFIL  DC    CL8'ZONE',AL2(ZONE-WKSTRT),AL2(ZLEN-WKSTRT),AL3(0)               
         DC    AL1(L'ZONE-1)                                                    
         DC    AL2(0),CL2'PQ',XL1'32',AL1(1),AL3(0)                             
         DC    CL18'ZONE'                                                       
EDFIL    DC    CL8'EDITION',AL2(EDITION-WKSTRT),AL2(EDLEN-WKSTRT)               
         DC    AL3(0),AL1(L'EDITION-1)                                          
         DC    AL2(0),CL2'PQ',XL1'32',AL1(1),AL3(0)                             
         DC    CL18'EDITION'                                                    
CLIFIL   DC    CL8'CLIENT',AL2(CLIENT-WKSTRT),AL2(0),AL3(CRD580)                
         DC    AL1(L'CLIENT-1)                                                  
         DC    AL2(0),2AL1(255),AL1(255),AL1(1),AL3(FILCLI)                     
         DC    CL18'CLIENT CODE'                                                
*&&                                                                             
SDATFIL  DC    CL8'SDATE',AL2(SDATEP-WKSTRT),AL2(0),AL3(CRD480)                 
         DC    AL1(L'SDATEP-1)                                                  
         DC    AL2(TRNSDATE-TRANSD+ACRECORD-ACKEYD),2AL1(255)                   
         DC    AL1(255),AL1(0),AL3(FILSDAT)                                     
         DC    CL18'START DATE'                                                 
EDATFIL  DC    CL8'EDATE',AL2(EDATEP-WKSTRT),AL2(0),AL3(CRD480)                 
         DC    AL1(L'EDATEP-1)                                                  
         DC    AL2(TRNSDATE-TRANSD+ACRECORD-ACKEYD),2AL1(255)                   
         DC    AL1(255),AL1(0),AL3(FILEDAT)                                     
         DC    CL18'END DATE'                                                   
DATFIL   DC    CL8'DATE',AL2(0),AL2(0),AL3(CRD480),AL1(0)                       
         DC    AL2(0),2AL1(255),AL1(255),AL1(0),AL3(0)                          
         DC    CL18'DATE'                                                       
MOSFIL   DC    CL8'MOS',AL2(MOS-WKSTRT),AL2(0),AL3(CRD500),AL1(L'MOS-1)         
         DC    AL2(0),2AL1(255),AL1(255),AL1(1),AL3(FILMOS)                     
         DC    CL18'MONTH OF SERVICE'                                           
MRKFIL   DC    CL8'MARKED',AL2(MARKED-WKSTRT),AL2(0),AL3(CRD540),AL1(0)         
         DC    AL2(0),2AL1(255),AL1(255),AL1(1),AL3(FILMRK)                     
         DC    CL18'Y, N OR A(LL)'                                              
*&&UK                                                                           
CHEKFIL  DC    CL8'SCHEQUE'                                                     
*&&                                                                             
*&&US                                                                           
CHEKFIL  DC    CL8'SCHECK'                                                      
*&&                                                                             
         DC    AL2(FRSTCHEK-WKSTRT),AL2(0),AL3(0),AL1(5)                        
         DC    AL2(ACKEYREF-ACKEYD),2AL1(255),AL1(255),AL1(1)                   
         DC    AL3(FILCHEK)                                                     
*&&US*&& DC    CL18'STARTING CHECK NUM'                                         
*&&UK*&& DC    CL18'STARTING CHEQUE NO'                                         
AMNTFIL  DC    CL8'AMOUNT',AL2(FILTAMNT-WKSTRT),AL2(0),AL3(CRD560)              
         DC    AL1(L'FILTAMNT-1)                                                
         DC    AL2(0),2AL1(255),AL1(255),AL1(1),AL3(FILAMNT)                    
         DC    CL18'AMOUNT FOR ITEMS'                                           
OFFFIL   DC    CL8'OFFICE',AL2(OFFICE-WKSTRT),AL2(0),AL3(0)                     
         DC    AL1(L'OFFICE-1)                                                  
         DC    AL2(0),2AL1(255),AL1(255),AL1(1),AL3(FILOFF)                     
         DC    CL18'OFFICE'                                                     
*                                                                               
         DC    CL8'TYPE',AL2(TYPEFILT-WKSTRT),AL2(0),AL3(CRDTYPF)               
         DC    AL1(L'TYPEFILT-1)                                                
         DC    AL2(0),3AL1(255),AL1(1),AL3(0)                                   
         DC    CL18'PAYMENTS/RECEIPTS'                                          
*                                                                               
AUTHFIL  DC    CL8'UNAUTH',AL2(UNAUTH-WKSTRT),AL2(0),AL3(CRDAUTHF)              
         DC    AL1(L'UNAUTH-1),AL2(0),2AL1(255),AL1(255),AL1(1),AL3(0)          
         DC    CL18'Y=INCLUDE UNAUTHED'                                         
*                                                                               
         DC    AL1(255)            TABLE END.                                   
CRDXXX   DS    0H                                                               
*                                                                               
*              TABLE OF OPTIONS                                                 
*                                                                               
OPTAB    DS    0F                                                               
         DC    CL8'ALL',AL3(CRD260)                                             
         DC    AL1(255)                                                         
*                                                                               
*                                                                               
*              TABLE OF VALID LEDGERS.                                          
*                                                                               
LEDGTAB  DS    0CL1                                                             
         DC    C'V'                                                             
         DC    C'X'                                                             
         DC    C'T'                                                             
*&&US                                                                           
         DC    C'S'                                                             
         DC    C'P'                                                             
         DC    C'E'                                                             
         DC    C'Q'                                                             
         DC    C'U'                                                             
         DC    C'W'                                                             
         DC    C'Y'                                                             
*&&                                                                             
*&&UK                                                                           
         DC    C'F'                                                             
*&&                                                                             
         DC    AL1(255)                                                         
*                                                                               
VTYPES   DS    0F                                                               
         DC    A(HIGH)                                                          
         DC    A(READ)                                                          
         DC    A(WRITE)                                                         
         DC    A(ADD)                                                           
         DC    A(SEQ)                                                           
         DC    A(ERROR)                                                         
         DC    A(IORTNS)                                                        
         DC    A(FILTERIT)                                                      
         DC    AL1(255)                                                         
*                                                                               
TYPTAB   DS    0CL6                                                             
         DC    CL8'APPROVE',AL1(APPROVE,APPMRK),X'02'                           
         DC    CL8'RECON',AL1(RECON,RECMRK),X'02'                               
*&&US*&& DC    CL8'REVERSE',AL1(REVERSE,REVMRK),X'20'                           
*&&UK*&& DC    CL8'MATCH',AL1(REVERSE,REVMRK),X'20'                             
*&&UK*&& DC    CL8'DISCOUNT',AL1(DISCOUNT,DISMRK),X'00'                         
         DC    AL1(255)                                                         
*&&US                                                                           
CLILEGS  DC    C'P',AL1(12)                                                     
         DC    C'Q',AL1(12)                                                     
         DC    C'S',AL1(12)                                                     
         DC    C'T',AL1(12)                                                     
         DC    C'U',AL1(12)                                                     
         DC    C'V',AL1(3)                                                      
         DC    C'W',AL1(3)                                                      
         DC    AL1(255)                                                         
*&&                                                                             
         EJECT                                                                  
T60DFFD  DSECT                                                                  
         DS    CL16                                                             
MYCOMPEL DS    CL40                                                             
         DS    CL8                                                              
*                                                                               
       ++INCLUDE ACCRDFFD                                                       
OLDKEY   DS    CL42                                                             
COMPSAVE DS    CL1                                                              
ULSAVE   DS    CL2                                                              
         EJECT                                                                  
*        FATWA                                                                  
*        ACGENBOTH                                                              
*        DDFLDIND                                                               
*        DDCOMFACS                                                              
*        DDACCFACS                                                              
*        FAFACTS                                                                
         PRINT OFF                                                              
       ++INCLUDE FATWA                                                          
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*                                                                               
*                                                                               
       ++INCLUDE ACCRDSECT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACCRD00   05/01/02'                                      
         END                                                                    
