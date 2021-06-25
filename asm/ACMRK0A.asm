*          DATA SET ACMRK0A    AT LEVEL 060 AS OF 03/28/14                      
*PHASE T6160AA                                                                  
ACMRK0A  TITLE 'GENERAL - REVERSE'                                              
* JFOX 048 USE SORAWRK IF AVAILABLE INSTEAD OF CPJWRK                           
* JFOX 049 SR - DON'T OPTIMISE KEY REFERENCE BUILDING.  YEAR 2000 FIX           
* JFOX 050 U.K./U.S. COMPLIANCE                                                 
ACMRK0A  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MRKA**,RA                                                    
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
                                                                                
         LA    R1,SAVED                                                         
         AH    R1,=Y(REVTAB-SAVED)                                              
         ST    R1,AREVTAB                                                       
                                                                                
         LH    R1,=Y(INPHEDH-TWAD) SET INPUT SCREEN DISPS FOR ROOT              
         STH   R1,DISPHED                                                       
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPHD2H-TWAD) SET INPUT SCREEN DISPS FOR ROOT              
         STH   R1,DISPHED2                                                      
         AR    R1,R6                                                            
         ST    R1,ADISHEA2         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPDETH-TWAD)                                              
         STH   R1,DISPDET                                                       
         AR    R1,R6                                                            
         ST    R1,ADISDET1         A(1ST DETAIL LINE)                           
         LH    R1,=Y(INPTOTH-TWAD)                                              
         STH   R1,DISPTOT                                                       
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LH    R1,=Y(INPPFKH-TWAD)                                              
         STH   R1,DISPPFK                                                       
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PFKEY LINE)                                
                                                                                
                                                                                
         CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              ROUTINE TO PRE-VALIDATE HEADER               
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              ROUTINE TO GET NEXT ACCOUNT                  
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTREVS                                                  
         BE    *+6                 REVERSE                                      
         DC    H'0'                                                             
         CLI   TWASCROV,GRSCR1                                                  
         BE    VALLDG              REVERSE - VALIDATE HEADER                    
         CLI   TWASCROV,GRSCR2                                                  
         BE    VALINP              REVERSE - VALIDATE SELECT/INPUT              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER SCREEN FIELD(S)                                 *         
***********************************************************************         
                                                                                
PREVAL   DS    0H                                                               
*&&UK                                                                           
         CLC   RVSLDG,RECVUL       TEST RECEIVABLES                             
         BNE   PREVAL02                                                         
         XC    RVSMOA,RVSMOA       CLEAR ANY MOA RANGE                          
         OI    RVSMOAH+(FVOIND-FVIHDR),FVOXMT                                   
         B     PREVAL06                                                         
                                                                                
PREVAL02 CLI   RVSMOAH+(FVILEN-FVIHDR),0                                        
         BNE   PREVAL06            LEAVE MOA RANGE INTACT, IF PRESENT           
         MVC   TEMP(L'TODAYB),TODAYB  TAKE TODAY IN BINARY                      
         SR    R1,R1                                                            
         IC    R1,TEMP+1                                                        
         SH    R1,=H'3'            GO BACK THREE MONTHS                         
         BNP   *+12                CROSSED YEAR BOUNDARY                        
         STC   R1,TEMP+1           SET START MONTH                              
         B     PREVAL04            DISPLAY DATE                                 
         AH    R1,=H'12'           SET RELEVANT MONTH IN PREVIOUS YEAR          
         STC   R1,TEMP+1                                                        
         ICM   R1,1,TEMP                                                        
         BNZ   *+12                TEST CENTURY BOUNDARY                        
         LA    R1,99               SET LAST YEAR OF PREVIOUS CENTURY            
         B     *+6                                                              
         BCTR  R1,0                GO BACK A YEAR                               
         STC   R1,TEMP                                                          
PREVAL04 LA    R2,RVSMOA                                                        
         GOTO1 VDATCON,DMCB,(3,TEMP),(9,(R2))                                   
         LA    R2,7(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C'-'                                                       
         GOTO1 (RF),(R1),(3,TODAYB),(9,2(R2))                                   
*&&                                                                             
PREVAL06 DS    0H                                                               
*&&US                                                                           
         CLI   LTYPE,TYPGEN        TEST LAST TYPE WAS GENERAL                   
         BNE   PREVALX                                                          
*&&                                                                             
         LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREVALX                                                          
         XC    RVSLDG,RVSLDG                                                    
         XC    RVSACC,RVSACC                                                    
         OI    RVSLDGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RVSACCH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   RVSLDG(L'ACTKUNT+L'ACTKLDG),ACTKUNT                              
         MVC   RVSACC(L'ACTKACT),ACTKACT                                        
         XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD(S)                        *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   RVSACCH+(FVILEN-FVIHDR),0  IF ACCOUNT FIELD CLEARED              
         BNE   *+16                                                             
         XC    TWASKEY,TWASKEY     RESET KEY SAVED IN TWA                       
         XC    ACCOUNT,ACCOUNT     AND ACCOUNT                                  
         OC    LEDGER,LEDGER       TEST LEDGER ALEADY SET                       
         BNZ   *+12                                                             
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
         BNE   NXTACCX             INVALID LEDGER                               
         OC    TWASKEY,TWASKEY     TEST KEY SAVED IN TWA                        
         BZ    *+14                                                             
         CLC   RVSLDG,TWASKEY      TEST UNIT/LEDGER MATCHES SAVED               
         BNE   *+14                                                             
         CLC   LEDGER,RVSLDG       TEST CHANGED                                 
         BE    NXTACC4                                                          
         XC    TWASKEY,TWASKEY     RESET KEY SAVED IN TWA                       
         XC    ACCOUNT,ACCOUNT     (RE)SET ACCOUNT TO ZERO                      
         CLC   LEDGER,RVSLDG       TEST NEW LEDGER NOT YET VALIDATED            
         BE    NXTACC4                                                          
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
NXTACC4  OC    ACCOUNT,ACCOUNT     TEST FIRST TIME FOR TWAM2NXA                 
         BNZ   NXTACC6                                                          
         LA    R2,ACCOUNT          YES - REBUILD ACCOUNT                        
         MVC   ACCOUNT,SPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'SUPPUL),LEDGER                                         
         OC    TWASKEY,TWASKEY     TEST ACCOUNT SAVED IN TWA                    
         BZ    NXTACC8                                                          
         MVC   ACTKUNT(L'ACTKCULA-1),TWASKEY  RESTORE ACCOUNT                   
NXTACC6  LA    R2,KEY                                                           
         MVC   ACTKEY,SPACES       BUILD KEY FOR IO ROUTINE                     
         MVC   ACTKCULA,ACCOUNT                                                 
         B     NXTACC12                                                         
                                                                                
NXTACC8  LA    R2,KEY                                                           
         MVC   ACTKCULA,ACCOUNT                                                 
NXTACC10 SR    RF,RF               BUMP KEY FOR NEXT ACCOUNT                    
         IC    RF,ACTKACT+L'ACTKACT-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,ACTKACT+L'ACTKACT-1                                           
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   ACTKCULA(ACTKACT-ACTRECD),ACCOUNT                                
         BNE   NXTACC16            END OF LEDGER                                
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         OC    RECABLEL,RECABLEL   TEST LOW LEVEL ACCOUNT                       
         BZ    NXTACC10            NO - TRY AGAIN                               
         MVC   ACCOUNT,ACTKCULA                                                 
         B     NXTACC14                                                         
                                                                                
NXTACC12 SR    RF,RF               BUMP KEY FOR NEXT ACCOUNT                    
         IC    RF,ACTKCULA+L'ACTKCULA-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,ACTKCULA+L'ACTKCULA-1                                         
         MVI   GETIND,GETIABLQ                                                  
         GOTO1 AGETACC,0                                                        
         BNE   NXTACC14            NOT FOUND/NOT VALID                          
         CLC   ACTKCULA(ACTKACT-ACTRECD),ACCOUNT                                
         BE    NXTACC18            UNIT/LEDGER IS STILL OK                      
         B     NXTACC16            UNIT/LEDGER HAS CHANGED - FINISH             
                                                                                
NXTACC14 CLC   ACTKCULA(ACTKACT-ACTRECD),ACCOUNT                                
         BNE   NXTACC16            PAST LEDGER                                  
         MVI   GETIND,GETIABLQ     RE-/READ ACCOUNT                             
         GOTO1 AGETACC,0                                                        
         BE    NXTACC18            VALID THIS TIME                              
         B     NXTACC12            STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC16 MVC   FVMSGNO,=AL2(EANOACCS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,RVSLDGH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC18 MVC   ACCOUNT,ACTKCULA    ACCOUNT NAME RETURNED BY GETACC              
         GOTO1 VACSRCHC,DMCB,RVSACCH,TWAD,LEDGER,                      X        
               (X'C0',ACCNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
                                                                                
         OI    RVSACCH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   FVMSGNO,=AL2(IAEPAP1N)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,RVSACCH                                                       
         ST    R1,FVADDR                                                        
                                                                                
NXTACCX  NI    TWAMODE2,255-TWAM2NXA                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE UNIT & LEDGER                                              *         
***********************************************************************         
                                                                                
VALLDG   SR    R0,R0               CLEAR R0                                     
         TM    TWAMODE2,TWAM2NXA   TEST CALLED BY NXTACC                        
         BNO   *+6                                                              
         LR    R0,RE               YES - SAVE A(RETURN) TO NXTACC               
         TM    RVSLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDG10                                                         
         XC    RVSLDGN,RVSLDGN                                                  
         OI    RVSLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   FVMINL,2            REQUIRED FIELD, MINIMUM LENGTH=2             
         GOTO1 AVALLDG,RVSLDGH                                                  
         BH    VALLDG08                                                         
                                                                                
         LA    R1,LDGXLST                                                       
         LA    RF,LDGXLSTN                                                      
         CLC   RVSLDG,0(R1)        TEST OTHER EXCLUDED LEDGER                   
         BE    VALLDG06                                                         
         LA    R1,LDGXLSTL(R1)                                                  
         BCT   RF,*-14                                                          
                                                                                
         MVC   RVSLDGN,RECNAME     LEDGER OK - NAME EXTRACTED BY GETLDG         
         L     R1,RECALDGT         SAVE OFFICE POSITION FOR LEDGER              
         MVC   SLDGOPOS,LEDGTOFF-LEDGTABD(R1)                                   
*&&UK                                                                           
         CLC   RVSLDG,RECVUL       TEST RECEIVABLES                             
         BNE   VALLDG10                                                         
         CLC   RVSMOA,SPACES       TEST MOA RANGE PRESENT                       
         BNH   VALLDG10                                                         
         XC    RVSMOA,RVSMOA       CLEAR ANY MOA RANGE                          
         OI    RVSMOAH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OVMSGNO,=AL2(AI$MOIGN)                                           
*&&                                                                             
         B     VALLDG10                                                         
                                                                                
VALLDG06 MVC   FVMSGNO,=AL2(EALDGINV)                                           
VALLDG08 LTR   RE,R0               ERROR - TEST CALLED BY NXTACC                
         BNZR  RE                  RETURN TO NXTACC (CC NEQ)                    
         B     EXIT                EXIT WITH ERROR MESSAGE SET                  
                                                                                
VALLDG10 LTR   RE,R0               OK - TEST CALLED BY NXTACC                   
         BZ    VALLDGX             VALIDATE REST OF HEADER SCREEN               
         CR    R0,R0               SET CC EQU                                   
         BR    RE                  RETURN TO NXTACC                             
                                                                                
VALLDGX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE ACCOUNT                                                    *         
***********************************************************************         
                                                                                
VALACC   TM    RVSACCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALACCX                                                          
         MVI   FVMINL,1            REQUIRED FIELD                               
*&&UK*&& XC    PRSTCURT,PRSTCURT   VALSUP WILL PRESET CURRENCY                  
         GOTO1 AVALSUP,RVSACCH     USE VALSUP TO VALIDATE ACCOUNT               
         BNE   EXIT                                                             
         GOTO1 VACSRCHC,DMCB,RVSACCH,TWAD,LEDGER,                      X        
               (X'C0',ACCNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
VALACCX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    RVSOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    RVSOFFN,RVSOFFN                                                  
         OI    RVSOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,RVSOFFH                                                  
         BL    VALOFFX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   RVSOFFN,RECNAME                                                  
         OI    RVSOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
VALOFFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    RVSCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    RVSCONN,RVSCONN                                                  
         OI    RVSCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         CLI   RVSCONH+(FVILEN-FVIHDR),L'DUMCON  TEST L'DUMMY CONTRA            
         BNE   VALCON2                           NO - VALIDATE                  
         CLC   RVSCON(L'DUMCON),DUMCON           COMPARE WITH DUMMY             
         BNE   VALCON2                           NO - VALIDATE                  
         MVC   CONTRA,SPACES                     YES - SET DUMMY CONTRA         
         MVC   CONTRA(L'ACTKCPY),COMPANY                                        
         MVC   CONTRA+L'ACTKCPY(L'DUMCON),DUMCON                                
         MVI   CONTRAXL,L'TRNKCULC-1             SET EXECUTE LENGTH             
         MVI   CONTIND,CONTILOQ                  SET LOW LEVEL A/C              
         OI    RVSCONH+(FVOIND-FVIHDR),FVOXMT    RE-TRANSMIT                    
         B     VALCONX                                                          
VALCON2  GOTO1 AVALCON,RVSCONH                                                  
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   RVSCONN,RECNAME                                                  
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SOURCE                                                     *         
***********************************************************************         
                                                                                
VALSRC   TM    RVSSRCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSRCX                                                          
         XC    RVSSRCN,RVSSRCN                                                  
         OI    RVSSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALSRC,RVSSRCH                                                  
         BL    VALSRCX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   RVSSRCN,RECNAME                                                  
         OI    RVSSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
VALSRCX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   TM    RVSREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,RVSREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE TRANSACTION DATE PERIOD                                    *         
***********************************************************************         
                                                                                
VALPER   TM    RVSPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,RVSPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE ACTIVITY DATE PERIOD                                       *         
***********************************************************************         
                                                                                
VALADA   TM    RVSADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,RVSADAH                                                  
         BL    VALADAX                                                          
         BH    EXIT                                                             
         CLC   RVSLDG,RECVUL       TEST RECEIVABLES LEDGER                      
         BNE   VALADAX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE MOS RANGE                                                  *         
***********************************************************************         
                                                                                
VALMOS   TM    RVSMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
VALMOS02 GOTO1 AVALMOS,RVSMOAH                                                  
         BL    VALMOSX             NO INPUT                                     
         BH    EXIT                ERROR                                        
         CLC   RVSLDG,RECVUL       TEST RECEIVABLES LEDGER                      
         BNE   VALMOSX                                                          
*&&US                                                                           
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
*&&                                                                             
         MVC   OVMSGNO,=AL2(AI$MOIGN)                                           
         XC    RVSMOA,RVSMOA       CLEAR THE INPUT                              
         B     VALMOS02            RECURSE AND REVALIDATE                       
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE INCLUDE REVERSED                                           *         
***********************************************************************         
                                                                                
VALMCH   TM    RVSRVSH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMCHX                                                          
         GOTO1 AVALICL,RVSRVSH                                                  
         BH    EXIT                                                             
VALMCHX  DS    0H                                                               
                                                                                
                                                                                
         B     READTRN             READ AND FILTER TRANSACTIONS                 
         EJECT                                                                  
***********************************************************************         
* READ AND FILTER TRANSACTIONS.  PUT QUALIFYING TRANSACTIONS TO TSAR  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
READTRN  OI    DISIND,DISIOFLO     PRESET OVERFLOW                              
                                                                                
         LA    R1,TOTALS           CLEAR TOTALS ACCUMULATORS                    
         LA    R0,TOTALSN                                                       
         ZAP   0(L'TOTALS,R1),PZERO                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
                                                                                
         LA    R2,KEY              BUILD START KEY                              
         GOTO1 SETKEY,SETALL                                                    
*&&US*&& GOTO1 AVALPUB,(R2)        VALIDATE SPECIAL PUBLICATION NUMBER          
         MVI   TSARLEN+1,TSARGRL                                                
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
         CLC   TRNKCULA,ACCOUNT                                                 
         BNE   READTRNX                                                         
*&&UK                                                                           
         NI    SAFCIND1,FF-SAFCIGBP ASSUME NO AGENCY CURRENCY                   
         XC    FORECURT,FORECURT   SET UP CURRENCY FIRST TIME THROUGH           
*&&                                                                             
         B     READ04                                                           
                                                                                
READ02   LA    R2,KEY                                                           
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
         CLC   TRNKCULA,ACCOUNT                                                 
         BNE   READTRNX                                                         
         CLC   TRNKEY(L'TRNKEY-L'TRNKSBR),KEYSAVE                               
         BE    *+8                                                              
READ04   BAS   RE,SETRAL                                                        
         TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   READ02                                                           
         CLI   ICLMARK,ICLMYES     INCLUDE REVERSED                             
         BE    READ06                                                           
         LA    RF,X'10'            BO IF EXCLUDING REVERSED                     
         CLI   ICLMARK,ICLMNO                                                   
         BE    *+8                                                              
         LA    RF,X'80'            BZ IF REVERSED ONLY                          
         TM    TRNKSTAT,TRNSREVS                                                
         EX    RF,*+4                                                           
         NOP   READ02                                                           
READ06   TM    TRNKSTA2,TRNSPEEL+TRNSUSED                                       
         BNZ   READ02                                                           
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READ08              NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READ08              YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    READ08                                                           
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   READTRNX                                                         
         CLC   TRNKOFF(0),OFFICE                                                
READ08   OC    CONTRA,CONTRA                                                    
         BZ    READ12                                                           
         IC    RF,CONTRAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKCULC(0),CONTRA                                               
         BE    READ12                                                           
         CLC   PRODUL,LEDGER       TEST PRODUCTION LEDGER                       
         BE    READ10              SET FOR NEXT WORKCODE (OFFICE)               
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READTRNX            YES - GONE TOO FAR                           
READ10   GOTO1 SETKEY,SETCON+NXTOFF                                             
         B     READ02                                                           
READ12   CLC   TRNKDATE,PERSTA                                                  
         BNL   READ14                                                           
         GOTO1 SETKEY,SETSDT                                                    
         B     READ02                                                           
READ14   CLC   TRNKDATE,PEREND                                                  
         BNH   READ18                                                           
         TM    CONTIND,CONTILOQ    TEST BONA FIDE LOW LEVEL CONTRA A/C          
         BNZ   READ16                                                           
         GOTO1 SETKEY,SETSDT+NXTCON                                             
         B     READ02                                                           
READ16   TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READTRNX            YES - GONE TOO FAR                           
         GOTO1 SETKEY,SETSDT+NXTOFF                                             
         B     READ02                                                           
READ18   OC    REFSTA,REFSTA                                                    
         BZ    READ20                                                           
         IC    RF,REFSTAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFSTA                                                
         BE    READ20                                                           
         BL    *+14                                                             
         OC    REFEND,REFEND                                                    
         BNZ   READ22                                                           
         GOTO1 SETKEY,SETREF                                                    
         B     READ02                                                           
READ20   OC    REFEND,REFEND                                                    
         BZ    READ24                                                           
READ22   IC    RF,REFENDXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFEND                                                
         BNH   READ24                                                           
         GOTO1 SETKEY,SETREF+NXTSDT                                             
         B     READ02                                                           
READ24   CLC   TRNKSMOS,MOSSTA                                                  
         BL    READ02                                                           
         CLC   TRNKSMOS,MOSEND                                                  
         BH    READ02                                                           
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                DATA RECORD READ ERROR                       
         DROP  R2                                                               
                                                                                
         GOTO1 ASETELAD,AIOBUFF    SET ELEMENT ADDRESSES                        
         BAS   RE,RFILTER          FILTER DATA RECORD                           
         BNE   READ02              TRANSACTION FAILS - GET NEXT                 
                                                                                
         MVI   TSARINDS,0          CLEAR INDICATOR                              
         NI    TSARIND2,255-(TSARALCR)                                          
         MVI   TSARCHA,C' '        SET CHANGE BYTE TO SPACE                     
         BAS   RE,TSTRAL           TEST RECEIVABLE ALLOCATION                   
         BE    READ26                                                           
         CLI   PROFXALR,C'Y'       TEST EXCLUDE ALLOCATED RECEIVABLES           
         BE    READ02                                                           
         MVI   TSARINDS,TSARDISQ   DISPLAY-ONLY                                 
         MVI   TSARCHA,C'*'                                                     
         OI    TSARIND2,TSARALCR                                                
                                                                                
READ26   L     R1,AIOSAVE          R1=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R1)      EXTRACT DATA RECORD DISK ADDRESS             
                                                                                
         USING TRNRECD,R1          R1=A(DATA RECORD KEY)                        
         L     R1,AIOBUFF          EXTRACT TRNKEY VALUES                        
         CLC   PRODUL,LEDGER       IF PRODUCTION LEDGER                         
         BNE   *+10                                                             
         MVC   TSAROFF,TRNKWORK    MOVE IN WORK CODE                            
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
         MVC   TSARMOS,TRNRSMOS                                                 
         TM    TRNRSTA2,TRNSUSED   TEST USED (CAN'T DEPEND ON TRNRSUSE          
         BZ    *+14                IF RECORD CAME FROM OLD FILE)                
         OI    TSARRSTA,TRNSUSED   SET RECORD IS USED                           
         MVC   TSARFUSE,TRNRSUSE   EMULATOR DOESN'T SET FOR OLD FILE            
         MVI   TSARRSTA,0          AMALGAM OF TRNRSTAT & TRNRSTA2               
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD IS ON ARCHIVE                     
         TM    TRNRSTAT,TRNSREVS   TEST REVERSED                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSREVS   SET RECORD IS REVERSED                       
                                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        EXTRACT TRNEL VALUES                         
         CLC   PRODUL,LEDGER       TEST PRODUCTION LEDGER                       
         BE    READ28              YES - TSAROFF ALREADY SET                    
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    READ28              NO - DON'T SET TSAROFF                       
         MVC   TSAROFF,TRNOFFC                                                  
         OC    TSAROFF,SPACES      ENSURE NO BINARY ZEROS                       
READ28   MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
*&&US                                                                           
         LA    RE,DISBTY           TEST FOR DISPLAY-ONLY BATCH TYPE             
         LA    RF,DISBTYN                                                       
         CLC   TSARBTY,0(RE)                                                    
         BE    *+16                                                             
         LA    RE,DISBTYL(RE)                                                   
         BCT   RF,*-14                                                          
         B     *+12                                                             
         OI    TSARINDS,TSARDISQ   DISPLAY-ONLY                                 
         MVI   TSARCHA,C'*'                                                     
*&&                                                                             
         MVC   TSARVAR,TRNTYPE     SET FOR KEY SEQUENCE                         
         MVC   TSARSTA,TRNSTAT                                                  
         ZAP   TSARAMNT,TRNAMNT                                                 
         TM    TRNSTAT,TRNSDR                                                   
         BNO   *+8                                                              
         OI    TSARINDS,TSARDRQ    SET DR TRANSACTION                           
         TM    TRNSTAT,TRNSREV                                                  
         BNO   READ30                                                           
         TM    TSARRSTA,TRNSREVS                                                
         BO    *+6                                                              
         DC    H'0'                MUST BE MARKED IN KEY TOO                    
         OI    TSARINDS,TSARINMQ+TSARMKQ  SET TRANSACTION IS REVERSED           
                                                                                
READ30   MVC   TSARFSAC,SRCWORK    EXTRACT ANY SOURCE ACCOUNT                   
*&&UK                                                                           
         USING SORELD,R2                                                        
         ICM   R2,15,ASOREL        USE SOREL WORKCODE, IF PRESENT               
         BZ    READ32                                                           
         CLI   SORSYS,SORSACC      TEST ACCOUNTING SOURCE A/C                   
         BNE   READ32                                                           
         CLI   SORLN,SORAL2Q       TEST LONG ACCOUNTING ELEMENT                 
         BL    READ32                                                           
         MVC   TSARFWRK,SORAWRK    YES TAKE WORKCODE                            
         B     READ34                                                           
                                                                                
         USING CPJELD,R2                                                        
READ32   ICM   R2,15,ACPJEL        TEST CPJEL PRESENT                           
         BZ    READ34                                                           
         CLI   CPJTYPE,CPJTJOB     TEST JOB TYPE                                
         BNE   READ34                                                           
         MVC   TSARFWRK,CPJWRK     TAKE WORKCODE                                
                                                                                
         USING OTHELD,R2                                                        
READ34   ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
                                                                                
         OC    ANOTELS,ANOTELS                                                  
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
                                                                                
         GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
*&&                                                                             
*&&US                                                                           
         USING SCIELD,R2                                                        
         ICM   R2,15,ASCICDSC                                                   
         BZ    *+10                                                             
         ZAP   TSARFDIS,SCIAMNT                                                 
*&&                                                                             
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         MVC   TSARADAT,TRSDATE                                                 
         MVC   TSARSSTA,TRSSTAT                                                 
         TM    TSARRSTA,TRNSREVS                                                
         BO    READ36                                                           
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BL    READ38              YES - REVERSED MOS CANNOT EXIST              
         OC    TRSRMOS,TRSRMOS     NO - TEST REVERSED MOS DOESN'T EXIST         
         BZ    READ38                                                           
         DC    H'0'                REVERSED MOS ON UNREVERSED TRANS.            
                                                                                
READ36   MVC   TSARFREV,TSARMOS    PRESET REVERSED MOS=TRANSACTION MOS          
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BL    READ38              YES - REVERSED MOS CANNOT EXIST              
         OC    TRSRMOS,TRSRMOS     NO - TEST REVERSED MOS EXISTS                
         BZ    READ38                                                           
         MVC   TSARFREV,TRSRMOS                                                 
                                                                                
READ38   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ40                                                           
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         LA    RF,TOTDRS           RF=A(DRS)                                    
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    RF,TOTCRS           RF=A(CRS)                                    
         AP    0(L'TOTALS,RF),TRNAMNT                                           
         MVI   ANYADD,1                                                         
*&&UK                                                                           
         USING AFCELD,R2                                                        
         ICM   R2,15,AAFCEL                                                     
         BZ    READ02                                                           
         AP    CURTOTS-TOTALS(L'TOTALS,RF),AFCAMNT                              
*&&                                                                             
         B     READ02              READ SEQUENTIAL                              
                                                                                
READ40   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                                                             
         B     DISPTRN                                                          
                                                                                
READTRNX NI    DISIND,255-DISIOFLO  RESET OVERFLOW                              
         CLI   ANYADD,1            TEST ANYTHING IN BUFFER                      
         BE    DISPTRN                                                          
         LA    R1,RVSACCH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         NI    TWAMODE,TWAMRSRV    SET TO RE-INITIALISE                         
         B     EXIT                                                             
         EJECT                                                                  
DISPTRN  GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),MRKOLAYH               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&UK                                                                           
         GOTO1 ABLDCURR            BUILD CURRENCY ENTRIES                       
         BNH   *+6                                                              
         DC    H'0'                BAD CURRENCY IN TABLE                        
         GOTO1 ASETFORE            SET UP SINGLE FOREIGN CURRENCY               
*&&                                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         GOTO1 AOVRSCR,GRSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
         MVC   TEMP,SPACES         DISPLAY ACCOUNT CODE & NAME                  
         MVC   TEMP(L'ACTKCULA-1),ACCOUNT+(ACTKUNT-ACTKEY)                      
         LA    R1,TEMP+L'ACTKCULA-1                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'ACCNAME,R1),ACCNAME  LEAVE SPACE BETWEEN CODE & NAME         
         CLI   TEMP+L'INPACC,C' '  TEST IF WE OVERRAN                           
         BE    DISP2                                                            
         LA    R1,TEMP+L'INPACC-1  ADDRESS LAST VALID POSITION                  
         CLI   0(R1),C' '          CHOP BACK TO LAST ENTIRE WORD                
         BE    DISP2                                                            
         MVI   0(R1),C' '                                                       
         BCT   R1,*-12                                                          
DISP2    MVC   INPACC,TEMP         MOVE OUT CODE & NAME                         
         OI    INPACCH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ADISPLAY                                                         
         OC    OVMSGNO,OVMSGNO     TEST INTERNAL INFORMATION MESSAGE            
         BZ    *+10                                                             
         MVC   FVMSGNO,OVMSGNO                                                  
         TM    DISIND,DISIOFLO                                                  
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNWRN)                                           
         LA    R1,INPAMTH                                                       
         ST    R1,FVADDR                                                        
         GOTO1 ABLDTOT,INPTOTH                                                  
         B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
         USING DISLINED,R2                                                      
VALINP   MVI   ANYMARK,0                                                        
         LA    R3,DISLIST                                                       
         L     R2,ADISDET1                                                      
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT                                                     
         BZ    VINP22                                                           
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VINP02                                                           
         NI    TWAMODE2,255-TWAM2SKP  RESET SKIP VALIDATION                     
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VINP22              SKIP VALIDATION AND CALL DISPLAY             
                                                                                
VINP02   CLC   DISLMARK,AC@PENDG   TEST PENDING INDICATED                       
         BNE   VINP06                                                           
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF       GET THE RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARHITQ   TEST IT WAS ACTUALLY PENDING                 
         BO    VINP04                                                           
         TM    DISLHDR2+(FVIIND-FVIHDR),FVIVAL                                  
         BO    VINP04              NO, BUT REVPROC CHANGED IT                   
         LA    R1,DISLHDR2                                                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALINPX             USER CAN'T SPECIFY 'PENDING'                 
VINP04   LA    RF,X'10'            BO IF PA=Y (ONLY PENDING UNMARKING)          
         CLC   OPTPAG,AC@YES       TEST MARKING A PAGE                          
         BE    *+8                                                              
         LA    RF,X'80'            BZ IF PA=N (ONLY PENDING MARKING)            
         CLC   OPTPAG,AC@NO        TEST UNMARKING A PAGE                        
         BNE   VINP18              PAGE OPTION NOT USED - JUST DISPLAY          
         TM    TSARINDS,TSARMKQ    TEST ALREADY MARKED                          
         EX    RF,*+4                                                           
         NOP   VINP18              NOT CHANGING - JUST DISPLAY                  
         BO    VINP14              UNMARK                                       
         B     VINP08              MARK                                         
                                                                                
VINP06   GOTO1 AVALMRK,DISLHDR2    VALIDATE MRK FIELD                           
         BH    VALINPX             ERROR - EXIT WITH ERROR SET                  
         BL    VINP20              NO INPUT - SKIP TO NEXT SCREEN LINE          
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,TSARCHGQ       TEST ONLY WANT TO DISPLAY                    
         BE    VINP18                                                           
         CLI   BYTE,TSARMKQ        TEST IF USER IS MARKING                      
         BNE   VINP12                                                           
         TM    TSARINDS,TSARMKQ    IS RECORD CURRENTLY MARKED?                  
         BO    VINP20                                                           
VINP08   BAS   RE,REVPROC                                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EATRNMX2)  TOO MANY PENDING TRANSACTIONS            
         B     VALINPX                                                          
         LA    RF,TOTDRDIF         RF=A(TOTAL DR DIFFERENCE)                    
         TM    TSARSTA,TRNSDR      TEST DEBIT                                   
         BO    *+8                                                              
         LA    RF,TOTCRDIF                                                      
         AP    0(8,RF),TSARAMNT    INCREASE/DECREASE DIFFERENCE                 
*&&UK*&& AP    CURTOTS-TOTALS(8,RF),TSARAFCA                                    
         LA    RF,L'TOTDRDIF(RF)   RF=A(REPORT DR/CR MARK TOTALS)               
         TM    TSARINDS,TSARINMQ   TEST RECORD MARKED ON FILE                   
         BO    VINP10                                                           
         AP    0(8,RF),TSARAMNT    NO, ADD TO MARKED THIS SESSION               
*&&UK*&& AP    CURTOTS-TOTALS(8,RF),TSARAFCA                                    
         B     VINP18                                                           
VINP10   SP    8(8,RF),TSARAMNT    YES, SUBTRACT FROM UNMARKED                  
*&&UK*&& SP    CURTOTS-TOTALS+8(8,RF),TSARAFCA                                  
         B     VINP18                                                           
                                                                                
VINP12   TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    VINP20                                                           
VINP14   BAS   RE,REVPROC                                                       
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EATRNMX2)  TOO MANY PENDING TRANSACTIONS            
         B     VALINPX                                                          
         LA    RF,TOTDRDIF                                                      
         TM    TSARSTA,TRNSDR      TEST DEBIT                                   
         BO    *+8                                                              
         LA    RF,TOTCRDIF                                                      
         SP    0(8,RF),TSARAMNT    INCREASE/DECREASE DIFFERENCE                 
*&&UK*&& SP    CURTOTS-TOTALS(8,RF),TSARAFCA                                    
         LA    RF,L'TOTDRDIF(RF)   RF=A(REPORT DR/CR MARK TOTALS)               
         TM    TSARINDS,TSARINMQ   TEST RECORD MARKED ON FILE                   
         BO    VINP16                                                           
         SP    0(8,RF),TSARAMNT    YES SUBTRACT FROM MARKED                     
*&&UK*&& SP    CURTOTS-TOTALS(8,RF),TSARAFCA                                    
         B     VINP18                                                           
VINP16   AP    8(8,RF),TSARAMNT    NO, ADD TO UNMARKED                          
*&&UK*&& AP    CURTOTS-TOTALS+8(8,RF),TSARAFCA                                  
VINP18   GOTO1 ABLDLIN,DISLHDR1    REBUILD LHS, TRANSMIT, HIGHLIGHT             
                                                                                
VINP20   LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VINP02                                                        
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VINP22                                                           
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   YES - SET SKIP VALIDATION                    
         OI    DISIND,DISINCOL     PRETEND NEW COLUMN DISPLAY                   
         MVC   OVBYTE,DISIND       SAVE DISIND                                  
         NI    DISIND,255-DISIRST  CLEAR FORCED RE-START IF ON                  
         GOTO1 ADISPLAY                                                         
         MVC   DISIND,OVBYTE       RESTORE DISIND                               
         OI    DISIND,DISIFFLT     FORCE RE-FILTERING OF DISLIST                
                                                                                
VINP22   CLI   INPAMTH+(FVILEN-FVIHDR),0                                        
         BNE   VINP24                                                           
         OC    FLTAMT,FLTAMT       TEST FILTER AMOUNT PRESENT                   
         BZ    VINP30                                                           
         OI    DISIND,DISIRST      SET TO RESTART FROM BEGINNING                
         XC    FLTAMT,FLTAMT       CLEAR SAVED AMOUNT                           
         B     VINP30                                                           
VINP24   CLI   INPAMT,C'*'         TEST CARRY DOWN OPTION AMOUNT                
         BNE   VINP26                                                           
         OC    OPTAMT,OPTAMT       TEST OPTION AMOUNT PRESENT                   
         BZ    VINP26              NO - LET VALAMT GIVE ERROR                   
         ZAP   FLTAMT,OPTAMT       YES - TAKE THAT AMOUNT                       
         NI    FLTAMT+L'FLTAMT-1,X'FE'  FORCE POSITIVE                          
         OI    DISIND,DISIRST      SET TO RESTART FROM BEGINNING                
         B     VINP28                                                           
VINP26   GOTO1 AVALAMT,OVPARM,INPAMTH,OVAMT                                     
         BH    VALINPX             INVALID INPUT                                
         NI    OVAMT+L'OVAMT-1,X'FE'  FORCE POSITIVE                            
         OC    FLTAMT,FLTAMT       TEST FIRST TIME FOR FILTER                   
         BZ    *+14                                                             
         CP    FLTAMT,OVAMT        TEST FILTER AMOUNT CHANGED                   
         BE    VINP28                                                           
         ZAP   FLTAMT,OVAMT        REPLACE WITH NEW AMOUNT                      
         OI    DISIND,DISIRST      SET TO RESTART FROM BEGINNING                
VINP28   LA    RF,INPAMT           ECHO INPUT                                   
         CURED FLTAMT,(L'INPAMT,(RF)),2,ALIGN=LEFT,MINUS=YES                    
         OI    INPAMTH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
VINP30   CLI   ANYMARK,1           TEST ANYTHING MARKED                         
         BE    VINP32              YES - ALREADY CALLED DISPLAY                 
         GOTO1 ADISPLAY            ELSE CALL DISPLAY                            
                                                                                
VINP32   MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IANOTRAN)  'NO TRANSACTIONS DISPLAYED'              
         LA    R1,INPAMTH          SET CURSOR TO AMOUNT FIELD                   
         OC    DISLCNT,DISLCNT                                                  
         BZ    VINP34                                                           
         MVC   FVMSGNO,=AL2(IAMKTNOM)  'NO MORE TRANSACTIONS'                   
         LA    R1,INPMRKH          SET CURSOR TO FIRST MARK FIELD               
         TM    DISIND,DISIBOF+DISIEOF                                           
         BNZ   VINP34              BOF/EOF                                      
         MVC   FVMSGNO,=AL2(IAMKTEPA)  'ENTER TO PAGE'                          
VINP34   ST    R1,FVADDR                                                        
                                                                                
VALINPX  GOTO1 ABLDTOT,INPTOTH                                                  
         CLI   ANYMARK,1                                                        
         BNE   *+8                                                              
         OI    TWAMODE2,TWAM2CHG                                                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS.  PRINT REPORT.  OR BOTH.                       *         
***********************************************************************         
                                                                                
         USING TSARD,RF                                                         
         USING REPD,R3                                                          
UPDATE   LA    R1,MRKSCRH                                                       
         TM    TWAMODE2,TWAM2CHG   TEST CHANGES MADE BIT                        
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(EANOTHIN)  YOU AIN'T DONE NOTHIN' YET               
         B     UPDATEX                                                          
                                                                                
         CLI   XACTION,ACTDRFT     TEST DRAFT REPORT                            
         BE    UPD04                                                            
         LA    R1,INPMRKH                                                       
         L     RF,AREVTAB                                                       
         OC    0(REVNTRYL,RF),0(RF)  TEST TABLE EMPTY                           
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(EARVPEND)                                           
         B     UPDATEX             EXIT WITH ERROR SET                          
                                                                                
         LA    R0,REVNTRYN-1       JUST CHECK TABLE IS REALLY EMPTY             
UPD02    LA    RF,REVNTRYL(RF)                                                  
         OC    0(REVNTRYL,RF),0(RF)                                             
         BZ    *+6                                                              
         DC    H'0'                REVPROC WENT WRONG                           
         BCT   R0,UPD02                                                         
         CP    TOTDRDIF,PZERO      TEST ZERO DR DIFFERENCE                      
         BE    *+6                                                              
         DC    H'0'                REVPROC DIDN'T WORK                          
         CP    TOTCRDIF,PZERO      TEST ZERO CR DIFFERENCE                      
         BE    UPD04                                                            
         DC    H'0'                REVPROC DIDN'T WORK                          
                                                                                
UPD04    OC    PRTSUB,PRTSUB       TEST REPORT REQUIRED                         
         BZ    UPD06                                                            
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@RVRSD),LC@RVRSD                          
         LA    R1,REPH5+L'DISLLINE+1+L'LC@RVRSD-1                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
                                                                                
UPD06    LA    R1,INPMRKH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                READ ALL TSAR RECORDS, UPDATE FILE           
         STCM  R1,3,TEMP                                                        
                                                                                
UPD08    GOTO1 ATSARGET,TEMP                                                    
         BE    UPD10                                                            
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD30                                                            
         DC    H'0'                                                             
                                                                                
UPD10    TM    TSARINDS,TSARCHGQ   TEST IF RECORD MAY HAVE CHANGED              
         BZ    UPD28                                                            
         TM    TSARINDS,TSARMKQ    TEST IF USER IS REVERSING                    
         BZ    UPD12                                                            
         MVI   OVNEWSTA,TSARMKQ    INDICATE WE ARE REVERSING                    
         B     UPD14               OK TO MATCH                                  
                                                                                
UPD12    TM    TSARINDS,TSARINMQ   WAS RECORD ORIGINALLY REVERSED               
         BZ    UPD28               NO - NOTHING TO DO                           
         MVI   OVNEWSTA,0                                                       
                                                                                
UPD14    OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD18               MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'          BUILD PRINTLINE USING REPDISP                
         GOTO1 ABLDLIN                                                          
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         TM    TSARINDS,TSARHITQ   TEST PENDING REVERSE/UNREVERSE               
         BNO   UPD16                                                            
         LA    R1,REPP1+L'DISLLINE+1+L'LC4NO-1                                  
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   2(R1),C'('                                                       
         MVC   3(L'LC@PENDG,R1),LC@PENDG                                        
         LA    R1,3+L'LC@PENDG-1(R1)                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C')'                                                       
UPD16    GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD28               YES - GET NEXT TSAR RECORD                   
                                                                                
UPD18    MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOBUFF          R2=A(KEY)                                    
         TM    TRNRSTA2-TRNRECD(R2),TRNSUSED  TEST USED                         
         BZ    *+6                                                              
         DC    H'0'                CAN'T CONTINUE                               
         GOTO1 ASETELAD,AIOBUFF    SET A(ELEMENTS)                              
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL        TEST TRSEL PRESENT                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BE    *+6                 NO - OK TO CONTINUE                          
         DC    H'0'                REVERSING MAY NOT BE TO ZERO, IF WE          
*                                  EXIT WITH UPDATE INCOMPLETE                  
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        TEST TRNEL PRESENT                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    *+6                 NO - OK TO CONTINUE                          
         DC    H'0'                REVERSING MAY NOT BE TO ZERO, IF WE          
*                                  EXIT WITH UPDATE INCOMPLETE                  
         NI    TRNSTAT,255-TRNSREV RESET REVERSAL BIT                           
         CLI   OVNEWSTA,0          TEST UNREVERSING                             
         BE    UPD20                                                            
         OI    TRNSTAT,TRNSREV     SET REVERSAL BIT                             
*&&US*&& NI    TRNSTAT,255-TRNSAPPR-TRNSHOLD                                    
                                                                                
         USING TRSELD,R2                                                        
UPD20    ICM   R2,15,ATRSEL                                                     
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD22                                                            
         GOTO1 AEXTRSL             EXTEND IT                                    
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD22    MVC   TRSRMOS,TSARFREV    SET REVERSE MOS OR ZEROES                    
         MVI   TRSMARK,TRSMGRQ     SET MARKER TYPE/ACTION                       
         XC    TRSREVD,TRSREVD     CLEAR REVERSED DATE                          
         CLI   OVNEWSTA,0                                                       
         BNE   *+12                                                             
         OI    TRSMARK,TRSMUMQ     SET ACTION IS NEGATIVE                       
         B     *+10                                                             
         MVC   TRSREVD,TODAYC      SET REVERSED DATE                            
                                                                                
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
         NI    TRNRSTAT,255-TRNSREVS                                            
         CLI   OVNEWSTA,0               TEST UNREVERSING                        
         BE    *+8                                                              
         OI    TRNRSTAT,TRNSREVS                                                
         MVC   BYTE,TRNRSTAT            SAVE RECORD STATUS                      
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   *+12                                                             
         NI    TRNRSTAT,255-TRNSARCH    CLEAR ACCARC INDICATOR                  
         LA    R1,IOADFR+IOACCMST+IO1Q  PROMOTE ACCARC RECORD TO ACCMST         
         GOTO1 AIOEXEC                                                          
         BE    UPD24                                                            
         DC    H'0'                                                             
                                                                                
UPD24    MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA                         
         TM    BYTE,TRNSARCH            TEST RECORD PROMOTED TO ACCMST          
         BNO   UPD26                                                            
         L     R2,AIOSAVE               R2=A(SAVED DATA RECORD VALUES)          
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
UPD26    LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    UPD28                                                            
         DC    H'0'                                                             
                                                                                
UPD28    ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD08                                                            
                                                                                
UPD30    OC    PRTSUB,PRTSUB       TEST REPORT WAS GENERATED                    
         BZ    UPD32               NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         LA    R1,MRKSCRH                                                       
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX                                                          
                                                                                
UPD32    LA    R1,MRKACTH                                                       
         NI    TWAMODE2,255-TWAM2CHG                                            
         MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         OC    PRTSUB,PRTSUB       TEST REPORT WAS GENERATED                    
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
                                                                                
UPDATEX  ST    R1,FVADDR           STORE FIELD ADDRESS                          
         XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         B     EXIT                                                             
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
***********************************************************************         
* QUIT                                                                *         
***********************************************************************         
                                                                                
QUIT     XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    TWAMODE2,255-TWAM2CHG                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
                                                                                
***********************************************************************         
* ROUTINE TO PROCESS TABLE OF REVERSALS                               *         
***********************************************************************         
                                                                                
         USING REVTABD,R2                                                       
         USING TSARD,R3                                                         
REVPROC  NTR1  ,                                                                
         LA    R0,REVNTRYN         R0=NUMBER OF ENTRIES                         
         L     R2,AREVTAB                                                       
         L     R3,ATSARBLK                                                      
         TM    TSARINDS,TSARHITQ   TEST RECORD HIT BY TABLE                     
         BZ    REVP02                                                           
         CLC   REVTRNUM,TSRNUM     TEST SAME TRANSACTION                        
         BE    *+14                                                             
         LA    R2,REVNTRYL(R2)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                DIE IF CAN'T FIND A MARKED RECORD            
         XI    TSARINDS,TSARMKQ    SWAP MARKED STATUS                           
         B     REVP24              GO TO WRITE IT BACK                          
                                                                                
REVP02   OC    REVNTRY(REVNTRYL),REVNTRY  TEST EMPTY SPACE                      
         BZ    REVP32              YES FILL IT                                  
         CP    REVTAMNT,TSARAMNT   TEST AMOUNT MATCHES                          
         BNE   REVP30              NO - GET NEXT                                
*&&UK                                                                           
         CLC   REVTAFCC,TSARAFCC   TEST SAME CURRENCY                           
         BNE   REVP30                                                           
         CP    REVTAFCA,TSARAFCA   TEST CURRENCY AMOUNT MATCHES                 
         BNE   REVP30              NO - GET NEXT                                
*&&                                                                             
         CLC   PRODUL,LEDGER       TEST THIS IS PRODUCTION                      
         BE    REVP06              ALWAYS TEST WORKCODE                         
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    REVP08              DON'T TEST OFFICE                            
         LA    R1,OFFLDGS          TEST COMPULSORY OFFICE MATCH LEDGER          
         LA    RF,OFFLDGSN                                                      
         CLC   0(OFFLDGSL,R1),ACCOUNT+(ACTKUNT-ACTRECD) TEST LEDGER             
         BE    REVP06              MUST TEST THIS LEDGER                        
         LA    R1,OFFLDGSL(R1)                                                  
         BCT   RF,*-14                                                          
         TM    COMPSTA5,CPYSOFPL   TEST OFFICES FOR P&L ONLY                    
         BNZ   REVP08              YES - DON'T TEST OFFICE                      
REVP06   CLC   REVTOFF,TSAROFF     TEST OFFICE/WORKCODE MATCHES                 
         BNE   REVP30                                                           
REVP08   CLC   REVTCON,TSARCON     TEST CONTRA A/C MATCHES                      
         BNE   REVP30                                                           
         CLI   PROFBATY,C'Y'       TEST BATCH TYPE RESTRICTION IN USE           
         BNE   REVP12                                                           
         MVC   OVBYTE,TSARBTY      EXTRACT BATCH TYPE                           
         CLI   OVBYTE,40           TEST BATCH TYPE 40                           
         BNE   *+12                                                             
         MVI   OVBYTE,39           SET TO ALLOW 39/40 CROSS-REVERSAL            
         B     REVP10                                                           
         CLI   OVBYTE,44           TEST BATCH TYPE 44                           
         BNE   REVP10                                                           
         MVI   OVBYTE,43           SET TO ALLOW 43/44 CROSS-REVERSAL            
REVP10   CLC   REVTBTY,OVBYTE      TEST BATCH TYPE MATCHES                      
         BNE   REVP30                                                           
REVP12   MVC   OVBYTE,TSARSTA      EXTRACT AND MASSAGE STATUS BYTE              
         NI    OVBYTE,RVSSTATQ     PRESERVE VARIOUS STATUS IF BITSON            
         TM    COMPSTA4,CPYSIREG   TEST INVOICE REGISTER IN USE                 
         BZ    REVP16              NO - DON'T PRESERVE AUTH'D BIT               
         LA    R1,AUTLDGS          TEST LEDGER FOR AUTH STATUS                  
         LA    RF,AUTLDGSN                                                      
         CLC   0(AUTLDGSL,R1),ACCOUNT+(ACTKUNT-ACTRECD) TEST LEDGER             
         BE    REVP18              MUST PRESERVE AUTH'D BIT                     
         LA    R1,AUTLDGSL(R1)                                                  
         BCT   RF,*-14                                                          
REVP16   NI    OVBYTE,255-TRNSAUTH CLEAR AUTH'D BIT                             
REVP18   TM    TSARRSTA,TRNSUSED   TEST USED                                    
         BZ    *+8                                                              
         OI    OVBYTE,RVSUSED      SET A BITON                                  
         CLC   OVBYTE,REVTSTA      TEST STATUS MATCHES                          
         BNE   REVP30                                                           
                                                                                
         OC    TSARFREV,TSARFREV   TEST REVERSING MOS PRESENT                   
         BNZ   REVP20              YES - UNREVERSE                              
         OC    REVTFREV,REVTFREV   TEST REVERSING MOS PRESENT IN TABLE          
         BNZ   REVP30                                                           
         MVC   TSARFREV,REVTMOS    TABLE MOS IS TRANSACTION REVERSE MOS         
         MVC   REVTFREV,TSARMOS    AND VICE VERSA                               
         OI    TSARINDS,TSARMKQ                                                 
         B     REVP22              PUT BACK CURRENT RECORD                      
                                                                                
REVP20   CLC   TSARMOS,REVTFREV    TEST TRANS. MOS = TABLE REVERSE MOS          
         BNE   REVP30                                                           
         CLC   TSARFREV,REVTMOS    TEST TRANS. REVERSE MOS = TABLE MOS          
         BNE   REVP30                                                           
         XC    TSARFREV,TSARFREV   CLEAR REVERSING MOS IN TRANSACTION           
         XC    REVTFREV,REVTFREV   CLEAR REVERSING MOS IN TABLE                 
         NI    TSARINDS,255-TSARMKQ                                             
                                                                                
REVP22   OI    TSARINDS,TSARCHGQ   SET RECORD HAS BEEN CHANGED                  
         MVI   TSACTN,TSAPUT       PUT IT BACK TO TSAR                          
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   HALF,REVTRNUM       GET REVERSING TRANSACTION                    
         MVC   REVTRNUM,TSRNUM     SAVE CURRENT TSAR NUMBER IN TABLE            
         GOTO1 ATSARGET,HALF       RETRIEVE ORIGINAL                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TSARFREV,REVTFREV   SET/CLEAR REVERSING MOS                      
                                                                                
REVP24   NI    TSARINDS,255-TSARHITQ  CLEAR HIT BY TABLE                        
         MVI   TSARCHA,C' '        CLEAR PENDING                                
         MVI   TSACTN,TSAPUT       PUT BACK ORIGINAL/CURRENT                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TSRNUM,REVTRNUM     TEST NEED TO RESTORE CURRENT                 
         BE    REVP26                                                           
         MVC   HALF,REVTRNUM       RESTORE CURRENT TSAR RECORD                  
         GOTO1 ATSARGET,HALF                                                    
         BE    REVP26                                                           
         DC    H'0'                                                             
REVP26   SH    R0,=Y(1)            DROP ONE FOR SHUFFLE LOOP                    
         BZ    REVP28              ARE WE AT LAST ENTRY?                        
         MVC   REVNTRY(REVNTRYL),REVNTRYL(R2)  SHUFFLE UP ENTRY                 
         LA    R2,REVNTRYL(R2)     R2=A(NEXT ENTRY)                             
         BCT   R0,*-10             DO FOR REMAINING ENTRIES BAR LAST            
REVP28   XC    REVNTRY(REVNTRYL),REVNTRY  CLEAR LAST ENTRY                      
         B     REVP42                                                           
                                                                                
REVP30   LA    R2,REVNTRYL(R2)     R2=A(NEXT ENTRY)                             
         BCT   R0,REVP02           PROCESS NEXT TABLE ENTRY                     
         LTR   RB,RB               SET CC NEQ IF NO MATCH AND NO SPACE          
         B     REVPROCX                                                         
                                                                                
REVP32   MVC   REVTOFF,TSAROFF     EXTRACT OFFICE/WORKCODE                      
         MVC   REVTCON,TSARCON     CONTRA A/C                                   
         ZAP   REVTAMNT,TSARAMNT   AMOUNT                                       
         CP    REVTAMNT,PZERO      TEST ZERO                                    
         BE    *+8                 DON'T REVERSE SIGN                           
         XI    REVTAMNT+L'REVTAMNT-1,X'01'  REVERSE SIGN                        
*&&UK                                                                           
         MVC   REVTAFCC,TSARAFCC                                                
         ZAP   REVTAFCA,TSARAFCA   AMOUNT                                       
         CP    REVTAFCA,PZERO      TEST ZERO                                    
         BE    *+8                 DON'T REVERSE SIGN                           
         XI    REVTAFCA+L'REVTAFCA-1,X'01'  REVERSE SIGN                        
*&&                                                                             
         MVC   REVTMOS,TSARMOS     TRANSACTION MOS                              
         MVC   REVTFREV,TSARFREV   REVERSING MOS                                
         CLI   PROFBATY,C'Y'       TEST BATCH TYPE RESTRICTION IN USE           
         BNE   REVP34                                                           
         MVC   REVTBTY,TSARBTY     BATCH TYPE                                   
         CLI   REVTBTY,40          TEST BATCH TYPE 40                           
         BNE   *+8                                                              
         MVI   REVTBTY,39          SET TO ALLOW REVERSE ACROSS 39/40            
         CLI   REVTBTY,44          TEST BATCH TYPE 44                           
         BNE   *+8                                                              
         MVI   REVTBTY,43          SET TO ALLOW REVERSE ACROSS 43/44            
REVP34   MVC   REVTSTA,TSARSTA     EXTRACT AND MASSAGE STATUS BYTE              
         NI    REVTSTA,RVSSTATQ    PRESERVE VARIOUS STATUS IF BITSON            
         TM    COMPSTA4,CPYSIREG   TEST INVOICE REGISTER IN USE                 
         BZ    REVP38              NO - DON'T PRESERVE AUTH'D BIT               
         LA    R1,AUTLDGS          TEST LEDGER FOR AUTH STATUS                  
         LA    RF,AUTLDGSN                                                      
         CLC   0(AUTLDGSL,R1),ACCOUNT+(ACTKUNT-ACTRECD) TEST LEDGER             
         BE    REVP40              MUST PRESERVE AUTH'D BIT                     
         LA    R1,AUTLDGSL(R1)                                                  
         BCT   RF,*-14                                                          
REVP38   NI    REVTSTA,255-TRNSAUTH  CLEAR AUTH'D BIT                           
REVP40   TM    TSARRSTA,TRNSUSED   TEST USED                                    
         BZ    *+8                                                              
         OI    REVTSTA,RVSUSED     SET A BITON                                  
         OI    TSARINDS,TSARCHGQ+TSARHITQ  SET RECORD CHANGED & HIT             
         XI    TSARINDS,TSARMKQ    SWAP MARKED STATUS                           
         MVC   TSARCHA,AC@PENDG    INDICATE PENDING                             
         MVC   REVTRNUM,TSRNUM     SAVE TSAR NUMBER                             
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
REVP42   MVI   ANYMARK,1                                                        
         CR    RB,RB               SET CC EQU                                   
         B     REVPROCX                                                         
                                                                                
REVPROCX B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DATA RECORD FILTERING                                               *         
***********************************************************************         
                                                                                
RFILTER  NTR1  ,                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        TEST THIS IS A TRANSACTION                   
         BZ    RFILTNEQ                                                         
*&&UK                                                                           
         CP    TRNAMNT,PZERO       TEST ZERO TRANSACTION                        
         BE    RFILTNEQ                                                         
*&&                                                                             
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   RFILTERX                                                         
                                                                                
         OC    OFFICE,OFFICE       TEST OFFICE FILTER SET                       
         BZ    RFILT02                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX                                                         
         CLC   TRNOFFC(0),OFFICE   TEST OFFICE MATCHES                          
                                                                                
RFILT02  GOTO1 ABLDSRC             BUILD SOURCE A/C IN SRCWORK                  
                                                                                
         OC    SRCACC,SRCACC       TEST SOURCE ACCOUNT FILTER                   
         BZ    RFILT04                                                          
         IC    RF,SRCACCXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            EXIT WITH CC NEQ                             
         CLC   SRCWORK(0),SRCACC                                                
                                                                                
         USING TRSELD,R2                                                        
RFILT04  ICM   R2,15,ATRSEL                                                     
         BZ    RFILTNEQ            DROP UNMATCHED ORDERS ETC.                   
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE                           
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
                                                                                
         L     RF,AIOBUFF                                                       
         USING TRNRECD,RF                                                       
         CLC   PRODLDG,TRNKULA     TEST SJ LEDGER                               
         BNE   RFILT06                                                          
*&&US                                                                           
         CLC   TRNKWORK,=C'99'     ALWAYS SHOW BILLING                          
         BE    RFILT06                                                          
*&&                                                                             
         LA    R2,PRORAD                                                        
         USING PRORAD,R2                                                        
         GOTO1 VPRORATA,PARM,TRNRECD,0,ACOM,0,PRORAD,0                          
         CP    PM$ANVBL,PA$NET                                                  
         BNE   RFILTNEQ                                                         
         DROP  R2,RF                                                            
                                                                                
RFILT06  DS    0H                                                               
                                                                                
RFILTEQU CR    RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTNEQ LTR   RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTERX B     EXIT                                                             
                                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
*                                                                     *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
*        R2=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
SETKEY   NTR1  ,                                                                
         STC   R1,WORK             SAVE KEY BUILD MASK                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY0                                                          
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                ACCOUNT MISSING                              
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY0  TM    WORK,SETOFF         SET OFFICE                                   
         BZ    SETKEY1                                                          
         MVC   TRNKOFF,SPACES                                                   
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    SETKEY1             NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    SETKEY1             YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    SETKEY1                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+4                                                           
         MVC   TRNKOFF(0),OFFICE                                                
                                                                                
SETKEY1  TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         LA    RF,TRNKCULC         POINT TO COMPANY IN CONTRA A/C               
         CLC   BANKUL,TRNKUNT      TEST ACCOUNT IS IN BANK U/L                  
         BE    *+8                 YES - SET TO SKIP '   ***VOID***'            
         LA    RF,L'TRNKCULC-1(RF) ELSE SET FOR 1ST CONTRA A/C > SPACES         
         MVI   0(RF),X'41'         SET FIRST/LAST BYTE OF TRNKCULC              
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY2  TM    WORK,SETSDT         SET MIN TRANSACTION DATE                     
         BZ    SETKEY3                                                          
         MVC   TRNKDATE,PERSTA                                                  
                                                                                
SETKEY3  TM    WORK,SETREF         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY4                                                          
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    REFSTA,REFSTA                                                    
         BZ    SETKEY4                                                          
         CLC   LEDGER,RECVUL       TEST RECEIVABLES                             
         BE    SETKEY4             CANNOT OPTIMISE (SETRAL ROUTINE)             
         MVC   TRNKREF,REFSTA                                                   
                                                                                
SETKEY4  TM    WORK,NXTOFF         BUMP OFFICE (NEW OFFICES ONLY)               
         BZ    SETKEY5                                                          
         IC    RE,TRNKOFF+(L'TRNKOFF-1)                                         
         LA    RE,1(RE)                                                         
         STC   RE,TRNKOFF+(L'TRNKOFF-1)                                         
                                                                                
SETKEY5  TM    WORK,NXTCON         BUMP CONTRA                                  
         BZ    SETKEY6                                                          
         IC    RE,TRNKCACT+(L'TRNKCACT-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKCACT+(L'TRNKCACT-1)                                       
                                                                                
SETKEY6  TM    WORK,NXTSDT         BUMP DATE                                    
         BZ    SETKEY7                                                          
         IC    RE,TRNKDATE+(L'TRNKDATE-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKDATE+(L'TRNKDATE-1)                                       
                                                                                
SETKEY7  DS    0H                                                               
                                                                                
SETKEYX  MVI   TRNKSBR,0           ALWAYS CLEAR SUB-REFERENCE                   
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET RECEIVABLE ALLOCATION INFORMATION BY OFFICE          *         
***********************************************************************         
                                                                                
SETRAL   XC    RALTAB,RALTAB       CLEAR FIRST RECEIVABLE TABLE ENTRY           
         CLC   LEDGER,RECVUL       TEST RECEIVABLES                             
         BNER  RE                                                               
*&&UK*&& CLC   KEY+(TRNKREF-TRNRECD)(L'TRNKREF),SPACES                          
*&&US*&& CLC   KEY+(TRNKDATE-TRNRECD)(L'TRNKDATE),SPACES                        
         BNHR  RE                                                               
SETRALNT NTR1  ,                                                                
         MVC   OVSKEY,KEY          SAVE CURRENT KEY                             
         B     SETRAL04                                                         
SETRAL02 LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         CLC   KEY(L'TRNKEY-L'TRNKSBR),KEYSAVE                                  
         BNE   SETRAL10                                                         
SETRAL04 TM    KEY+(TRNKSTA-TRNRECD),TRNSREVS                                   
         BNZ   SETRAL02            IGNORE REVERSALS                             
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    KEY+(TRNKSTA-TRNRECD),TRNSARCH                                   
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOBUFF                                                       
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    WORK(L'TRNOFFC),WORK                                             
         LA    R1,RALTAB           R1=A(RECEIVABLE TABLE, ENTRY #1)             
         USING RALTABD,R1                                                       
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    SETRAL08                                                         
         MVC   WORK(L'TRNOFFC),TRNOFFC                                          
         OC    WORK(L'TRNOFFC),SPACES                                           
         LA    R0,RALMAXN                                                       
SETRAL06 OC    RALTOFFC,RALTOFFC   TEST EMPTY ENTRY                             
         BZ    SETRAL08                                                         
         CLC   RALTOFFC,WORK       TEST OFFICE MATCHES                          
         BE    SETRAL08                                                         
         LA    R1,RALTABL(R1)                                                   
         BCT   R0,SETRAL06                                                      
         DC    H'0'                RECEIVABLE ALLOCATION TABLE FULL             
                                                                                
SETRAL08 MVC   RALTOFFC,WORK       SET OFFICE (OR ZERO)                         
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+12                                                             
         OI    RALTIND1,RALTIDR    SET DEBIT FOUND (FOR OFFICE)                 
         B     *+8                                                              
         OI    RALTIND1,RALTICR    SET CREDIT FOUND (FOR OFFICE)                
         B     SETRAL02            READ SEQUENTIAL                              
                                                                                
SETRAL10 MVC   KEY,OVSKEY          RESTORE KEY AND SEQUENTIAL PATHWAY           
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST FOR RECEIVABLE ALLOCATION                           *         
***********************************************************************         
                                                                                
TSTRAL   NTR1  ,                                                                
         OC    RALTAB,RALTAB       TEST RECEIVABLE TABLE IN USE                 
         BZ    TSTRALEQ                                                         
         LA    R1,RALTAB           R1=A(RECEIVABLE TABLE, ENTRY #1)             
         USING RALTABD,R1                                                       
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    TSTRAL04                                                         
         L     R2,ATRNEL           R2=A(TRANSACTION ELEMENT)                    
         MVC   WORK(L'TRNOFFC),TRNOFFC-TRNELD(R2)                               
         OC    WORK(L'TRNOFFC),SPACES                                           
         LA    R0,RALMAXN                                                       
TSTRAL02 CLC   RALTOFFC,WORK                                                    
         BE    TSTRAL04                                                         
         LA    R1,RALTABL(R1)                                                   
         BCT   R0,TSTRAL02                                                      
         DC    H'0'                                                             
TSTRAL04 TM    RALTIND1,RALTIDR+RALTICR                                         
         BO    TSTRALNE            IF DEBIT(S) & CREDIT(S) FOUND, FAIL          
                                                                                
TSTRALEQ CR    RB,RB                                                            
         B     EXIT                                                             
                                                                                
TSTRALNE LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT                                                        *         
***********************************************************************         
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
                                                                                
DUMCON   DC    C'SJ999'                                                         
                                                                                
ACCNDSP  EQU   19                                                               
                                                                                
LDGXLST  DS    0X                  EXCLUDED LEDGERS                             
         DC    XL2'00'             MAY BE PATCHED                               
LDGXLSTL EQU   *-LDGXLST                                                        
LDGXLSTN EQU   (*-LDGXLST)/LDGXLSTL                                             
                                                                                
OFFLDGS  DS    0X                  LEDGERS ON WHICH OFFICE MUST MATCH           
         DC    C'GP'               GENERAL P&L LEDGER                           
OFFLDGSL EQU   *-OFFLDGS                                                        
         DC    C'SE'               EXPENSES LEDGER                              
         DC    C'SI'               INCOME LEDGER                                
OFFLDGSN EQU   (*-OFFLDGS)/OFFLDGSL                                             
                                                                                
AUTLDGS  DS    0X                  LEDGERS ON WHICH TO TEST AUTH STATUS         
         DC    C'SE'               EXPENSES LEDGER                              
AUTLDGSL EQU   *-AUTLDGS                                                        
PRODLDG  DC    C'SJ'               PRODUCTION LEDGER                            
         DC    C'SQ'               BALANCE SHEET LEDGER                         
         DC    C'SV'               PRODUCTION SUPPLIERS LEDGER                  
         DC    C'SX'               HOUSE SUPPLIERS LEDGER                       
AUTLDGSN EQU   (*-AUTLDGS)/AUTLDGSL                                             
*&&US                                                                           
DISBTY   DS    0X                  DISPLAY-ONLY BATCH TYPES                     
         DC    AL1(53)                                                          
DISBTYL  EQU   *-DISBTY                                                         
         DC    AL1(54)                                                          
         DC    AL1(55)                                                          
         DC    AL1(56)                                                          
DISBTYN  EQU   (*-DISBTY)/L'DISBTY                                              
*&&                                                                             
PRORAD   DS    0C                                                               
       ++INCLUDE ACPRORATAD                                                     
         EJECT                                                                  
OVRWRKD  DSECT                                                                  
OVPARM   DS    6F                                                               
AREVTAB  DS    A                   A(REVERSAL TABLE)                            
OVWORK   DS    CL10                                                             
OVAMT    DS    PL6                                                              
OVBYTE   DS    XL1                                                              
OVMSGNO  DS    XL(L'FVMSGNO)                                                    
OVPVOUTB DS    CL(L'PVALOUTB)      PERVAL OUTPUT BLOCK                          
OVNEWSTA DS    XL1                 NEW TRANSACTION STATUS                       
OVSKEY   DS    CL(L'TRNKEY)        SAVED TRANSACTION KEY                        
                                                                                
RVSSTATQ EQU   TRNSDR+TRNSREV+TRNSAPPR+TRNSHOLD+TRNSNOCM+TRNSAUTH               
RVSUSED  EQU   X'10'               USE TO INDICATE USED                         
                                                                                
RALTAB   DS    (RALMAXN)XL(RALTABL)                                             
                                                                                
                                                                                
* ACMRKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMRKWRK                                                       
         PRINT ON                                                               
SAVED    DSECT                                                                  
         ORG   TOTALS                                                           
TOTDRS   DS    PL8                 DEBITS                                       
TOTDRDIF DS    PL8                 DEBIT DIFFERENCE                             
REPDRMRK DS    PL8                 REPORT DEBITS MARKED                         
REPDRUMK DS    PL8                 REPORT DEBITS UNMARKED                       
TOTCRS   DS    PL8                 CREDITS                                      
TOTCRDIF DS    PL8                 CREDIT DIFFERENCE                            
REPCRMRK DS    PL8                 REPORT CREDITS MARKED                        
REPCRUMK DS    PL8                 REPORT CREDITS UNMARKED                      
*&&UK                                                                           
         ORG   CURTOTS                                                          
CURDRS   DS    PL8                 DEBITS                                       
CURDRDIF DS    PL8                 DEBIT DIFFERENCE                             
RCPDRMRK DS    PL8                 REPORT DEBITS MARKED                         
RCPDRUMK DS    PL8                 REPORT DEBITS UNMARKED                       
CURCRS   DS    PL8                 CREDITS                                      
CURCRDIF DS    PL8                 CREDIT DIFFERENCE                            
RCPCRMRK DS    PL8                 REPORT CREDITS MARKED                        
RCPCRUMK DS    PL8                 REPORT CREDITS UNMARKED                      
*&&                                                                             
         ORG   SOVRWRK             OVERLAY SAVE AREA                            
SLDGOPOS DS    XL1                 MAIN LEDGER OFFICE POSITION                  
                                                                                
         ORG   SOVRWRK2            OVERLAY SAVE AREA 2                          
REVTAB   DS    (REVNTRYN)XL(REVNTRYL)                                           
         EJECT                                                                  
RALTABD  DSECT                     ** DSECT COVERS RECEIVABLE TABLE **          
RALTIND1 DS    XL1                 INDICATOR 1                                  
RALTIDR  EQU   X'80'               DEBIT FOUND (FOR OFFICE)                     
RALTICR  EQU   X'40'               CREDIT FOUND (FOR OFFICE)                    
RALTOFFC DS    CL2                 OFFICE (OR ZEROES)                           
RALTABL  EQU   *-RALTABD           ENTRY LENGTH                                 
RALMAXN  EQU   255                                                              
                                                                                
                                                                                
REVTABD  DSECT                     ** DSECT COVERS REVERSE TABLE **             
REVNTRY  DS    0X                                                               
REVTRNUM DS    XL2                 TSAR RECORD NUMBER                           
REVTOFF  DS    XL2                 OFFICE/WORKCODE                              
REVTCON  DS    XL15                CONTRA A/C                                   
REVTAMNT DS    PL(L'TRNAMNT)       AMOUNT                                       
REVTAFCA DS    PL(L'AFCAMNT)       AMOUNT IN CURRENCY                           
REVTAFCC DS    CL(L'AFCCURR)       CURRENCY CODE                                
REVTSTA  DS    XL1                 STATUS                                       
REVTMOS  DS    PL2                 YM - PWOS TRANSACTION MOS                    
REVTFREV DS    PL2                 YM - PWOS REVERSING MOS                      
REVTBTY  DS    XL1                 BATCH TYPE (IF PROFBATY=Y)                   
         DS    XL1                 N/D                                          
REVNTRYL EQU   *-REVNTRY                                                        
REVNTRYN EQU   16                                                               
                                                                                
                                                                                
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKFAD                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKEAD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACMRK0A   03/28/14'                                      
         END                                                                    
