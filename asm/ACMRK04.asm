*          DATA SET ACMRK04    AT LEVEL 039 AS OF 07/12/18                      
*PHASE T61604A                                                                  
ACMRK04  TITLE 'CREDITOR - SELECT (APPROVE)'                                    
**********************************************************************          
* JFOX 022 EARLIEST PAYMENT DATE TO SET (SELECT) OR FILTER (DESELECT)*          
* JFOX 023 USE SORAWRK IF AVAILABLE INSTEAD OF CPJWRK                *          
* ECLI 024 MOVE IN SPLIT SOURCE ACCOUNT FOR SPLIT INVOICES           *          
* JFOX 025 YEAR 2000 FIX                                             *          
* JFOX 026 MAKE EARLIEST PAYMENT DATE U.K. ONLY.  MOVE LABEL READ25  *          
* RGUP 039 21MAY18  <SPEC-20692> ADDITIONAL MEDIA FOR DIGIAL AUDIO   *          
**********************************************************************          
ACMRK04  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MRK4**,RA                                                    
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
*                                  SET INPUT SCREEN DISPS FOR ROOT              
         LHI   R1,INPHEDH-TWAD                                                  
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LHI   R1,INPHD2H-TWAD                                                  
         STH   R1,DISPHED2         DISPLACEMENT OF INPUT 2ND HEADLINE           
         AR    R1,R6                                                            
         ST    R1,ADISHEA2         A(INPUT 2ND HEADLINE)                        
         LHI   R1,INPDETH-TWAD                                                  
         STH   R1,DISPDET          DISPLACEMENT OF 1ST DETAIL LINE              
         AR    R1,R6                                                            
         ST    R1,ADISDET1         A(1ST DETAIL LINE)                           
         LHI   R1,INPTOTH-TWAD                                                  
         STH   R1,DISPTOT          DISPLACEMENT OF TOTALS LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LHI   R1,INPPFKH-TWAD                                                  
         STH   R1,DISPPFK          DISPLACEMENT OF PF KEY LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PF KEY LINE)                               
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    INIT02                                                           
         LHI   R1,GRDCONFH-TWAD                                                 
         STH   R1,DISPTOT          DISPLACEMENT OF TOTALS LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LHI   R1,GRDPFAH-TWAD                                                  
         STH   R1,DISPPFK          DISPLACEMENT OF PF KEY LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PF KEY LINE)                               
*                                                                               
INIT02   CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              PRE-VALIDATE HEADER SCREEN                   
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              SET NEXT ACCOUNT                             
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTSELC                                                  
         BE    *+6                 SELECT                                       
         DC    H'0'                                                             
         CLI   TWASCROV,CSSCR1                                                  
         BE    VALHED              SELECT - VALIDATE HEADER                     
         CLI   TWASCROV,CSSCR2                                                  
         BE    VALINP              SELECT - VALIDATE INPUT                      
         CLI   TWASCROV,GRDSCR                                                  
         BE    VALINP              GRID SCREEN - VALIDATE FURTHER               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER                                                 *         
***********************************************************************         
PREVAL   LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREVAL02                                                         
         CLI   LTYPE,TYPCRD        TEST CREDITOR ACTION LAST                    
         BNE   PREVAL02                                                         
         XC    SELLDG,SELLDG                                                    
         XC    SELSUP,SELSUP                                                    
         OI    SELLDGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    SELSUPH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   SELLDG(L'ACTKLDG),ACTKLDG                                        
         MVC   SELSUP(L'ACTKACT),ACTKACT                                        
         XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
PREVAL02 TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    PREVAL04                                                         
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),('TYPCRD',FULL)                
         BE    PREVAL06                                                         
                                                                                
PREVAL04 DS    0H                                                               
*&&UK                                                                           
         XC    SELCURT,SELCURT                                                  
         OI    SELCURTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    SELCURH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    SELCURH+(FVOIND-FVIHDR),FVOXMT                                   
*&&                                                                             
PREVAL06 DS    0H                                                               
                                                                                
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD                           *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   SELSUPH+(FVILEN-FVIHDR),0  TEST INPUT TO SUPPLIER                
         BNE   *+16                                                             
         XC    TWASKEY,TWASKEY     NO - CLEAR KEY SAVED IN TWA                  
         XC    ACCOUNT,ACCOUNT     AND MUST CLEAR SUPPLIER ACCOUNT              
         OC    LEDGER,LEDGER       TEST LEDGER ALEADY SET                       
         BNZ   *+12                                                             
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
         BNE   NXTACCX             INVALID LEDGER                               
         OC    TWASKEY,TWASKEY     TEST SAVED IN TWA                            
         BZ    *+14                                                             
         CLC   SELLDG,TWASKEY+L'ACTKUNT  TEST LEDGER MATCHES SAVED              
         BNE   *+14                                                             
         CLC   SELLDG,LEDGER+L'ACTKUNT  TEST LEDGER CHANGE                      
         BE    NXTACC4                                                          
         XC    TWASKEY,TWASKEY     YES - CLEAR KEY SAVED IN TWA                 
         XC    ACCOUNT,ACCOUNT     (RE)SET ACCOUNT TO ZERO                      
         CLC   SELLDG,LEDGER+L'ACTKUNT  TEST NEW LEDGER TO BE VALIDATED         
         BE    NXTACC4                                                          
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
         BNE   NXTACCX             INVALID LEDGER                               
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
NXTACC10 SR    RF,RF               BUMP KEY FOR NEXT SUPPLIER                   
         IC    RF,ACTKACT+L'ACTKACT-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,ACTKACT+L'ACTKACT-1                                           
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC16            END OF LEDGER                                
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AACCELS                                                          
         OC    RECABLEL,RECABLEL   TEST LOW LEVEL ACCOUNT                       
         BZ    NXTACC10            NO - TRY AGAIN                               
         MVC   ACCOUNT,ACTKCULA                                                 
         B     NXTACC14                                                         
                                                                                
NXTACC12 SR    RF,RF               BUMP KEY FOR NEXT SUPPLIER                   
         IC    RF,ACTKCULA+L'ACTKCULA-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,ACTKCULA+L'ACTKCULA-1                                         
         MVI   GETIND,GETIABLQ                                                  
         GOTO1 AGETACC,0                                                        
         BNE   NXTACC14            NOT FOUND/NOT VALID                          
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BE    NXTACC18            UNIT/LEDGER IS STILL OK                      
         B     NXTACC16            UNIT/LEDGER HAS CHANGED - FINISH             
                                                                                
NXTACC14 CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC16            PAST SUPPLIER LEDGER                         
         MVI   GETIND,GETIABLQ     RE-/READ ACCOUNT                             
         GOTO1 AGETACC,0                                                        
         BE    NXTACC18            VALID THIS TIME                              
         B     NXTACC12            STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC16 MVC   FVMSGNO,=AL2(EANOACCS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,SELLDGH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC18 MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 VACSRCHC,DMCB,SELSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   SELSUP(L'ACTKACT),ACTKACT   MOVE OUT SUPPLIER CODE               
                                                                                
         OI    SELSUPH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         MVC   FVMSGNO,=AL2(IAEPAP1N)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,SELSUPH                                                       
         ST    R1,FVADDR                                                        
                                                                                
NXTACCX  NI    TWAMODE2,255-TWAM2NXA                                            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                       *         
***********************************************************************         
                                                                                
VALHED   DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE UNIT & LEDGER                                              *         
***********************************************************************         
                                                                                
VALLDG   SR    R0,R0               CLEAR R0                                     
         TM    TWAMODE2,TWAM2NXA   TEST CALLED BY NXTACC                        
         BNO   *+6                                                              
         LR    R0,RE               YES - SAVE A(RETURN) TO NXTACC               
         TM    SELLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDGX                                                          
         XC    SELLDGN,SELLDGN                                                  
         OI    SELLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,SELLDGH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
         LA    R1,LDGLIST          R1=A(LIST OF VALID LEDGERS)                  
VALLDG2  CLC   SELLDG,0(R1)                                                     
         BE    VALLDG4             LEDGER IN LIST - VALIDATE IT                 
         CLI   0(R1),EOT                                                        
         BE    *+12                                                             
         LA    R1,L'LDGLIST(R1)                                                 
         B     VALLDG2                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALLERR             ERROR - NOT IN VALID LEDGER LIST             
                                                                                
VALLDG4  MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALLDG,SELLDGH                                                  
         BH    VALLERR                                                          
         MVC   SELLDGN,RECNAME     NAME EXTRACTED BY GETLDG                     
         B     VALLDGX                                                          
                                                                                
VALLERR  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BNZR  RE                  YES - EXIT WITH CC NEQ                       
         B     EXIT                NO - EXIT TO ROOT WITH MESSAGE SET           
                                                                                
VALLDGX  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BZ    *+8                 NO - CONTINUE HDR SCREEN VALIDATION          
         CR    R0,R0               SET CC EQU                                   
         BR    RE                  AND RETURN                                   
         DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
***********************************************************************         
                                                                                
VALSUP   TM    SELSUPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSUPX                                                          
         MVI   FVMINL,1                                                         
*&&UK*&& XC    PRSTCURT,PRSTCURT   VALSUP WILL PRESET CURRENCY                  
         GOTO1 AVALSUP,SELSUPH                                                  
         BNE   EXIT                                                             
         GOTO1 VACSRCHC,DMCB,SELSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
VALSUPX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    SELOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    SELOFFN,SELOFFN                                                  
         OI    SELOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,SELOFFH                                                  
         BL    VALOFFX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   SELOFFN,RECNAME                                                  
         OI    SELOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
VALOFFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    SELCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    SELCONN,SELCONN                                                  
         OI    SELCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         CLI   SELCONH+(FVILEN-FVIHDR),L'DUMCON                                 
         BNE   VALCON2                                                          
         CLC   SELCON(L'DUMCON),DUMCON  TEST FOR DUMMY CONTRA                   
         BNE   VALCON2                                                          
         MVC   CONTRA,SPACES                                                    
         MVC   CONTRA(L'ACTKCPY),COMPANY                                        
         MVC   CONTRA+L'ACTKCPY(L'DUMCON),DUMCON                                
         MVI   CONTRAXL,L'TRNKCULC-1                                            
         MVI   CONTIND,CONTILOQ    SET BONA FIDE LOW-LEVEL ACCOUNT              
         OI    SELCONH+(FVOIND-FVIHDR),FVOXMT  RE-TRANSMIT                      
         B     VALCONX                                                          
                                                                                
VALCON2  GOTO1 AVALCON,SELCONH                                                  
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   SELCONN,RECNAME                                                  
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SOURCE                                                     *         
***********************************************************************         
                                                                                
VALSRC   TM    SELSRCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSRCX                                                          
         XC    SELSRCN,SELSRCN                                                  
         OI    SELSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALSRC,SELSRCH                                                  
         BH    EXIT                                                             
         BL    VALSRCX             NOT REQUIRED, NOT INPUT                      
         MVC   SELSRCN,RECNAME                                                  
         OI    SELSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
VALSRCX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   TM    SELREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,SELREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE PERIOD                                                     *         
***********************************************************************         
                                                                                
VALPER   TM    SELPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,SELPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE PERIOD                                       *         
***********************************************************************         
                                                                                
VALADA   TM    SELADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,SELADAH                                                  
         BH    EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE MOS RANGE                                                  *         
***********************************************************************         
                                                                                
VALMOS   TM    SELMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,SELMOAH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
*&&UK                                                                           
***********************************************************************         
* VALIDATE CURRENCY FILTER                                            *         
***********************************************************************         
                                                                                
VALCUR   TM    SELCURH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCURX                                                          
         GOTO1 AVALCUR,SELCURH     PUT VALID CURRENCIES INTO TABLE              
         BH    EXIT                                                             
         BE    VALCURX                                                          
         PUSH  USING               TEST UNACCEPABLE CURRENCY FILTER             
PRESET   USING CURTABD,PRSTCURT                                                 
         CLC   PRESET.CURTCUR,SELCUR                                            
         BNE   *+14                                                             
         CLC   SELCUR+L'CURTCUR(L'SELCUR-L'CURTCUR),SPACES                      
         BNH   VALCURX                                                          
         MVC   SELCUR,SPACES       UNACCEPTABLE CURRENCY FILTER                 
         OI    SELCURH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   SELCUR,C'*'                                                      
         MVI   SELCURH+(FVILEN-FVIHDR),1                                        
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    *+14                                                             
         MVC   SELCUR(L'CURTCUR),PRESET.CURTCUR                                 
         MVI   SELCURH+(FVILEN-FVIHDR),L'CURTCUR                                
         MVC   FVMSGNO,=AL2(AI$CURFC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         POP   USING                                                            
VALCURX  DS    0H                                                               
*&&                                                                             
***********************************************************************         
* VALIDATE INCLUDE SELECTED                                           *         
***********************************************************************         
                                                                                
VALSEL   TM    SELSELH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSELX                                                          
         GOTO1 AVALICL,SELSELH                                                  
         BH    EXIT                                                             
VALSELX  DS    0H                                                               
*&&UK                                                                           
***********************************************************************         
* VALIDATE EARLIEST PAYMENT DATE (SELECT), DATE RANGE (DESELECT)      *         
***********************************************************************         
                                                                                
VALEPD   TM    SELEPDH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALEPDX                                                          
         CLI   ICLMARK,ICLMONLY    TEST EXCLUSIVELY DESELECTING                 
         BE    VALEPD02                                                         
         XC    ERPDSTA,ERPDSTA     SELECT - ALLOW SINGLE DATE ONLY              
         GOTO1 AVALDAT,SELEPDH     TO BE SET IN MARKED TRANSACTIONS             
         BH    EXIT                                                             
         BL    VALEPDX                                                          
         MVC   ERPDSTA,WORK+(PVALPSTA-PERVALD)                                  
         B     VALEPDX                                                          
VALEPD02 MVC   ERPDSTA,PERSTA      DESELECT - SAVE PERIOD START/END             
         MVC   ERPDEND,PEREND                                                   
         GOTO1 AVALPER,SELEPDH     ALLOW DATE RANGE                             
         BE    VALEPD04            TO FILTER TRANSACTIONS                       
         MVC   PERSTA,ERPDSTA      RESTORE PERIOD START/END                     
         MVC   PEREND,ERPDEND                                                   
         BH    EXIT                ERROR EXIT                                   
         XC    ERPDSTA,ERPDSTA     PRESET EARLIEST PAYMENT DATE RANGE           
         MVI   ERPDEND,FF          X'000000'-X'FFFFFF'                          
         MVC   ERPDEND+1(L'ERPDEND-1),ERPDEND                                   
         B     VALEPDX                                                          
VALEPD04 XC    PERSTA,ERPDSTA      SWAP VALUES                                  
         XC    ERPDSTA,PERSTA                                                   
         XC    PERSTA,ERPDSTA                                                   
         XC    PEREND,ERPDEND                                                   
         XC    ERPDEND,PEREND                                                   
         XC    PEREND,ERPDEND                                                   
VALEPDX  DS    0H                                                               
*&&                                                                             
         B     READTRN             READ AND FILTER TRANSACTIONS                 
         EJECT                                                                  
***********************************************************************         
* READ AND FILTER TRANSACTIONS.  PUT QUALIFYING TRANSACTIONS TO TSAR  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
READTRN  OI    DISIND,DISIOFLO     PRESET OVERFLOW (WHICH IS ALLOWED)           
                                                                                
         LA    R1,TOTALS           CLEAR TOTALS ACCUMULATORS                    
         LA    R0,TOTALSN                                                       
         ZAP   0(L'TOTALS,R1),PZERO                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
                                                                                
         LA    R2,KEY              BUILD START KEY                              
         GOTO1 SETKEY,SETALL                                                    
*&&US*&& GOTO1 AVALPUB,TRNRECD     VALIDATE SPECIAL PUBLICATION NUMBER          
         MVI   TSARLEN+1,TSARCSL                                                
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
*&&UK                                                                           
         NI    SAFCIND1,FF-SAFCIGBP                                             
         XC    FORECURT,FORECURT   SET UP CURRENCY 1ST TIME THROUGH             
*&&                                                                             
         B     READ04                                                           
                                                                                
READ02   LA    R2,KEY                                                           
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
READ04   TM    TRNKSTAT,TRNSDELT+TRNSDRFT+TRNSREVS                              
         BNZ   READ02                                                           
         TM    TRNKSTA2,TRNSPEEL+TRNSUSED                                       
         BNZ   READ02                                                           
         CLC   TRNKCULA,ACCOUNT                                                 
         BNE   READTRNX                                                         
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READ06              NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READ06              YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    READ06                                                           
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   READTRNX                                                         
         CLC   TRNKOFF(0),OFFICE                                                
READ06   OC    CONTRA,CONTRA                                                    
         BZ    READ08                                                           
         IC    RF,CONTRAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKCULC(0),CONTRA                                               
         BE    READ08                                                           
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READTRNX            YES - GONE TOO FAR                           
         GOTO1 SETKEY,SETCON+NXTOFF                                             
         B     READ02                                                           
READ08   CLC   TRNKDATE,PERSTA                                                  
         BNL   READ10                                                           
         GOTO1 SETKEY,SETSDT                                                    
         B     READ02                                                           
READ10   CLC   TRNKDATE,PEREND                                                  
         BNH   READ14                                                           
         TM    CONTIND,CONTILOQ    TEST REAL LOW-LEVEL ACCOUNT                  
         BNZ   READ12                                                           
         GOTO1 SETKEY,SETSDT+NXTCON                                             
         B     READ02                                                           
READ12   TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READTRNX            YES - GONE TOO FAR                           
         GOTO1 SETKEY,SETSDT+NXTOFF                                             
         B     READ02                                                           
READ14   OC    REFSTA,REFSTA                                                    
         BZ    READ16                                                           
         IC    RF,REFSTAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFSTA                                                
         BE    READ16                                                           
         BL    *+14                                                             
         OC    REFEND,REFEND                                                    
         BNZ   READ18                                                           
         GOTO1 SETKEY,SETREF                                                    
         B     READ02                                                           
READ16   OC    REFEND,REFEND                                                    
         BZ    READ20                                                           
READ18   IC    RF,REFENDXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFEND                                                
         BNH   READ20                                                           
         GOTO1 SETKEY,SETREF+NXTSDT                                             
         B     READ02                                                           
READ20   CLC   TRNKSMOS,MOSSTA                                                  
         BL    READ02                                                           
         CLC   TRNKSMOS,MOSEND                                                  
         BH    READ02                                                           
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         DROP  R2                                                               
                                                                                
         GOTO1 AGENFILT,AIOBUFF    FILTERS                                      
         BNE   READ02                                                           
         BAS   RE,RFILTER          OVERLAY SPECIFIC FILTERING                   
         BNE   READ02                                                           
                                                                                
         L     R1,AIOSAVE          R1=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R1)      EXTRACT DATA RECORD DISK ADDRESS             
                                                                                
         USING TRNRECD,R1          R1=A(DATA RECORD KEY)                        
         L     R1,AIOBUFF          EXTRACT TRNKEY VALUES                        
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
         MVC   TSARMOS,TRNRSMOS                                                 
         MVI   TSARRSTA,0                                                       
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD IS ON ARCHIVE                     
                                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        EXTRACT TRNEL VALUES                         
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    *+10                NO - DON'T SET TSAROFF                       
         MVC   TSAROFF,TRNOFFC                                                  
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         ZAP   TSARAMNT,TRNAMNT                                                 
         TM    TRNSTAT,TRNSHOLD    TEST ALREADY HELD FROM PAYING                
         BO    READ22                                                           
         TM    COMPSTA4,CPYSIREG   TEST AGENCY USES INVOICE REGISTER            
         BNO   READ24                                                           
         CLC   SUPPUL,TRNKUNT      TEST PRODUCTION SUPPLIER                     
         BE    *+14                                                             
         CLC   SUPXUL,TRNKUNT      TEST HOUSE SUPPLIER                          
         BNE   READ24                                                           
         TM    TRNSTAT,TRNSAUTH                                                 
         BO    READ24                                                           
READ22   OI    TSARINDS,TSARDISQ   UNAUTH/HELD TRANS - DISPLAY ONLY             
         MVI   TSARCHA,C'*'                                                     
READ24   TM    TRNSTAT,TRNSAPPR                                                 
         BNO   *+12                                                             
         MVI   TSARVAR,TRNSAPPR    SET VARIABLE KEY BYTE FOR SORT SEQ.          
         OI    TSARINDS,TSARMKQ+TSARINMQ                                        
                                                                                
         MVC   TSARFSAC,SRCWORK    EXTRACT ANY SOURCE ACCOUNT                   
         CLC   TSARFSAC,SPACES     HAS THE SOURCE ACCOUNT BEEN SET?             
         BNE   READ25                                                           
         ICM   R2,15,AGINEL        NO - IS IT A SPLIT INVOICE?                  
         BZ    READ25                                                           
         MVC   TSARFSAC,LC@SPLIT                                                
READ25   DS    0H                                                               
*&&UK                                                                           
         USING SORELD,R2                                                        
         ICM   R2,15,ASOREL        USE SOREL WORKCODE, IF PRESENT               
         BZ    READ26                                                           
         CLI   SORSYS,SORSACC      TEST ACCOUNTING SOURCE A/C                   
         BNE   READ26                                                           
         CLI   SORLN,SORAL2Q       TEST LONG ACCOUNTING ELEMENT                 
         BL    READ26                                                           
         MVC   TSARFWRK,SORAWRK    YES TAKE WORKCODE                            
         B     READ28                                                           
                                                                                
         USING CPJELD,R2                                                        
READ26   ICM   R2,15,ACPJEL1       TEST CPJEL FOUND                             
         BZ    READ28                                                           
         CLI   CPJTYPE,CPJTJOB     TEST JOB TYPE                                
         BNE   READ28                                                           
         MVC   TSARFWRK,CPJWRK     TAKE WORKCODE                                
                                                                                
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
READ28   ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
*&&                                                                             
         GOTO1 ABLDMOS             EXTRACT MEDIA MOS                            
         GOTO1 ABLDWRKC            EXTRACT WORK CODE                            
*                                                                               
*        USING GDAELD,R2           EXTRACT MEDIA MOS                            
*        ICM   R2,15,AGDAMMOS      MEDIA MONTH OF SERVICE                       
*        BZ    *+14                                                             
*        MVC   TSARFMMD,GDAYYMM                                                 
*        B     READ29                                                           
*                                                                               
*        USING MBIELD,R2           EXTRACT MEDIA MOS                            
*        ICM   R2,15,AMBIEL                                                     
*        BZ    *+14                                                             
*        MVC   TSARFMMD(L'MBIMOS),MBIMOS                                        
*        B     READ29                                                           
*                                                                               
*        USING OTHELD,R2           EXTRACT MEDIA MOS                            
*        ICM   R2,15,AOTHEL                                                     
*        BZ    READ29                                                           
*        MVC   TSARFMMD(L'OTHDATE),OTHDATE                                      
*                                                                               
         USING DUEELD,R2           EXTRACT DUEEL VALUES                         
READ29   ICM   R2,15,ADUEEL                                                     
         BZ    *+14                                                             
         MVC   TSARFDUE,DUEDATE                                                 
         B     READ29A                                                          
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(2,TSARFDUE)                            
READ29A  DS    0H                                                               
*&&UK                                                                           
         USING GDAELD,R2           EXTRACT GDAEL VALUES                         
         ICM   R2,15,AGDAERPD      TEST EARLIEST PAYMENT DATE GDAEL             
         BZ    *+10                                                             
         MVC   TSARERPD,GDADATE                                                 
*&&                                                                             
         USING SCIELD,R2           EXTRACT SCIEL VALUES                         
         ZAP   TSARFDIS,PZERO      DISCOUNT                                     
         ICM   R2,15,ASCIEL                                                     
         BZ    READ30                                                           
         CLI   SCITYPE,SCITCDSC    TEST CASH DISCOUNT TYPE                      
         BNE   READ30                                                           
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         ZAP   TSARFDIS,SCIAMNT                                                 
         CLI   PROFDISC,C'Y'       TEST ADDING ANY CASH DISCOUNT                
         BNE   READ30                                                           
         AP    TSARAMNT,SCIAMNT    ADD TO TRANSACTION AMOUNT                    
                                                                                
         USING TRSELD,R2           EXTRACT TRSEL VALUES                         
READ30   ICM   R2,15,ATRSEL        R2=A(TRANSACTION STATUS ELEMENT)             
         MVC   TSARADAT,TRSDATE    EXTRACT TRANSACTION ACTIVITY DATE            
         MVC   TSARSSTA,TRSSTAT    EXTRACT STATUS BYTE                          
                                                                                
         OC    ANOTELS,ANOTELS     TEST MEMO'S ATTACHED                         
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
                                                                                
*&&UK*&& GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
*&&US                                                                           
         GOTO1 ABLDINV             GET LONG INVOICE NUMBER                      
*                                                                               
         USING XPYELD,R2                                                        
         ICM   R2,15,AXPYEL                                                     
         BZ    READ33                                                           
*        MVC   TSARFINV(L'XPYINV),XPYINV                                        
         CP    XPYCD,PZERO         TEST CASH DISCOUNT                           
         BE    READ33                                                           
         ZAP   TSARFDIS,XPYCD                                                   
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         CLI   PROFDISC,C'Y'       TEST ADDING ANY CASH DISCOUNT                
         BNE   READ33                                                           
         AP    TSARAMNT,XPYCD      ADD TO TRANSACTION AMOUNT                    
         B     READ33                                                           
                                                                                
*        USING FFTELD,R2                                                        
*EAD32   ICM   R2,15,AFFTLEL                                                    
*        BZ    READ32A                                                          
*        XR    RE,RE                                                            
*        IC    RE,FFTDLEN                                                       
*        C     RE,=F'20'                                                        
*        BNH   *+8                                                              
*        LA    RE,20                                                            
*        BCTR  RE,0                TAKE MAX OF 20 CHAR                          
*        EX    RE,*+8                                                           
*        B     *+10                                                             
*        MVC   TSARFINV(0),FFTDATA                                              
*        B     READ33                                                           
*EAD32A  CLC   TSARFINV,SPACES                                                  
*        BH    READ33                                                           
*        MVC   TSARFINV(L'TSARREF),TSARREF                                      
*                                                                               
READ33   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ34                                                           
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
*&&UK                                                                           
         USING AFCELD,R3                                                        
         ICM   R3,15,AAFCEL                                                     
         BZ    *+10                                                             
         AP    CURCRS,AFCAMNT                                                   
*&&                                                                             
         AP    TOTCRS,TRNAMNT                                                   
         LA    RF,TOTBAL           RF=A(BALANCE TOTAL)                          
         TM    TRNSTAT,TRNSAPPR    TEST SELECTED FOR PAYMENT                    
         BNO   *+8                                                              
         LA    RF,TOTMRK           RF=A(SELECTED TOTAL)                         
*&&UK                                                                           
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         AP    CURTOTS-TOTALS(L'TOTALS,RF),AFCAMNT                              
*&&                                                                             
         AP    0(L'TOTALS,RF),TRNAMNT                                           
         MVI   ANYADD,1            SET TRANSACTION ADDED                        
         B     READ02              READ SEQUENTIAL                              
         DROP   R2                                                              
*&&UK*&& DROP   R3                                                              
                                                                                
READ34   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                                                             
         B     DISPTRN                                                          
                                                                                
READTRNX NI    DISIND,255-DISIOFLO  CLEAR OVERFLOW (DID NOT OCCUR)              
         CLI   ANYADD,1            TEST ANYTHING IN BUFFER                      
         BE    DISPTRN                                                          
         LA    R1,SELSUPH          NO - SET CURSOR TO SUPPLIER FIELD            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
DISPTRN  GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),MRKOLAYH               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&UK                                                                           
         GOTO1 ABLDCURR            BUILD CURRENCY ENTRIES                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETFORE            SET UP SINGLE FOREIGN CURRENCY               
*&&                                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         LA    R1,CSSCR2           OVERLAY ACTION INPUT SCREEN                  
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    *+8                                                              
         LA    R1,GRDSCR           LOAD GRID SCREEN                             
         GOTO1 AOVRSCR             OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BO    DTRN02                                                           
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
                                                                                
DTRN02   OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    DTRN04              NO                                           
         GOTO1 ADISGRID            YES                                          
*        NI    DISIND,X'FF'-DISIRST                                             
         B     DTRN06                                                           
DTRN04   GOTO1 ADISPLAY                                                         
         TM    DISIND,DISIOFLO     TEST BUFFER OVERFLOW                         
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNWRN)  WARN USER                                
DTRN06   L     R2,ADISTOTS                                                      
         GOTO1 ABLDTOT,(R2)                                                     
                                                                                
DISPTRNX B     EXIT                                                             
         EJECT                                                                  
         USING DISLINED,R2                                                      
VALINP   LA    RF,MRKSCRH          SET A(SCROLL FIELD) FOR EARLY EXIT           
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    VALINP01            NO                                           
         LA    RF,MRKOPTH          SET A(OPTION FIELD) FOR EARLY EXIT           
VALINP01 ST    RF,FVADDR                                                        
         CLI   OPTALL,0            TEST GLOBAL MARK/UNMARK                      
         BE    VALINP02                                                         
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BO    VALINP16            YES - CALL DISPLAY                           
         BAS   RE,MRKALL           MARK ALL TRANSACTIONS                        
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         OI    DISIND,DISIRST      SET TO RESTART DISPLAY                       
         NI    DISIND,X'FF'-DISINIT  REINITIALISE FOR GRIDS                     
         B     VALINP16                                                         
                                                                                
VALINP02 MVI   ANYMARK,0                                                        
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BNZ   VALINP16            GET NEXT SCREEN                              
         LA    R3,DISLIST          R3=A(LIST OF TSAR RECDS ON DISPLAY)          
         L     R2,ADISDET1         R2=A(1ST DETAIL LINE)                        
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT        NUMBER OF DISPLAY LINES                      
         BZ    VALINP16            NO RECORDS TO DISPLAY                        
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VALINP04                                                         
         NI    TWAMODE2,255-TWAM2SKP  RESET SKIP VALIDATION                     
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VALINP16            SKIP VALIDATION AND CALL DISPLAY             
                                                                                
VALINP04 GOTO1 AVALZMRK,DISLHDR2   ZOOM INPUT                                   
         BL    VALINP06            NO ZOOM - TRY OTHER MARKS                    
         BH    VALINPX             ZOOM INVALID - EXIT WITH ERROR SET           
         GOTO1 AZOOMFAC,(R3)       PREPARE ZOOM SCREEN AND EXIT TO USER         
         B     EXIT                                                             
                                                                                
VALINP06 GOTO1 AVALMRK,DISLHDR2                                                 
         BH    VALINPX             EXIT WITH ERROR SET                          
         BL    VALINP14            NO INPUT - NEXT SCREEN LINE                  
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,TSARCHGQ       TEST ONLY WANT TO REDISPLAY                  
         BE    VALINP12                                                         
         CLI   BYTE,TSARMKQ        TEST IF USER IS MARKING                      
         BNE   VALINP08                                                         
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    VALINP14                                                         
         OI    TSARINDS,TSARMKQ                                                 
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
*&&UK*&& AP    CURMRK,TSARAFCA     ADD TO MARKED (AFC)                          
*&&UK*&& SP    CURBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
*&&UK*&& BO    *+20                                                             
*&&US*&& BO    *+14                                                             
         AP    REPMRK,TSARAMNT     NO, ADD TO MARKED THIS SESSION               
*&&UK*&& AP    RCPMRK,TSARAFCA     NO, ADD TO MARKED THIS SESSION (AFC)         
         B     VALINP10                                                         
         SP    REPUMK,TSARAMNT     YES, MUST HAVE BEEN ADDED TO UNMARKD         
*&&UK*&& SP    RCPUMK,TSARAFCA     YES, MUST HAVE BEEN ADDED (AFC)              
         B     VALINP10            THIS SESSION, SO SUBTRACT IT                 
                                                                                
VALINP08 TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    VALINP14                                                         
         NI    TSARINDS,255-TSARMKQ                                             
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
*&&UK*&& SP    CURMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
*&&UK*&& AP    CURBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
*&&UK*&& BO    *+20                                                             
*&&US*&& BO    *+14                                                             
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO MARKED           
*&&UK*&& SP    RCPMRK,TSARAFCA     NO, MUST HAVE BEEN ADDED (AFC)               
         B     VALINP10            THIS SESSION, SO SUBTRACT IT                 
         AP    REPUMK,TSARAMNT     YES, ADD TO UNMARKED THIS SESSION            
*&&UK*&& AP    RCPUMK,TSARAFCA     YES, ADD TO UNMARKED THIS SESSION            
         B     VALINP10                                                         
                                                                                
VALINP10 L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ANYMARK,1                                                        
         DROP  RF                                                               
VALINP12 GOTO1 ABLDLIN,DISLHDR1    REBUILD LHS, XMIT, (UN)HIGHLIGHT             
                                                                                
VALINP14 LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VALINP04                                                      
                                                                                
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+16                                                             
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   YES - SET SKIP VALIDATION                    
         TM    DISIND,DISINCOL     TEST NEW COLUMN DISPLAY                      
         BNZ   VALINP16            DISPLAY NEW COLUMNS (NO SCROLLING)           
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VALINP16                                                         
         OI    DISIND,DISIFFLT     FORCE FILTERING OF DISLIST NEXT TIME         
         LA    R1,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     VALINPX                                                          
*                                                                               
VALINP16 TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    VALINP18            NO                                           
         GOTO1 ADISGRID            YES                                          
         B     VALINPX                                                          
VALINP18 GOTO1 ADISPLAY                                                         
*                                                                               
VALINPX  L     R2,ADISTOTS                                                      
         GOTO1 ABLDTOT,(R2)                                                     
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+8                                                              
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TRANSACTIONS                                                 *         
***********************************************************************         
         USING TSARD,RF                                                         
         USING REPD,R3                                                          
UPDATE   TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD02                                                            
         ZAP   REPMRK,PZERO        INIT MARKED TOTALS                           
         ZAP   REPUMK,PZERO        INIT UNMARKED TOTALS                         
         LA    R1,MRKOPTH                                                       
         GOTO1 AUPDGRID            READ IN DATA AND CLEAR SCREEN                
         BNE   UPDATEX             HAVEN'T COME TO END OF DATA                  
                                                                                
UPD02    TM    TWAMODE2,TWAM2CHG                                                
         BO    UPD04                                                            
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         LA    R1,MRKSCRH                                                       
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPDATEX                                                          
         LA    R1,MRKOPTH          YES - SET OPTION FIELD                       
         B     UPDATEX                                                          
                                                                                
UPD04    OC    PRTSUB,PRTSUB                                                    
         BZ    UPD06                                                            
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD05                                                            
         GOTO1 ABLDDIS             GET DISPLACEMENTS FROM PROFILE               
UPD05    GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@APRVD),LC@APRVD                          
         LA    R1,REPH5+L'DISLLINE+1+L'LC@APRVD-1                               
         CLI   0(R1),C' '          SEEK FIRST NON-BLANK                         
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
UPD06    LA    R1,INPMRKH                                                       
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    *+8                                                              
         LA    R1,MRKOPTH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,TEMP                                                        
                                                                                
UPD08    GOTO1 ATSARGET,TEMP                                                    
         BE    UPD10                                                            
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD50                                                            
         DC    H'0'                                                             
                                                                                
UPD10    TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    UPD14                                                            
         TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD12                                                            
         TM    TSARSTA,TRNSAPPR    TEST ALREADY SELECTED                        
         BO    UPD14                                                            
         MVI   BYTE,TRNSAPPR                                                    
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD16                                                            
         AP    REPMRK,TSARAMNT     ADD TO MARKED TOTALS                         
         B     UPD16                                                            
                                                                                
UPD12    TM    TSARSTA,TRNSAPPR    TEST ALREADY SELECTED                        
         BZ    UPD14                                                            
*        BZ    UPD48                                                            
         MVI   BYTE,0                                                           
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD16                                                            
         AP    REPUMK,TSARAMNT     ADD TO UNMARKED TOTALS                       
         B     UPD16                                                            
*                                                                               
UPD14    TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    UPD48                                                            
         TM    TSARIND3,TSARZEPD+TSARZDUE                                       
         BZ    UPD48                                                            
         MVI   BYTE,X'FF'                                                       
         B     UPD18                                                            
*                                                                               
UPD16    OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD18               MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD48               YES - GET NEXT TSAR RECORD                   
*                                                                               
UPD18    MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF    SET A(ELEMENTS)                              
*                                                                               
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BNE   UPD20                                                            
*                                                                               
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    UPD22                                                            
*                                                                               
UPD20    NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IAUPDCUT)  UPDATE CUT SHORT                         
         LA    R1,MRKACTH                                                       
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPDATEX                                                          
         LA    R1,PARM                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,=AL2(RARAEUCS)                                           
         MVI   GTMTYP,GTMREP       REPORT MESSAGE                               
         MVI   GTMAXL,L'REPP1                                                   
         OI    GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,REPP1                                                         
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT                                                          
         GOTO1 VREPORT,REPD                                                     
         GOTO1 APRTCLO                                                          
         MVC   FVMSGNO,=AL2(IAUPCUTR)  UPDATE CUT SHORT, REPORT SPOOLED         
         LA    R1,MRKACTH                                                       
         B     UPDATEX                                                          
         DROP  R1                                                               
*                                                                               
UPD22    CLI   BYTE,X'FF'                                                       
         BE    UPD23                                                            
         NI    TRNSTAT,255-TRNSAPPR     DESELECT                                
         CLI   BYTE,0                   TEST DESELECTING                        
         BE    *+8                                                              
         OI    TRNSTAT,TRNSAPPR         SELECT                                  
         USING TRSELD,R2                                                        
UPD23    ICM   R2,15,ATRSEL                                                     
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD24                                                            
         GOTO1 AEXTRSL             YES - EXTEND IT                              
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD24    CLI   BYTE,X'FF'                                                       
         BNE   UPD25                                                            
         MVI   TRSMARK,TRSZOOMQ    SET MARKER TYPE/ACTION                       
         B     UPD26                                                            
UPD25    MVI   TRSMARK,TRSMCSQ     SET MARKER TYPE/ACTION                       
         CLI   BYTE,0              TEST SELECT/DESELECT                         
         BNE   UPD26                                                            
         OI    TRSMARK,TRSMUMQ     DESELECT - SET ACTION IS NEGATIVE            
         B     UPD36                                                            
UPD26    TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    UPD28               NO                                           
         TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BNZ   UPD30                                  CHANGED                   
UPD28    OC    ERPDSTA,ERPDSTA     SELECT - TEST EARLIEST PAYMENT DATE          
         BZ    UPD38                                                            
         TM    TSARIND3,TSARZEPD   TEST ZOOM ADDED/CHANGED/DELETED DATE         
         BO    UPD42                                                            
         USING GDAELD,R2                                                        
UPD30    ICM   R2,15,AGDAERPD      TEST EARLIEST PAYMENT DATE CARRIED           
         BZ    UPD32                                                            
         MVC   GDADATE,ERPDSTA     SET DATE FROM HEADER SCREEN                  
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    UPD42               NO                                           
         TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BZ    UPD38                                  CHANGED                   
*&&US*&& B     UPD36                                                            
*&&UK*&& OC    TSARERPD,TSARERPD                                                
*&&UK*&& BZ    UPD36                                                            
*&&UK*&& MVC   GDADATE,TSARERPD    SET DATE FROM HEADER SCREEN                  
*&&UK*&& B     UPD38                                                            
UPD32    LA    R2,ELEMT                                                         
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATERPD                                                 
         MVC   GDADATE,ERPDSTA     SET DATE FROM HEADER SCREEN                  
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    UPD34               NO                                           
         TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BZ    UPD34                                  CHANGED                   
*&&US*&& B     UPD38                                                            
*&&UK*&& OC    TSARERPD,TSARERPD                                                
*&&UK*&& BZ    UPD38                                                            
*&&UK*&& MVC   GDADATE,TSARERPD    SET DATE FROM HEADER SCREEN                  
UPD34    GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,GDAELD                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         B     UPD38                                                            
UPD36    ICM   R2,15,AGDAERPD      TEST EARLIEST PAYMENT DATE CARRIED           
         BZ    UPD38               NO                                           
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),('GDAELQ',AIOBUFF),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
*                                                                               
         USING GDAELD,R2                                                        
UPD38    ICM   R2,15,AGDAAPPR      TEST APPROVAL ELEMENT CARRIED                
         BZ    UPD38B                                                           
         CLI   BYTE,TRNSAPPR       ARE WE APPROVING?                            
         BE    UPD38A                                                           
         MVI   0(R2),X'FF'         DELETE OLD GDAELD                            
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),(X'FF',AIOBUFF),0                     
         B     UPD39                                                            
*                                                                               
UPD38A   TIME  DEC                                                              
         SRL   R0,8                SHIFT OUT TENTHS & HUNDREDTHS                
         SLL   R0,4                MAKE ROOM FOR SIGN                           
         XC    DUB,DUB                                                          
         STCM  R0,15,DUB+4                                                      
         OI    DUB+7,X'0F'                                                      
         AP    DUB,=P'60000'       BUMP UP HOURS FROM DDS TO ACTUAL             
         ICM   R0,15,DUB+4                                                      
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,GDAAPTIM       SAVE OFF CURRENT TIME                        
         MVC   GDAAPPDT,TODAYP     SET DATE FROM HEADER SCREEN                  
         B     UPD39                                                            
UPD38B   CLI   BYTE,TRNSAPPR       ARE WE APPROVING?                            
         BNE   UPD39                                                            
         LA    R2,ELEMT                                                         
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALN3Q                                                    
         MVI   GDATYPE,GDAAPP                                                   
         MVI   GDATSUB,GDAAPPMK                                                 
         MVC   GDAAPPDT,TODAYP     SET DATE FROM HEADER SCREEN                  
         TIME  DEC                                                              
         SRL   R0,8                SHIFT OUT TENTHS & HUNDREDTHS                
         SLL   R0,4                MAKE ROOM FOR SIGN                           
         XC    DUB,DUB                                                          
         STCM  R0,15,DUB+4                                                      
         OI    DUB+7,X'0F'                                                      
         AP    DUB,=P'60000'       BUMP UP HOURS FROM DDS TO ACTUAL             
         ICM   R0,15,DUB+4                                                      
         SRL   R0,4                SHIFT OUT SIGN                               
         STCM  R0,7,GDAAPTIM       SAVE OFF CURRENT TIME                        
         GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,GDAELD                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
*                                                                               
UPD39    TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    UPD42                                                            
         TM    TSARIND3,TSARZDUE   WAS THE DUE DATE CHANGED                     
         BZ    UPD42               NO                                           
         L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         USING TRNRECD,R2                                                       
         TM    TRNRSTA2,TRNSUSED   TEST NOT USED DATE                           
         BZ    *+6                 OVERLAY DON'T DISPLAY USED ITEMS             
         DC    H'0'                                                             
*        BO    UPD41               OVERLAY DON'T DISPLAY USED ITEMS             
         MVC   TRNRSDUE,TSARFDUE                                                
         USING DUEELD,R2                                                        
         ICM   R2,15,ADUEEL                                                     
         BZ    UPD40                                                            
         OC    TSARFDUE,TSARFDUE                                                
         BNZ   UPD39A                                                           
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),('DUEELQ',AIOBUFF),0                  
         CLI   12(R1),0                                                         
         BE    UPD41                                                            
         DC    H'0'                DIE ON ANY ERROR                             
UPD39A   MVC   DUEDATE,TSARFDUE                                                 
         B     UPD42                                                            
*                                                                               
UPD40    LA    R2,ELEMT                                                         
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,TSARFDUE                                                 
         GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,(R2)                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
UPD41    GOTO1 ASETELAD,AIOBUFF    AND REFRESH ELEMENT ADDRESSES                
*                                                                               
UPD42    L     R2,AIOBUFF          R2=A(DATA RECORD)                            
         USING TRNRECD,R2                                                       
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
*&&UK                                                                           
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   UPD44                                                            
         GOTO1 VPROMOTE,DMCB,AIOBUFF,ACOM  PROMOTE TO ACCMST AND                
         B     UPD46                    CHANGE ACCDIR POINTER                   
*&&                                                                             
UPD44    GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY(L'TRNKEY),TRNKEY  EXTRACT TRANSACTION KEY                    
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA                         
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
*                                                                               
* MARK AUTO APPROVAL POINTERS AS APPROVED(UNAPPROVED)                           
*                                                                               
UPD0130  MVC   SVKEY,KEY           SAVE OFF KEY FOR LATER RESTORE               
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF                                                       
*                                                                               
         LA    R4,TRNRFST          FIND TRSEL                                   
         LA    RF,0                                                             
         XC    SVMOS,SVMOS                                                      
         MVC   SVEST,SPACES                                                     
         MVC   SVOFF,SPACES                                                     
         MVI   SVSYS,0                                                          
UPD0140  CLI   0(R4),0             END OF RECORD                                
         BE    UPD0240                                                          
         CLI   0(R4),X'1A'         MEDIA TRANSFER ELEMENT                       
         BE    UPD0160                                                          
         CLI   0(R4),X'23'         OTHERS ELEMENT                               
         BE    UPD0170                                                          
         CLI   0(R4),X'44'         TRANSACTION ELEMENT                          
         BE    UPD0180                                                          
         CLI   0(R4),X'46'         EXTRA PAYMENT ELEMENT                        
         BE    UPD0190                                                          
         CLI   0(R4),X'6A'         MEDIA TRANSFER (PACKED DATA)                 
         BE    UPD0160                                                          
         CLI   0(R4),X'E5'         GENERAL DATE ELEMENT                         
         BE    UPD0220                                                          
         CLI   0(R4),X'F3'         NEW BILLING XFER(ONLY) ELEMENT               
         BE    UPD0230                                                          
UPD0150  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     UPD0140                                                          
*                                                                               
         USING MDTELD,R4                                                        
UPD0160  CLI   MDTSYS,C'J'         IS SYSTEM PRODUCTION?                        
         BE    UPD46               YES NO POINTER NEEDED                        
*                                                                               
         MVC   SVSYS,MDTSYS        SYSTEM                                       
*                                                                               
         LA    R1,PSYSTAB          TABLE OF EQUIVALENT SYSTEMS-PRINT            
UPD0162  CLI   0(R1),X'FF'                                                      
         BE    UPD0168                                                          
         CLC   MDTSYS,0(R1)        IF MATCH ON SYSTEM THAN THE ACTUAL           
         BE    UPD0165             SYSTEM MUST BE PRINT                         
         LA    R1,1(R1)                                                         
         B     UPD0162                                                          
*                                                                               
UPD0165  MVI   SVSYS,C'P'          YES SO SAVE AS PRINT IN SVSYS                
UPD0168  MVC   SVMOS,MDTMOS        MOVE IN MONTH OF SERVICE                     
         MVC   SVEST,MDTEST        SAVE CHARACTER ESTIMATE                      
         OC    SVEST,SPACES                                                     
         B     UPD0150                                                          
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4                                                        
UPD0170  CHI   RF,23                                                            
         BH    UPD0150                                                          
         CLI   OTHPROF-3,C' '      IF SOMETHING IN THIS FIELD THAN NO           
         BH    UPD0150             MOS IN THIS ELEMENT                          
         LA    RF,23                                                            
         MVC   SVMOS,OTHDATE                                                    
         B     UPD0150                                                          
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4                                                        
UPD0180  MVC   SVOFF,TRNOFFC                                                    
         B     UPD0150                                                          
         DROP  R4                                                               
*                                                                               
         USING XPYELD,R4                                                        
UPD0190  OC    SVMOS,SVMOS                                                      
         BNZ   UPD0210                                                          
         CLC   XPYPER,SPACES       CHECK FOR ANY PERIOD DATE(S)                 
         BNH   UPD0210             NO - SKIP                                    
         CLI   TRNKCULA+3,C'N'       IF NETWORK                                 
         BE    *+8                                                              
         OI    FLAG,FLGBRD           GET DATE FROM BROADCAST CAL                
         MVC   WORK(L'XPYPER),XPYPER                                            
         LA    R5,WORK                                                          
         CLI   WORK+6,C' '                                                      
         BNH   *+8                                                              
         LA    R5,WORK+6                                                        
         CLI   WORK+6,C'-'                                                      
         BNE   *+8                                                              
         LA    R5,WORK+7                                                        
         TM    FLAG,FLGBRD                                                      
         BNO   UPD0200                                                          
         GOTO1 VGETBRD,DMCB,(1,(R5)),WORK+20,VGETDAY,VADDAY                     
         LA    R5,WORK+20                                                       
UPD0200  GOTO1 VDATCON,DMCB,(0,(R5)),(1,SVMOS)                                  
*                                                                               
UPD0210  CLC   XPYEST,SPACES       IS THERE AN ESTIMATE?                        
         BE    UPD0150                                                          
         OC    XPYEST,XPYEST                                                    
         BZ    UPD0150                                                          
         LH    RE,XPYEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST(3),DUB           UNPACK THE ESTIMATE                       
         B     UPD0150                                                          
         DROP  R4                                                               
*                                                                               
         USING GDAELD,R4                                                        
UPD0220  CLI   GDATYPE,GDAMMOS     IS THIS AN MOS DATE?                         
         BNE   UPD0150                                                          
         LA    RF,X'E5'                                                         
         MVC   SVMOS,GDAYYMM                                                    
         B     UPD0150                                                          
         DROP  R4                                                               
*                                                                               
         USING MBIELD,R4                                                        
UPD0230  CHI   RF,X'E5'                                                         
         BE    *+10                                                             
         MVC   SVMOS,MBIMOS                                                     
         MVC   SVEST,MBIEST                                                     
         B     UPD0150                                                          
         DROP  R4                                                               
*                                                                               
         USING AAVPASD,R4                                                       
UPD0240  LA    R4,KEY                                                           
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,TRNKCPY                                                  
         MVC   AAVPCLT,TRNKCACT+9  LAST 3 CHAR OF CONTRA IS CLIENT              
         MVC   AAVPPRD,TRNKREF     1ST 3 CHAR OF REFERENCE IS PRODUCT           
         MVC   AAVPEST,SVEST       ESTIMATE                                     
         MVC   AAVPMOS,SVMOS       MOS                                          
         LA    RF,LDSYTAB          TABLE OF LEDGER/SYSTEM                       
UPD0242  CLI   0(RF),X'FF'         IF NOT IN TABLE THAN NO PASSIVE FOR          
         BE    UPD0250             THIS LEDGER.                                 
         CLC   TRNKLDG,0(RF)                                                    
         BE    UPD0244                                                          
         LA    RF,2(RF)                                                         
         B     UPD0242                                                          
*                                                                               
UPD0244  MVC   AAVPSYS,1(RF)       SYSTEM                                       
         CLI   AAVPSYS,C'S'        IF IT'S SPOT CHECK TO SEE IF IT'S            
         BNE   *+16                REALLY NET BY CHECKING THE 1ST CHAR          
         CLI   TRNKACT,C'N'        OF THE ACCOUNT FOR 'N' (SSN)                 
         BNE   *+8                                                              
         MVI   AAVPSYS,C'N'                                                     
         MVC   AAVPOFF,SVOFF       OFFICE                                       
         MVC   AAVPACCT,TRNKLDG    STATION (SS ACCOUNT)                         
         MVC   AAVPKDA,IODA        DISK ADDRESS                                 
         MVI   AAVPFLG1,0          SET FOR GOOD ESTIMATE AS DEFAULT             
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   AAVPKEY(AAVPFLG1-AAVPKEY),KEYSAVE                                
         BNE   UPD0250             END OF LEDGER                                
*                                                                               
         CLI   BYTE,X'FF'                                                       
         BE    UPD0245                                                          
         NI    AAVPSTAT,X'FF'-AAVPAPP   DESELECT                                
         CLI   BYTE,0                   TEST DESELECTING                        
         BE    *+8                                                              
         OI    AAVPSTAT,AAVPAPP          SELECT                                 
*                                                                               
UPD0245  GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO1Q                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPD0250  MVC   KEY,SVKEY                                                        
         GOTO1 AIOEXEC,IORD+IOACCDIR+IO1Q                                       
         BE    *+6                 RE-ESTABLISH SEQUENCE                        
         DC    H'0'                                                             
*                                                                               
UPD46    MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
                                                                                
UPD48    ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD08                                                            
                                                                                
UPD50    OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD60               NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         LA    R1,MRKSCRH                                                       
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD52               NO                                           
         LA    R1,MRKOPTH          YES - SET OPTION FIELD                       
UPD52    CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX             PRTCLO HAS SET MESSAGE                       
                                                                                
UPD60    LA    R1,MRKACTH                                                       
         NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
         MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
                                                                                
UPDATEX  ST    R1,FVADDR           STORE FIELD ADDRESS                          
         XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         B     EXIT                                                             
         DROP  R2,R3,RF                                                         
*                                  TABLE OF LEDGER AND SYSTEM                   
LDSYTAB  DC    C'SS'               LEDGER S - SYS SPOT (BUT MAYBE NET)          
         DC    C'TS'               LEDGER T - SYS SPOT                          
         DC    C'PP'               LEDGER P - SYS PRINT                         
         DC    C'QP'               LEDGER Q - SYS PRINT                         
         DC    C'UN'               LEDGER U - SYS NET                           
         DC    X'FF'                                                            
*                                                                               
*                                   TABLE OF PRINT SYSTEMS                      
PSYSTAB  DC    C'I'                 INTERACTIVE                                 
         DC    C'L'                 SOCIAL                                      
         DC    C'B'                 MOBILE                                      
         DC    C'V'                 NATIONAL VIDEO                              
         DC    C'W'                 LOCAL VIDEO                                 
         DC    C'D'                 DIGITAL AUDIO                               
         DC    X'FF'                                                            
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
* ROUTINE TO SELECT/DESELECT ALL TRANSACTIONS                         *         
***********************************************************************         
                                                                                
MRKALL   NTR1  ,                                                                
         LA    R1,1                                                             
MRKALL2  STH   R1,HALF                                                          
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    MRKALL8                                                          
         GOTO1 AFILTER             FILTER TRANSACTION                           
         BNE   MRKALL8             NOT REQUIRED - GET NEXT                      
         CLC   OPTALL,AC@YES       TEST IF USER IS MARKING                      
         BNE   MRKALL4                                                          
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    MRKALL8                                                          
         OI    TSARINDS,TSARMKQ                                                 
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
*&&UK*&& AP    CURMRK,TSARAFCA     ADD TO MARKED (AFC)                          
*&&UK*&& SP    CURBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
*&&UK*&& BO    *+20                                                             
*&&US*&& BO    *+14                                                             
         AP    REPMRK,TSARAMNT     NO, ADD TO MARKED THIS SESSION               
*&&UK*&& AP    RCPMRK,TSARAFCA     NO, ADD TO MARKED (AFC)                      
         B     MRKALL6                                                          
         SP    REPUMK,TSARAMNT     YES, MUST HAVE BEEN ADDED TO UNMARKD         
*&&UK*&& SP    RCPUMK,TSARAFCA     YES, MUST HAVE BEEN ADDED (AFC)              
         B     MRKALL6                                                          
                                                                                
MRKALL4  TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    MRKALL8                                                          
         NI    TSARINDS,255-TSARMKQ                                             
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
*&&UK*&& SP    CURMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
*&&UK*&& AP    CURBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         TM    TSARINDS,TSARINMQ   IS TRANS MARKED ON FILE?                     
*&&UK*&& BO    *+20                                                             
*&&US*&& BO    *+14                                                             
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO MARKED           
*&&UK*&& SP    RCPMRK,TSARAFCA     NO, MUST HAVE BEEN ADDED (AFC)               
         B     MRKALL6             THIS SESSION, SO SUBTRACT IT                 
         AP    REPUMK,TSARAMNT     YES, ADD TO UNMARKED THIS SESSION            
*&&UK*&& AP    RCPUMK,TSARAFCA     YES, ADD TO UNMARKED (AFC)                   
         B     MRKALL6                                                          
                                                                                
MRKALL6  L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         B     MRKALL8                                                          
         DROP  RF                                                               
                                                                                
MRKALL8  LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX                                                        
         BNH   MRKALL2                                                          
                                                                                
         LA    R1,MRKOPTH          SET CURSOR TO OPTIONS FIELD                  
         ST    R1,FVADDR                                                        
                                                                                
MRKALLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                          *         
***********************************************************************         
                                                                                
RFILTER  NTR1  ,                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        TEST THIS IS A TRANSACTION                   
         BZ    RFILTNEQ                                                         
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   RFILTERX                                                         
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    RFILTNEQ                                                         
         CLI   ICLMARK,ICLMYES     INCLUDE SELECTED                             
         BE    RFILT02                                                          
         LA    RF,X'10'            BO IF EXCLUDING SELECTED                     
         CLI   ICLMARK,ICLMNO                                                   
         BE    *+8                                                              
         LA    RF,X'80'            BZ IF SELECTED ONLY                          
         TM    TRNSTAT,TRNSAPPR                                                 
         EX    RF,*+4                                                           
         NOP   RFILTNEQ            BO OR BZ                                     
                                                                                
RFILT02  OC    OFFICE,OFFICE       TEST OFFICE FILTER SET                       
         BZ    RFILT04                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            WRONG OFFICE                                 
         CLC   TRNOFFC(0),OFFICE                                                
                                                                                
RFILT04  GOTO1 ABLDSRC             BUILD SOURCE A/C IN SRCWORK                  
                                                                                
         OC    SRCACC,SRCACC       TEST FILTER FOR SOURCE A/C                   
         BZ    RFILT06                                                          
         IC    RF,SRCACCXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            SOURCE ACCOUNT DOES NOT MATCH                
         CLC   SRCWORK(0),SRCACC                                                
                                                                                
         USING TRSELD,R2                                                        
RFILT06  ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE                           
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
*&&UK                                                                           
         USING GDAELD,R2                                                        
         CLI   ICLMARK,ICLMONLY    TEST EXCLUSIVELY DESELECTING                 
         BNE   RFILT10                                                          
         OC    ERPDSTA,ERPDSTA     IF START=X'000000'                           
         BNZ   RFILT08                                                          
         OC    ERPDEND,ERPDEND     AND END=X'FFFFFF'                            
         BO    RFILT10             NO FILTER                                    
RFILT08  ICM   R2,15,AGDAERPD                                                   
         BZ    RFILTNEQ            EARLIEST PAYMENT DATE MISSING                
         CLC   GDADATE,ERPDSTA     TEST ACTIVITY DATE                           
         BL    RFILTNEQ                                                         
         CLC   GDADATE,ERPDEND                                                  
         BH    RFILTNEQ                                                         
                                                                                
RFILT10  DS    0H                                                               
*&&                                                                             
RFILTEQU CR    RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTNEQ LTR   RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTERX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
*                                                                     *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
*        R2=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
SETKEY   NTR1  ,                                                                
         STC   R1,WORK                                                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY0                                                          
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                SUPPLIER MISSING                             
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
         MVC   TRNKOFF,OFFICE                                                   
                                                                                
SETKEY1  TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCULC+L'TRNKCULC-1,X'41'                                      
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
* GENERAL EXIT                                                        *         
***********************************************************************         
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
                                                                                
LDGLIST  DS    0CL1                VALID LEDGERS IN UNIT S                      
*&&UK*&& DC    C'FTVX'                                                          
*&&US*&& DC    C'PQSTUVWXY'                                                     
         DC    AL1(EOT)                                                         
                                                                                
DUMCON   DC    C'SJ999'                                                         
OACCMST  DC    C'ACCMST  '                                                      
         EJECT                                                                  
                                                                                
SUPNDSP  EQU   19                                                               
                                                                                
OVRWRKD  DSECT                                                                  
                                                                                
* ACMRKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMRKWRK                                                       
         PRINT ON                                                               
SAVED    DSECT                                                                  
         ORG   SOVRWRK             ** OVERLAY SAVED W/S REDEFINED **            
SVEST    DS    CL6                 SAVED ESTIMATE                               
SVMOS    DS    XL2                 SAVED MOS                                    
         DS    CL1                 SPARE                                        
SVOFF    DS    CL2                 SAVED OFFICE                                 
SVSYS    DS    CL1                 SAVED SYSTEM                                 
SVKEY    DS    XL(L'KEY)                                                        
* FLAG BYTE                                                                     
FLGBRD   EQU   X'40'               GET DATE FROM BRAODCAST CALENDAR             
         ORG   SOVRWRK+L'SOVRWRK                                                
         ORG   TOTALS                                                           
TOTCRS   DS    PL8                                                              
TOTMRK   DS    PL8                                                              
TOTBAL   DS    PL8                                                              
REPMRK   DS    PL8                                                              
REPUMK   DS    PL8                                                              
*&&UK                                                                           
         ORG   CURTOTS                                                          
CURCRS   DS    PL8                                                              
CURMRK   DS    PL8                                                              
CURBAL   DS    PL8                                                              
RCPMRK   DS    PL8                                                              
RCPUMK   DS    PL8                                                              
*&&                                                                             
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKF4D                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE3D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039ACMRK04   07/12/18'                                      
         END                                                                    
