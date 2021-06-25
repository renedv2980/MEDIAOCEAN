*          DATA SET ACMRK08    AT LEVEL 030 AS OF 07/12/18                      
*PHASE T61608A                                                                  
ACMRK08  TITLE 'CREDITOR - HOLD (FROM PAYMENT)'                                 
**********************************************************************          
* JFOX 18 USE SORAWRK IF AVAILABLE INSTEAD OF CPJWRK                 *          
* JFOX 19 YEAR 2000 FIX                                              *          
* RGUP 030 21MAY18  <SPEC-20692> ADDITIONAL MEDIA FOR DIGIAL AUDIO   *          
**********************************************************************          
ACMRK08  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MRK8**,RA                                                    
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
         STH   R1,DISPHED2         DISPLACEMENT OF 2ND INPUT HEADLINE           
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
         BO    NXTACC              SPECIAL ROUTINE TO SET NEXT ACCOUNT          
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTHOLD                                                  
         BE    *+6                 HOLD                                         
         DC    H'0'                                                             
         CLI   TWASCROV,CHSCR1                                                  
         BE    VALHED              HOLD - VALIDATE HEADER                       
         CLI   TWASCROV,CHSCR2                                                  
         BE    VALINP              HOLD - VALIDATE INPUT                        
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
         XC    HOLLDG,HOLLDG                                                    
         XC    HOLSUP,HOLSUP                                                    
         OI    HOLLDGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    HOLSUPH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   HOLLDG(L'ACTKLDG),ACTKLDG                                        
         MVC   HOLSUP(L'ACTKACT),ACTKACT                                        
         XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
*                                                                               
PREVAL02 TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    PREVAL04                                                         
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),('TYPCRD',FULL)                
         BE    PREVAL06                                                         
                                                                                
PREVAL04 DS    0H                                                               
*&&UK                                                                           
         XC    HOLCURT,HOLCURT                                                  
         OI    HOLCURTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    HOLCURH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    HOLCURH+(FVOIND-FVIHDR),FVOXMT                                   
*&&                                                                             
PREVAL06 DS    0H                                                               
                                                                                
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD                           *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   HOLSUPH+(FVILEN-FVIHDR),0  TEST NO INPUT                         
         BNE   *+16                                                             
         XC    TWASKEY,TWASKEY     YES - CLEAR KEY SAVED IN TWA                 
         XC    ACCOUNT,ACCOUNT     AND MUST CLEAR SUPPLIER ACCOUNT              
         OC    LEDGER,LEDGER       TEST LEDGER ALEADY SET                       
         BNZ   *+12                                                             
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
         BNE   NXTACCX             INVALID LEDGER                               
         OC    TWASKEY,TWASKEY     TEST KEY SAVED IN TWA                        
         BZ    *+14                                                             
         CLC   HOLLDG,TWASKEY+L'ACTKUNT  TEST LEDGER MATCHES SAVED              
         BNE   *+14                                                             
         CLC   HOLLDG,LEDGER+L'ACTKUNT  TEST LEDGER CHANGED                     
         BE    NXTACC4                                                          
         XC    TWASKEY,TWASKEY     CLEAR KEY SAVED IN TWA                       
         XC    ACCOUNT,ACCOUNT     (RE)SET ACCOUNT TO ZERO                      
         CLC   HOLLDG,LEDGER+L'ACTKUNT  TEST NEW LEDGER TO BE VALIDATED         
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
         LA    R1,HOLLDGH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC18 MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 VACSRCHC,DMCB,HOLSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
                                                                                
         OI    HOLSUPH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         MVC   FVMSGNO,=AL2(IAEPAP1N)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,HOLSUPH                                                       
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
         TM    HOLLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDGX                                                          
         XC    HOLLDGN,HOLLDGN                                                  
         OI    HOLLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,HOLLDGH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
         LA    R1,LDGLIST          R1=A(LIST OF VALID LEDGERS)                  
VALLDG2  CLC   HOLLDG,0(R1)                                                     
         BE    VALLDG4             LEDGER IN LIST - VALIDATE IT                 
         CLI   0(R1),EOT                                                        
         BE    *+12                                                             
         LA    R1,L'LDGLIST(R1)                                                 
         B     VALLDG2                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALLERR             ERROR - NOT IN VALID LEDGER LIST             
                                                                                
VALLDG4  MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALLDG,HOLLDGH                                                  
         BH    VALLERR                                                          
         MVC   HOLLDGN,RECNAME     NAME EXTRACTED BY GETLDG                     
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
                                                                                
VALSUP   TM    HOLSUPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSUPX                                                          
         MVI   FVMINL,1                                                         
*&&UK*&& XC    PRSTCURT,PRSTCURT                                                
         GOTO1 AVALSUP,HOLSUPH                                                  
         BNE   EXIT                                                             
         GOTO1 VACSRCHC,DMCB,HOLSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
VALSUPX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    HOLOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    HOLOFFN,HOLOFFN                                                  
         OI    HOLOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,HOLOFFH                                                  
         BL    VALOFFX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   HOLOFFN,RECNAME                                                  
         OI    HOLOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
VALOFFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    HOLCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    HOLCONN,HOLCONN                                                  
         OI    HOLCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         CLI   HOLCONH+(FVILEN-FVIHDR),L'DUMCON  TEST L'DUMMY CONTRA            
         BNE   VALCON2                           NO - VALIDATE                  
         CLC   HOLCON(L'DUMCON),DUMCON           COMPARE WITH DUMMY             
         BNE   VALCON2                           NO - VALIDATE                  
         MVC   CONTRA,SPACES                     YES - SET DUMMY CONTRA         
         MVC   CONTRA(L'ACTKCPY),COMPANY                                        
         MVC   CONTRA+L'ACTKCPY(L'DUMCON),DUMCON                                
         OI    HOLCONH+(FVOIND-FVIHDR),FVOXMT    RE-TRANSMIT                    
         MVI   CONTRAXL,L'TRNKCULC-1                                            
         MVI   CONTIND,CONTILOQ    SET CONTRA IS A BONA FIDO ACCOUNT            
         B     VALCONX                                                          
VALCON2  GOTO1 AVALCON,HOLCONH                                                  
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   HOLCONN,RECNAME                                                  
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SOURCE                                                     *         
***********************************************************************         
                                                                                
VALSRC   TM    HOLSRCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSRCX                                                          
         XC    HOLSRCN,HOLSRCN                                                  
         OI    HOLSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALSRC,HOLSRCH                                                  
         BH    EXIT                                                             
         BL    VALSRCX             NOT REQUIRED, NOT INPUT                      
         MVC   HOLSRCN,RECNAME                                                  
         OI    HOLSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
VALSRCX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   TM    HOLREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,HOLREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE PERIOD                                                     *         
***********************************************************************         
                                                                                
VALPER   TM    HOLPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,HOLPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE PERIOD                                       *         
***********************************************************************         
                                                                                
VALADA   TM    HOLADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,HOLADAH                                                  
         BH    EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE MOS RANGE                                                  *         
***********************************************************************         
                                                                                
VALMOS   TM    HOLMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,HOLMOAH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE INCLUDE HELD                                                         
***********************************************************************         
                                                                                
VALHOL   TM    HOLHOLH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALHOLX                                                          
         GOTO1 AVALICL,HOLHOLH                                                  
         BH    EXIT                                                             
VALHOLX  DS    0H                                                               
                                                                                
                                                                                
*&&UK                                                                           
***********************************************************************         
* VALIDATE CURRENCY FILTER                                            *         
***********************************************************************         
                                                                                
VALCUR   TM    HOLCURH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCURX                                                          
         GOTO1 AVALCUR,HOLCURH     PUT VALID CURRENCIES INTO TABLE              
         BH    EXIT                                                             
         BE    VALCURX                                                          
         PUSH  USING               TEST UNACCEPTABLE CURRENCY FILTER            
PRESET   USING CURTABD,PRSTCURT                                                 
         CLC   PRESET.CURTCUR,HOLCUR                                            
         BNE   *+14                                                             
         CLC   HOLCUR+L'CURTCUR(L'HOLCUR-L'CURTCUR),SPACES                      
         BNH   VALCURX                                                          
         MVC   HOLCUR,SPACES       UNACCEPTABLE CURRENCY FILTER                 
         OI    HOLCURH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   HOLCUR,C'*'                                                      
         MVI   HOLCURH+(FVILEN-FVIHDR),1                                        
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    *+14                                                             
         MVC   HOLCUR(L'CURTCUR),PRESET.CURTCUR                                 
         MVI   HOLCURH+(FVILEN-FVIHDR),L'CURTCUR                                
         MVC   FVMSGNO,=AL2(AI$CURFC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         POP   USING                                                            
VALCURX  DS    0H                                                               
*&&                                                                             
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
         MVI   TSARLEN+1,TSARCHL                                                
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
*&&UK                                                                           
         NI    SAFCIND1,FF-SAFCIGBP                                             
         XC    FORECURT,FORECURT    SET UP CURRENCY FIRST TIME THROUGH          
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
         BE    READ08                                                           
         CLC   TRNKCULC(0),CONTRA                                               
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
         TM    CONTIND,CONTILOQ    TEST REAL LOW LEVEL ACCOUNT                  
         BNZ   READ12              YES - DON'T SET NEXT                         
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
READ22   TM    TRNSTAT,TRNSAPPR    TEST ALREADY SELECTED FOR PAYMENT            
         BNO   READ24                                                           
         OI    TSARINDS,TSARDISQ   YES - DISPLAY ONLY                           
         MVI   TSARCHA,C'*'                                                     
         MVI   TSARVAR,TRNSAPPR    SET VARIABLE KEY BYTE                        
         TM    TRNSTAT,TRNSHOLD                                                 
         BNO   READ26                                                           
         OI    TSARVAR,TRNSHOLD    ADD HELD STATUS                              
         B     READ26                                                           
                                                                                
READ24   TM    TRNSTAT,TRNSHOLD                                                 
         BNO   READ26                                                           
         MVI   TSARVAR,TRNSHOLD    SET VARIABLE KEY BYTE FOR SORT SEQ.          
         OI    TSARINDS,TSARMKQ+TSARINMQ                                        
                                                                                
READ26   MVC   TSARFSAC,SRCWORK    EXTRACT ANY SOURCE ACCOUNT                   
*&&UK                                                                           
         USING SORELD,R2                                                        
         ICM   R2,15,ASOREL        USE SOREL WORKCODE, IF PRESENT               
         BZ    READ28                                                           
         CLI   SORSYS,SORSACC      TEST ACCOUNTING SOURCE A/C                   
         BNE   READ28                                                           
         CLI   SORLN,SORAL2Q       TEST LONG ACCOUNTING ELEMENT                 
         BL    READ28                                                           
         MVC   TSARFWRK,SORAWRK    YES TAKE WORKCODE                            
         B     READ30                                                           
                                                                                
         USING CPJELD,R2                                                        
READ28   ICM   R2,15,ACPJEL                                                     
         BZ    READ30                                                           
         MVC   TSARFWRK,SPACES                                                  
         CLI   CPJTYPE,CPJTJOB     TEST PRODUCTION TYPE                         
         BNE   READ30                                                           
         MVC   TSARFWRK,CPJWRK                                                  
         B     READ30                                                           
                                                                                
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
READ30   ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
*&&                                                                             
*                                                                               
         GOTO1 ABLDMOS             GET MEDIA MOS                                
         GOTO1 ABLDWRKC            GET WORKCODE                                 
*                                                                               
*        USING GDAELD,R2           EXTRACT MEDIA MOS                            
*        ICM   R2,15,AGDAMMOS      MEDIA MONTH OF SERVICE                       
*        BZ    *+14                                                             
*        MVC   TSARFMMD,GDAYYMM                                                 
*        B     READ31                                                           
*                                                                               
*        USING MBIELD,R2           EXTRACT MEDIA MOS                            
*        ICM   R2,15,AMBIEL                                                     
*        BZ    *+14                                                             
*        MVC   TSARFMMD(L'MBIMOS),MBIMOS                                        
*        B     READ31                                                           
*                                                                               
*        USING OTHELD,R2           EXTRACT MEDIA MOS                            
*EAD28A  ICM   R2,15,AOTHEL                                                     
*        BZ    READ31                                                           
*        MVC   TSARFMMD(L'OTHDATE),OTHDATE                                      
*                                                                               
         USING DUEELD,R2           EXTRACT DUEEL VALUES                         
READ31   ICM   R2,15,ADUEEL                                                     
         BZ    *+14                                                             
         MVC   TSARFDUE,DUEDATE     SAVE OFF DUE DATE                           
         B     READ31A                                                          
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(2,TSARFDUE)                            
*                                                                               
         USING SCIELD,R2           EXTRACT SCIEL VALUES                         
READ31A  ZAP   TSARFDIS,PZERO      DISCOUNT                                     
         ICM   R2,15,ASCIEL                                                     
         BZ    READ32                                                           
         CLI   SCITYPE,SCITCDSC    TEST LIVE DISCOUNT TYPE                      
         BNE   READ32                                                           
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         ZAP   TSARFDIS,SCIAMNT                                                 
         CLI   PROFDISC,C'Y'       TEST ADDING ANY CASH DISCOUNT                
         BNE   READ32                                                           
         AP    TSARAMNT,SCIAMNT    ADD TO TRANSACTION AMOUNT                    
                                                                                
READ32   DS    0H                                                               
*&&US                                                                           
         GOTO1 ABLDINV             GET LONG INVOICE NUMBER                      
*                                                                               
         USING XPYELD,R2                                                        
         ICM   R2,15,AXPYEL                                                     
         BZ    READ34                                                           
*        MVC   TSARFINV(L'XPYINV),XPYINV                                        
         CP    XPYCD,PZERO                                                      
         BE    READ34                                                           
         ZAP   TSARFDIS,XPYCD                                                   
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         CLI   PROFDISC,C'Y'       TEST ADDING ANY CASH DISCOUNT                
         BNE   READ34                                                           
         AP    TSARAMNT,XPYCD      ADD TO TRANSACTION AMOUNT                    
         B     READ34                                                           
                                                                                
*        USING FFTELD,R2                                                        
*EAD33   ICM   R2,15,AFFTLEL                                                    
*        BZ    READ33A                                                          
*        XR    RE,RE                                                            
*        IC    RE,FFTDLEN                                                       
*        C     RE,=F'20'                                                        
*        BNH   *+8                                                              
*        LA    RE,20                                                            
*        BCTR  RE,0                TAKE MAX OF 20 CHAR                          
*        EX    RE,*+8                                                           
*        B     *+10                                                             
*        MVC   TSARFINV(0),FFTDATA                                              
*        B     READ34                                                           
*EAD33A  CLC   TSARFINV,SPACES                                                  
*        BH    READ34                                                           
*        MVC   TSARFINV(L'TSARREF),TSARREF                                      
*&&                                                                             
         USING TRSELD,R2                                                        
READ34   ICM   R2,15,ATRSEL        R2=A(TRANSACTION STATUS ELEMENT)             
         MVC   TSARADAT,TRSDATE    EXTRACT TRANSACTION ACTIVITY DATE            
         MVC   TSARSSTA,TRSSTAT    AND STATUS ELEMENT STATUS BYTE               
                                                                                
         OC    ANOTELS,ANOTELS     TEST MEMO ITEMS ATTACHED                     
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
                                                                                
*&&UK*&& GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
                                                                                
         GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ36              DEAL WITH ERROR                              
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
         TM    TRNSTAT,TRNSHOLD                                                 
         BNO   *+8                                                              
         LA    RF,TOTMRK           RF=A(HELD TOTAL)                             
*&&UK                                                                           
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         AP    CURTOTS-TOTALS(L'TOTALS,RF),AFCAMNT                              
         DROP  R3                                                               
*&&                                                                             
         AP    0(L'TOTALS,RF),TRNAMNT                                           
         MVI   ANYADD,1            SET TSAR RECORD(S) ADDED                     
         B     READ02              READ SEQUENTIAL                              
                                                                                
READ36   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                                                             
         B     DISPTRN                                                          
                                                                                
READTRNX NI    DISIND,255-DISIOFLO  CLEAR OVERFLOW (DID NOT OCCUR)              
         CLI   ANYADD,1            TEST ANYTHING ADDED                          
         BE    DISPTRN                                                          
                                                                                
         LA    R1,HOLSUPH          NO - SET CURSOR TO SUPPLIER FIELD            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     EXIT                                                             
         DROP  R1,R2                                                            
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
*                                                                               
DTRN02   OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    DTRN04              NO                                           
         GOTO1 ADISGRID            YES                                          
*        NI    DISIND,X'FF'-DISIRST                                             
         B     DTRN06                                                           
DTRN04   GOTO1 ADISPLAY                                                         
         TM    DISIND,DISIOFLO                                                  
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNWRN)                                           
DTRN06   L     R2,ADISTOTS                                                      
         GOTO1 ABLDTOT,(R2)                                                     
*                                                                               
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
         NI    DISIND,X'FF'-DISINIT  REINITIALIZE FOR GRIDS                     
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
*                                                                               
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
*                                                                               
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
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO UNMARKED         
*&&UK*&& SP    RCPMRK,TSARAFCA     NO, MUST HAVE BEEN ADDED (AFC)               
         B     VALINP10            THIS SESSION, SO SUBTRACT IT                 
         AP    REPUMK,TSARAMNT     YES, ADD TO UNMARKED THIS SESSION            
*&&UK*&& AP    RCPUMK,TSARAFCA     YES, ADD TO UNMARKED (AFC)                   
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
         LA    R1,MRKOPTH                                                       
         GOTO1 AUPDGRID            READ IN DATA AND CLEAR SCREEN                
         BNE   UPDATEX             HAVEN'T COME TO END OF DATA                  
*                                                                               
UPD02    TM    TWAMODE2,TWAM2CHG                                                
         BO    UPD0010                                                          
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         LA    R1,MRKSCRH                                                       
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPDATEX                                                          
         LA    R1,MRKOPTH          YES - SET OPTION FIELD                       
         B     UPDATEX                                                          
                                                                                
UPD0010  OC    PRTSUB,PRTSUB                                                    
         BZ    UPD0020                                                          
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD0015                                                          
         GOTO1 ABLDDIS             GET DISPLACEMENTS FROM PROFILE               
UPD0015  GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@HELD),LC@HELD                            
         LA    R1,REPH5+L'DISLLINE+1+L'LC@HELD-1                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
UPD0020  LA    R1,INPMRKH                                                       
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    *+8                                                              
         LA    R1,MRKOPTH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,TEMP                                                        
*                                                                               
UPD0030  GOTO1 ATSARGET,TEMP                                                    
         BE    UPD0040                                                          
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD0250                                                          
         DC    H'0'                                                             
                                                                                
UPD0040  TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    UPD0055                                                          
         TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD0050                                                          
         TM    TSARSTA,TRNSHOLD    TEST ALREADY HELD                            
         BO    UPD0240                                                          
         MVI   BYTE,TRNSHOLD                                                    
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD0060                                                          
         AP    REPMRK,TSARAMNT     ADD TO MARKED TOTALS                         
         B     UPD0060                                                          
                                                                                
UPD0050  TM    TSARSTA,TRNSHOLD    TEST ALREADY HELD                            
         BZ    UPD0055                                                          
*        BZ    UPD0240                                                          
         MVI   BYTE,0                                                           
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD0060                                                          
         AP    REPUMK,TSARAMNT     ADD TO UNMARKED TOTALS                       
         B     UPD0060                                                          
*                                                                               
UPD0055  TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    UPD0240                                                          
         TM    TSARIND3,TSARZEPD+TSARZDUE                                       
         BZ    UPD0240                                                          
         MVI   BYTE,X'FF'                                                       
         B     UPD0070                                                          
*                                                                               
UPD0060  OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD0070             MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD0240             YES - GET NEXT TSAR RECORD                   
                                                                                
UPD0070  MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF    GET ELEMENT ADDRESSES                        
*                                                                               
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BNE   UPD0080                                                          
*                                                                               
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    UPD0090                                                          
*                                                                               
UPD0080  NI    TWAMODE2,255-TWAM2CHG  RESET CHANGES MADE BIT                    
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
UPD0090  CLI   BYTE,X'FF'                                                       
         BE    UPD0095                                                          
         NI    TRNSTAT,255-TRNSHOLD     UNHOLD                                  
         CLI   BYTE,0                   TEST UNHOLDING                          
         BE    *+8                                                              
         OI    TRNSTAT,TRNSHOLD         HOLD                                    
         USING TRSELD,R2                                                        
UPD0095  ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSLN,TRSLNQ        TEST LONG ELEMENT                            
         BNL   UPD0100                                                          
         GOTO1 AEXTRSL                                                          
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD0100  CLI   BYTE,X'FF'                                                       
         BNE   UPD0105                                                          
         MVI   TRSMARK,TRSZOOMQ    SET MARKER TYPE/ACTION                       
         B     UPD0106                                                          
UPD0105  MVI   TRSMARK,TRSMCHQ     SET MARKER TYPE/ACTION IN ELEMENT            
         CLI   BYTE,0                   TEST UNHOLDING                          
         BNE   UPD0106                                                          
         BNE   *+8                                                              
         OI    TRSMARK,TRSMUMQ     SET NEGATIVE ACTION                          
         B     UPD0106D                                                         
UPD0106  TM    PCDRIVEN,PCGRIDQ     ARE WE RUNNING UNDER GRIDS                  
         BZ    UPD0109              NO                                          
         TM    TSARIND3,TSARZEPD    WAS THE EARLIEST PAYMENT DATE CHNGD         
         BNZ   UPD0106A                                                         
         OC    ERPDSTA,ERPDSTA      SELECT - TEST EARLIEST PAYMENT DATE         
         BZ    UPD0106E                                                         
         TM    TSARIND3,TSARZEPD   TEST ZOOM ADDED/CHANGED/DELETED DATE         
         BO    UPD0109                                                          
         USING GDAELD,R2                                                        
UPD0106A ICM   R2,15,AGDAERPD       TEST EARLIEST PAYMENT DATE CARRIED          
         BZ    UPD0106B                                                         
         MVC   GDADATE,ERPDSTA     SET DATE FROM HEADER SCREEN                  
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    UPD0109             NO                                           
         TM    TSARIND3,TSARZEPD   IF YES,WAS THE EARLIEST PAYMENT DATE         
         BZ    UPD0106E                               CHANGED                   
*&&US*&& B     UPD0106D                                                         
*&&UK*&& OC    TSARERPD,TSARERPD                                                
*&&UK*&& BZ    UPD0106D                                                         
*&&UK*&& MVC   GDADATE,TSARERPD    SET DATE FROM HEADER SCREEN                  
*&&UK*&& B     UPD0106E                                                         
UPD0106B LA    R2,ELEMT                                                         
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATERPD                                                 
         MVC   GDADATE,ERPDSTA     SET DATE FROM HEADER SCREEN                  
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRIDS                   
         BZ    UPD0106C            NO                                           
         TM    TSARIND3,TSARZEPD   IF YES WAS THE EARLIEST PAYMENT DATE         
         BZ    UPD0106C                               CHANGED                   
*&&US*&& B     UPD0106E                                                         
*&&UK*&& OC    TSARERPD,TSARERPD                                                
*&&UK*&& BZ    UPD0106E                                                         
*&&UK*&& MVC   GDADATE,TSARERPD    SET DATE FROM HEADER SCREEN                  
UPD0106C GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,GDAELD                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         B     UPD0106E                                                         
UPD0106D ICM   R2,15,AGDAERPD      TEST EARLIEST PAYMENT DATE CARRIED           
         BZ    UPD0106E            NO                                           
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),('GDAELQ',AIOBUFF),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
*                                                                               
UPD0106E DS    0H                                                               
         TM    PCDRIVEN,PCGRIDQ                                                 
         BZ    UPD0109                                                          
         TM    TSARIND3,TSARZDUE                                                
         BZ    UPD0109                                                          
*                                                                               
         L     R2,AIOBUFF          R2=A(DATA RECORD)                            
         USING TRNRECD,R2                                                       
         TM    TRNRSTA2,TRNSUSED   TEST NOT USED DATE                           
         BZ    *+6                 OVERLAY DON'T DISPLAY USED ITEMS             
         DC    H'0'                                                             
*        BO    UPD0108A            OVERLAY DON'T DISPLAY USED ITEMS             
         MVC   TRNRSDUE,TSARFDUE   UPDATE RECORD WITH DUE DATE                  
         USING DUEELD,R2                                                        
         ICM   R2,15,ADUEEL                                                     
         BZ    UPD0108                                                          
         OC    TSARFDUE,TSARFDUE                                                
         BNZ   UPD0107                                                          
         GOTO1 VHELLO,DMCB,(C'D',LACCMST),('DUEELQ',AIOBUFF),0                  
         CLI   12(R1),0                                                         
         BE    UPD0108A                                                         
         DC    H'0'                DIE ON ANY ERROR                             
UPD0107  MVC   DUEDATE,TSARFDUE                                                 
         B     UPD0109                                                          
*                                                                               
UPD0108  LA    R2,ELEMT                                                         
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,TSARFDUE                                                 
         GOTO1 VHELLO,DMCB,(C'P',LACCMST),AIOBUFF,(R2)                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
UPD0108A GOTO1 ASETELAD,AIOBUFF    AND REFRESH ELEMENT ADDRESSES                
*                                                                               
UPD0109  L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         USING TRNRECD,R2                                                       
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   *+12                                                             
         NI    TRNRSTAT,255-TRNSARCH    CLEAR ACCARC INDICATOR                  
         LA    R1,IOADFR+IOACCMST+IO1Q  RE-ADD ACCARC RECORD TO ACCMST          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
         TM    IOCTCOMM,IOADFR          TEST RECORD RE-ADDED TO ACCMST          
         BNO   UPD0110                                                          
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNRSTA),TRNRSTA                         
         L     R2,AIOSAVE               R2=A(SAVED DATA RECORD VALUES)          
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    UPD0110                                                          
         DC    H'0'                                                             
*                                                                               
* UPDATE AUTO APPROVAL POINTERS WITH HELD(UNHELD)                               
*                                                                               
UPD0110  MVC   SVKEY,KEY           SAVE OFF KEY FOR LATER RESTORE               
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF                                                       
*                                                                               
         LA    R4,TRNRFST          FIND TRSEL                                   
         LA    RF,0                                                             
         XC    SVMOS,SVMOS                                                      
         MVC   SVEST,SPACES                                                     
         MVC   SVOFF,SPACES                                                     
         MVI   SVSYS,0                                                          
UPD0120  CLI   0(R4),0             END OF RECORD                                
         BE    UPD0220                                                          
         CLI   0(R4),X'1A'         MEDIA TRANSFER ELEMENT                       
         BE    UPD0140                                                          
         CLI   0(R4),X'23'         OTHERS ELEMENT                               
         BE    UPD0150                                                          
         CLI   0(R4),X'44'         TRANSACTION ELEMENT                          
         BE    UPD0160                                                          
         CLI   0(R4),X'46'         EXTRA PAYMENT ELEMENT                        
         BE    UPD0170                                                          
         CLI   0(R4),X'6A'         MEDIA TRANSFER (PACKED DATA)                 
         BE    UPD0140                                                          
         CLI   0(R4),X'E5'         GENERAL DATE ELEMENT                         
         BE    UPD0200                                                          
         CLI   0(R4),X'F3'         NEW BILLING XFER(ONLY) ELEMENT               
         BE    UPD0210                                                          
UPD0130  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     UPD0120                                                          
*                                                                               
         USING MDTELD,R4                                                        
UPD0140  CLI   MDTSYS,C'J'         IS SYSTEM PRODUCTION?                        
         BE    UPD0240             YES NO POINTER NEEDED                        
*                                                                               
         MVC   SVSYS,MDTSYS        SYSTEM                                       
*                                                                               
         LA    R1,PSYSTAB          TABLE OF EQUIVALENT SYSTEMS-PRINT            
UPD0142  CLI   0(R1),X'FF'                                                      
         BE    UPD0147                                                          
         CLC   MDTSYS,0(R1)        IF MATCH ON SYSTEM THAN THE ACTUAL           
         BE    UPD0145             SYSTEM MUST BE PRINT                         
         LA    R1,1(R1)                                                         
         B     UPD0142                                                          
*                                                                               
UPD0145  MVI   SVSYS,C'P'          YES SO SAVE AS PRINT IN SVSYS                
UPD0147  MVC   SVMOS,MDTMOS        MOVE IN MONTH OF SERVICE                     
         MVC   SVEST,MDTEST        SAVE CHARACTER ESTIMATE                      
         OC    SVEST,SPACES                                                     
         B     UPD0130                                                          
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4                                                        
UPD0150  CHI   RF,23                                                            
         BH    UPD0130                                                          
         CLI   OTHPROF-3,C' '      IF SOMETHING IN THIS FIELD THAN NO           
         BH    UPD0130             MOS IN THIS ELEMENT                          
         LA    RF,23                                                            
         MVC   SVMOS,OTHDATE                                                    
         B     UPD0130                                                          
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4                                                        
UPD0160  MVC   SVOFF,TRNOFFC                                                    
         B     UPD0130                                                          
         DROP  R4                                                               
*                                                                               
         USING XPYELD,R4                                                        
UPD0170  OC    SVMOS,SVMOS                                                      
         BNZ   UPD0190                                                          
         CLC   XPYPER,SPACES       CHECK FOR ANY PERIOD DATE(S)                 
         BNH   UPD0190             NO - SKIP                                    
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
         BNO   UPD0180                                                          
         GOTO1 VGETBRD,DMCB,(1,(R5)),WORK+20,VGETDAY,VADDAY                     
         LA    R5,WORK+20                                                       
UPD0180  GOTO1 VDATCON,DMCB,(0,(R5)),(1,SVMOS)                                  
*                                                                               
UPD0190  CLC   XPYEST,SPACES       IS THERE AN ESTIMATE?                        
         BE    UPD0130                                                          
         OC    XPYEST,XPYEST                                                    
         BZ    UPD0130                                                          
         LH    RE,XPYEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST(3),DUB           UNPACK THE ESTIMATE                       
         B     UPD0130                                                          
         DROP  R4                                                               
*                                                                               
         USING GDAELD,R4                                                        
UPD0200  CLI   GDATYPE,GDAMMOS     IS THIS AN MOS DATE?                         
         BNE   UPD0130                                                          
         LA    RF,X'E5'                                                         
         MVC   SVMOS,GDAYYMM                                                    
         B     UPD0130                                                          
         DROP  R4                                                               
*                                                                               
         USING MBIELD,R4                                                        
UPD0210  CHI   RF,X'E5'                                                         
         BE    *+10                                                             
         MVC   SVMOS,MBIMOS                                                     
         MVC   SVEST,MBIEST                                                     
         B     UPD0130                                                          
         DROP  R4                                                               
*                                                                               
         USING AAVPASD,R4                                                       
UPD0220  LA    R4,KEY                                                           
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,TRNKCPY                                                  
         MVC   AAVPCLT,TRNKCACT+9  LAST 3 CHAR OF CONTRA IS CLIENT              
         MVC   AAVPPRD,TRNKREF     1ST 3 CHAR OF REFERENCE IS PRODUCT           
         MVC   AAVPEST,SVEST       ESTIMATE                                     
         MVC   AAVPMOS,SVMOS       MOS                                          
         LA    RF,LDSYTAB          TABLE OF LEDGER/SYSTEM                       
UPD0222  CLI   0(RF),X'FF'         IF NOT IN TABLE THAN NO PASSIVE              
         BE    UPD0230             FOR THIS LEDGER.                             
         CLC   TRNKLDG,0(RF)                                                    
         BE    UPD0224                                                          
         LA    RF,2(RF)                                                         
         B     UPD0222                                                          
*                                                                               
UPD0224  MVC   AAVPSYS,1(RF)       SYSTEM                                       
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
         BNE   UPD0230             END OF LEDGER                                
*                                                                               
         CLI   BYTE,X'FF'               ARE WE ONLY UPDATING DUE DATE           
         BE    UPD0225                                                          
         NI    AAVPSTAT,X'FF'-AAVPHLD   DESELECT                                
         CLI   BYTE,0                   TEST DESELECTING                        
         BE    UPD0225                                                          
         OI    AAVPSTAT,AAVPHLD         SELECT                                  
*                                                                               
UPD0225  GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO1Q                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPD0230  MVC   KEY,SVKEY                                                        
         GOTO1 AIOEXEC,IORD+IOACCDIR+IO1Q                                       
         BE    *+6                 RE-ESTABLISH SEQUENCE                        
         DC    H'0'                                                             
*                                                                               
UPD0240  SR    R1,R1                                                            
         ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD0030                                                          
                                                                                
UPD0250  OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD0260             NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         LA    R1,MRKSCRH                                                       
         TM    PCDRIVEN,PCGRIDQ    ARE WE RUNNING UNDER GRID                    
         BZ    UPD0255             NO                                           
         LA    R1,MRKOPTH          YES -  SET OPTION FIELD.                     
UPD0255  CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX             PRTCLO HAS SET MESSAGE                       
                                                                                
UPD0260  LA    R1,MRKACTH                                                       
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
* ROUTINE TO HOLD/UNHOLD ALL TRANSACTIONS                             *         
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
         B     MRKALL6             THIS SESSION, SO SUBTRACT IT                 
                                                                                
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
         SP    REPMRK,TSARAMNT     NO, MUST HAVE BEEN ADDED TO UNMARKED         
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
         OI    TWAMODE2,TWAM2CHG   SET CHANGES BIT                              
         B     MRKALL8                                                          
         DROP  RF                                                               
                                                                                
MRKALL8  LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX                                                        
         BNH   MRKALL2                                                          
                                                                                
MRKALLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                          *         
***********************************************************************         
                                                                                
RFILTER  NTR1  ,                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        TEST THIS IS A TRANSACTION                   
         BZ    RFILTNEQ            NO - SET CC NEQ AND EXIT                     
         GOTO1 ATCLOSE             TEST CLOSED/LIMIT ACCESS USING OFFAL         
         BNE   RFILTERX            EXIT WITH CC NEQ                             
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    RFILTNEQ            YES - SET CC NEQ AND EXIT                    
                                                                                
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BZ    RFILT02                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            WRONG OFFICE                                 
         CLC   TRNOFFC(0),OFFICE                                                
                                                                                
RFILT02  CLI   ICLMARK,ICLMYES     INCLUDE HELD                                 
         BE    RFILT04                                                          
         LA    RF,X'10'            BO IF EXCLUDING HELD                         
         CLI   ICLMARK,ICLMNO                                                   
         BE    *+8                                                              
         LA    RF,X'80'            BZ IF HELD ONLY                              
         TM    TRNSTAT,TRNSHOLD                                                 
         EX    RF,*+4                                                           
         NOP   RFILTNEQ            BO OR BZ                                     
                                                                                
         USING TRSELD,R2                                                        
RFILT04  ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO TRANSACTION STATUS ELEMENT                
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE RANGE                     
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
                                                                                
         GOTO1 ABLDSRC             BUILD SOURCE ACCOUNT IN SRCWORK              
                                                                                
         OC    SRCACC,SRCACC       TEST ANY SOURCE A/C FILTER INPUT             
         BZ    RFILTEQU                                                         
         IC    RF,SRCACCXL                                                      
         EX    RF,*+8                                                           
         B     RFILTERX            EXIT WITH CC SET EQU/NEQ                     
         CLC   SRCWORK(0),SRCACC                                                
                                                                                
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
         IC    RE,OFFICEXL                                                      
         EX    RE,*+4                                                           
         MVC   TRNKOFF(0),OFFICE                                                
                                                                                
SETKEY1  TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCULC+L'TRNKCULC-1,X'41'                                      
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY2                                                          
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY2  TM    WORK,SETSDT         SET TRANSACTION DATE                         
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
       ++INCLUDE ACMRKF8D                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE3D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACMRK08   07/12/18'                                      
         END                                                                    
