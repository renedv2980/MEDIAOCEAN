*          DATA SET ACMRK05    AT LEVEL 060 AS OF 07/12/18                      
*PHASE T61605A                                                                  
ACMRK05  TITLE 'CREDITOR/GENERAL - OFFSET (CONTRA)'                             
                                                                                
***********************************************************************         
* THIS OVERLAY IS USED FOR TYPE CREDITOR AND TYPE GENERAL             *         
* GENERAL OFFSET HAS A SEPARATE HEADER SCREEN FOR FIELD NAME TEXT,    *         
* BUT THE SCREEN DSECTS ACMRKF5D & ACMRKFBD MUST REMAIN IDENTICAL     *         
* VALID LEDGERS FOR CREDITOR OFFSET ARE IN LDGLIST1                   *         
* VALID LEDGERS FOR GENERAL OFFSET ARE IN LDGLIST2                    *         
***********************************************************************         
*                                                                     *         
* JFOX 39 USE SORAWRK IF AVAILABLE INSTEAD OF CPJWRK                  *         
* JFOX 40 YEAR 2000 FIX                                               *         
* JFOX 41 REINSTATE SOME NOP-ED CODE                                  *         
* ECLI 42 IF NON DDS TERMINAL - ONLY ALLOW LEDGER Q                   *         
* JNEW 43 IF NON DDS TERMINAL - ALLOW LEDGER SA TOO                   *         
* ???? 44                                                             *         
* JFOS 45 AFCEL STATUS BIT CHANGES                                    *         
* JFOX 46 OFFSET (USED) DATE HEADER FILTER                            *         
* RGUP 060 21MAY18  <SPEC-20692> ADDITIONAL MEDIA FOR DIGIAL AUDIO    *         
***********************************************************************         
                                                                                
ACMRK05  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MRK5**,RA                                                    
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
*                                  SET INPUT SCREEN DISPS FOR ROOT              
         LH    R1,=Y(INPHEDH-TWAD)                                              
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPHD2H-TWAD)                                              
         STH   R1,DISPHED2         DISPLACEMENT OF 2ND INPUT HEADLINE           
         AR    R1,R6                                                            
         ST    R1,ADISHEA2         A(INPUT 2ND HEADLINE)                        
         LH    R1,=Y(INPDETH-TWAD)                                              
         STH   R1,DISPDET          DISPLACEMENT OF 1ST DETAIL LINE              
         AR    R1,R6                                                            
         ST    R1,ADISDET1         A(1ST DETAIL LINE)                           
         LH    R1,=Y(INPTOTH-TWAD)                                              
         STH   R1,DISPTOT          DISPLACEMENT OF TOTALS LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISTOTS         A(TOTALS LINE)                               
         LH    R1,=Y(INPPFKH-TWAD)                                              
         STH   R1,DISPPFK          DISPLACEMENT OF PF KEY LINE                  
         AR    R1,R6                                                            
         ST    R1,ADISPFKS         A(PF KEY LINE)                               
                                                                                
                                                                                
         CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              PRE-VALIDATE HEADER SCREEN                   
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              ROUTINE TO GET NEXT SUPPLIER                 
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTOFFS                                                  
         BE    *+6                 OFFSET (CONTRA)                              
         DC    H'0'                                                             
         CLI   TWASCROV,COSCR1                                                  
         BE    VALHED              CREDITOR OFFSET - VALIDATE HEADER            
         CLI   TWASCROV,GOSCR1                                                  
         BE    VALHED              GENERAL OFFSET - VALIDATE HEADER             
         CLI   TWASCROV,COSCR2                                                  
         BE    VALINP              CREDITOR OFFSET - VALIDATE INPUT             
         CLI   TWASCROV,GOSCR2                                                  
         BE    VALINP              GENERAL OFFSET - VALIDATE INPUT              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER                                                 *         
***********************************************************************         
                                                                                
PREVAL   LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREVAL02                                                         
         CLI   XTYPE,TYPGEN        TEST TYPE IS GENERAL                         
         BE    *+12                                                             
         CLI   LTYPE,TYPCRD        NO - TEST CREDITOR ACTION LAST               
         BNE   PREVAL02                                                         
         XC    OFSLDG,OFSLDG                                                    
         XC    OFSSUP,OFSSUP                                                    
         OI    OFSLDGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    OFSSUPH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   OFSLDG(L'ACTKLDG),ACTKLDG                                        
         MVC   OFSSUP(L'ACTKACT),ACTKACT                                        
         XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
                                                                                
PREVAL02 TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    PREVAL04                                                         
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),('TYPCRD',FULL)                
         BE    PREVAL06                                                         
                                                                                
PREVAL04 DS    0H                                                               
*&&UK                                                                           
         XC    OFSCURT,OFSCURT                                                  
         OI    OFSCURTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    OFSCURH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    OFSCURH+(FVOIND-FVIHDR),FVOXMT                                   
*&&                                                                             
PREVAL06 DS    0H                                                               
                                                                                
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD(S)                        *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   OFSSUPH+(FVILEN-FVIHDR),0  TEST INPUT TO SUPPLIER                
         BNE   *+16                                                             
         XC    TWASKEY,TWASKEY     NO - CLEAR KEY SAVED IN TWA                  
         XC    ACCOUNT,ACCOUNT     AND MUST CLEAR SUPPLIER ACCOUNT              
         OC    LEDGER,LEDGER       TEST LEDGER ALREADY SET                      
         BNZ   *+12                                                             
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
         BNE   NXTACCX             INVALID LEDGER                               
         OC    TWASKEY,TWASKEY     TEST KEY SAVED IN TWA                        
         BZ    *+14                                                             
         CLC   OFSLDG,TWASKEY+L'ACTKUNT  TEST LEDGER MATCHES SAVED              
         BNE   *+14                                                             
         CLC   OFSLDG,LEDGER+L'ACTKUNT  TEST LEDGER CHANGE                      
         BE    NXTACC4                                                          
         XC    TWASKEY,TWASKEY     YES - CLEAR KEY SAVED IN TWA                 
         XC    ACCOUNT,ACCOUNT     (RE)SET ACCOUNT TO ZERO                      
         CLC   OFSLDG,LEDGER+L'ACTKUNT  TEST NEW LEDGER TO BE VALIDATED         
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
                                                                                
NXTACC12 SR    RF,RF               BUMP KEY FOR NEXT SUPPLIER                   
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
         BNE   NXTACC16            PAST SUPPLIER LEDGER                         
         MVI   GETIND,GETIABLQ     RE-/READ ACCOUNT                             
         GOTO1 AGETACC,0                                                        
         BE    NXTACC18            VALID THIS TIME                              
         B     NXTACC12            STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC16 MVC   FVMSGNO,=AL2(EANOACCS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,OFSLDGH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC18 MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 VACSRCHC,DMCB,OFSSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
                                                                                
         OI    OFSSUPH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         MVC   FVMSGNO,=AL2(IAEPAP1N)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,OFSSUPH                                                       
         ST    R1,FVADDR                                                        
                                                                                
NXTACCX  NI    TWAMODE2,FF-TWAM2NXA                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                       *         
***********************************************************************         
                                                                                
VALHED   DS    0H                                                               
***********************************************************************         
* VALIDATE UNIT AND LEDGER                                            *         
***********************************************************************         
                                                                                
VALLDG   SR    R0,R0               CLEAR R0                                     
         TM    TWAMODE2,TWAM2NXA   TEST CALLED BY NXTACC                        
         BNO   *+6                                                              
         LR    R0,RE               YES - SAVE A(RETURN)                         
         TM    OFSLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDGX                                                          
         MVI   LEDGBRCC,0          CLEAR LEDGER BRANCH CC                       
         XC    OFSLDGN,OFSLDGN                                                  
         OI    OFSLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,OFSLDGH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
         LA    R1,LDGLIST1         R1=A(VALID LEDGERS - TYPE CREDITOR)          
         CLI   XTYPE,TYPGEN        TEST TYPE IS GENERAL                         
         BNE   VALLDG2                                                          
         LA    R1,LDGLIST2         R1=A(VALID LEDGERS - TYPE GENERAL)           
VALLDG2  CLC   OFSLDG,0(R1)                                                     
         BNE   *+14                                                             
         MVC   LEDGBRCC,1(R1)      EXTRACT THIS LEDGER BRANCH CC                
         B     VALLDG4             LEDGER IN LIST - VALIDATE IT                 
         CLI   0(R1),EOT                                                        
         BE    *+12                                                             
         LA    R1,L'LDGLIST1(R1)                                                
         B     VALLDG2                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALLERR             ERROR - NOT IN VALID LEDGER LIST             
                                                                                
VALLDG4  CLI   TWAOFFC,C'*'        TEST DDS TERMINAL                            
         BE    VALLDG6                                                          
         CLI   XTYPE,TYPGEN        TEST TYPE IS GENERAL                         
         BNE   VALLDG6                                                          
         CLI   OFSLDG,C'Q'         NO - CAN ONLY INPUT A 'Q' LEDGER             
         BE    VALLDG6                                                          
         CLI   OFSLDG,C'A'         OR 'A'                                       
         BE    VALLDG6                                                          
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALLERR             ERROR - NOT A VALID LEDGER                   
                                                                                
VALLDG6  MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALLDG,OFSLDGH                                                  
         BH    VALLERR                                                          
         MVC   OFSLDGN,RECNAME     NAME EXTRACTED BY GETLDG                     
         B     VALLDGX                                                          
                                                                                
VALLERR  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BNZR  RE                  RETURN TO CALLER WITH CC SET NEQ             
         B     EXIT                NO - EXIT WITH ERROR MSG SET                 
                                                                                
VALLDGX  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BZ    *+8                 NO - CONTINUE WITH HDR VALIDATION            
         CR    R0,R0               YES -  SET CC EQU                            
         BR    RE                  AND RETURN TO CALLER                         
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
***********************************************************************         
                                                                                
VALSUP   TM    OFSSUPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSUPX                                                          
         MVI   FVMINL,1                                                         
*&&UK*&& XC    PRSTCURT,PRSTCURT   VALSUP WILL PRESET CURRENCY                  
         GOTO1 AVALSUP,OFSSUPH                                                  
         BNE   EXIT                                                             
         GOTO1 VACSRCHC,DMCB,OFSSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
VALSUPX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    OFSOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    OFSOFFN,OFSOFFN                                                  
         OI    OFSOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,OFSOFFH                                                  
         BL    VALOFFX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   OFSOFFN,RECNAME                                                  
         OI    OFSOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
VALOFFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    OFSCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    OFSCONN,OFSCONN                                                  
         OI    OFSCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         CLI   OFSCONH+(FVILEN-FVIHDR),L'DUMCON                                 
         BNE   VALCON2                                                          
         CLC   OFSCON(L'DUMCON),DUMCON    TEST DUMMY CONTRA INPUT               
         BNE   VALCON2                                                          
         MVC   CONTRA,SPACES                                                    
         MVC   CONTRA(L'ACTKCPY),COMPANY                                        
         MVC   CONTRA+L'ACTKCPY(L'DUMCON),DUMCON                                
         MVI   CONTRAXL,L'TRNKCULC-1                                            
         MVI   CONTIND,CONTILOQ    SET REAL LOW-LEVEL ACCOUNT IND               
         OI    OFSCONH+(FVOIND-FVIHDR),FVOXMT                                   
         B     VALCONX                                                          
VALCON2  GOTO1 AVALCON,OFSCONH                                                  
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   OFSCONN,RECNAME                                                  
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE SOURCE                                                     *         
***********************************************************************         
                                                                                
VALSRC   TM    OFSSRCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSRCX                                                          
         XC    OFSSRCN,OFSSRCN                                                  
         OI    OFSSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALSRC,OFSSRCH                                                  
         BH    EXIT                                                             
         BL    VALSRCX             NOT REQUIRED, NOT INPUT                      
         MVC   OFSSRCN,RECNAME                                                  
         OI    OFSSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
VALSRCX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   TM    OFSREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,OFSREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE TRANSACTION DATE PERIOD                                    *         
***********************************************************************         
                                                                                
VALPER   TM    OFSPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,OFSPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE PERIOD                                       *         
***********************************************************************         
                                                                                
VALADA   TM    OFSADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,OFSADAH                                                  
         BH    EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE MOS RANGE                                                  *         
***********************************************************************         
                                                                                
VALMOS   TM    OFSMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,OFSMOAH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE INCLUDE OFFSET (CONTRA'D)                                  *         
***********************************************************************         
                                                                                
VALOFS   TM    OFSOFSH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFSX                                                          
         GOTO1 AVALICL,OFSOFSH                                                  
         BH    EXIT                                                             
VALOFSX  DS    0H                                                               
                                                                                
*&&UK                                                                           
***********************************************************************         
* VALIDATE CURRENCY FILTER                                            *         
***********************************************************************         
                                                                                
VALCUR   TM    OFSCURH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCURX                                                          
         GOTO1 AVALCUR,OFSCURH     PUT VALID CURRENCIES INTO TABLE              
         BH    EXIT                                                             
         BE    VALCURX                                                          
         PUSH  USING               TEST UNACCEPTABLE CURRENCY FILTER            
PRESET   USING CURTABD,PRSTCURT                                                 
         CLC   PRESET.CURTCUR,OFSCUR                                            
         BNE   *+14                                                             
         CLC   OFSCUR+L'CURTCUR(L'OFSCUR-L'CURTCUR),SPACES                      
         BNH   VALCURX                                                          
         MVC   OFSCUR,SPACES       UNACCEPTABLE CURRENCY FILTER                 
         OI    OFSCURH+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   OFSCUR,C'*'                                                      
         MVI   OFSCURH+(FVILEN-FVIHDR),1                                        
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    *+14                                                             
         MVC   OFSCUR(L'CURTCUR),PRESET.CURTCUR                                 
         MVI   OFSCURH+(FVILEN-FVIHDR),L'CURTCUR                                
         MVC   FVMSGNO,=AL2(AI$CURFC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         POP   USING                                                            
VALCURX  DS    0H                                                               
*&&                                                                             
                                                                                
***********************************************************************         
* VALIDATE USED (CONTRA'D) DATE PERIOD                                *         
***********************************************************************         
                                                                                
VALUDA   TM    OFSODAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALUDAX                                                          
         GOTO1 AVALUDA,OFSODAH                                                  
         BH    EXIT                                                             
VALUDAX  DS    0H                                                               
                                                                                
                                                                                
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
         GOTO1 SETKEY,SETACC                                                    
*&&US*&& GOTO1 AVALPUB,TRNRECD     VALIDATE SPECIAL PUBLICATION NUMBER          
         GOTO1 SETKEY,SETOFF+SETCON+SETSDT+SETREF                               
         MVI   TSARLEN+1,TSARCOL   SET TSAR RECORD LENGTH                       
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
*&&UK                                                                           
         NI    SAFCIND1,FF-SAFCIGBP                                             
         XC    FORECURT,FORECURT   SET UP CURRENCY FIRST TIME THROUGH           
*&&                                                                             
         B     READ04                                                           
                                                                                
READ02   LA    R2,KEY                                                           
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
READ04   TM    TRNKSTAT,TRNSDELT+TRNSDRFT+TRNSREVS                              
         BNZ   READ02                                                           
         TM    TRNKSTA2,TRNSPEEL                                                
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
         TM    CONTIND,CONTILOQ    TEST REAL LOW LEVEL CONTRA                   
         BNZ   READ12              YES - DON'T SET NEXT CONTRA                  
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
         BNE   READ02              TRANSACTION FAILS - GET NEXT                 
         BAS   RE,RFILTER          FILTER DATA RECORD                           
         BNE   READ02              TRANSACTION FAILS - GET NEXT                 
                                                                                
         L     R1,AIOSAVE          R1=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R1)      EXTRACT DATA RECORD DISK ADDRESS             
                                                                                
         USING TRNRECD,R1          R1=A(DATA RECORD KEY)                        
         L     R1,AIOBUFF          EXTRACT TRNKEY VALUES                        
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
         MVC   TSARMOS,TRNRSMOS                                                 
         TM    TRNRSTA2,TRNSUSED                                                
         BZ    *+10                                                             
         MVC   TSARFUSE,TRNRSUSE                                                
         MVI   TSARRSTA,0                                                       
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD IS ON ARCHIVE                     
                                                                                
         MVI   TSARINDS,0          CLEAR INDICATOR                              
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        EXTRACT TRNEL VALUES                         
         TM    COMPSTA4,CPYSIREG   TEST INVOICE REGISTER IN USE                 
         BZ    READ24                                                           
         CLC   SUPPUL,TRNKUNT      TEST PRODUCTION SUPPLIER                     
         BE    *+14                                                             
         CLC   SUPXUL,TRNKUNT      TEST HOUSE SUPPLIER                          
         BNE   READ24                                                           
         TM    TRNSTAT,TRNSAUTH    TEST UNAUTHORISED                            
         BO    READ24                                                           
         OI    TSARINDS,TSARDISQ                                                
         MVI   TSARCHA,C'*'                                                     
         DROP  R1                                                               
                                                                                
READ24   TM    COMPSTAT,CPYSOROE   TEST AGENCY IS ON OFFICES                    
         BNO   *+16                                                             
         MVC   TSAROFF,TRNOFFC     YES - SET TSAROFF                            
         OC    TSAROFF,SPACES      AND TURN ANYTHING < C' ' TO C' '             
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         ZAP   TSARAMNT,TRNAMNT    KEEP TRNAMNT IN TSARREC AS FOUND             
*&&UK*&& GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
                                                                                
         ZAP   OVDUB,TRNAMNT       WILL BE REVERSED IF A CREDIT WHEN            
*&&UK*&& ZAP   OVDUB2,TSARAFCA     DEBITS ARE BEING INCLUDED (& AFC)            
         CLI   LEDGBRCC,X'10'      TEST EXCLUDING DEBITS                        
         BE    READ30              YES - LEAVE AMOUNT ALONE                     
         TM    TRNSTAT,TRNSDR      INCLUDING DEBITS - TEST DEBIT                
         BZ    *+12                                                             
         OI    TSARINDS,TSARDRQ    DEBIT - SET INDICATOR BIT                    
         B     READ30                                                           
         MP    OVDUB,PONENEG       CREDIT - REVERSE SIGN FOR TOTALS             
*&&UK*&& MP    OVDUB2,PONENEG      CREDIT - REVERSE SIGN FOR AFC TOTALS         
                                                                                
         USING TRSELD,R3                                                        
READ30   ICM   R3,15,ATRSEL                                                     
         MVC   TSARADAT,TRSDATE    EXTRACT ACTIVITY DATE                        
         MVC   TSARSSTA,TRSSTAT    EXTRACT STATUS BYTE                          
         TM    TRSSTAT,TRSSOFFS    TEST TRANSACTION IS OFFSET                   
         BO    READ32                                                           
         AP    TOTAOFS,OVDUB                                                    
*&&UK*&& AP    CURAOFS,OVDUB2                                                   
         B     READ34                                                           
READ32   OI    TSARINDS,TSARMKQ+TSARINMQ  MARKED (INCOMING FROM FILE)           
         NI    TSARINDS,FF-(TSARDISQ)                                           
         MVI   TSARCHA,C' '        DON'T STOP UNMARKING                         
READ34   CLI   TRSLN,TRSLNQ        TEST ELEMENT CARRIES USED DATE               
         BL    *+10                                                             
         MVC   TSARUSDT,TRSUDAT    EXTRACT USED DATE (OR 0)                     
                                                                                
         MVC   TSARFSAC,SRCWORK    EXTRACT SOURCE ACCOUNT                       
*&&UK                                                                           
         USING SORELD,R2                                                        
         ICM   R2,15,ASOREL        USE SOREL WORKCODE, IF PRESENT               
         BZ    READ36                                                           
         CLI   SORSYS,SORSACC      TEST ACCOUNTING SOURCE A/C                   
         BNE   READ36                                                           
         CLI   SORLN,SORAL2Q       TEST LONG ACCOUNTING ELEMENT                 
         BL    READ36                                                           
         MVC   TSARFWRK,SORAWRK    YES TAKE WORKCODE                            
         B     READ38                                                           
                                                                                
         USING CPJELD,R3                                                        
READ36   ICM   R3,15,ACPJEL        R3=0 IF CPJEL ABSENT                         
         BZ    READ38                                                           
         MVC   TSARFWRK,SPACES                                                  
         CLI   CPJTYPE,CPJTJOB     TEST PRODUCTION SOURCE                       
         BNE   READ38                                                           
         MVC   TSARFWRK,CPJWRK     TAKE WORKCODE                                
         B     READ38                                                           
                                                                                
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
READ38   ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
*                                                                               
         USING DUEELD,R2           EXTRACT DUEEL VALUES                         
         ICM   R2,15,ADUEEL                                                     
         BZ    *+10                                                             
         MVC   TSARFDU2,DUEDATE                                                 
*                                                                               
*&&                                                                             
         USING DUEELD,R2           EXTRACT DUEEL VALUES                         
         ICM   R2,15,ADUEEL                                                     
         BZ    *+14                                                             
         MVC   TSARFDUE,DUEDATE                                                 
         B     READ39                                                           
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(2,TSARFDUE)                            
*                                                                               
         USING SCIELD,R2           EXTRACT SCIEL VALUES                         
READ39   ZAP   TSARFDIS,PZERO      DISCOUNT                                     
         ICM   R2,15,ASCIEL                                                     
         BZ    READ40                                                           
         CLI   SCITYPE,SCITCDSC    TEST LIVE DISCOUNT                           
         BNE   READ40                                                           
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
         ZAP   TSARFDIS,SCIAMNT                                                 
                                                                                
READ40   OC    ANOTELS,ANOTELS                                                  
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
*&&US                                                                           
         GOTO1 ABLDINV             GET LONG INVOICE NUMBER                      
*                                                                               
         USING XPYELD,R2                                                        
         ICM   R2,15,AXPYEL                                                     
         BZ    READ43                                                           
*        MVC   TSARFINV(L'XPYINV),XPYINV                                        
         CP    XPYCD,PZERO                                                      
         BE    READ43                                                           
         ZAP   TSARFDIS,XPYCD                                                   
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
                                                                                
*        USING FFTELD,R2                                                        
*EAD42   ICM   R2,15,AFFTLEL                                                    
*        BZ    READ43                                                           
*        XR    RE,RE                                                            
*        IC    RE,FFTDLEN                                                       
*        C     RE,=F'20'                                                        
*        BNH   *+8                                                              
*        LA    RE,20                                                            
*        BCTR  RE,0                TAKE MAX OF 20 CHAR                          
*        EX    RE,*+8                                                           
*        B     *+10                                                             
*        MVC   TSARFINV(0),FFTDATA                                              
*&&                                                                             
READ43   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   *+12                                                             
         MVI   ANYADD,1                                                         
         B     READ02              READ SEQUENTIAL                              
                                                                                
         TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                                                             
         LA    R1,OFSSUPH          NOT ALLOWED IN THIS OVERLAY                  
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATRNMAX)  TOO MANY TRANSACTIONS                    
         NI    TWAMODE,TWAMRSRV    SET TO RE-INITIALISE                         
         B     EXIT                                                             
                                                                                
READTRNX NI    DISIND,FF-DISIOFLO                                               
         CLI   ANYADD,1            TEST ANYTHING IN BUFFER                      
         BE    DISPTRN                                                          
         LA    R1,OFSSUPH          NO - SET CURSOR TO SUPPLIER FIELD            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         DROP  R2,R3                                                            
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
         GOTO1 AOVRSCR,COSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
                                                                                
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ADISPLAY                                                         
         GOTO1 ABLDTOT,INPTOTH                                                  
DISPTRNX B     EXIT                                                             
         EJECT                                                                  
         USING DISLINED,R2                                                      
VALINP   CLI   OPTALL,0            TEST GLOBAL MARK/UNMARK                      
         BE    VALINP02                                                         
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BO    VALINP26            YES - CALL DISPLAY                           
         BAS   RE,MRKALL           OVERLAY MARKS ALL TRANSACTIONS               
         BNE   VALINPX             MRKALL SETS ERROR MESSAGE                    
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         OI    DISIND,DISIRST      SET TO RESTART DISPLAY                       
         B     VALINP26                                                         
                                                                                
VALINP02 MVI   ANYMARK,0                                                        
         LA    R3,DISLIST          R3=A(LIST OF TSAR RECDS ON DISPLAY)          
         L     R2,ADISDET1         R2=A(1ST DETAIL LINE)                        
         SR    R0,R0                                                            
         ICM   R0,3,DISLCNT        NUMBER OF DISPLAY LINES                      
         BZ    VALINP26            NO RECORDS TO DISPLAY                        
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VALINP04                                                         
         NI    TWAMODE2,FF-TWAM2SKP  RESET SKIP VALIDATION                      
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VALINP26            SKIP VALIDATION AND CALL DISPLAY             
                                                                                
VALINP04 GOTO1 AVALZMRK,DISLHDR2   ZOOM INPUT                                   
         BL    VALINP06            NO ZOOM - TRY OTHER MARKS                    
         BH    VALINPX             ZOOM INVALID - EXIT WITH ERROR SET           
         GOTO1 AZOOMFAC,(R3)       PREPARE ZOOM SCREEN AND EXIT TO USER         
         B     EXIT                                                             
                                                                                
VALINP06 GOTO1 AVALMRK,DISLHDR2                                                 
         BH    VALINPX             EXIT WITH ERROR SET                          
         BL    VALINP24            NO INPUT - NEXT SCREEN LINE                  
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,TSARCHGQ       TEST ONLY WANT TO REDISPLAY                  
         BE    VALINP22                                                         
         ZAP   OVDUB,TSARAMNT      REVERSE CREDIT WHEN INCLUDING DEBITS         
*&&UK*&& ZAP   OVDUB2,TSARAFCA     REVERSE (AFC) CR WHEN INCLUDING DR           
         CLI   LEDGBRCC,X'10'      TEST EXCLUDING DEBITS                        
         BE    VALINP08            YES - LEAVE AMOUNT ALONE                     
         TM    TSARINDS,TSARDRQ    INCLUDING DEBITS - TEST DEBIT                
         BNZ   VALINP08                                                         
         MP    OVDUB,PONENEG       CREDIT - REVERSE SIGN FOR TOTALS             
*&&UK*&& MP    OVDUB2,PONENEG      CR - REVERSE SIGN FOR (AFC) TOTALS           
VALINP08 CLI   BYTE,TSARMKQ        TEST IF USER IS MARKING                      
         BNE   VALINP16                                                         
         TM    TSARINDS,TSARMKQ    IS RECORD CURRENTLY MARKED?                  
         BO    VALINP24                                                         
         BAS   RE,CHKVAL           TEST OK TO OFFSET AND UPDATE HITS            
         BE    VALINP12            OK TO OFFSET                                 
         BL    VALINP10            WRONG OFFICE OR CURRENCY                     
         MVC   FVMSGNO,=AL2(EATRNSEL)  MIXING SELECTED WITH UNSELECTED          
         TM    TSARSTA,TRNSAPPR    TEST SELECTED                                
         BNZ   VALINP10                                                         
         MVC   FVMSGNO,=AL2(EATUNSEL)  MIXING UNSELECTED WITH SELECTED          
VALINP10 MVI   DISLMARK,C' '       CLEAR MARK FIELD AND RE-TRANSMIT             
         OI    DISLHDR2+(FVOIND-FVIHDR),FVOXMT                                  
         B     VALINPX             EXIT WITH ERROR SET                          
                                                                                
VALINP12 OI    TSARINDS,TSARMKQ                                                 
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BZ    *+20                                                             
         AP    TOTCUR,PONE                                                      
         BNZ   *+10                                                             
         XC    STSARAFC,STSARAFC                                                
*&&                                                                             
         TM    TSARINDS,TSARINMQ   TEST INCOMING RECORD MARKED ON FILE          
         BNO   VALINP14                                                         
         SP    TOTUOF,OVDUB        YES - SUBTRACT FROM UNMARKED                 
*&&UK*&& SP    CURUOF,OVDUB2       YES - SUBTRACT FROM UNMARKED (AFC)           
         B     VALINP20                                                         
VALINP14 AP    TOTOFS,OVDUB        NO - ADD TO MARKED                           
*&&UK*&& AP    CUROFS,OVDUB2       NO - ADD TO MARKED (AFC)                     
         B     VALINP20                                                         
                                                                                
VALINP16 TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    VALINP24                                                         
         BAS   RE,CHKVAL           UPDATE HITS IF NECESSARY                     
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BZ    *+20                                                             
         SP    TOTCUR,PONE                                                      
         BNZ   *+10                                                             
         XC    STSARAFC,STSARAFC                                                
*&&                                                                             
         NI    TSARINDS,FF-TSARMKQ                                              
         TM    TSARINDS,TSARINMQ   TEST INCOMING RECORD MARKED ON FILE          
         BNO   VALINP18                                                         
         AP    TOTUOF,OVDUB        YES - ADD TO UNMARKED                        
*&&UK*&& AP    CURUOF,OVDUB2       YES - ADD TO UNMARKED (AFC)                  
         B     VALINP20                                                         
VALINP18 SP    TOTOFS,OVDUB        NO - SUBTRACT FROM MARKED                    
*&&UK*&& SP    CUROFS,OVDUB2       NO - SUBTRACT FROM MARKED (AFC)              
         B     VALINP20                                                         
                                                                                
VALINP20 L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ANYMARK,1                                                        
         DROP  RF                                                               
VALINP22 GOTO1 ABLDLIN,DISLHDR1    REBUILD LHS, TRANSMIT, UNHIGHLIGHT           
                                                                                
VALINP24 LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VALINP04                                                      
                                                                                
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+16                                                             
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   YES - SET SKIP VALIDATION                    
         TM    DISIND,DISINCOL     TEST CHANGED DISPLAY COLUMNS                 
         BO    VALINP26                                                         
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VALINP26                                                         
         OI    DISIND,DISIFFLT     FORCE FILTERING OF DISLIST NEXT TIME         
         LA    R1,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     VALINPX                                                          
                                                                                
VALINP26 GOTO1 ADISPLAY                                                         
                                                                                
VALINPX  GOTO1 ABLDTOT,INPTOTH                                                  
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
UPDATE   TM    TWAMODE2,TWAM2CHG   TEST CHANGES MADE BIT                        
         BO    UPD0010                                                          
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         LA    R1,MRKSCRH                                                       
         B     UPDATEX                                                          
                                                                                
UPD0010  OC    PRTSUB,PRTSUB                                                    
         BZ    UPD0020                                                          
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@CTRD),LC@CTRD                            
         LA    R1,REPH5+L'DISLLINE+1+L'LC@CTRD-1                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
UPD0020  LA    R1,INPMRKH                                                       
         ST    R1,FVADDR                                                        
         XC    TEMP(2),TEMP                                                     
*&&UK*&& MVI   OVAFCXS,0                                                        
         CLI   XACTION,ACTDRFT     TEST DRAFT REPORT                            
         BE    UPD0030             DON'T TEST IN BALANCE                        
         CP    TOTOFS,PZERO        TEST OFFSETS BALANCE TO ZERO                 
         BNE   *+14                                                             
         CP    TOTUOF,PZERO        TEST UNOFFSETS BALANCE TO ZERO               
         BE    UPD0030                                                          
         LA    R1,INPMRKH                                                       
         MVC   FVMSGNO,=AL2(EAMUMTNZ)                                           
         B     UPDATEX                                                          
                                                                                
UPD0030  ICM   R1,3,TEMP           ESTABLISH LASTEST USED MOS (TSARMOS)         
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         GOTO1 ATSARGET,TEMP                                                    
         BE    UPD0040                                                          
         L     RF,ATSARBLK                                                      
         USING TSARD,RF                                                         
         TM    TSERRS,TSEEOF                                                    
         BO    UPD0060             MAIN UPDATE PROCESS                          
         DC    H'0'                                                             
UPD0040  TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD0050                                                          
         TM    TSARINDS,TSARINMQ   TEST ALREADY MARKED ON FILE                  
         BO    UPD0030                                                          
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BNZ   *+8                                                              
         OI    OVAFCXS,AFCXSMEM    MUST ENSURE MEMO AFCEL'S                     
*&&                                                                             
         CLC   TSARMOS,NEWUSED     TEST LATER USED MOS                          
         BNH   UPD0030                                                          
         MVC   NEWUSED,TSARMOS     SET/UPDATE OFFSET USED MOS                   
         B     UPD0030             GET NEXT TSAR RECORD                         
                                                                                
UPD0050  TM    TSARINDS,TSARINMQ   TEST ALREADY MARKED ON FILE                  
         BNO   UPD0030                                                          
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BNZ   *+8                                                              
         OI    OVAFCXS,AFCXSMEM    MUST ENSURE MEMO AFCEL'S                     
*&&                                                                             
         CLC   OLDUSED,TSARFUSE    TEST SAME UNOFFSET MOS                       
         BE    UPD0030             (REINSTATED 24/JUL/1997)                     
         OC    OLDUSED,OLDUSED     TEST FIRST TIME                              
         BNZ   *+14                                                             
         MVC   OLDUSED,TSARFUSE    SET UNOFFSET MOS                             
         B     UPD0030                                                          
         MVC   FVMSGNO,=AL2(AE$CUAMO)                                           
         LA    R1,INPMRKH                                                       
         B     UPDATEX                                                          
                                                                                
UPD0060  LA    R1,1                READ ALL TSAR RECORDS, UPDATE FILE           
         STCM  R1,3,TEMP                                                        
         CLI   XACTION,ACTDRFT     TEST DRAFT REPORT                            
         BE    UPD0070             DON'T TEST IN BALANCE                        
*&&UK                                                                           
         TM    OVAFCXS,AFCXSMEM                                                 
         BO    UPD0070                                                          
         CP    CUROFS,PZERO        TEST OFFSETS BALANCE TO ZERO                 
         BNE   *+14                                                             
         CP    CURUOF,PZERO        TEST UNOFFSETS BALANCE TO ZERO               
         BE    UPD0070                                                          
         LA    R1,INPMRKH                                                       
         MVC   FVMSGNO,=AL2(AE$MUMCZ)  CURRENCY TOTALS NON-ZERO                 
         B     UPDATEX                                                          
*&&                                                                             
                                                                                
UPD0070  GOTO1 ATSARGET,TEMP                                                    
         BE    UPD0080                                                          
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    UPD0290                                                          
         DC    H'0'                                                             
                                                                                
UPD0080  TM    TSARINDS,TSARMKQ    TEST IF USER IS MARKING                      
         BZ    UPD0090                                                          
         TM    TSARINDS,TSARINMQ   OFFSET - TEST ALREADY OFFSET                 
         BO    UPD0280             ALREADY OFFSET - SKIP                        
         MVI   NEWSTA,TSARINMQ     INDICATE WE ARE OFFSETTING                   
         B     UPD0100             OK TO OFFSET                                 
                                                                                
UPD0090  TM    TSARINDS,TSARINMQ   UNOFFSET - TEST ALREADY OFFSET               
         BNO   UPD0280             NOT OFFSET - SKIP                            
         MVI   NEWSTA,0            OK TO UNOFFSET                               
                                                                                
UPD0100  OC    PRTSUB,PRTSUB       PRINT REPORT IF REQUIRED                     
         BZ    UPD0110             MUST BE LIVE IF NO REPORT                    
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'          BUILD PRINT LINE USING REPDISP               
         GOTO1 ABLDLIN                                                          
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD0280             YES - GET NEXT TSAR RECORD                   
                                                                                
UPD0110  MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF          R1=A(KEY)                                    
         LA    RF,BZQ              PASS IF NOT USED                             
         TM    TRNRSTA2-TRNRECD(R1),TRNSUSED                                    
         BZ    *+8                                                              
         LA    RF,BNZQ             PASS IF USED                                 
         OC    TSARUSDT,TSARUSDT                                                
         EX    RF,*+4                                                           
         NOP   *+6                                                              
         DC    H'0'                USED STATUS HAS CHANGED                      
                                                                                
         GOTO1 ASETELAD,AIOBUFF    SET A(ELEMENTS)                              
                                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        R2=A(TRANSACTION ELEMENT)                    
         BNZ   *+6                                                              
         DC    H'0'                NO TRANSACTION ELEMENT                       
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BE    *+6                 NO - OK TO CONTINUE                          
         DC    H'0'                OFFSET ITEMS MAY NOT BALANCE, IF WE          
*                                  EXIT WITH UPDATE INCOMPLETE                  
*&&US                                                                           
         CLI   NEWSTA,0            TEST UNOFFSETTING                            
         BE    *+8                                                              
         NI    TRNSTAT,FF-(TRNSAPPR+TRNSHOLD)                                   
*&&                                                                             
*&&UK                                                                           
         USING AFCELD,R2                                                        
         ICM   R2,15,AAFCEL                                                     
         BZ    *+10                                                             
         OC    AFCXSTA2,OVAFCXS    MEMO ONLY                                    
*&&                                                                             
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL        R2=A(TRANSACTION STATUS ELEMENT)             
         BNZ   *+6                                                              
         DC    H'0'                NO TRANSACTION STATUS ELEMENT                
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BE    *+6                 NO - OK TO CONTINUE                          
         DC    H'0'                OFFSET ITEMS MAY NOT BALANCE, IF WE          
*                                  EXIT WITH UPDATE INCOMPLETE                  
                                                                                
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD0120                                                          
         GOTO1 AEXTRSL             YES - EXTEND IT                              
         GOTO1 ASETELAD,AIOBUFF    RE-ESTABLISH ELEMENT ADDRESSES               
         ICM   R2,15,ATRSEL                                                     
UPD0120  MVI   TRSMARK,TRSMCOQ     SET TYPE/ACTION IS CREDITOR/OFFSET           
         CLI   XTYPE,TYPCRD        TEST CREDITOR/OFFSET                         
         BE    UPD0130                                                          
         MVI   TRSMARK,TRSMGOQ     SET TYPE/ACTION IS GENERAL/OFFSET            
         CLI   XTYPE,TYPGEN        TEST GENERAL/OFFSET                          
         BE    *+6                                                              
         DC    H'0'                XTYPE IS CRAZY                               
UPD0130  NI    TRSSTAT,FF-TRSSOFFS CLEAR OFFSET STATUS                          
         XC    TRSUMOS,TRSUMOS     CLEAR OFFSET (USED) MOS                      
         XC    TRSUDAT,TRSUDAT     CLEAR OFFSET (USED) DATE                     
         NI    TRSSTAT,FF-TRSSVOID CLEAR VOID STATUS, IF SET                    
         XC    TRSVOID,TRSVOID     CLEAR SAVED USED DATE, IF SET                
         CLI   NEWSTA,0            TEST UNOFFSETTING                            
         BNE   *+12                                                             
         OI    TRSMARK,TRSMUMQ     SET ACTION IS NEGATIVE                       
         B     UPD0140                                                          
                                                                                
         OI    TRSSTAT,TRSSOFFS    SET OFFSET STATUS                            
         MVC   TRSUMOS,NEWUSED     SET OFFSET (USED) MOS                        
         MVC   TRSUDAT,TODAYC      SET OFFSET (USED) DATE                       
                                                                                
         USING TRNRECD,R2                                                       
UPD0140  L     R2,AIOBUFF          R2=A(DATA RECORD)                            
         NI    TRNRSTA2,FF-TRNSUSED  UNOFFSET                                   
         XC    TRNRSUSE,TRNRSUSE   CLEAR OFFSET MOS                             
*&&UK                                                                           
         ICM   R1,15,ADUEEL        SET DUE DATE IF AVAILABLE                    
         BZ    *+10                (REINSTATED 24/JUL/1997)                     
         MVC   TRNRSDUE,DUEDATE-DUEELD(R1)                                      
*&&                                                                             
         CLI   NEWSTA,0            TEST UNOFFSETTING                            
         BE    *+14                                                             
         OI    TRNRSTA2,TRNSUSED   OFFSET                                       
         MVC   TRNRSUSE,NEWUSED    SET OFFSET MOS                               
         LA    R1,IOPUT+IOACCMST+IO1Q  PUT BACK TO ACCMST                       
         MVC   BYTE,TRNRSTAT       SAVE RECORD STATUS                           
         TM    TRNRSTAT,TRNSARCH   TEST TRANSACTION ON ACCARC                   
         BNO   *+12                                                             
         NI    TRNRSTAT,FF-TRNSARCH  CLEAR ACCARC INDICATOR                     
         LA    R1,IOADFR+IOACCMST+IO1Q  PROMOTE ACCARC RECORD TO ACCMST         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   KEY(L'TRNKEY),TRNKEY  EXTRACT TRANSACTION KEY                    
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNKSTA),TRNRSTA                         
         TM    BYTE,TRNSARCH       TEST RECORD PROMOTED TO ACCMST               
         BNO   UPD0150                                                          
         L     R2,AIOSAVE          R2=A(SAVED DATA RECORD VALUES)               
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
UPD0150  LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* UPDATE AUTO APPROVAL POINTERS WITH NEW USED DATE                              
*                                                                               
         MVC   SVKEY,KEY           SAVE OFF KEY FOR LATER RESTORE               
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF                                                       
*                                                                               
         LA    R4,TRNRFST          FIND TRSEL                                   
         LA    RF,0                                                             
         XC    SVMOS,SVMOS                                                      
         MVC   SVEST,SPACES                                                     
         MVC   SVOFF,SPACES                                                     
         MVI   SVSYS,0                                                          
UPD0160  CLI   0(R4),0             END OF RECORD                                
         BE    UPD0260                                                          
         CLI   0(R4),X'1A'         MEDIA TRANSFER ELEMENT                       
         BE    UPD0180                                                          
         CLI   0(R4),X'23'         OTHERS ELEMENT                               
         BE    UPD0190                                                          
         CLI   0(R4),X'44'         TRANSACTION ELEMENT                          
         BE    UPD0200                                                          
         CLI   0(R4),X'46'         EXTRA PAYMENT ELEMENT                        
         BE    UPD0210                                                          
         CLI   0(R4),X'6A'         MEDIA TRANSFER (PACKED DATA)                 
         BE    UPD0180                                                          
         CLI   0(R4),X'E5'         GENERAL DATE ELEMENT                         
         BE    UPD0240                                                          
         CLI   0(R4),X'F3'         NEW BILLING XFER(ONLY) ELEMENT               
         BE    UPD0250                                                          
UPD0170  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     UPD0160                                                          
*                                                                               
         USING MDTELD,R4                                                        
UPD0180  CLI   MDTSYS,C'J'         IS SYSTEM PRODUCTION?                        
         BE    UPD0280             YES NO POINTER NEEDED                        
*                                                                               
         MVC   SVSYS,MDTSYS        SYSTEM                                       
*                                                                               
         LA    R1,PSYSTAB          TABLE OF EQUIVALENT SYSTEMS-PRINT            
UPD0182  CLI   0(R1),X'FF'                                                      
         BE    UPD0188                                                          
         CLC   MDTSYS,0(R1)        IF MATCH ON SYSTEM THAN THE ACTUAL           
         BE    UPD0185             SYSTEM MUST BE PRINT                         
         LA    R1,1(R1)                                                         
         B     UPD0182                                                          
*                                                                               
UPD0185  MVI   SVSYS,C'P'          YES SO SAVE AS PRINT IN SVSYS                
UPD0188  MVC   SVMOS,MDTMOS        MOVE IN MONTH OF SERVICE                     
         MVC   SVEST,MDTEST        SAVE CHARACTER ESTIMATE                      
         OC    SVEST,SPACES                                                     
         B     UPD0170                                                          
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4                                                        
UPD0190  CHI   RF,23                                                            
         BH    UPD0170                                                          
         CLI   OTHPROF-3,C' '      IF SOMETHING IN THIS FIELD THAN NO           
         BH    UPD0170             MOS IN THIS ELEMENT                          
         LA    RF,23                                                            
         MVC   SVMOS,OTHDATE                                                    
         B     UPD0170                                                          
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4                                                        
UPD0200  MVC   SVOFF,TRNOFFC                                                    
         B     UPD0170                                                          
         DROP  R4                                                               
*                                                                               
         USING XPYELD,R4                                                        
UPD0210  OC    SVMOS,SVMOS                                                      
         BNZ   UPD0230                                                          
         CLC   XPYPER,SPACES       CHECK FOR ANY PERIOD DATE(S)                 
         BNH   UPD0230             NO - SKIP                                    
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
         BNO   UPD0220                                                          
         GOTO1 VGETBRD,DMCB,(1,(R5)),WORK+20,VGETDAY,VADDAY                     
         LA    R5,WORK+20                                                       
UPD0220  GOTO1 VDATCON,DMCB,(0,(R5)),(1,SVMOS)                                  
*                                                                               
UPD0230  CLC   XPYEST,SPACES       IS THERE AN ESTIMATE?                        
         BE    UPD0170                                                          
         OC    XPYEST,XPYEST                                                    
         BZ    UPD0170                                                          
         LH    RE,XPYEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST(3),DUB           UNPACK THE ESTIMATE                       
         B     UPD0170                                                          
         DROP  R4                                                               
*                                                                               
         USING GDAELD,R4                                                        
UPD0240  CLI   GDATYPE,GDAMMOS     IS THIS AN MOS DATE?                         
         BNE   UPD0170                                                          
         LA    RF,X'E5'                                                         
         MVC   SVMOS,GDAYYMM                                                    
         B     UPD0170                                                          
         DROP  R4                                                               
*                                                                               
         USING MBIELD,R4                                                        
UPD0250  CHI   RF,X'E5'                                                         
         BE    *+10                                                             
         MVC   SVMOS,MBIMOS                                                     
         MVC   SVEST,MBIEST                                                     
         B     UPD0170                                                          
         DROP  R4                                                               
*                                                                               
         USING AAVPASD,R4                                                       
UPD0260  LA    R4,KEY                                                           
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,TRNKCPY                                                  
         MVC   AAVPCLT,TRNKCACT+9  LAST 3 CHAR OF CONTRA IS CLIENT              
         MVC   AAVPPRD,TRNKREF     1ST 3 CHAR OF REFERENCE IS PRODUCT           
         MVC   AAVPEST,SVEST       ESTIMATE                                     
         MVC   AAVPMOS,SVMOS       MOS                                          
         LA    RF,LDSYTAB          TABLE OF LEDGER/SYSTEM                       
UPD0262  CLI   0(RF),X'FF'         IF NOT IN TABLE THAN NO PASSIVE FOR          
         BE    UPD0270             THIS LEDGER.                                 
         CLC   TRNKLDG,0(RF)                                                    
         BE    UPD0264                                                          
         LA    RF,2(RF)                                                         
         B     UPD0262                                                          
*                                                                               
UPD0264  MVC   AAVPSYS,1(RF)       SYSTEM                                       
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
         BNE   UPD0270             END OF LEDGER                                
*                                                                               
         XC    AAVPUSED,AAVPUSED   CLEAR OUT USED DATE                          
         CLI   NEWSTA,0            TEST UNOFFSETTING                            
         BE    *+10                                                             
         MVC   AAVPUSED,TODAYC     SET COMPRESSED OFFSET DATE                   
*                                                                               
         GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO1Q                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPD0270  MVC   KEY,SVKEY                                                        
         GOTO1 AIOEXEC,IORD+IOACCDIR+IO1Q                                       
         BE    *+6                 RE-ESTABLISH SEQUENCE                        
         DC    H'0'                                                             
*                                                                               
UPD0280  SR    R1,R1                                                            
         ICM   R1,3,TEMP                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,TEMP                                                        
         B     UPD0070                                                          
                                                                                
UPD0290  OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD0300             NO - MUST BE LIVE UPDATE                     
         GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         LA    R1,MRKSCRH                                                       
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPDATEX             PRTCLO HAS SET MESSAGE                       
                                                                                
UPD0300  LA    R1,MRKACTH                                                       
         NI    TWAMODE2,FF-TWAM2CHG  RESET CHANGES MADE BIT                     
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
                                                                                
QUIT     XC    MRKSCR,MRKSCR                                                    
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    TWAMODE2,FF-TWAM2CHG                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
                                                                                
***********************************************************************         
* ROUTINE TO COUNT ALL TRANSACTIONS OFFSET/UNOFFSET FOR AN OFFICE &   *         
* GIVEN STATUS                                                        *         
***********************************************************************         
                                                                                
CHKVAL   NTR1  ,                                                                
         TM    TSARINDS,TSARMKQ    TEST ALREADY MARKED                          
         BNO   CHKVAL2             NO - WE ARE MARKING                          
         TM    TSARINDS,TSARHITQ   TEST THIS TRANSACTION WAS RECORDED           
         BNO   CHKVALEQ            NO - EXIT                                    
         NI    TSARINDS,FF-TSARHITQ  YES - RESET STATUS                         
         SR    R1,R1               WE ARE UNMARKING                             
         ICM   R1,3,STSARHIT                                                    
         BNZ   *+6                                                              
         DC    H'0'                OUT OF STEP                                  
         BCTR  R1,0                DECREMENT HITS                               
         STCM  R1,3,STSARHIT                                                    
         B     CHKVALEQ            EXIT                                         
                                                                                
CHKVAL2  TM    TSARINDS,TSARHITQ   TEST THIS TRANSACTION WAS RECORDED           
         BO    CHKVALEQ            YES - EXIT                                   
         OC    STSARHIT,STSARHIT   MARKING - TEST ANY HITS                      
         BNZ   CHKVAL4             YES - TEST OFFICE/SELECT STATUS              
         MVC   STSAROFF,TSAROFF    NO - TAKE OFFICE                             
*&&UK*&& MVC   STSARAFC,TSARAFCC   NO - TAKE CURRENCY                           
         MVC   STSARSTT,TSARSTA    EXTRACT AND MASSAGE STATUS BYTE              
         NI    STSARSTT,TRNSAPPR   PRESERVE SELECT STATUS IF BITON              
         B     CHKVAL8                                                          
                                                                                
CHKVAL4  MVC   OVBYTE,TSARSTA      EXTRACT AND MASSAGE STATUS BYTE              
         NI    OVBYTE,TRNSAPPR     PRESERVE SELECT STATUS IF BITON              
         CLC   OVBYTE,STSARSTT     TEST SAME SELECTED STATUS                    
         BNE   CHKVALHI            NO - SET CC HIGH & EXIT                      
*&&UK                                                                           
         MVC   FVMSGNO,=AL2(AE$CURCC)                                           
         OC    TSARAFCC,TSARAFCC   TEST CURRENCY TRANSACTION                    
         BZ    CHKVAL6                                                          
         OC    STSARAFC,STSARAFC   TEST PREVIOUS CURRENCY TRANSACTION           
         BNZ   *+10                                                             
         MVC   STSARAFC,TSARAFCC                                                
         CLC   STSARAFC,TSARAFCC   TEST SAME CURRENCY                           
         BNE   CHKVALLO                                                         
*&&                                                                             
CHKVAL6  MVC   FVMSGNO,=AL2(EAMIXOF1)                                           
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BNO   *+22                NO - INCREASE HITS                           
         CLC   STSAROFF,TSAROFF    TEST OFFICE MATCHES                          
         BE    *+12                YES                                          
         TM    COMPSTA5,CPYSOFPL   TEST CROSS-OFFICE CONTRAS ALLOWED            
         BNO   CHKVALLO            NO                                           
                                                                                
         XC    FVMSGNO,FVMSGNO                                                  
                                                                                
CHKVAL8  OI    TSARINDS,TSARHITQ   SET RECORD HAS BEEN HIT                      
         ICM   R1,3,STSARHIT                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,STSARHIT                                                    
         B     CHKVALEQ            SET CC EQU AND EXIT                          
                                                                                
CHKVALHI MVI   OVBYTE,2            SET CC HIGH                                  
         B     *+16                                                             
CHKVALLO MVI   OVBYTE,0            SET CC LOW                                   
         B     *+8                                                              
CHKVALEQ MVI   OVBYTE,1            SET CC EQU                                   
         CLI   OVBYTE,1            SET CC EQU/HIGH/LOW                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MARK ALL TRANSACTIONS AS OFFSET/UNOFFSET                 *         
***********************************************************************         
                                                                                
MRKALL   NTR1  ,                   GLOBAL MARK/UNMARK                           
         MVC   OVHALF,STSARHIT     SAVE TOTAL HITS ON TSAR RECORDS              
         LA    R1,1                SET R1 FOR FIRST TSAR RECORD                 
MRKALL0  STH   R1,HALF             LOOK AHEAD FOR MIXED OFFICE/SEL.STAT         
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    MRKALL2                                                          
         GOTO1 AFILTER             FILTER TRANSACTION                           
         BNE   MRKALL2             NOT REQUIRED - GET NEXT                      
         LA    RF,X'10'            BO                                           
         CLC   OPTALL,AC@YES       IS USER MARKING?                             
         BE    *+8                                                              
         LA    RF,X'80'            BZ                                           
         TM    TSARINDS,TSARMKQ                                                 
         EX    RF,*+4                                                           
         NOP   MRKALL2             AVOID MULTIPLE CHKVAL CALLS                  
         BAS   RE,CHKVAL           TEST VALID TO MATCH/UNMATCH                  
         BE    MRKALL2             YES - CC EQU                                 
         MVC   STSARHIT,OVHALF     RESTORE TOTAL HITS ON TSAR RECORDS           
         MVC   FVMSGNO,=AL2(EASELMIX)                                           
         BH    MRKALLX             SELECT STATUS ERROR - CC HIGH                
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC   TEST CURRENCY TRANSACTION                    
         CLC   FVMSGNO,=AL2(AE$CURCC) CURRENCY CODE CONFLICT ERROR              
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(EAMIXOF2) MIXED OFFICE ERROR                        
*&&                                                                             
         CLI   SPACES,FF                                                        
         B     MRKALLX             ERROR - CC LOW                               
                                                                                
MRKALL2  LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX           TEST E-O-F                                   
         BNH   MRKALL0                                                          
                                                                                
         LA    R1,1                SET TO RESTART                               
         MVC   STSARHIT,OVHALF     RESTORE TOTAL HITS ON TSAR RECORDS           
MRKALL4  STH   R1,HALF                                                          
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    MRKALL40                                                         
         GOTO1 AFILTER             FILTER TRANSACTION                           
         BNE   MRKALL40            NOT REQUIRED - GET NEXT                      
         ZAP   OVDUB,TSARAMNT      WILL BE REVERSED IF A CREDIT WHEN            
*&&UK*&& ZAP   OVDUB2,TSARAFCA     DEBITS ARE BEING INCLUDED (& AFC)            
         CLI   LEDGBRCC,X'10'      TEST EXCLUDING DEBITS                        
         BE    MRKALL6             YES - LEAVE AMOUNT ALONE                     
         TM    TSARINDS,TSARDRQ    TEST DEBIT                                   
         BNZ   MRKALL6                                                          
         MP    OVDUB,PONENEG       CREDIT - REVERSE SIGN FOR TOTALS             
*&&UK*&& MP    OVDUB2,PONENEG      CR - REVERSE SIGN FOR AFC TOTALS             
                                                                                
MRKALL6  CLC   OPTALL,AC@YES       TEST IF USER IS MARKING                      
         BNE   MRKALL20                                                         
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    MRKALL40                                                         
         BAS   RE,CHKVAL           CALL CHKVAL AGAIN TO UPDATE RECORD           
         BE    *+6                                                              
         DC    H'0'                DIE IF SECOND CALL HITS A PROBLEM            
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BZ    *+20                                                             
         AP    TOTCUR,PONE                                                      
         BNZ   *+10                                                             
         XC    STSARAFC,STSARAFC                                                
*&&                                                                             
         OI    TSARINDS,TSARMKQ                                                 
         TM    TSARINDS,TSARINMQ   TEST INCOMING RECORD MARKED ON FILE          
         BNO   MRKALL8                                                          
         SP    TOTUOF,OVDUB        YES - SUBTRACT FROM UNMARKED                 
*&&UK*&& SP    CURUOF,OVDUB2       YES - SUBTRACT FROM UNMARKED (AFC)           
         B     MRKALL24                                                         
MRKALL8  AP    TOTOFS,OVDUB        NO - ADD TO MARKED                           
*&&UK*&& AP    CUROFS,OVDUB2       NO - ADD TO MARKED (AFC)                     
         B     MRKALL24                                                         
                                                                                
MRKALL20 TM    TSARINDS,TSARMKQ    IS RECORD CURRENTLY MARKED?                  
         BZ    MRKALL40                                                         
         BAS   RE,CHKVAL           CALL CHKVAL AGAIN TO UPDATE RECORD           
         BE    *+6                                                              
         DC    H'0'                DIE IF SECOND CALL HITS A PROBLEM            
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BZ    *+20                                                             
         SP    TOTCUR,PONE                                                      
         BNZ   *+10                                                             
         XC    STSARAFC,STSARAFC                                                
*&&                                                                             
         NI    TSARINDS,FF-TSARMKQ                                              
         TM    TSARINDS,TSARINMQ   TEST INCOMING RECORD MARKED ON FILE          
         BNO   MRKALL22                                                         
         AP    TOTUOF,OVDUB        YES - ADD TO UNMARKED                        
*&&UK*&& AP    CURUOF,OVDUB2       YES - ADD TO UNMARKED (AFC)                  
         B     MRKALL24                                                         
MRKALL22 SP    TOTOFS,OVDUB        NO - SUBTRACT FROM MARKED                    
*&&UK*&& SP    CUROFS,OVDUB2       NO - SUBTRACT FROM MARKED (AFC)              
                                                                                
MRKALL24 OI    TWAMODE2,TWAM2CHG   SET CHANGE HAS OCCURRED                      
         L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         B     MRKALL40                                                         
         DROP  RF                                                               
                                                                                
MRKALL40 LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX                                                        
         BNH   MRKALL4                                                          
         CR    RB,RB               SET CC EQU                                   
                                                                                
MRKALLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA RECORD FILTERING                                               *         
***********************************************************************         
                                                                                
RFILTER  NTR1  ,                                                                
         ICM   R2,15,ATRNEL        TEST TRANSACTION ELEMENT                     
         BZ    RFILTNEQ                                                         
         USING TRNELD,R2                                                        
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   RFILTERX                                                         
         SR    RF,RF                                                            
         IC    RF,LEDGBRCC         INCLUDE/EXCLUDE/ONLY DEBITS                  
         TM    TRNSTAT,TRNSDR      TEST TRANSACTION IS A DEBIT                  
         EX    RF,*+4                                                           
         NOP   RFILTNEQ            NOP/BO/BZ                                    
                                                                                
         OC    OFFICE,OFFICE       TEST OFFICE FILTER SET                       
         BZ    RFILT02                                                          
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   RFILTERX            WRONG OFFICE - EXIT WITH CC SET NEQ          
         CLC   TRNOFFC(0),OFFICE                                                
                                                                                
         USING MPYELD,R3                                                        
RFILT02  ICM   R3,15,AMPYEL                                                     
         BZ    RFILT04                                                          
         CLC   MPYNO,SPACES        TEST SPARE ELEMENT                           
         BH    RFILTNEQ                                                         
         CP    MPYAMNT,PZERO                                                    
         BNE   RFILTERX                                                         
         CLC   MPYBNK,SPACES                                                    
         BH    RFILTNEQ                                                         
                                                                                
         USING TRSELD,R3                                                        
RFILT04  ICM   R3,15,ATRSEL        R3=A(TRSEL)                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE IN RANGE                  
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
         CLC   TRSUDAT,UDASTA      TEST OFFSET (USED) DATE IN RANGE             
         BL    RFILTNEQ                                                         
         CLC   TRSUDAT,UDAEND                                                   
         BH    RFILTNEQ                                                         
         TM    TRSSTAT,TRSSOFFS    TEST ALREADY OFFSET                          
         BNO   RFILT06             NO - TEST NOT OFFSET                         
         CLI   ICLMARK,ICLMNO      YES - TEST EXCLUDE OFFSET INVOICES           
         BNE   RFILT08             NO - CARRY ON                                
         CLC   UDASTA,=X'0000'     TEST EXPLICIT USED DATE START                
         BNE   RFILT08             YES - CARRY ON                               
         CLC   UDAEND,=X'FFFF'     TEST EXPLICIT USED DATE END                  
         BNE   RFILT08             YES - CARRY ON                               
         B     RFILTNEQ            OTHERWISE SET CC NEQ AND EXIT                
                                                                                
RFILT06  CLI   ICLMARK,ICLMONLY    NOT OFFSET - TEST OFFSET ONLY                
         BE    RFILTNEQ            YES - SET CC NEQ AND EXIT                    
         CLC   UDASTA,=X'0000'     TEST EXPLICIT USED DATE START                
         BNE   RFILTNEQ            YES - DROP UNUSED ITEM                       
         CLC   UDAEND,=X'FFFF'     TEST EXPLICIT USED DATE END                  
         BNE   RFILTNEQ            YES - DROP UNUSED ITEM                       
         L     R1,AIOBUFF          R1=A(DATA RECORD KEY)                        
         USING TRNRECD,R1                                                       
         TM    TRNRSTA2,TRNSUSED   TEST USED STATUS IN RECORD KEY               
         BNZ   RFILTNEQ                                                         
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNE   RFILT08             YES - OFFSET MOS DOES NOT EXIST              
         OC    TRSUMOS,TRSUMOS     TEST OFFSET MOS IN TRSEL                     
         BNZ   RFILTNEQ                                                         
         OC    TRSUDAT,TRSUDAT     TEST USED DATE IN TRSEL                      
         BNZ   RFILTNEQ                                                         
                                                                                
RFILT08  GOTO1 ABLDSRC             BUILD SOURCE ACCOUNT IN SRCWORK              
                                                                                
         OC    SRCACC,SRCACC       TEST SOURCE FILTER INPUT                     
         BZ    RFILTEQU                                                         
         IC    RF,SRCACCXL                                                      
         EX    RF,*+8                                                           
         B     RFILTERX            EXIT WITH CC SET                             
         CLC   SRCWORK(0),SRCACC   TEST ELEMENT TYPE=LEDGER CODE                
                                                                                
RFILTEQU CR    RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTNEQ LTR   RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTERX B     EXIT                                                             
         DROP  R1,R2,R3                                                         
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
         BZ    SETKEY00                                                         
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                SUPPLIER MISSING                             
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY00 TM    WORK,SETOFF         SET OFFICE                                   
         BZ    SETKEY02                                                         
         MVC   TRNKOFF,SPACES                                                   
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    SETKEY02            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    SETKEY02            YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    SETKEY02                                                         
         IC    RE,OFFICEXL                                                      
         EX    RE,*+4                                                           
         MVC   TRNKOFF(0),OFFICE                                                
                                                                                
SETKEY02 TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY04                                                         
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCULC+L'TRNKCULC-1,X'41'                                      
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY04                                                         
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY04 TM    WORK,SETSDT         SET TRANSACTION DATE                         
         BZ    SETKEY06                                                         
         MVC   TRNKDATE,PERSTA                                                  
                                                                                
SETKEY06 TM    WORK,SETREF         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY08                                                         
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    REFSTA,REFSTA                                                    
         BZ    SETKEY08                                                         
         MVC   TRNKREF,REFSTA                                                   
                                                                                
SETKEY08 TM    WORK,NXTOFF         BUMP OFFICE (NEW OFFICES ONLY)               
         BZ    SETKEY10                                                         
         IC    RE,TRNKOFF+(L'TRNKOFF-1)                                         
         LA    RE,1(RE)                                                         
         STC   RE,TRNKOFF+(L'TRNKOFF-1)                                         
                                                                                
SETKEY10 TM    WORK,NXTCON         BUMP CONTRA                                  
         BZ    SETKEY12                                                         
         IC    RE,TRNKCACT+(L'TRNKCACT-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKCACT+(L'TRNKCACT-1)                                       
                                                                                
SETKEY12 TM    WORK,NXTSDT         BUMP DATE                                    
         BZ    SETKEY14                                                         
         IC    RE,TRNKDATE+(L'TRNKDATE-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKDATE+(L'TRNKDATE-1)                                       
                                                                                
SETKEY14 DS    0H                                                               
                                                                                
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
                                                                                
***********************************************************************         
* CL1 LEDGER, XL1 BRANCH CC FOR FILTERING DR TRANSACTIONS             *         
***********************************************************************         
                                                                                
LDGLIST1 DS    0CL2                CREDITOR CONTRA - VALID LEDGERS              
*&&UK                                                                           
         DC    C'F',X'10'          EXCLUDE DEBITS                               
         DC    C'T',X'10'          EXCLUDE DEBITS                               
         DC    C'V',X'10'          EXCLUDE DEBITS                               
         DC    C'X',X'10'          EXCLUDE DEBITS                               
*&&                                                                             
*&&US                                                                           
         DC    C'P',X'10'          EXCLUDE DEBITS                               
         DC    C'Q',X'10'          EXCLUDE DEBITS                               
         DC    C'S',X'10'          EXCLUDE DEBITS                               
         DC    C'T',X'10'          EXCLUDE DEBITS                               
         DC    C'U',X'10'          EXCLUDE DEBITS                               
         DC    C'V',X'10'          EXCLUDE DEBITS                               
         DC    C'W',X'10'          EXCLUDE DEBITS                               
         DC    C'X',X'10'          EXCLUDE DEBITS                               
         DC    C'Y',X'10'          EXCLUDE DEBITS                               
*&&                                                                             
         DC    AL1(EOT)                                                         
                                                                                
LDGLIST2 DS    0CL(L'LDGLIST1)     GENERAL CONTRA - VALID LEDGERS               
         DC    C'A',X'00'          INCLUDE DEBITS                               
         DC    C'K',X'00'          INCLUDE DEBITS                               
         DC    C'Q',X'00'          INCLUDE DEBITS                               
         DC    C'Z',X'00'          INCLUDE DEBITS                               
         DC    AL1(EOT)                                                         
                                                                                
DUMCON   DC    C'SJ999'                                                         
         EJECT                                                                  
                                                                                
SUPNDSP  EQU   19                                                               
                                                                                
OVRWRKD  DSECT                                                                  
                                                                                
OVDUB    DS    D                                                                
*&&UK                                                                           
OVDUB2   DS    D                                                                
*&&                                                                             
OVFULL   DS    F                                                                
OVHALF   DS    H                                                                
OVBYTE   DS    XL1                                                              
OVCHAR   DS    XL1                                                              
*&&UK                                                                           
OVAFCXS  DS    XL(L'AFCXSTA2)                                                   
*&&                                                                             
                                                                                
NEWSTA   DS    XL1                 NEW TRANSACTION STATUS                       
NEWUSED  DS    PL2                 OFFSET USED MOS (PWOS YYMM)                  
OLDUSED  DS    PL2                 UNOFFSET USED MOS (PWOS YYMM)                
                                                                                
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
TOTAOFS  DS    PL8                 CREDITS AVAILABLE FOR OFFSET                 
TOTOFS   DS    PL8                 OFFSET THIS TIME                             
TOTUOF   DS    PL8                 UNOFFSET THIS TIME                           
TOTCUR   DS    PL8                 TOTAL NUMBER OF CURRENCY CODES               
*&&UK                                                                           
         ORG   CURTOTS                                                          
CURAOFS  DS    PL8                 CREDITS AVAILABLE FOR OFFSET                 
CUROFS   DS    PL8                 OFFSET THIS TIME                             
CURUOF   DS    PL8                 UNOFFSET THIS TIME                           
*&&                                                                             
                                                                                
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKF5D                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE3D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACMRK05   07/12/18'                                      
         END                                                                    
