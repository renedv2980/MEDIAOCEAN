*          DATA SET ACMRK0D    AT LEVEL 114 AS OF 05/14/20                      
*PHASE T6160DB                                                                  
*INCLUDE PAY2JOB                                                                
ACMRK0D  TITLE 'CREDITOR - MANUAL'                                              
***********************************************************************         
*                                                                     *         
* ABID 114 16MAY20  EXPENSES EU MF A55/5P - SEED CLAIM ON   DSRD-25289*         
*                   PAYMENTS - RELINK FOR ACPAY2JOB                   *         
* NMAL 113 09MAR20  FIX-CLEARING 31-BIT MEMORY              SPEC-43879*         
* NMAL 112 24AUG18  RECOMPILE TO PICK LATEST PAY2JOB                  *         
* NMAL 111 12JUN18  INCLUDED ACDCPAYJD IN ACMRKWRK                    *         
* NMAL 110 17OCT17  PAY2JOB PAYMENT SEEDING TO SJ            SPEC-9021*         
***********************************************************************         
* JFOX 043 FIX BAD LABEL                                                        
* JFOX 044 CREDIT NOTES (ALLOW -CR TO BANK)                                     
* JFOX 045 ADVANCES/MINOR DIFFERENCES                                           
* JFOX 046 FIX TO LOGIC FOR MERGED BANK AND/OR P&L POSTINGS BY OFFICE           
* JFOX 047 SWAP DR/CR AND +/- IF INCOME FOR DISCOUNT/DIFFERENCE                 
* JFOX 048 FIX OVERFLOWING GETTXT CALL FOR EXCHANGE DIFFERENCE MESSAGE          
*          USE SORAWRK IF PRESENT, INSTEAD OF CPJWRK                            
* JFOX 049 SET EARLIEST PAYMENT DATE IN INVOICE TSAR RECORD                     
* JFOX 050 FIX CURRENCY DISCOUNT CALCULATION (TRNAMNT=0, AFCAMNT=NON-0)         
* JFOX 051 ALLOW SETACT ERROR - EXIT WITH MISSING ACCOUNT IN FVXTRA             
* JFOX 052 PROFILE FOR MAXIMUM DIFFERENCE                                       
* JFOX 053 MAKE DEFAULT TAX RATE 15%.  RENAME DSECT                             
* JFOX 054 ALLOW NO TRANSACTIONS IF ADVANCES/DIFFERENCES PERMITTED              
* JNEW 055 OUTPUT ERROR MESSAGE IF CAN'T CREATE ADVANCE/DIFFERENCE              
* JNEW 056 FIX BUG IN TRSEL CODE                                                
* JFOX 057 CLEAR TRSUMOS WHEN CLEARING TRSUDAT                                  
* JFOX 058 CHECK ROOM FOR ADVANCE/DIFF BEFORE NEW SESSION. Y2000 FIX            
* JFOX 059 U.S./U.K. COMPATIBILITY CHANGES                                      
* JFOX 060 ABEND IF SOMEONE AMENDING ELSEWHERE                                  
* JNEW 061 ADD SG ATTRIBUTE TO GERMAN INCOME POSTINGS                           
* JFOX 062 SUPPORT INVOICE LEVEL TAX ACCOUNTS/RATES BY OFFICE (GERMANY)         
* JFOX 063 ALLOW ZERO CHEQUE AMOUNT (GERMANY)                                   
* JFOX 064 USE CLI/PRO AS INCOME C/A AND POST TO COSTING (GERMANY)              
* JNEW 065 SUPPORT FOR EXCHANGE DIFFERENCE POSTING TO SJ NOT SE                 
* /JFOX    ONLY POST TO SJ SOURCE.  FIX CONTRA NAME/LOCKED JOB BUGS             
* JFOX 066 CHANGE DEFAULT TAX RATE FROM 15%->16% FOR DISCOUNT TAX ADJ.          
* JFOS 067 AFCEL STATUS BIT CHANGES                                             
* JFOX 068 USE EUREKA INSTEAD OF EXCHP, FOR EURO COMPLIANCE                     
* JFOX 069 SUPPORT SECONDARY CURRENCY CHEQUE.  EUREKA REPLACES GETCUR           
*     -070 & FIX MULTIPLE MPYEL TEST                                            
* JFOX 071 DON'T SKIP OUT IF ZERO RATED TAX.  FIX OFXTAB ACCUMULATION           
* JFOX 072 MAINTAIN SECONDARY CURRENCY EXCHANGE DIFFERENCES                     
* JFOX 073 UK - USE TOBACCO, NOT HELLO TO ADD/DEL CASH ELEMENTS EXCEPT          
*          AFCELS AND DUMMY MPYELS WITH MPYAMNT NOT YET SET PACKED              
* JFOX 074 SCREEN DSECTS                                                        
* JFOX 075 SKIP UNKNOWN COSTING GROUPS                                          
* JFOX 076 PASS TOBACCO VALUES TO ADDTRN                                        
* JFOX 077 DON'T SWAP FROM/TO CURRENCY CODES FOR EUREKA INVERT CALL             
* JFOX 078 SET CREDIT OCAEL QMPYAMNT, IF CONVERTED TO OCAELS                    
*          MAINTAIN 2ND CURRENCY FOR COSTING & INTERCOMPANY POSTINGS            
*          SET TRNS2NDC FOR ALL SECONDARY CURRENCY POSTINGS                     
* JFOX 079 PASS MORE TOBACCO VALUES TO ADDTRN                                   
* JFOX 080 FIX BUG                                                              
* JFOX 081 DON'T TEST MIN/MAX V GBP IF FOREIGN CURRENCY IS GBP                  
*          PROTECT EURO MEMBER EXCHANGE RATE PASSED BY EUREKA                   
* JFOX 082 DON'T SKIP TAX ADJUSTMENT TO INCOME SECONDARY AMOUNT                 
*          DELETE OCAEL AFTER ADDTRN TO ENSURE A FRESH ONE                      
* JFOX 083 MORE EURO/FOREIGN CURRENCY CHANGES                                   
* JFOX 084 TIDY UP EXCHANGE RATE/CURRENCY BANK CHARGE DISPLAY                   
* JFOX 085 FIX BUG - 2ND CURRENCY DIFFERENCE IN TSARSCUA (NOT TSARSCUD)         
* JFOX 086 DIFFERENCE - SWAP TSARSCUA ARITHMETIC AS IF DISCOUNT                 
* JFOX 087 RETAIN SYSTEM EXCHANGE RATE                                          
* JFOX 088 TEST NON-ZERO 2ND CURRENCY BEFORE SKIPPING ZERO POSTING              
* JFOX 089 FIX BANK CHARGE FROM/TO CURRENCY (LOCAL CURRENCY BANK A/C)           
* JFOX 090 FURTHER DIFFERENCE TSARSCUA ARITHMETIC FIX                           
* JFOX 091 FURTHER EURO MEMBER CURRENCY SUPPORT                                 
* JFOX 092 SAVE CURRENCY CODES FROM SCREEN                                      
* JFOX 093 US/UK COMPATIBILITY                                                  
* JFOX 094 FIX DISCOUNT ACCOUNT OFFICE IN KEY (READING FOR COSTING)             
* JFOX 095 FIX OFFICE EXTRA DISCOUNT ACCOUNT MISSING COMPANY                    
ACMRK0D  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STORAGEL,**MRKD**,RA,R9,R5,CLEAR=YES,RR=RE                       
         LR    RF,RC               SAVED A(ACQUIRED STORAGE)                    
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
*&&UK                                                                           
S        USING CURTABD,SCNDCURT                                                 
P        USING CURTABD,PRSTCURT                                                 
C        USING CURTABD,COMPCURT                                                 
*&&                                                                             
         L     R8,AOVERWRK                                                      
         USING OVRWRKD,R8                                                       
         ST    RE,OVRELO                                                        
         ST    RF,AOFFTABT         SAVE A(OFFICE TABLE TOTALS)                  
         LA    RF,OFFTABL(RF)                                                   
         ST    RF,AOFFTAB          SAVE A(OFFICE TABLE)                         
         AH    RF,=Y(OFFLTAB)                                                   
         TM    COMPSTA4,CPYSICPY   TEST MAKING INTERCOMPANY POSTINGS            
         BZ    *+8                                                              
         ST    RF,AICOTAB          SAVE A(INTERCOMPANY TABLE)                   
         LA    RF,ICOLTAB(RF)                                                   
         ST    RF,AOFXTAB          SAVE A(OFFICE EXTRA TABLE)                   
         AH    RF,=Y(OFXLTAB)                                                   
         ST    RF,AOVIOS1          SAVE A(OVERLAY IOAREA 1 - SAVE               
         LA    RF,L'IOSAVE(RF)                                                  
         ST    RF,AOVIOB1                                  - BUFFER             
         LA    RF,L'IOBUFF(RF)                                                  
         ST    RF,AOVIOS2          SAVE A(OVERLAY IOAREA 2 - SAVE               
         LA    RF,L'IOSAVE(RF)                                                  
         ST    RF,AOVIOB2                                  - BUFFER             
         LA    RF,L'IOBUFF(RF)                                                  
         ST    RF,AOVIOS3          SAVE A(OVERLAY IOAREA 3 - SAVE               
         LA    RF,L'IOSAVE(RF)                                                  
         ST    RF,AOVIOB3                                  - BUFFER             
                                                                                
         SAM31 ,                                                                
         L     R1,A31BWS           GET AJARAY FROM 31BIT W/S                    
         AHI   R1,JARAY-SVR31BD                                                 
         ST    R1,AJARAY                                                        
         XC    0(255,R1),0(R1)     CLEARING THE MEMORY                          
         SAM24 ,                                                                
                                                                                
         LH    R1,=Y(INPHEDH-TWAD) SET INPUT SCREEN DISPS FOR ROOT              
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPHD2H-TWAD)                                              
         STH   R1,DISPHED2         DISPLACEMENT OF INPUT 2ND HEADLINE           
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
                                                                                
         LA    R1,OVROUT1          SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,OVROUT1N                                                      
         XR    RE,RE                                                            
         L     RF,=A(OVROU1)                                                    
         A     RF,OVRELO           RF=A(GLOBAL ROUTINES)                        
INIT02   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT02                                                        
                                                                                
         LA    R1,OVROUT2          SET A(ROOT ROUTINES TWO IN W/S)              
         LA    R0,OVROUT2N                                                      
         XR    RE,RE                                                            
         L     RF,=A(OVROU2)                                                    
         A     RF,OVRELO           RF=A(GLOBAL ROUTINES)                        
INIT04   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT04                                                        
                                                                                
         LA    R1,OVLIDSL          MOVE LITERALS INTO W/S                       
         LA    RF,OVLIDCL                                                       
         CR    RF,R1                                                            
         BE    *+6                                                              
         DC    H'0'                LITERALS/STORAGE ARE NOT IN STEP             
         LA    R0,OVLIDS                                                        
         LA    RE,OVLIDC                                                        
         MVCL  R0,RE                                                            
                                                                                
         CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              PRE-VALIDATE HEADER SCREEN                   
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              SET NEXT ACCOUNT                             
         CLI   XACTION,ACTUPDT                                                  
         BE    UPDATE              UPDATE                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTMANU                                                  
         BE    *+6                 MANUAL                                       
         DC    H'0'                                                             
         CLI   TWASCROV,CMSCR1                                                  
         BE    VALHED              MANUAL - VALIDATE HEADER                     
         CLI   TWASCROV,CMSCR2                                                  
         BE    VALINP              MANUAL - VALIDATE INPUT                      
         CLI   TWASCROV,CMSCR3                                                  
         BNE   INIT06                                                           
         GOTO1 AVALPWI             MANUAL - PAYMENT WITHOUT INVOICE             
         B     EXIT                                                             
INIT06   CLI   TWASCROV,CMSCR4                                                  
         BNE   INIT08                                                           
         GOTO1 AVALDIF             MANUAL - DIFFERENCE                          
         B     EXIT                                                             
INIT08   DC    H'0'                UNKNOWN EVENT                                
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER SCREEN                                          *         
***********************************************************************         
                                                                                
PREVAL   DS    0H                                                               
*&&UK                                                                           
         TM    SSIND1,SSI1XSYS     TEST SYSTEM EXCHANGE RATE                    
         BZ    *+12                                                             
         NI    MANRATH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         OI    MANRATH+(FVOIND-FVIHDR),FVOXMT                                   
*&&                                                                             
         CLI   PROFBTIC,C'C'                                                    
         BNE   PREVAL02                                                         
         OI    MANBRFTH+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    MANBMOTH+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    MANBNATH+(FVATRB-FVIHDR),FVAHIGH                                 
         OI    MANBRFTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    MANBMOTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    MANBNATH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
PREVAL02 OC    MANBNA,MANBNA       TEST BATCH NAME PRESENT                      
         BNZ   PREVAL04            LEAVE AS FOUND                               
         L     RE,AUTL             SET LUID AS DEFAULT NAME                     
         MVC   MANBNA(L'TSYM),TSYM-UTLD(RE)                                     
         OI    MANBNAH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
PREVAL04 LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREVAL08                                                         
         CLI   LTYPE,TYPBNK        TEST BANK ACTION LAST                        
         BNE   PREVAL06                                                         
         XC    MANBNK,MANBNK                                                    
         OI    MANBNKH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   MANBNK(L'ACTKACT),ACTKACT                                        
         CLC   ACTKUNT(L'BANKUL),BANKUL                                         
         BE    PREVAL08                                                         
         MVI   MANBNK,C'*'         OVERRIDE BANK UNIT/LEDGER                    
         MVC   MANBNK+1(L'ACTKULA),ACTKULA                                      
         B     PREVAL08                                                         
                                                                                
PREVAL06 CLI   LTYPE,TYPCRD        TEST CREDITOR ACTION LAST                    
         BNE   PREVAL08                                                         
         XC    MANLDG,MANLDG                                                    
         XC    MANSUP,MANSUP                                                    
         OI    MANLDGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANSUPH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   MANLDG,ACTKLDG                                                   
         MVC   MANSUP(L'ACTKACT),ACTKACT                                        
         B     PREVAL08                                                         
                                                                                
PREVAL08 XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
                                                                                
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    PREVAL10                                                         
         MVI   OVFULL,ACTCURR                                                   
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),('TYPCRD',OVFULL)              
         BE    PREVAL12                                                         
                                                                                
PREVAL10 DS    0H                                                               
*&&UK                                                                           
         XC    MANCAMC,MANCAMC     CLEAR CURRENCY FIELDS                        
         OI    MANCAMCH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    S.CURTCUR,S.CURTCUR TEST SECONDARY CURRENCY                      
         BZ    *+10                                                             
         MVC   MANCAMC,C.CURTCUR   DISPLAY CHEQUE PRIMARY CURRENCY              
         NI    MANCAMH+(FVATRB-FVIHDR),FF-(FVAHIGH)                             
         XC    MANBCHC,MANBCHC                                                  
         OI    MANBCHCH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANCURC,MANCURC                                                  
         OI    MANCURCH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    MANCURCH+(FVATRB-FVIHDR),FVAPROT                                 
         XC    MANCURT,MANCURT                                                  
         OI    MANCURTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANCUR,MANCUR                                                    
         OI    MANCURH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANCURH+(FVATRB-FVIHDR),FVAPROT                                  
         NI    MANCURH+(FVATRB-FVIHDR),FF-(FVAHIGH)                             
         XC    MANBCHT,MANBCHT                                                  
         OI    MANBCHTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANBCH,MANBCH                                                    
         OI    MANBCHH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANBCHH+(FVATRB-FVIHDR),FVAPROT                                  
         XC    MANBCH2,MANBCH2                                                  
         OI    MANBCH2H+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANRATT,MANRATT                                                  
         OI    MANRATTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANRAT,MANRAT                                                    
         OI    MANRATH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANRATH+(FVATRB-FVIHDR),FVAPROT                                  
         NI    MANRATH+(FVATRB-FVIHDR),FF-(FVAHIGH)                             
         XC    MANRATY,MANRATY                                                  
         OI    MANRATYH+(FVOIND-FVIHDR),FVOXMT                                  
*&&                                                                             
PREVAL12 DS    0H                                                               
*&&UK*&& XC    PRSTCURT,PRSTCURT                                                
*&&UK                                                                           
         OC    S.CURTCUR,S.CURTCUR TEST SECONDARY CURRENCY                      
         BNZ   PREVAL14                                                         
                                                                                
         XC    MANSCUC,MANSCUC     CLEAR SECONDARY CURRENCY FIELDS              
         OI    MANSCUCH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANSCUT,MANSCUT                                                  
         OI    MANSCUTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANSCU,MANSCU                                                    
         OI    MANSCUH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANSCUH+(FVATRB-FVIHDR),FVAPROT                                  
         B     PREVAL16                                                         
                                                                                
PREVAL14 MVC   MANSCUC,S.CURTCUR   SET SECOND CURRENCY                          
         OI    MANSCUCH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   MANCAMC,C.CURTCUR   SET PRIMARY CURRENCY                         
         OI    MANCAMCH+(FVOIND-FVIHDR),FVOXMT                                  
*&&                                                                             
PREVAL16 DS    0H                                                               
                                                                                
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT SUPPLIER ACCOUNT IN RELEVANT SCREEN FIELD                  *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   MANSUPH+(FVILEN-FVIHDR),0  TEST INPUT TO SUPPLIER                
         BNE   *+16                                                             
         XC    TWASKEY,TWASKEY     NO - CLEAR KEY SAVED IN TWA                  
         XC    ACCOUNT,ACCOUNT     AND MUST CLEAR SUPPLIER ACCOUNT              
         OC    LEDGER,LEDGER       TEST LEDGER ALEADY SET                       
         BNZ   *+12                                                             
         BAS   RE,VALLDG           VALIDATE LEDGER FIRST                        
         BNE   NXTACCX             INVALID LEDGER                               
         OC    TWASKEY,TWASKEY     TEST SAVED IN TWA                            
         BZ    *+14                                                             
         CLC   MANLDG,TWASKEY+L'ACTKUNT  TEST LEDGER MATCHES SAVED              
         BNE   *+14                                                             
         CLC   MANLDG,LEDGER+L'ACTKUNT  TEST LEDGER CHANGE                      
         BE    NXTACC4                                                          
         XC    TWASKEY,TWASKEY     YES - CLEAR KEY SAVED IN TWA                 
         XC    ACCOUNT,ACCOUNT     (RE)SET ACCOUNT TO ZERO                      
         CLC   MANLDG,LEDGER+L'ACTKUNT  TEST NEW LEDGER TO BE VALIDATED         
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
NXTACC10 XR    RF,RF               BUMP KEY FOR NEXT SUPPLIER                   
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
                                                                                
NXTACC12 XR    RF,RF               BUMP KEY FOR NEXT SUPPLIER                   
         IC    RF,ACTKCULA+L'ACTKCULA-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,ACTKCULA+L'ACTKCULA-1                                         
         MVI   GETIND,GETIABLQ+GETINLOK                                         
         GOTO1 AGETACC,0                                                        
         BNE   NXTACC14            NOT FOUND/NOT VALID                          
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BE    NXTACC18            UNIT/LEDGER IS STILL OK                      
         B     NXTACC16            UNIT/LEDGER HAS CHANGED - FINISH             
                                                                                
NXTACC14 CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC16            PAST SUPPLIER LEDGER                         
         MVI   GETIND,GETIABLQ+GETINLOK  RE-/READ ACCOUNT                       
         GOTO1 AGETACC,0                                                        
         BE    NXTACC18            VALID THIS TIME                              
         B     NXTACC12            STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC16 MVC   FVMSGNO,=AL2(EANOACCS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,MANLDGH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC18 MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 VACSRCHC,DMCB,MANSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
                                                                                
*&&UK                                                                           
         MVI   FVMINL,1                                                         
         XC    PRSTCURT,PRSTCURT                                                
         XC    MANSUPC,MANSUPC                                                  
         OI    MANSUPCH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALSUP,MANSUPH                                                  
         PUSH  USING                                                            
PRESET   USING CURTABD,PRSTCURT                                                 
         OI    MANSUPCH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   MANSUPC,PRESET.CURTCUR                                           
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BNE   *+14                                                             
         XC    MANSUPC,MANSUPC                                                  
         MVI   MANSUPC,C'*'                                                     
         POP   USING                                                            
*&&                                                                             
         OI    MANSUPH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
         MVC   FVMSGNO,=AL2(IAEPAP1N)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,MANSUPH                                                       
         ST    R1,FVADDR                                                        
                                                                                
NXTACCX  NI    TWAMODE2,FF-TWAM2NXA                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                       *         
***********************************************************************         
                                                                                
VALHED   DS    0H                                                               
                                                                                
*&&US                                                                           
***********************************************************************         
* VALIDATE UNIT & LEDGER                                              *         
***********************************************************************         
                                                                                
VALLDG   XR    R0,R0               CLEAR R0                                     
         TM    TWAMODE2,TWAM2NXA   TEST CALLED BY NXTACC                        
         BNO   *+6                                                              
         LR    R0,RE               YES - SAVE A(RETURN) TO NXTACC               
         TM    MANLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDGX                                                          
         XC    MANLDGN,MANLDGN                                                  
         OI    MANLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,MANLDGH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
         LA    R1,LDGLIST          R1=A(LIST OF VALID LEDGERS)                  
VALLDG02 CLC   MANLDG,0(R1)                                                     
         BE    VALLDG04            LEDGER IN LIST - VALIDATE IT                 
         CLI   0(R1),EOT                                                        
         BE    *+12                                                             
         LA    R1,L'ACTKLDG(R1)                                                 
         B     VALLDG02                                                         
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALLERR             ERROR - NOT IN VALID LEDGER LIST             
                                                                                
VALLDG04 MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALLDG,MANLDGH                                                  
         BH    VALLERR                                                          
         MVC   MANLDGN,RECNAME     NAME EXTRACTED BY GETLDG                     
         B     VALLDGX                                                          
                                                                                
VALLERR  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BNZR  RE                  YES - EXIT WITH CC NEQ                       
         B     EXIT                NO - EXIT TO ROOT WITH MESSAGE SET           
                                                                                
VALLDGX  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BZ    *+8                 NO - CONTINUE HDR SCREEN VALIDATION          
         CR    R0,R0               SET CC EQU                                   
         BR    RE                  AND RETURN                                   
                                                                                
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
***********************************************************************         
                                                                                
VALSUP   TM    MANSUPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSUPX                                                          
         MVI   FVMINL,1                                                         
         GOTO1 AVALSUP,MANSUPH                                                  
         BNE   EXIT                                                             
         GOTO1 VACSRCHC,DMCB,MANSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
         OC    RECCDSC,RECCDSC                                                  
         BZ    *+16                                                             
         MVC   CDSCACC(L'ACTKCPY),COMPANY                                       
         MVC   CDSCACC+(ACTKULA-ACTKCULA)(L'ACTKULA),RECCDSC                    
         MVC   SUPPACC,ACCOUNT                                                  
                                                                                
VALSUPX  DS    0H                                                               
*&&                                                                             
                                                                                
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
                                                                                
VALBNK   TM    MANBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNKX                                                          
         XC    BANKIND1,BANKIND1                                                
         XC    BANKXTRA(BANKXTRL),BANKXTRA                                      
*&&UK                                                                           
         XC    BANKCUR,BANKCUR                                                  
         NI    SAFCIND1,FF-(SAFCI1BC)                                           
         XC    MANCAMC,MANCAMC                                                  
         OI    MANCAMCH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    S.CURTCUR,S.CURTCUR TEST SECONDARY CURRENCY                      
         BZ    *+10                                                             
         MVC   MANCAMC,C.CURTCUR   DISPLAY CHEQUE PRIMARY CURRENCY              
*&&                                                                             
         CLI   MANBNK,C'*'         TEST NON-STANDARD BANK U/L                   
         BNE   VALBNK06                                                         
         LA    R1,BNKLIST                                                       
VALBNK02 CLI   0(R1),EOT                                                        
         BNE   VALBNK04                                                         
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         LA    R1,MANBNKH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                EXIT WITH ERROR SET                          
VALBNK04 CLC   0(L'ACTKLDG,R1),MANBNK+1                                         
         BE    VALBNK06                                                         
         LA    R1,L'ACTKLDG(R1)                                                 
         B     VALBNK02                                                         
                                                                                
VALBNK06 MVC   BANKACC,ACCOUNT     SAVE ACCOUNT                                 
         MVC   BANKIND1,ACCIND1    SAVE ACCOUNT INDICATOR - 1                   
         MVI   FVMINL,1                                                         
         GOTO1 AVALBNK,MANBNKH                                                  
         BE    VALBNK08                                                         
         MVC   ACCOUNT,BANKACC     ERROR - RESTORE ACCOUNT                      
         MVC   ACCIND1,BANKIND1                                                 
         B     EXIT                                                             
                                                                                
VALBNK08 MVI   CHQIND1,0           CLEAR CHEQUE INDICATOR                       
         MVC   TEMP(L'RECCDSC),RECCDSC                                          
         MVC   TEMP+L'RECCDSC(L'RECDTAX),RECDTAX                                
         MVC   TEMP+L'RECCDSC+L'RECDTAX(L'RECEXDF),RECEXDF                      
         GOTO1 AGETLDG,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TEMP(L'RECCDSC),SPACES                                           
         BNH   *+10                                                             
         MVC   RECCDSC,TEMP                                                     
         CLC   TEMP+L'RECCDSC(L'RECDTAX),SPACES                                 
         BNH   *+10                                                             
         MVC   RECDTAX,TEMP+L'RECCDSC                                           
         CLC   TEMP+L'RECCDSC+L'RECDTAX(L'RECEXDF),SPACES                       
         BNH   *+10                                                             
         MVC   RECEXDF,TEMP+L'RECCDSC+L'RECDTAX                                 
*&&UK                                                                           
         TM    COMPSTA5,CPYSOFPL   TEST OFFICE FOR P&L                          
         BZ    *+12                                                             
         OI    CHQIND1,CHQISPAL    SPLIT PROFIT & LOSS BY OFFICE                
         B     VALBNK10                                                         
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    VALBNK10                                                         
         OI    CHQIND1,CHQISBNK    SPLIT BANK BY OFFICE                         
         OI    CHQIND1,CHQISPAL    SPLIT PROFIT & LOSS BY OFFICE                
*&&                                                                             
VALBNK10 TM    COMPSTA4,CPYSICPY   TEST MAKING INTERCOMPANY POSTINGS            
         BZ    VALBNK16                                                         
         CLC   RECOFFC,SPACES                                                   
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$BOUNK)                                           
         BNH   VALBNK12                                                         
         ICM   R1,15,RECALDGT      R1=A(LEDGER TABLE ENTRY)                     
         CLI   LEDGTOFF-LEDGTABD(R1),1                                          
         BE    VALBNK14            OFFICE MUST BE IN ACCOUNT BYTE 1             
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
VALBNK12 MVC   ACCOUNT,BANKACC        ERROR - RESTORE ACCOUNT                   
         MVC   ACCIND1,BANKIND1                                                 
         B     EXIT                                                             
VALBNK14 MVC   BANKOF,RECOFFC                                                   
         NI    CHQIND1,FF-(CHQISBNK)                                            
                                                                                
VALBNK16 XC    BANKACC,ACCOUNT     SAVE BANK ACCOUNT/INDICATOR                  
         XC    ACCOUNT,BANKACC                                                  
         XC    BANKACC,ACCOUNT     RESTORE ACCOUNT (USED FOR SUPPLIER)          
         XC    BANKIND1,ACCIND1                                                 
         XC    ACCIND1,BANKIND1                                                 
         XC    BANKIND1,ACCIND1                                                 
         GOTO1 VACSRCHC,DMCB,MANBNKH,TWAD,BANKUL,                      X        
               (X'C0',BNKNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         ZAP   BANKBAL,RECBAL      SAVE BALANCE FOR LATER                       
         LH    RF,=Y(SOVRWRK2-SAVED)                                            
         LA    RF,SAVED(RF)                                                     
         USING SOVRWRK2,RF                                                      
         MVC   SBNKNAME,RECNAME    SAVE NAME FOR LATER                          
         DROP  RF                                                               
                                                                                
         OC    RECEXDF,RECEXDF     EXTRACT EXCHANGE DIFFERENCES A/C             
         BZ    *+16                                                             
         MVC   EXDFACC(L'ACTKCPY),COMPANY                                       
         MVC   EXDFACC+(ACTKULA-ACTKCULA)(L'ACTKULA),RECEXDF                    
         OC    RECBCHA,RECBCHA     EXTRACT BANK CHARGES A/C                     
         BZ    *+16                                                             
         MVC   BCHAACC(L'ACTKCPY),COMPANY                                       
         MVC   BCHAACC+(ACTKULA-ACTKCULA)(L'ACTKULA),RECBCHA                    
                                                                                
         MVC   BNKASTS1,RECASTS1   EXTRACT ASTEL STATUS BYTE - 1                
*&&UK                                                                           
         XC    PRSTCURT,PRSTCURT                                                
         MVC   BANKCUR,RECCURR     EXTRACT ACCOUNT CURRENCY                     
         OC    BANKCUR,BANKCUR     TEST BANK ACCOUNT CURRENCY KNOWN             
         BNZ   *+10                                                             
         MVC   BANKCUR,COMPCURT+(CURTCUR-CURTABD)                               
         CLC   BANKCUR,COMPCURT+(CURTCUR-CURTABD)                               
         BE    VALBNK24                                                         
                                                                                
         CLI   BANKCUR,ASTCANY     TEST CURRENCY ACCEPTABLE                     
         BE    *+14                                                             
         CLC   BANKCUR,SPACES                                                   
         BNH   VALBNK24                                                         
         MVC   FVMSGNO,=AL2(AE$SECUB)                                           
         MVI   FVXTRA,C'*'                                                      
         CLI   BANKCUR,ASTCANY                                                  
         BE    *+10                                                             
         MVC   FVXTRA(L'CURTCUR),BANKCUR                                        
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    EXIT                                                             
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),(XTYPE,FULL)                   
         BNE   EXIT                                                             
         XC    FVMSGNO,FVMSGNO     ACCEPTED - NO ERROR MESSAGE                  
         XC    FVXTRA,FVXTRA                                                    
                                                                                
         OI    SAFCIND1,SAFCI1BC   SET BANK ACCOUNT IN CURRENCY                 
         MVC   MANCAMC,COMPCURT+(CURTCUR-CURTABD)                               
         PUSH  USING                                                            
PRESET   USING CURTABD,PRSTCURT                                                 
         MVC   PRESET.CURTCUR,BANKCUR                                           
         POP   USING                                                            
*&&                                                                             
VALBNK24 DS    0H                                                               
*&&UK                                                                           
         TM    MANBCHH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    *+10                NOT FC - DON'T SET PRIMARY CURRENCY          
         MVC   MANBCHC,MANCAMC                                                  
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BZ    VALBNK26                                                         
         TM    SAFCIND1,SAFCI1BC   TEST FOREIGN CURRENCY BANK ACCOUNT           
         BO    *+6                                                              
         DC    H'0'                LOCAL BANK ACCOUNT MUST BE CURRENCY          
         MVC   MANBCHC,BANKCUR                                                  
VALBNK26 OI    MANBCHCH+(FVOIND-FVIHDR),FVOXMT                                  
         TM    SAFCIND1,SAFCI1BC   TEST FOREIGN CURRENCY BANK ACCOUNT           
         BZ    VALBNK28                                                         
                                                                                
         GOTO1 ATSTEUR,BANKCUR     TEST EURO IS THE FOREIGN CURRENCY            
         BNL   *+8                 CC LOW=EURO                                  
         OI    SAFCIND1,SAFCIEUR   SET SINGLE CURRENCY IS EUROS                 
                                                                                
         MVC   MANCURC,BANKCUR     GIVE CURRENCY SYMBOL                         
         MVI   MANCURCH+(FVILEN-FVIHDR),L'BANKCUR                               
         OI    MANCURCH+(FVIIND-FVIHDR),FVIVAL                                  
         OI    MANCURCH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    MANCURH+(FVOIND-FVIHDR),FVOXMT                                   
*&&                                                                             
VALBNK28 DS    0H                                                               
                                                                                
VALBNKX  DS    0H                                                               
                                                                                
*&&US                                                                           
***********************************************************************         
* VALIDATE BANK ACCOUNT OFFICE                                        *         
***********************************************************************         
                                                                                
VALBOF   MVC   OVWORK(L'OCNPOFF),SPACES                                         
         LA    R2,KEY              BUILD KEY FOR CHEQUE RECORD                  
         USING CHARECD,R2                                                       
*                                                                               
*&&UK*&& MVC   CHAKEY,SPACES                                                    
*&&US*&& XC    CHAKEY,CHAKEY                                                    
*                                                                               
         MVI   CHAKTYP,CHAKTYPQ                                                 
         MVC   CHAKCULA,ACCOUNT                                                 
         GOTO1 AIOEXEC,IOHIGH+IOACCDIR+IO1Q                                     
         CLC   KEY(4),KEYSAVE                                                   
         BE    VALBOF02                                                         
*                                                                               
*&&UK*&& USING LDGRECD,R2                                                       
*&&UK*&& MVC   LDGKEY,SPACES                                                    
*&&UK*&& MVC   LDGKEY(LDGKEND),ACCOUNT                                          
*&&UK*&& GOTO1 AIOEXEC                                                          
*&&UK*&& BE    *+6                                                              
*                                                                               
         DC    H'0'                                                             
         DROP  R2                                                               
VALBOF02 GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
VALBOF03 L     R2,AIOBUFF          R2=A(CHEQUE AUTH OR LEDGER RECORD)           
         LA    R2,ACTRFST-ACTRECD(R2)                                           
         USING OCNELD,R2                                                        
VALBOF04 DS    0H                                                               
         CLI   OCNEL,OCNELQ                                                     
         BE    VALBOF08                                                         
         CLI   OCNEL,0                                                          
*&&UK*&& BE    VALBOF10                                                         
*&&US*&& BNE   VALBOF06                                                         
*                                                                               
*&&US                                                                           
         GOTO1 AIOEXEC,IOSEQ+IOACCDIR+IO1Q                                      
         CLC   KEY(4),KEYSAVE                                                   
         BE    VALBOF02                                                         
         B     VALBOF10                                                         
*&&                                                                             
*                                                                               
VALBOF06 XR    R0,R0                                                            
         IC    R0,OCNLN                                                         
         AR    R2,R0                                                            
         B     VALBOF04                                                         
                                                                                
VALBOF08 CLC   OCNBANK,BANKACC     TEST CORRECT BANK ACCOUNT                    
         BNE   VALBOF06                                                         
         CLC   OCNOFFID,TWAUSRID   TEST CORRECT USER-ID                         
         BNE   VALBOF06                                                         
         CLI   OCNFILT,C'*'        TEST OCNFILT IS LIMIT ACCESS OFFICE          
         BNE   *+10                                                             
         MVC   OVWORK(L'OCNPOFF),OCNFILT+1                                      
         CLI   OCNLN,OCNLN3Q       SET BANK POSTING OFFICE, IF PRESENT          
         BL    VALBOF10                                                         
         MVC   OVWORK(L'OCNPOFF),OCNPOFF                                        
         B     VALBOF10                                                         
         DROP  R2                                                               
                                                                                
VALBOF10 TM    MANBOFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBOFX                                                          
         LA    R1,MANBOFH                                                       
         ST    R1,FVADDR                                                        
         XC    MANBOFN,MANBOFN                                                  
         OI    MANBOFNH+(FVOIND-FVIHDR),FVOXMT                                  
         CLC   OVWORK(L'BANKOF),SPACES                                          
         BNH   VALBOF12                                                         
         CLC   MANBOF,SPACES       TEST OFFICE SET                              
         BH    *+14                                                             
         MVC   MANBOF,OVWORK       SET DEFAULT OFFICE                           
         MVI   MANBOFH+(FVILEN-FVIHDR),L'OCNPOFF                                
         OC    MANBOF,SPACES       SET ANYTHING < C' ' TO C' '                  
         CLC   MANBOF,OVWORK                                                    
         BE    VALBOF12                                                         
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
                                                                                
VALBOF12 MVC   BANKOF,OFFICE       SAVE INVOICE OFFICE VALUES                   
         MVC   BANKOFXL,OFFICEXL                                                
         MVI   FVMINL,1            COMPULSORY                                   
         GOTO1 AVALOFF,MANBOFH                                                  
         BE    VALBOF14            BANK OFFICE PRESENT                          
         MVC   OFFICE,BANKOF       RESTORE INVOICE OFFICE VALUES                
         MVC   OFFICEXL,BANKOFXL                                                
         B     EXIT                ERROR                                        
                                                                                
VALBOF14 XC    BANKOF,OFFICE       SAVE/RESTORE BANK/INVOICE OFFICES            
         XC    OFFICE,BANKOF                                                    
         XC    BANKOF,OFFICE                                                    
         XC    BANKOFXL,OFFICEXL   SAVE/RESTORE EXECUTE L'OFFICES               
         XC    OFFICEXL,BANKOFXL                                                
         XC    BANKOFXL,OFFICEXL                                                
         MVC   MANBOFN,RECNAME                                                  
         OI    MANBOFNH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
         CLI   BANKOF,C' '         SET ANYTHING < C' ' TO C' '                  
         BNL   *+8                                                              
         MVI   BANKOF,C' '                                                      
         CLI   BANKOF+1,C' '                                                    
         BNL   *+8                                                              
         MVI   BANKOF+1,C' '                                                    
*&&                                                                             
VALBOFX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE CHEQUE NUMBER                                              *         
***********************************************************************         
                                                                                
VALCHQ   TM    MANCHQH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCHQX                                                          
         MVI   FVMINL,1                                                         
         GOTO1 AVALCHQ,MANCHQH                                                  
         BNE   EXIT                                                             
VALCHQX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE CHEQUE DATE                                                *         
***********************************************************************         
                                                                                
         MVI   OVBYTE,0                                                         
VALCDT   TM    MANCDTH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCDTX                                                          
VALCDT02 MVI   FVMINL,1                                                         
         GOTO1 AVALCDT,MANCDTH                                                  
         BNE   EXIT                                                             
         TM    COMPSTA4,CPYSOV12   TEST OPEN DATE                               
         BO    VALCDTX                                                          
         GOTO1 VDATCON,DMCB,(1,TODAYP),(0,WORK)                                 
         MVC   TEMP(2),WORK+4      SAVE ACTUAL DAY                              
         MVC   WORK+4(2),=C'01'    SET LOW DAY                                  
*&&UK*&& L     RF,=F'-350'         BACK 12 MONTHS                               
*&&US*&& L     RF,=F'-50'          BACK 2 MONTHS                                
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(RF)                                     
         MVC   WORK+10(2),TEMP     SET REAL DAY                                 
         CLC   CHQDATE,WORK+6      TEST > LOW DATE                              
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DTFIP)                                           
         B     EXIT                                                             
         MVC   WORK+4(2),=C'28'    SET HIGH DAY                                 
*&&UK*&& L     RF,=F'350'          FORWARD 12 MONTHS                            
*&&US*&& L     RF,=F'25'           FORWARD 1 MONTH                              
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(RF)                                     
         MVC   WORK+10(2),TEMP     SET REAL DAY                                 
         CLC   CHQDATE,WORK+6      TEST < HIGH DATE                             
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$DTFIF)                                           
         B     EXIT                                                             
                                                                                
VALCDTX  DS    0H                                                               
                                                                                
*&&US                                                                           
***********************************************************************         
* READ FOR DUPLICATE CHEQUE                                           *         
***********************************************************************         
                                                                                
CHKDUP   ICM   RF,3,BANKOF                                                      
         BNZ   CHKDUP02            OFFICE IS KNOWN                              
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    CHKDUP02            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE                  
         BE    CHKDUPX             OFFICE IN KEY AND UNKNOWN                    
                                                                                
CHKDUP02 LA    R1,KEY              READ FOR THIS CHEQUE                         
         USING TRNRECD,R1                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,BANKACC                                                 
                                                                                
         LHI   R0,1                                                             
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    CHKDUP04            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    CHKDUP04            YES - OFFICE NOT IN KEY                      
         STCM  RF,3,TRNKOFF        SET OFFICE IN KEY                            
                                                                                
         USING OFFALD,RF                                                        
         L     RF,AOFFBLK                                                       
         ICM   R0,3,OFFAWORK                                                    
         CHI   R0,1                                                             
         BE    *+8                                                              
         AHI   R0,1                                                             
         LA    R3,OFFAWORK+2                                                    
         B     CHKDUP04                                                         
                                                                                
CHKDUP03 LA    R1,KEY              READ FOR THIS CHEQUE                         
         MVC   TRNKOFF,0(R3)                                                    
         LA    R3,L'TRNKOFF(R3)                                                 
                                                                                
CHKDUP04 MVC   TRNKCULC,ACCOUNT                                                 
         MVC   TRNKDATE,CHQDATP                                                 
         MVC   TRNKREF,CHQNUM                                                   
         MVI   TRNKSBR,0                                                        
         GOTO1 AIOEXEC,IOHIGH+IOACCDIR+IO1Q                                     
         CLC   KEY(TRNKEND-L'TRNKSBR),KEYSAVE                                   
         BNE   CHKDUP08                                                         
                                                                                
CHKDUP06 MVC   FVMSGNO,=AL2(EGRECAOF)                                           
         LA    R1,MANCHQH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                CHEQUE ALREADY ON FILE                       
                                                                                
CHKDUP08 BCT   R0,CHKDUP03                                                      
                                                                                
CHKDUPX  DS    0H                                                               
         DROP  R1                                                               
*&&                                                                             
*&&UK                                                                           
***********************************************************************         
* VALIDATE SECONDARY CURRENCY CHEQUE AMOUNT                           *         
***********************************************************************         
                                                                                
VALSCU   NI    SCURIND1,FF-(SCURIOMT) CLEAR SECONDARY CURRENCY CHEQUE           
         TM    MANSCUH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSCUX                                                          
         OI    MANSCUH+(FVOIND-FVIHDR),FVOXMT                                   
         ZAP   CHQAMS,PZERO                                                     
         MVI   FVMINL,0                                                         
         GOTO1 AVALAMT,DMCB,MANSCUH,CHQAMS                                      
         BH    EXIT                                                             
         BL    VALSCUX                                                          
         TM    COMPSTA9,CPYS2CNV   TEST FILE NOT CONVERTED                      
         BO    VALSCU04                                                         
         XC    WORK(L'BATMONP),WORK                                             
         OC    WORK(L'BATMONP),OBATMONP                                         
         BNZ   VALSCU02                                                         
         TM    MANBMOH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSCU04            CAN'T CHECK MONTH YET                        
         MVI   FVMINL,0            VALIDATE BATCH MONTH, NOW                    
         GOTO1 AVALBMO,MANBMOH                                                  
         BL    VALSCU04            CAN'T MONTH CHECK YET                        
         BH    EXIT                INVALID BATCH MONTH                          
         MVC   WORK(L'BATMONP),BATMONP                                          
VALSCU02 CLC   COMPSCMO,WORK       TEST AFTER SECOND CURRENCY START             
         BNH   VALSCU04                                                         
         MVC   FVMSGNO,=AL2(AE$SCCNV)                                           
         B     EXIT                                                             
                                                                                
VALSCU04 OI    SCURIND1,SCURIOMT   SET SECONDARY CURRENCY CHEQUE                
         OI    SCURIND1,SCURITOT   SHOW SECOND CURRENCY TOTALS                  
         ZAP   DUB,CHQAMS          CALCULATE CHEQUE AMOUNT                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,S.CURTCUR  FROM CURRENCY                                
         MVC   EURKCUTO,C.CURTCUR  TO CURRENCY                                  
         XC    EURKRULE,EURKRULE   EXCHANGE RATE RULE NOT NECESSARY             
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   CHQAMT,DUB          CALCULATE PRIMARY CHEQUE AMOUNT              
         CURED CHQAMT,(L'MANCAM,MANCAM),COMPCURT,ALIGN=LEFT,FLOAT=-             
         OI    MANCAMH+(FVATRB-FVIHDR),FVAHIGH  INDICATE CALCULATED             
VALSCUX  DS    0H                                                               
                                                                                
*&&                                                                             
***********************************************************************         
* VALIDATE CHEQUE AMOUNT                                              *         
***********************************************************************         
                                                                                
VALCAM   DS    0H                                                               
*&&UK                                                                           
         NI    MANCAMH+(FVATRB-FVIHDR),FF-(FVAHIGH)                             
         NI    MANCURH+(FVATRB-FVIHDR),FF-(FVAHIGH)                             
         NI    MANRATH+(FVATRB-FVIHDR),FF-(FVAHIGH)                             
*&&                                                                             
         TM    MANCAMH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCAMX                                                          
                                                                                
*&&US                                                                           
         MVI   FVMINL,1                                                         
         GOTO1 AVALAMT,DMCB,MANCAMH,CHQAMT                                      
         BNE   EXIT                                                             
         CP    CHQAMT,PZERO                                                     
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     EXIT                                                             
*&&                                                                             
*&&UK                                                                           
         OI    MANCAMH+(FVOIND-FVIHDR),FVOXMT                                   
         ZAP   CHQAMT,PZERO                                                     
         MVI   FVMINL,0            DOESN'T NEED TO BE ENTERED                   
         GOTO1 AVALAMT,DMCB,MANCAMH,CHQAMT                                      
         BH    EXIT                                                             
         BE    *+12                                                             
         NI    SINDS,FF-(SLCNZ)    FIELD HAS NO INPUT                           
         B     VALCAMX                                                          
*&&                                                                             
         CP    CHQAMT,PZERO        ZERO IS ALLOWED HERE                         
         BE    *+8                                                              
         OI    SINDS,SLCNZ         FIELD IS NON-ZERO                            
VALCAMX  OI    SINDS,SLCVAL        FIELD HAS BEEN VALIDATED                     
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE CURRENCY CODE                                              *         
***********************************************************************         
                                                                                
VALCUC   MVC   OVRATE,MANRAT                                                    
         TM    MANRATH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    *+10                                                             
         XC    OVRATE,OVRATE                                                    
         OI    MANCURH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   FVMSGNO,=AL2(AE$INCUR)                                           
         GOTO1 AVALCUR,MANCURCH                                                 
         BH    EXIT                                                             
         BE    VALCUC02                                                         
         PUSH  USING               TEST UNACCEPTABLE CURRENCY FILTER            
PRESET   USING CURTABD,PRSTCURT                                                 
         CLC   PRESET.CURTCUR,MANCURC                                           
         BE    VALCUC02                                                         
         TM    SAFCIND1,SAFCI1BC                                                
         BO    *+14                                                             
         MVC   PRESET.CURTCUR,MANCURC                                           
         B     VALCUC02                                                         
         MVC   MANCURC,SPACES      UNACCEPTABLE CURRENCY FILTER                 
         OI    MANCURCH+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   MANCURC,C'*'                                                     
         MVI   MANCURCH+(FVILEN-FVIHDR),1                                       
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    *+14                                                             
         MVC   MANCURC(L'CURTCUR),PRESET.CURTCUR                                
         MVI   MANCURCH+(FVILEN-FVIHDR),L'CURTCUR                               
         MVC   FVMSGNO,=AL2(AI$CURFC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*ALCUC02 TM    MANCURCH+(FVIIND-FVIHDR),FVIVAL                                  
*        BO    *+16                                                             
*        TM    MANRATH+(FVATRB-FVIHDR),FVAHIGH                                  
*        BZ    *+8                                                              
*        NI    MANRATH+(FVIIND-FVIHDR),FF-FVIVAL                                
VALCUC02 L     RF,ACURRTAB         EXTRACT CURRENCY DETAILS                     
         USING CURTABD,RF                                                       
         LA    R0,CURRTABN                                                      
         CLC   CURTCUR,MANCURC                                                  
         BE    VALCUC04                                                         
         LA    RF,L'CURRTAB(RF)                                                 
         BCT   R0,*-14                                                          
         LA    R1,MANCURCH         CURRENCY NOT FOUND                           
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$INCUR)                                           
         B     EXIT                                                             
VALCUC04 MVC   PRSTCURT,CURTABD                                                 
         DROP  RF                                                               
         POP   USING                                                            
VALCUCX  OI    MANCURCH+(FVIIND-FVIHDR),FVIVAL                                  
         NI    SAFCIND1,FF-SAFCI1BC  DETERMINE IF EXTRA COLUMNS NEEDED          
         CLC   MANCURC,SPACES                                                   
         BNH   *+8                                                              
         OI    SAFCIND1,SAFCI1BC                                                
         MVC   SMANCURC,MANCURC    SAVE BANK CURRENCY CODE                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE CURRENCY AMOUNT                                            *         
***********************************************************************         
                                                                                
VALCUR   OI    MANCURH+(FVOIND-FVIHDR),FVOXMT                                   
         ZAP   CHQAMC,PZERO                                                     
         GOTO1 AFLDVAL,MANCURH                                                  
         BH    EXIT                                                             
         BE    *+12                                                             
         NI    SINDS,FF-(SFCNZ)    FIELD HAS NO INPUT                           
         B     VALCURX                                                          
                                                                                
         MVC   FVMSGNO,=AL2(AE$MISIF) MISSING INPUT                             
         LA    R1,MANCURCH                                                      
         ST    R1,FVADDR                                                        
         CLC   MANCURC,SPACES      TEST CURRENCY CODE GIVEN                     
         BNH   EXIT                                                             
                                                                                
         LA    R1,MANCURH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$EXDAM)                                           
         OC    EXDFACC,EXDFACC     TEST DIFFERENCE ACCOUNT KNOWN                
         BNZ   *+12                                                             
         TM    COMPSTA9,CPYSXDSJ   OR USING THE JOB FOR DIFFERENCE              
         BZ    EXIT                                                             
                                                                                
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         XR    R0,R0               TEST VALID AMOUNT                            
         IC    R0,FVILEN                                                        
         XR    RF,RF                                                            
         IC    RF,PRSTCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),MANCUR),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   EXIT                INVALID AMOUNT                               
                                                                                
         ZAP   CHQAMC,4(8,R1)                                                   
         CP    CHQAMC,PZERO                                                     
         BE    VALCUR02                                                         
         BH    *+12                                                             
         CLI   PROFCRNT,C'Y'       NEGATIVE - TEST ALLOW CREDIT NOTES           
         BNE   EXIT                NO NEGATIVES ALLOWED                         
         OI    SINDS,SFCNZ         FIELD IS NON-ZERO                            
                                                                                
VALCUR02 CURED CHQAMC,(L'MANCUR,MANCUR),PRSTCURT,ALIGN=LEFT,FLOAT=-             
         OI    SINDS,SFCVAL        FIELD IS VALIDATED                           
                                                                                
VALCURX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE EXCHANGE RATE                                              *         
***********************************************************************         
                                                                                
VALRAT   TM    MANRATH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALRATX                                                          
         TM    MANRATH+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    *+8                                                              
         NI    SSIND1,FF-SSI1XSYS  CLEAR ANY SYSTEM RATE INDICATOR              
         TM    SSIND1,SSI1XSYS     TEST SYSTEM RATE                             
         BZ    VALRAT02                                                         
         OC    ATLX,ATLX           TEST RATE KNOWN                              
         BNZ   VALRAT02                                                         
         MVC   ATLX,SSATLX         RESTORE SUPER SAVE AREA RATE                 
         B     *+10                                                             
VALRAT02 XC    ATLX,ATLX           CLEAR EXCHANGE RATE VALUES                   
         OI    MANRATH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANRATYH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   FVMSGNO,=AL2(AE$INEXR)                                           
         MVI   FVMINL,0                                                         
         GOTO1 AFLDVAL,MANRATH                                                  
         BE    VALRAT04                                                         
         BL    *+14                                                             
         XC    MANRATY,MANRATY     CLEAR TYPE ON ERROR                          
         B     EXIT                                                             
         XC    MANRATY,MANRATY     CLEAR TYPE IF NO INPUT                       
         NI    SINDS,FF-(SEXNZ)                                                 
         B     VALRATX                                                          
                                                                                
VALRAT04 MVC   FVMSGNO,=AL2(AE$MISIF) MISSING INPUT                             
         LA    R1,MANCURCH                                                      
         ST    R1,FVADDR                                                        
         CLC   MANCURC,SPACES      TEST CURRENCY CODE GIVEN                     
         BNH   EXIT                                                             
                                                                                
         LA    RE,L'MANRAT         FINDS FIRST SPACE FROM FRONT                 
         LA    RF,MANRAT                                                        
         CLI   0(RF),C' '                                                       
         BNH   VALRAT06                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,*-12                                                          
         B     VALRAT10                                                         
                                                                                
VALRAT06 CH    RE,=AL2(L'CURTCUR)   CURRENCY CODE MAY FOLLOW IF ROOM            
         BNH   VALRAT10                                                         
                                                                                
         CLC   1(L'CURTCUR,RF),SPACES                                           
         BNH   VALRAT10                                                         
                                                                                
         OC    1(L'CURTCUR,RF),SPACES   UPPER CASE CURRENCY CODE                
         CLC   PRSTCURT+(CURTCUR-CURTABD)(L'CURTCUR),1(RF)                      
         BE    VALRAT10                                                         
                                                                                
         CLC   EURO,1(RF)          TEST EURO RATE                               
         BNE   VALRAT08                                                         
         LR    R0,RE                                                            
         GOTO1 ATSTEUR,PRSTCURT+(CURTCUR-CURTABD)                               
         LR    RE,R0                                                            
         BE    VALRAT10            EURO MEMBER - NOT A PROBLEM                  
                                                                                
VALRAT08 LA    R1,MANRATH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$CURCC) CURRENCY CODE CONFLICT                    
         B     EXIT                                                             
                                                                                
VALRAT10 LCR   RE,RE               VALIDATE INPUT RATE                          
         LA    RE,L'MANRAT(RE)                                                  
         LR    R0,RE                                                            
         GOTO1 VCASHVAL,DMCB,(X'85',MANRAT),(R0)                                
         LA    RE,MANRATH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$INEXR)                                           
         CLI   0(R1),0                                                          
         BNE   EXIT                                                             
                                                                                
         ZAP   PKWK16A(8),4(8,R1)                                               
         LM    R0,R1,PKWK16A       FIRST 8 BYTES IS QUOTIENT                    
         SRDL  R0,4                                                             
         STM   R0,R1,PKWK16A       LOSE PACKED SIGN                             
         OC    PKWK16A(3),PKWK16A  ENSURE RATE WILL FIT                         
         BNZ   EXIT                                                             
         TM    SSIND1,SSI1XSYS     TEST SYSTEM RATE                             
         BO    VALRAT12            RATE UNCHANGED                               
         XC    MANRATY,MANRATY                                                  
         MVC   MANRATY(L'LC@ENTRD),LC@ENTRD                                     
VALRAT12 MVC   ATLXRATE,PKWK16A+3                                               
VALRAT14 GOTO1 AVERRAT             VERIFY EXCHANGE RATE                         
         BNE   EXIT                                                             
         OC    ATLXRATE,ATLXRATE                                                
         BZ    *+8                                                              
         OI    SINDS,SEXNZ         NON-ZERO RATE                                
         OI    SINDS,SEXVAL        RATE VALIDATED                               
         TM    SSIND1,SSI1XSYS     TEST SYSTEM RATE                             
         BZ    VALRAT16                                                         
         TM    ATLXSTAT,QINTOEUR+QFROMEUR                                       
         BZ    VALRAT16                                                         
         OI    MANRATH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
VALRAT16 DS    0H                                                               
                                                                                
VALRATX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* DETERMINE MISSING EXCHANGE RATE FIELDS - CAM, CUR AND RAT           *         
***********************************************************************         
                                                                                
DETRAT   CLC   MANCURC,SPACES      CURRENCY CODE?                               
         BNH   DETRATX                                                          
                                                                                
         TM    MANRATH+(FVATRB-FVIHDR),FVAPROT                                  
         BZ    *+12                                                             
         OI    SINDS,SEXVAL+SEXNZ  EXCHANGE RATE VALID AND NON-ZERO             
         B     *+8                                                              
         NI    SSIND1,FF-(SSI1FORB+SSI1LOCB)                                    
                                                                                
         TM    SINDS,SLCVAL+SLCNZ  LOCAL CURRENCY VALID & N.Z.?                 
         BNO   DETRAT10            NO                                           
                                                                                
         TM    SINDS,SEXVAL+SEXNZ  EXCHANGE RATE VALID AND NON-ZERO?            
         BNO   DETRAT02            NO                                           
                                                                                
         TM    SINDS,SFCNZ+SFCVAL  FOREIGN CURRENCY VALID & NON-ZERO?           
         BO    DETRAT04            YES - FURTHER THOUGHT NEEDED                 
         B     DETRAT36            CALCULATE FOREIGN CURRENCY                   
                                                                                
DETRAT02 TM    SINDS,SFCNZ+SFCVAL  FOREIGN CURRENCY VALID & NON-ZERO?           
         BO    DETRAT40            YES - CALCULATE EXCHANGE RATE                
         OI    SSIND1,SSI1LOCB     LOCAL CURRENCY BASIS                         
         B     DETRAT12            READ EXCHANGE RATE & CALC. FOR. CURR         
                                                                                
DETRAT04 LH    RF,=Y(SOVRWRK2-SAVED)                                            
         LA    RF,SAVED(RF)                                                     
         USING SOVRWRK2,RF                                                      
         CLC   CHQAMT,SACHQAMT     CHEQUE AMOUNT CHANGED?                       
         BE    DETRAT06            NO                                           
         NI    SSIND1,FF-SSI1FORB  YES - CLEAR FOREIGN CURRENCY BASIS           
         OI    SSIND1,SSI1LOCB     SET LOCAL CURRENCY BASIS                     
         B     DETRAT36            AND RECALCULATE                              
                                                                                
DETRAT06 CLC   CHQAMC,SACHQAMC     FOREIGN AMOUNT CHANGED?                      
         BE    DETRAT08            NO                                           
         NI    SSIND1,FF-SSI1LOCB  YES - CLEAR LOCAL CURRENCY BASIS             
         OI    SSIND1,SSI1FORB     SET FOREIGN CURRENCY BASIS                   
         B     DETRAT30            AND RECALCULATE                              
                                                                                
DETRAT08 CLC   SAOVRATE,MANRAT     EXCHANGE RATE THE SAME?                      
         BNE   DETRAT30            NO - RECALCULATE LOCAL CURRENCY              
         B     DETRATX             ALL THE SAME                                 
         DROP  RF                                                               
                                                                                
DETRAT10 TM    SINDS,SFCNZ+SFCVAL  FOREIGN CURRENCY VALID & NON-ZERO            
         BNO   DETRATX             NO FOREIGN OR LOCAL VALUES                   
                                                                                
         TM    SINDS,SEXNZ+SEXVAL  EXCHANGE RATE VALID & NON-ZERO               
         BO    DETRAT30            CALCULATE LOCAL AMOUNT                       
         OI    SSIND1,SSI1FORB     FOREIGN CURRENCY BASIS                       
         B     DETRAT12            READ EXCHANGE & CALCULATE LOCAL              
                                                                                
         USING EURKBLKD,R2         BUILD EUREKA BLOCK FOR EXCHANGE RATE         
DETRAT12 LA    R2,WORK             READ EXCHANGE RECORD FOR RATE                
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,C.CURTCUR                                               
         MVC   EURKCUTO,P.CURTCUR                                               
         MVI   EURKTYPE,ACCQ       SET TO GET ACCOUNTING RATE                   
         TM    COMPSTA6,CPYSFTXR   TEST FT RATES PERMITTED                      
         BZ    *+8                                                              
         MVI   EURKTYPE,ACCQ+ALLOWFTQ+SWAPQ  ALLOW FT/SWAP FOR LOOKUP           
         MVC   EURKALPH,COMPALFA                                                
         MVI   EURKACT,FF                                                       
         MVC   EURKACT+1(L'EURKACT-1),EURKACT                                   
         OC    EXACT,EXACT         TEST ACCOUNT KNOWN                           
         BZ    *+10                                                             
         MVC   EURKACT,EXACT       SET EXCHANGE RATE ACCOUNT, IF KNOWN          
         XC    TEMP(L'EURKACT),TEMP                                             
         CLI   MANCAM,C'0'         TEST NUMERIC VALUE IN CHEQUE AMOUNT          
         BNL   DETRAT14                                                         
         XR    RF,RF                                                            
         ICM   RF,1,MANCAMH+(FVILEN-FVIHDR)                                     
         BZ    DETRAT14                                                         
         LA    R1,MANCAM                                                        
         CLI   MANCAM,C'*'         TEST PREFIX FOR NUMERIC CLIENT               
         BNE   *+16                                                             
         LA    R1,MANCAM+1                                                      
         BCT   RF,*+8                                                           
         B     DETRAT14                                                         
         MVC   EURKACT,SPACES                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   EURKACT(0),0(R1)                                                 
         MVC   TEMP(L'EURKACT),EURKACT                                          
                                                                                
DETRAT14 MVC   EURKDATE,CHQDATB    SET COMPRESSED DEPOSIT DATE                  
         MVC   EURKAFAC,ACOM                                                    
         GOTO1 VEUREKA,DMCB,('GETQ',EURKBLKD),0,0,0,0                           
         CLI   0(R1),0                                                          
         BNE   DETRAT16                                                         
         OC    TEMP(L'EURKACT),TEMP  TEST SEEKING CLIENT RATE                   
         BZ    DETRAT18                                                         
         CLC   EURKACT,TEMP        TEST CLIENT RATE FOUND                       
         BE    DETRAT18                                                         
                                                                                
DETRAT16 LA    RE,MANCAMH          EXCHANGE RATE ERROR                          
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$NOEXR)                                           
         B     EXIT                                                             
                                                                                
DETRAT18 MVC   EXACT,EURKACT       SAVE EXCHANGE RATE ACCOUNT                   
         OI    SSIND1,SSI1XSYS     SET SYSTEM EXCHANGE RATE                     
         XC    MANRATY,MANRATY                                                  
         TM    EURKRLST,GEFIXFT    TEST FT RATE PASSED                          
         BNO   *+14                                                             
         MVC   MANRATY(L'LC@FTRAT),LC@FTRAT                                     
         B     DETRAT20                                                         
         CLI   EURKACT,FF          X'FF'S MEANS DEFAULT RATE                    
         BNE   *+14                                                             
         MVC   MANRATY(L'LC@DEF),LC@DEF                                         
         B     DETRAT20                                                         
         MVC   MANRATY(L'EURKACT),EURKACT                                       
                                                                                
DETRAT20 MVC   ATLXSTAT,EURKRLST   EXTRACT EXCHANGE RATE VALUES                 
         NI    ATLXSTAT,FF-(GEFIXFT)                                            
         MVC   ATLXRATE,EURKRLRT                                                
         MVC   ATLXSHFT,EURKRLSH                                                
         TM    SSIND1,SSI1XSYS     TEST SYSTEM RATE                             
         BZ    DETRAT22                                                         
         TM    ATLXSTAT,QINTOEUR+QFROMEUR                                       
         BZ    DETRAT22                                                         
         OI    MANRATH+(FVATRB-FVIHDR),FVAPROT                                  
         DROP  R2                                                               
DETRAT22 GOTO1 AVERRAT             VERIFY EXCHANGE RATE                         
         BNE   EXIT                                                             
         MVC   SSATLX,ATLX                                                      
         TM    SINDS,SLCNZ+SLCVAL  LOCAL CURRENCY VALID & NON-ZERO?             
         BNO   DETRAT30            CALCULATE LOCAL CURRENCY                     
         B     DETRAT36            CALCULATE FOREIGN CURRENCY                   
                                                                                
DETRAT30 ZAP   DUB,CHQAMC          CALCULATE CHEQUE AMOUNT                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,MANCAMC    FROM CURRENCY                                
         MVC   EURKCUTO,MANCURC    TO CURRENCY                                  
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('INVERTQ+APPLYQ',EURKBLKD),DUB,DUB,0,0             
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,MANCAMH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         ZAP   CHQAMT,DUB          TEST/SET CHEQUE AMOUNT                       
         BZ    EXIT                MUST BE NON-ZERO                             
         BP    *+12                                                             
         CLI   PROFCRNT,C'Y'       NEGATIVE - TEST ALLOW CREDIT NOTE            
         BNE   EXIT                                                             
         CURED CHQAMT,(L'MANCAM,MANCAM),COMPCURT,ALIGN=LEFT,FLOAT=-             
         OI    MANCAMH+(FVATRB-FVIHDR),FVAHIGH  INDICATE CALCULATED             
         B     DETRAT50                                                         
                                                                                
DETRAT36 ZAP   DUB,CHQAMT          CALCULATE CHEQUE AMOUNT                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,MANCAMC    FROM CURRENCY                                
         MVC   EURKCUTO,MANCURC    TO CURRENCY                                  
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,MANCURH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         ZAP   CHQAMC,DUB          TEST/SET CHEQUE AMOUNT (CURRENCY)            
         BZ    EXIT                MUST BE NON-ZERO                             
         BP    *+12                                                             
         CLI   PROFCRNT,C'Y'       NEGATIVE - TEST ALLOW CREDIT NOTE            
         BNE   EXIT                                                             
         CURED CHQAMC,(L'MANCUR,MANCUR),PRSTCURT,ALIGN=LEFT,FLOAT=-             
         OI    MANCURH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANCURH+(FVATRB-FVIHDR),FVAHIGH INDICATE CALCULATED              
         B     DETRAT50                                                         
                                                                                
DETRAT40 XC    MANRATY,MANRATY     CALCULATE EXCHANGE RATE                      
         MVC   MANRATY(L'LC@CALC),LC@CALC                                       
         ZAP   TEMPAM1,CHQAMC      CHEQUE CURRENCY                              
         ZAP   TEMPAM2,CHQAMT      CHEQUE                                       
         MVI   BYTE,0                                                           
         GOTO1 ATSTEUR,P.CURTCUR   TEST CURRENCY EURO(/MEMBER)                  
         BH    *+8                                                              
         OI    BYTE,QFROMEUR       FROM EURO/EURO MEMBER                        
         GOTO1 ATSTEUR,C.CURTCUR   TEST AGENCY EURO(/MEMBER)                    
         BH    *+8                                                              
         OI    BYTE,QINTOEUR       TO EURO/EURO MEMBER                          
         TM    BYTE,QFROMEUR+QINTOEUR                                           
         BZ    DETRAT46            NEITHER CURRENCY IS EURO(/MEMBER)            
         BNO   DETRAT42            ONE CURRENCY IS EURO(/MEMBER)                
         LA    RE,MANRATH          BOTH CURRENCIES EURO(/MEMBERS)               
         ST    RE,FVADDR           ERROR - CANNOT DERIVE RATE                   
         MVC   FVMSGNO,=AL2(AE$IIERF)                                           
         B     EXIT                                                             
DETRAT42 MVC   WORK+L'CURTCUR(L'CURTCUR),EURO                                   
         TM    BYTE,QFROMEUR       TEST FROM/TO EURO(/MEMBER)                   
         BZ    DETRAT44                                                         
         MVC   WORK(L'CURTCUR),P.CURTCUR                                        
         GOTO1 VCASHVAL,DMCB,(X'80',TEMPAM1),(X'24',0),WORK,0,0,0               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   TEMPAM1,12(8,R1)    CHEQUE CURRENCY EURO EQUIVALENT              
         B     DETRAT46                                                         
DETRAT44 MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         GOTO1 VCASHVAL,DMCB,(X'80',TEMPAM2),(X'24',0),WORK,0,0,0               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   TEMPAM2,12(8,R1)    CHEQUE EURO EQUIVALENT                       
DETRAT46 GOTO1 ACALCRAT,DMCB,TEMPAM1,TEMPAM2,PRSTCURT                           
         BH    EXIT                                                             
         BL    DETRATX                                                          
         MVC   ATLXRATE,0(R1)                                                   
         NI    MANRATH+(FVATRB-FVIHDR),FF-FVAHIGH                               
         GOTO1 AVERRAT             VERIFY EXCHANGE RATE                         
         BNE   EXIT                                                             
         OI    MANRATH+(FVATRB-FVIHDR),FVAHIGH  INDICATES CALCULATED            
                                                                                
DETRAT50 CP    CHQAMT,PZERO        TEST POSITIVE/NEGTAIVE CHEQUE                
         BL    DETRAT52                                                         
         CP    CHQAMC,PZERO        MUST BE POSITIVE IN CURRENCY                 
         BH    DETRATX                                                          
         MVC   FVMSGNO,=AL2(AE$PNMIX)                                           
         B     EXIT                                                             
DETRAT52 CP    CHQAMC,PZERO        MUST BE NEGATIVE IN CURRENCY                 
         BL    DETRATX                                                          
         MVC   FVMSGNO,=AL2(AE$PNMIX)                                           
         B     EXIT                                                             
                                                                                
DETRATX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCEPTABLE CHEQUE AMOUNT                                   *         
***********************************************************************         
                                                                                
VALCHQA  CP    CHQAMT,PZERO                                                     
         BH    VALCHQAX                                                         
         BL    VALCHQA2                                                         
         CLI   AGYCTRY,CTRYGER     ZERO ALLOWED IN GERMANY                      
         BE    VALCHQAX                                                         
         B     VALCHQA4                                                         
VALCHQA2 CLI   PROFCRNT,C'Y'       NEGATIVE - TEST ALLOW CREDIT NOTES           
         BE    VALCHQAX                                                         
VALCHQA4 LA    R1,MANCAMH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     EXIT                                                             
VALCHQAX DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE BANK CHARGE AMOUNT                                         *         
***********************************************************************         
                                                                                
VALBCH   ZAP   BCHAAMT,PZERO       CLEAR BANK CHARGE AMOUNT                     
         ZAP   BCHAAM2,PZERO       CLEAR BANK CHARGE AMOUNT (CURRENCY)          
         GOTO1 AFLDVAL,MANBCHH                                                  
         BH    EXIT                ERROR                                        
         BE    VALBCH02                                                         
         XC    BCHAACC,BCHAACC                                                  
         XC    MANBCH2,MANBCH2                                                  
         OI    MANBCH2+(FVOIND-FVIHDR),FVOXMT                                   
         B     VALBCHX                                                          
                                                                                
VALBCH02 MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CP    CHQAMT,PZERO        TEST CHEQUE PRESENT                          
         BE    EXIT                                                             
         MVC   FVMSGNO,=AL2(AE$BCHAM)                                           
         OC    BCHAACC,BCHAACC     TEST BANK CHARGE ACCOUNT KNOWN               
         BZ    EXIT                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         XR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BZ    *+8                                                              
         IC    RF,PRSTCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),MANBCH),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   EXIT                INVALID AMOUNT                               
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BO    VALBCH04                                                         
         ZAP   BCHAAMT,4(8,R1)     BANK CHARGES IN AGENCY CURRENCY              
         ZAP   DUB,BCHAAMT                                                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,MANBCHC    FROM CURRENCY                                
         MVC   EURKCUTO,MANCURC    TO CURRENCY                                  
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   BCHAAM2,DUB         SAVE BANK CHARGES IN LOCAL CURRENCY          
         CURED BCHAAMT,(L'MANBCH,MANBCH),COMPCURT,ALIGN=LEFT,FLOAT=-            
         CURED BCHAAM2,(L'MANBCH2,MANBCH2),PRSTCURT,ALIGN=LEFT,        *        
               FLOAT=-,CURSYMB=Y                                                
         B     VALBCH06                                                         
                                                                                
VALBCH04 ZAP   BCHAAM2,4(8,R1)     BANK CHARGES IN LOCAL CURRENCY               
         ZAP   DUB,BCHAAM2                                                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,MANCAMC    FROM CURRENCY                                
         MVC   EURKCUTO,MANBCHC    TO CURRENCY                                  
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('INVERTQ+APPLYQ',EURKBLKD),DUB,DUB,0,0             
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   BCHAAMT,DUB         SAVE BANK CHARGES IN AGENCY CURRENCY         
         CURED BCHAAM2,(L'MANBCH,MANBCH),PRSTCURT,ALIGN=LEFT,FLOAT=-            
         CURED BCHAAMT,(L'MANBCH2,MANBCH2),COMPCURT,ALIGN=LEFT,        *        
               FLOAT=-,CURSYMB=Y                                                
                                                                                
VALBCH06 CP    BCHAAMT,PZERO       TEST ZERO AMOUNT                             
         BE    EXIT                                                             
         CP    BCHAAM2,PZERO       TEST ZERO AMOUNT (CURRENCY)                  
         BE    EXIT                                                             
         OI    MANBCHH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANBCH2H+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
VALBCHX  MVC   SMANBCHC,MANBCHC    SAVE BANK CHARGES CURRENCY CODE              
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
                                                                                
VALBRF   TM    MANBRFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBRFX                                                          
         CLI   PROFBTIC,C'C'                                                    
         BNE   *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AVALBRF,MANBRFH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALBRFX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
                                                                                
VALBMO   TM    MANBMOH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBMOX                                                          
         CLI   PROFBTIC,C'C'                                                    
         BNE   *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AVALBMO,MANBMOH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALBMOX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE BATCH NAME                                                 *         
***********************************************************************         
                                                                                
VALBNA   TM    MANBNAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNAX                                                          
         CLI   PROFBTIC,C'C'                                                    
         BNE   *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AVALBNA,MANBNAH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALBNAX  DS    0H                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE UNIT & LEDGER                                              *         
***********************************************************************         
                                                                                
VALLDG   XR    R0,R0               CLEAR R0                                     
         TM    TWAMODE2,TWAM2NXA   TEST CALLED BY NXTACC                        
         BNO   *+6                                                              
         LR    R0,RE               YES - SAVE A(RETURN) TO NXTACC               
         TM    MANLDGH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALLDGX                                                          
         XC    MANLDGN,MANLDGN                                                  
         OI    MANLDGNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R1,MANLDGH                                                       
         ST    R1,FVADDR           SET A(LEDGER FIELD) FOR FVERR                
         LA    R1,LDGLIST          R1=A(LIST OF VALID LEDGERS)                  
VALLDG02 CLC   MANLDG,0(R1)                                                     
         BE    VALLDG04            LEDGER IN LIST - VALIDATE IT                 
         CLI   0(R1),EOT                                                        
         BE    *+12                                                             
         LA    R1,L'ACTKLDG(R1)                                                 
         B     VALLDG02                                                         
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         B     VALLERR             ERROR - NOT IN VALID LEDGER LIST             
                                                                                
VALLDG04 MVI   FVMINL,1            REQUIRED FIELD                               
         GOTO1 AVALLDG,MANLDGH                                                  
         BH    VALLERR                                                          
         MVC   MANLDGN,RECNAME     NAME EXTRACTED BY GETLDG                     
         B     VALLDGX                                                          
                                                                                
VALLERR  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BNZR  RE                  YES - EXIT WITH CC NEQ                       
         B     EXIT                NO - EXIT TO ROOT WITH MESSAGE SET           
                                                                                
VALLDGX  LTR   RE,R0               TEST CALLED BY NXTACC                        
         BZ    *+8                 NO - CONTINUE HDR SCREEN VALIDATION          
         CR    R0,R0               SET CC EQU                                   
         BR    RE                  AND RETURN                                   
                                                                                
***********************************************************************         
* VALIDATE SUPPLIER                                                   *         
***********************************************************************         
                                                                                
VALSUP   TM    MANSUPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSUPX                                                          
         MVI   FVMINL,1                                                         
         MVC   OVCURT,PRSTCURT                                                  
         XC    PRSTCURT,PRSTCURT                                                
         OI    MANSUPCH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANSUPC,MANSUPC                                                  
         GOTO1 AVALSUP,MANSUPH                                                  
         BNE   EXIT                                                             
         MVC   TEMP(L'RECCDSC),RECCDSC                                          
         MVC   TEMP+L'RECCDSC(L'RECDTAX),RECDTAX                                
         GOTO1 AGETLDG,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TEMP(L'RECCDSC),SPACES                                           
         BNH   *+10                                                             
         MVC   RECCDSC,TEMP                                                     
         CLC   TEMP+L'RECCDSC(L'RECDTAX),SPACES                                 
         BNH   *+10                                                             
         MVC   RECDTAX,TEMP+L'RECCDSC                                           
         OC    RECCDSC,RECCDSC     EXTRACT CASH DISCOUNT A/C                    
         BZ    *+16                                                             
         MVC   CDSCACC(L'ACTKCPY),COMPANY                                       
         MVC   CDSCACC+(ACTKULA-ACTKCULA)(L'ACTKULA),RECCDSC                    
         OC    RECDTAX,RECDTAX     EXTRACT DISCOUNT TAX ADJUSTMENT A/C          
         BZ    *+16                                                             
         MVC   DTAXACC(L'ACTKCPY),COMPANY                                       
         MVC   DTAXACC+(ACTKULA-ACTKCULA)(L'ACTKULA),RECDTAX                    
                                                                                
         PUSH  USING                                                            
PRESET   USING CURTABD,PRSTCURT                                                 
OVCUR    USING CURTABD,OVCURT                                                   
         OI    MANSUPCH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    MANSUPC,MANSUPC                                                  
         MVI   MANSUPC,C'*'                                                     
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    VALSUP02                                                         
         MVC   MANSUPC,PRESET.CURTCUR                                           
         CLC   PRESET.CURTCUR,OVCUR.CURTCUR                                     
         BE    VALSUP02                                                         
         CLC   PRESET.CURTCUR,SPACES                                            
         BNH   VALSUP02                                                         
         CLC   PRESET.CURTCUR,COMPCURT+(CURTCUR-CURTABD)                        
         BE    VALSUP02                                                         
         CLC   OVCUR.CURTCUR,COMPCURT+(CURTCUR-CURTABD)                         
         BE    VALSUP04                                                         
         CLC   OVCUR.CURTCUR,SPACES                                             
         BNH   VALSUP04                                                         
         GOTO1 ATSTEUR,PRESET.CURTCUR   TEST MEMBER CURRENCY SUPPLIER           
         BNE   *+12                                                             
         TM    SAFCIND1,SAFCIEUR        AND SINGLE CURRENCY IS EUROS            
         BO    VALSUP02                                                         
         MVC   FVMSGNO,=AL2(AE$SUPCC)                                           
         B     EXIT                                                             
VALSUP02 MVC   PRSTCURT,OVCURT                                                  
         CLC   PRESET.CURTCUR,COMPCURT+(CURTCUR-CURTABD)                        
         BE    *+14                                                             
         CLC   PRESET.CURTCUR,SPACES                                            
         BH    *+12                                                             
         USING FILTD,FILTVAL                                                    
         NI    FILT1,FF-FILTAC          DON'T FILTER ANY CURRENCIES             
VALSUP04 NI    FILT1,FF-FILTFC                                                  
         POP   USING                                                            
                                                                                
         GOTO1 VACSRCHC,DMCB,MANSUPH,TWAD,LEDGER,                      X        
               (X'C0',SUPNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
                                                                                
         OC    CDSCACC,CDSCACC     EXTRACT CASH DISCOUNT A/C                    
         BNZ   VALSUP05                                                         
         GOTO1 AGETLDG,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    RECCDSC,RECCDSC                                                  
         BZ    *+16                                                             
         MVC   CDSCACC(L'ACTKCPY),COMPANY                                       
         MVC   CDSCACC+(ACTKULA-ACTKCULA)(L'ACTKULA),RECCDSC                    
                                                                                
VALSUP05 ICM   RF,3,OFFICE         TEST/SET OFFICE                              
         BNZ   VALSUP06            OFFICE IS KNOWN                              
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    VALSUP06            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE                  
         BE    VALSUP10            OFFICE IN KEY AND UNKNOWN                    
                                                                                
VALSUP06 LA    R1,KEY              READ FOR THIS CHEQUE                         
         USING TRNRECD,R1                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,BANKACC                                                 
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    VALSUP08            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    VALSUP08            YES - OFFICE NOT IN KEY                      
         STCM  RF,3,TRNKOFF        SET OFFICE IN KEY                            
VALSUP08 MVC   TRNKCULC,ACCOUNT                                                 
         MVC   TRNKDATE,CHQDATP                                                 
         MVC   TRNKREF,CHQNUM                                                   
         MVI   TRNKSBR,0                                                        
         DROP  R1                                                               
         GOTO1 AIOEXEC,IOHIGH+IOACCDIR+IO1Q                                     
         CLC   KEY(TRNKEND-L'TRNKSBR),KEYSAVE                                   
         BNE   VALSUP10                                                         
         MVC   FVMSGNO,=AL2(EGRECAOF)                                           
         LA    R1,MANCHQH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                CHEQUE ALREADY ON FILE                       
                                                                                
VALSUP10 DS    0H                                                               
         MVC   SUPPACC,ACCOUNT                                                  
                                                                                
VALSUPX  DS    0H                                                               
*&&                                                                             
                                                                                
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    MANOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    MANOFFN,MANOFFN                                                  
         OI    MANOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,MANOFFH                                                  
         BL    VALOFF02            NOT REQUIRED, NOT INPUT                      
         BH    EXIT                ERROR                                        
         MVC   MANOFFN,RECNAME                                                  
         OI    MANOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
VALOFF02 CLI   OFFICE,C' '         SET ANYTHING < C' ' TO C' '                  
         BNL   *+8                                                              
         MVI   OFFICE,C' '                                                      
         CLI   OFFICE+1,C' '                                                    
         BNL   *+8                                                              
         MVI   OFFICE+1,C' '                                                    
                                                                                
VALOFFX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    MANCONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    MANCONN,MANCONN                                                  
         OI    MANCONNH+(FVOIND-FVIHDR),FVOXMT                                  
         CLI   MANCONH+(FVILEN-FVIHDR),L'DUMCON                                 
         BNE   VALCON2                                                          
         CLC   MANCON(L'DUMCON),DUMCON  TEST FOR DUMMY CONTRA                   
         BNE   VALCON2                                                          
         MVC   CONTRA,SPACES                                                    
         MVC   CONTRA(L'ACTKCPY),COMPANY                                        
         MVC   CONTRA+L'ACTKCPY(L'DUMCON),DUMCON                                
         MVI   CONTRAXL,L'TRNKCULC-1                                            
         MVI   CONTIND,CONTILOQ    SET BONA FIDE LOW-LEVEL ACCOUNT              
         OI    MANCONH+(FVOIND-FVIHDR),FVOXMT  RE-TRANSMIT                      
         B     VALCONX                                                          
                                                                                
VALCON2  GOTO1 AVALCON,MANCONH                                                  
         BL    VALCONX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   MANCONN,RECNAME                                                  
VALCONX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE SOURCE                                                     *         
***********************************************************************         
                                                                                
VALSRC   TM    MANSRCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALSRCX                                                          
         XC    MANSRCN,MANSRCN                                                  
         OI    MANSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALSRC,MANSRCH                                                  
         BH    EXIT                                                             
         BL    VALSRCX             NOT REQUIRED, NOT INPUT                      
         MVC   MANSRCN,RECNAME                                                  
         OI    MANSRCNH+(FVOIND-FVIHDR),FVOXMT                                  
VALSRCX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE REFERENCE NUMBER RANGE                                     *         
***********************************************************************         
                                                                                
VALREF   TM    MANREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,MANREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE PERIOD                                                     *         
***********************************************************************         
                                                                                
VALPER   TM    MANPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,MANPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE PERIOD                                       *         
***********************************************************************         
                                                                                
VALADA   TM    MANADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,MANADAH                                                  
         BH    EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE MOS RANGE                                                  *         
***********************************************************************         
                                                                                
VALMOS   TM    MANMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,MANMOAH                                                  
         BH    EXIT                                                             
         XC    FVMSGNO,FVMSGNO     IGOK SET HERE - NOT WANTED                   
VALMOSX  DS    0H                                                               
*&&UK                                                                           
***********************************************************************         
* REPORT CHANGED EXCHANGE RATE                                        *         
***********************************************************************         
                                                                                
REPCEXC  LH    RF,=Y(SOVRWRK2-SAVED)                                            
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    REPCEXCX            FOREIGN CURRENCY?                            
         LH    RF,=Y(SOVRWRK2-SAVED)                                            
         LA    RF,SAVED(RF)                                                     
         USING SOVRWRK2,RF                                                      
         LA    R1,MANCAMH                                                       
         CLC   CHQAMT,SACHQAMT     CHEQUE AMOUNT THE SAME?                      
         BNE   REPEXC02                                                         
         LA    R1,MANCURH                                                       
         CLC   CHQAMC,SACHQAMC     FOREIGN AMOUNT THE SAME?                     
         BNE   REPEXC02                                                         
         LA    R1,MANRATH                                                       
         OC    SAOVRATE,SPACES                                                  
         MVC   WORK(L'MANRAT),MANRAT                                            
         OC    WORK(L'MANRAT),SPACES                                            
         CLC   SAOVRATE,WORK       EXCHANGE RATE THE SAME?                      
         BE    REPEXC03            ALL THE SAME                                 
         DROP  RF                                                               
                                                                                
REPEXC02 ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EXCRD) 'EXCH RATE RE-DETERMINED'                 
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
                                                                                
REPEXC03 CLI   PROFCLEX,C'Y'       TEST CLEAR RATE PROFILE                      
         BNE   REPCEXCX                                                         
         XC    MANRAT,MANRAT                                                    
         XC    MANRATY,MANRATY                                                  
         TM    MANCAMH+(FVATRB-FVIHDR),FVAHIGH                                  
         BO    *+10                                                             
         XC    MANCAM,MANCAM                                                    
         TM    MANCURH+(FVATRB-FVIHDR),FVAHIGH                                  
         BO    *+10                                                             
         XC    MANCUR,MANCUR                                                    
REPCEXCX DS    0H                                                               
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
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    READ00                                                           
         L     R1,ASCUTOTS         CLEAR SECOND CURRENCY TOTALS                 
         LA    R0,SCUTOTL/L'SCUTOTS                                             
         ZAP   0(L'SCUTOTS,R1),PZERO                                            
         LA    R1,L'SCUTOTS(R1)                                                 
         BCT   R0,*-10                                                          
         L     R1,ASCUTOTS                                                      
         USING SCUTOTS,R1                                                       
         ZAP   STOCHQ,CHQAMS       SET SECONDARY CURRENCY CHEQUE                
         ZAP   STOBAL,CHQAMS       SET SECONDARY CURRENCY BALANCE               
*&&                                                                             
READ00   ZAP   TOTCHQ,CHQAMT                                                    
         ZAP   TOTBAL,CHQAMT                                                    
*&&UK*&& ZAP   CTOCHQ,CHQAMC                                                    
*&&UK*&& ZAP   CTOBAL,CHQAMC                                                    
                                                                                
         LA    R2,KEY              BUILD START KEY                              
         GOTO1 ASETKEY,SETALL                                                   
*&&US*&& GOTO1 AVALPUB,TRNRECD     VALIDATE SPECIAL PUBLICATION NUMBER          
         MVI   TSARLEN+1,TSARCML                                                
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READTRNX                                                         
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
         CLC   OFFICE,SPACES                                                    
         BNH   READ06                                                           
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
         CLC   OFFICE,SPACES       TEST FIXED OFFICE                            
         BH    READTRNX            YES - GONE TOO FAR                           
         GOTO1 ASETKEY,SETCON+NXTOFF                                            
         B     READ02                                                           
READ08   CLC   TRNKDATE,PERSTA                                                  
         BNL   READ10                                                           
         GOTO1 ASETKEY,SETSDT                                                   
         B     READ02                                                           
READ10   CLC   TRNKDATE,PEREND                                                  
         BNH   READ14                                                           
         TM    CONTIND,CONTILOQ    TEST REAL LOW-LEVEL ACCOUNT                  
         BNZ   READ12                                                           
         GOTO1 ASETKEY,SETSDT+NXTCON                                            
         B     READ02                                                           
READ12   TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READTRNX            NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    READTRNX            YES - GONE TOO FAR                           
         CLC   OFFICE,SPACES       TEST FIXED OFFICE                            
         BH    READTRNX            YES - GONE TOO FAR                           
         GOTO1 ASETKEY,SETSDT+NXTOFF                                            
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
         GOTO1 ASETKEY,SETREF                                                   
         B     READ02                                                           
READ16   OC    REFEND,REFEND                                                    
         BZ    READ20                                                           
READ18   IC    RF,REFENDXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   TRNKREF(0),REFEND                                                
         BNH   READ20                                                           
         GOTO1 ASETKEY,SETREF+NXTSDT                                            
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
*&&UK                                                                           
         PUSH  USING                                                            
         USING FILTD,FILTVAL                                                    
         OI    FILT1,FILTXD        FILTER EXCHANGE DIFFERENCE POSTINGS          
         POP   USING                                                            
*&&                                                                             
         GOTO1 AGENFILT,AIOBUFF                                                 
         BNE   READ02                                                           
         GOTO1 ARECFLT             OVERLAY SPECIFIC FILTERING                   
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
                                                                                
         ZAP   OVTRNAM,PZERO       CLEAR TEMPORARY AMOUNTS                      
         ZAP   OVAFCAM,PZERO                                                    
         ZAP   OVTRNAS,PZERO                                                    
                                                                                
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
                                                                                
READ24   TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    *+10                NO - DON'T SET TSAROFF                       
         MVC   TSAROFF,TRNOFFC                                                  
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         ZAP   TSARAMNT,TRNAMNT                                                 
         ZAP   OVTRNAM,TRNAMNT     FOR TOTALS                                   
         TM    TRNSTAT,TRNSAPPR    TEST TRANSACTION SELECTED                    
         BZ    *+8                                                              
         MVI   TSARVAR,TRNSAPPR    YES - SET VARIABLE KEY BYTE FOR SORT         
         MVC   TSARFSAC,SRCWORK    EXTRACT ANY SOURCE ACCOUNT                   
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
READ26   ICM   R2,15,ACPJEL        TEST CPJEL FOUND                             
         BZ    READ28                                                           
         CLI   CPJTYPE,CPJTJOB     TEST JOB TYPE                                
         BNE   READ28                                                           
         MVC   TSARFWRK,CPJWRK     TAKE WORKCODE                                
                                                                                
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
READ28   ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
*&&                                                                             
         USING DUEELD,R2           EXTRACT DUEEL VALUES                         
         ICM   R2,15,ADUEEL                                                     
         BZ    *+14                                                             
         MVC   TSARFDUE,DUEDATE                                                 
         B     READ29                                                           
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(2,TSARFDUE)                            
*                                                                               
READ29   DS    0H                                                               
*&&UK                                                                           
         USING GDAELD,R2           EXTRACT GDAEL VALUES                         
         ICM   R2,15,AGDAERPD                                                   
         BZ    *+10                                                             
         MVC   TSARERPD,GDADATE                                                 
*&&                                                                             
         USING SCIELD,R2           EXTRACT SCIEL VALUES                         
         ZAP   TSARFDIS,PZERO      DISCOUNT                                     
*&&UK*&& ZAP   TSARFDIC,PZERO      DISCOUNT (CURRENCY)                          
         ICM   R2,15,ASCICDSC                                                   
         BZ    READ30                                                           
         ZAP   TSARFDIS,SCIAMNT                                                 
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
*&&UK                                                                           
         CLI   SCILN,SCILN2Q                                                    
         BL    READ30                                                           
         ZAP   TSARFDIC,SCIADMN    SET DISCOUNT IN CURRENCY                     
*&&                                                                             
                                                                                
         USING TRSELD,R2           EXTRACT TRSEL VALUES                         
READ30   ICM   R2,15,ATRSEL        R2=A(TRANSACTION STATUS ELEMENT)             
         MVC   TSARADAT,TRSDATE    EXTRACT TRANSACTION ACTIVITY DATE            
         MVC   TSARSSTA,TRSSTAT    EXTRACT STATUS BYTE                          
                                                                                
         OC    ANOTELS,ANOTELS                                                  
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
*&&US                                                                           
         GOTO1 ABLDINV             GET LONG INVOICE NUMBER                      
*                                                                               
         MVC   TSARAAES,SPACES     MAKE SURE WE IT IS INIT                      
         MVC   TSARFINV,SPACES                                                  
         USING XPYELD,R2                                                        
         ICM   R2,15,AXPYEL                                                     
         BZ    READ33                                                           
*        MVC   TSARFINV(L'XPYINV),XPYINV                                        
         MVC   TSARFINV(L'XPYINV),XPYINV                                        
         OC    XPYEST,XPYEST                                                    
         BZ    READ31                                                           
         EDIT  XPYEST,(3,TSARAAES),ALIGN=LEFT                                   
READ31   CP    XPYCD,PZERO         TEST CASH DISCOUNT                           
         BE    READ33                                                           
         ZAP   TSARFDIS,XPYCD                                                   
         OI    TSARIND2,TSARLDSC   SET LIVE DISCOUNT                            
                                                                                
*        USING FFTELD,R2                                                        
*EAD32   ICM   R2,15,AFFTLEL                                                    
*        BZ    READ33                                                           
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
READ33   DS    0H                                                               
*&&UK                                                                           
         USING AFCELD,R2                                                        
         ZAP   TSARAFCA,PZERO                                                   
         ICM   R2,15,AAFCEL                                                     
         BZ    READ36                                                           
         GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
         ZAP   TSARAFCA,AFCAMNT                                                 
         ZAP   OVAFCAM,AFCAMNT     FOR TOTALS                                   
         MVC   TSARAFCC,AFCCURR                                                 
         MVC   TSARAFCX,AFCX                                                    
         DROP  R2                                                               
         CP    TSARFDIC,PZERO                                                   
         BNE   READ34                                                           
         ZAP   PKWK16A,TSARFDIS    TEST/SET AGENCY DISCOUNT                     
         BZ    READ34              ZERO - CAN'T SET CURRENCY DISCOUNT           
         MP    PKWK16A,TSARAFCA                                                 
         SRP   PKWK16A,RNDING,0                                                 
         DP    PKWK16A,TSARAMNT                                                 
         ZAP   PKWK16B,PKWK16A(L'PKWK16A-L'TSARAMNT)                            
         SRP   PKWK16B,64-RNDING,5                                              
         ZAP   TSARFDIC,PKWK16B                                                 
READ34   ZAP   SAVROUND,PZERO      CALCULATE EXCHANGE DIFFERENCE                
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
*&&                                                                             
READ36   DS    0H                  NEXT TRANSACTION ELEMENT                     
READ38   DS    0H                  NEXT TRANSACTION ELEMENT                     
READ40   DS    0H                  NEXT TRANSACTION ELEMENT                     
READ42   DS    0H                  NEXT TRANSACTION ELEMENT                     
READ44   DS    0H                  NEXT TRANSACTION ELEMENT                     
                                                                                
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    READ50                                                           
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         LA    R0,TOBAAOUT                                                      
         TM    COMPSTA7,CPYSSCNV                                                
         BZ    *+8                                                              
         LA    R0,TOBAACVS                                                      
         GOTO1 VTOBACCO,DMCB,((R0),WORK),AIOBUFF,ACOM,0,0,0                     
***********************************************************************         
* TRNREC IS IN SECONDARY CURRENCY AFTER TOBACCO CALL                  *         
***********************************************************************         
         GOTO1 ASETELAD,AIOBUFF                                                 
         USING TRNELD,R2           SET SECOND CURRENCY VALUES                   
         ICM   R2,15,ATRNEL                                                     
         ZAP   TSARSCUA,TRNAMNT                                                 
         ZAP   OVTRNAS,TRNAMNT                                                  
         ZAP   TSARSCUD,PZERO                                                   
         USING SCIELD,R2                                                        
         ICM   R2,15,ASCICDSC      TEST LIVE DISCOUNT                           
         BZ    *+10                                                             
         ZAP   TSARSCUD,SCIAMNT                                                 
         DROP  R2                                                               
*&&                                                                             
READ50   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ54                                                           
         AP    TOTCRS,OVTRNAM                                                   
         MVI   ANYADD,1            SET TRANSACTION ADDED                        
*&&UK                                                                           
         AP    CTOCRS,OVAFCAM                                                   
                                                                                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    READ52                                                           
         L     R1,ASCUTOTS                                                      
         USING SCUTOTS,R1                                                       
         AP    STOCRS,OVTRNAS                                                   
         DROP  R1                                                               
*&&                                                                             
READ52   B     READ02              READ SEQUENTIAL                              
                                                                                
READ54   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                                                             
         B     DISPTRN                                                          
                                                                                
READTRNX NI    DISIND,FF-DISIOFLO  CLEAR OVERFLOW (DID NOT OCCUR)               
         CLI   ANYADD,1            TEST ANYTHING IN BUFFER                      
         BE    DISPTRN                                                          
         CLI   PROFDFAV,C'A'       TEST ADVANCES ALLOWED                        
         BE    DISPTRN                                                          
         CLI   PROFDFAV,C'D'       OR DIFFERENCES ALLOWED                       
         BE    DISPTRN                                                          
         CLI   PROFDFAV,C'B'       OR BOTH ALLOWED                              
         BE    DISPTRN                                                          
         LA    R1,MANSUPH          NO - SET CURSOR TO SUPPLIER FIELD            
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF                                                   
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
         CLC   MANCURC,SPACES      TEST DEALING IN CURRENCY                     
         BH    *+8                                                              
         NI    SAFCIND1,FF-SAFCI1SC                                             
*&&                                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         NI    TWAMODE3,FF-(TWAM3ASC)                                           
         GOTO1 AOVRSCR,CSSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
                                                                                
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
                                                                                
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
*&&UK*&& ZAP   SAVROUND,PZERO      INITIALISE ROUNDING ERRORS                   
*&&UK*&& ZAP   SAVROUNS,PZERO                                                   
         GOTO1 ADISPLAY                                                         
         TM    DISIND,DISIOFLO     TEST BUFFER OVERFLOW                         
         BNO   *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNWRN)  WARN USER                                
         GOTO1 ABLDTOT,INPTOTH                                                  
                                                                                
         ZAP   TOTITEM,PZERO       CLEAR MARKED INVOICE COUNT                   
                                                                                
DISPTRNX B     EXIT                                                             
         EJECT                                                                  
         USING DISLINED,R2                                                      
VALINP   L     RF,AINP             TEST SUB-ACTION                              
         CLI   TIOBAID-TIOBD(RF),PFK05                                          
         BNE   VALINP00                                                         
         GOTO1 AVALPWI             SET UP PAYMENT WITHOUT INVOICE               
         B     EXIT                                                             
VALINP00 CLI   TIOBAID-TIOBD(RF),PFK02                                          
         BNE   VALINP01                                                         
         GOTO1 AVALDIF             SET UP DIFFERENCE                            
         B     EXIT                                                             
                                                                                
VALINP01 LA    RF,MRKSCRH          SET A(SCROLL FIELD) FOR EARLY EXIT           
         ST    RF,FVADDR                                                        
         CLI   OPTALL,0            TEST GLOBAL MARK/UNMARK                      
         BE    VALINP02                                                         
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BO    VALINP34            YES - CALL DISPLAY                           
         GOTO1 AMRKALL             MARK ALL TRANSACTIONS                        
         BNE   VALINPX             ERROR - DISPLAY THIS PAGE                    
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         OI    DISIND,DISIRST      SET TO RESTART DISPLAY                       
         B     VALINP34                                                         
                                                                                
VALINP02 MVI   ANYMARK,0                                                        
         LA    R3,DISLIST          R3=A(LIST OF DISPLAYED TSAR RECORDS)         
         L     R2,ADISDET1         R2=A(1ST DETAIL LINE)                        
         XR    R0,R0                                                            
         ICM   R0,3,DISLCNT        NUMBER OF DISPLAY LINES                      
         BZ    VALINP34            NO RECORDS TO DISPLAY                        
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VALINP04                                                         
         NI    TWAMODE2,FF-TWAM2SKP  RESET SKIP VALIDATION                      
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VALINP34            SKIP VALIDATION AND CALL DISPLAY             
                                                                                
VALINP04 GOTO1 AVALZMRK,DISLHDR2   ZOOM INPUT                                   
         BL    VALINP06            NO ZOOM - TRY OTHER MARKS                    
         BH    VALINPX             ZOOM INVALID - EXIT WITH ERROR SET           
         MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    TSARDADR,TSARDADR   TEST RECORD CREATED THIS SESSION             
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALINPX             ZOOM INVALID - EXIT WITH ERROR SET           
         GOTO1 AZOOMFAC,(R3)       PREPARE ZOOM SCREEN AND EXIT TO USER         
         B     EXIT                                                             
                                                                                
VALINP06 MVI   CHAR,0                                                           
         GOTO1 AVALMRK,DISLHDR2                                                 
         BE    VALINP08                                                         
         BL    VALINP32            NO INPUT - NEXT SCREEN LINE                  
         CLC   DISLMARK,AC4GROSS   TEST TAKE GROSS                              
         BNE   VALINPX             NO - EXIT WITH ERROR SET                     
         MVI   CHAR,TSARGRSS       SET GROSS                                    
                                                                                
VALINP08 MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    CHAR,TSARGRSS       TEST GROSS                                   
         BZ    VALINP10                                                         
         TM    TSARIND3,TSARPWOI+TSARDIFF                                       
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALINPX                                                          
         TM    TSARIND2,TSARLDSC   TEST DISCOUNT IS LIVE                        
         BO    VALINP12                                                         
         MVC   FVMSGNO,=AL2(AE$NODSC)                                           
         B     VALINPX                                                          
                                                                                
VALINP10 CLI   BYTE,TSARCHGQ       TEST ONLY WANT TO REDISPLAY                  
         BE    VALINP30                                                         
         CLI   BYTE,TSARMKQ        TEST IF USER IS MARKING                      
         BNE   VALINP18                                                         
                                                                                
VALINP12 TM    TSARINDS,TSARMKQ    MARKING - WAS RECORD ALREADY MARKED?         
         BO    VALINP32                                                         
         CP    TOTITEM,MAXINVS                                                  
         BL    VALINP13                                                         
         MVC   FVMSGNO,=AL2(AE$MXINV)                                           
         MVC   FVXTRA,SPACES                                                    
         CURED MAXINVS,(((2*L'MAXINVS)-1),FVXTRA),0,ALIGN=LEFT                  
         B     VALINPX             MAXIMUM ITEMS EXCEEDED                       
                                                                                
VALINP13 AP    TOTITEM,PONE                                                     
         OI    TSARINDS,TSARMKQ                                                 
                                                                                
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
         AP    REPMRK,TSARAMNT     ADD TO MARKED THIS SESSION                   
                                                                                
         TM    CHAR,TSARGRSS       TEST GROSS                                   
         BZ    *+8                                                              
         OI    TSARIND2,TSARGRSS   SET GROSS                                    
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         AP    TOTEXDF,TMPEXDF     ADD EXCHANGE DIFFERENCES                     
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         AP    CTOMRK,TSARAFCA     ADD TO MARKED (AFC)                          
         SP    CTOBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         AP    CREMRK,TSARAFCA     ADD TO MARKED THIS SESSION (AFC)             
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    VALINP14                                                         
         USING SCUTOTS,R1                                                       
         L     R1,ASCUTOTS                                                      
         AP    STOMRK,TSARSCUA     ADD TO MARKED (2ND)                          
         SP    STOBAL,TSARSCUA     SUBTRACT FROM BALANCE (2ND)                  
         AP    SREMRK,TSARSCUA     ADD TO MARKED THIS SESSION (2ND)             
         AP    STOEXDF,TMPEXDFS    ADD EXCHANGE DIFFERENCES                     
                                                                                
VALINP14 TM    CHAR,TSARGRSS       TEST GROSS                                   
         BNZ   VALINP28                                                         
                                                                                
         SP    TOTMRK,TSARFDIS     MINUS DISCOUNT                               
         AP    TOTDSC,TSARFDIS     ADD TO DISCOUNT                              
         AP    TOTBAL,TSARFDIS     ADD TO BALANCE                               
         SP    REPMRK,TSARFDIS     MINUS DISCOUNT                               
                                                                                
         SP    CTOMRK,TSARFDIC     MINUS DISCOUNT (AFC)                         
         AP    CTODSC,TSARFDIC     ADD TO DISCOUNT (AFC)                        
         AP    CTOBAL,TSARFDIC     ADD TO BALANCE (AFC)                         
         SP    CREMRK,TSARFDIC     MINUS DISCOUNT (AFC)                         
                                                                                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    VALINP16                                                         
         L     R1,ASCUTOTS                                                      
         SP    STOMRK,TSARSCUD     MINUS DISCOUNT (2ND)                         
         AP    STODSC,TSARSCUD     ADD TO DISCOUNT (2ND)                        
         AP    STOBAL,TSARSCUD     ADD TO BALANCE (2ND)                         
         SP    SREMRK,TSARSCUD     MINUS DISCOUNT (2ND)                         
*&&                                                                             
VALINP16 B     VALINP28                                                         
                                                                                
VALINP18 TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    VALINP32                                                         
         SP    TOTITEM,PONE                                                     
         BNM   *+6                                                              
         DC    H'0'                CANNOT UNMARK MORE THAN YOU MARKED           
         NI    TSARINDS,FF-TSARMKQ                                              
                                                                                
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
         SP    REPMRK,TSARAMNT     SUBTRACT FROM MARKED THIS SESSION            
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         SP    TOTEXDF,TMPEXDF     REMOVE EXCHANGE DIFFERENCES                  
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         SP    CTOMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
         AP    CTOBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         SP    CREMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    VALINP20                                                         
         L     R1,ASCUTOTS                                                      
         SP    STOMRK,TSARSCUA     SUBTRACT FROM MARKED (2ND)                   
         AP    STOBAL,TSARSCUA     ADD TO BALANCE (2ND)                         
         SP    SREMRK,TSARSCUA     SUBTRACT FROM MARKED (2ND)                   
         SP    STOEXDF,TMPEXDFS    REMOVE EXCHANGE DIFFERENCES                  
*&&                                                                             
VALINP20 TM    TSARIND2,TSARGRSS   TEST TAKEN GROSS                             
         BZ    *+12                                                             
         NI    TSARIND2,FF-TSARGRSS                                             
         B     VALINP28                                                         
*&&UK                                                                           
         AP    TOTMRK,TSARFDIS     PLUS DISCOUNT                                
         SP    TOTDSC,TSARFDIS     SUBTRACT FROM DISCOUNT                       
         SP    TOTBAL,TSARFDIS     SUBTRACT FROM BALANCE                        
         AP    REPMRK,TSARFDIS     MINUS DISCOUNT                               
                                                                                
         AP    CTOMRK,TSARFDIC     PLUS DISCOUNT (AFC)                          
         SP    CTODSC,TSARFDIC     SUBTRACT FROM DISCOUNT (AFC)                 
         SP    CTOBAL,TSARFDIC     SUBTRACT FROM BALANCE (AFC)                  
         AP    CREMRK,TSARFDIC     MINUS DISCOUNT (AFC)                         
                                                                                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    VALINP22                                                         
         L     R1,ASCUTOTS                                                      
         AP    STOMRK,TSARSCUD     PLUS DISCOUNT (2ND)                          
         SP    STODSC,TSARSCUD     SUBTRACT FROM DISCOUNT (2ND)                 
         SP    STOBAL,TSARSCUD     SUBTRACT FROM BALANCE (2ND)                  
         AP    SREMRK,TSARSCUD     MINUS DISCOUNT (2ND)                         
         DROP  R1                                                               
*&&                                                                             
VALINP22 DS    0H                                                               
         B     VALINP28                                                         
                                                                                
VALINP28 L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ANYMARK,1                                                        
         DROP  RF                                                               
VALINP30 GOTO1 ABLDLIN,DISLHDR1    REBUILD LHS, XMIT, (UN)HIGHLIGHT             
                                                                                
VALINP32 LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VALINP04                                                      
                                                                                
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+16                                                             
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   YES - SET SKIP VALIDATION                    
         TM    DISIND,DISINCOL     TEST NEW COLUMN DISPLAY                      
         BNZ   VALINP34            DISPLAY NEW COLUMNS (NO SCROLLING)           
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VALINP34                                                         
         OI    DISIND,DISIFFLT     FORCE FILTERING OF DISLIST NEXT TIME         
         LA    R1,MRKSCRH          SET CURSOR TO SCROLL FIELD                   
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         TM    DISIND,DISIEOF+DISIBOF                                           
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         B     VALINPX                                                          
                                                                                
VALINP34 DS    0H                                                               
*&&UK*&& ZAP   SAVROUND,PZERO                                                   
*&&UK*&& ZAP   SAVROUNS,PZERO                                                   
         GOTO1 ADISPLAY                                                         
                                                                                
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
                                                                                
         USING REPD,R3                                                          
UPDATE   L     R3,AREPWRK          R3=A(REPORT W/S)                             
         TM    TWAMODE2,TWAM2CHG                                                
         BO    UPD02                                                            
         MVC   FVMSGNO,=AL2(EANOTHIN)  NOTHING DONE YET                         
         LA    R1,MRKSCRH                                                       
         B     UPDATEX                                                          
                                                                                
UPD02    MVI   UPDIND1,0           CLEAR UPDATE INDICATORS                      
         MVI   UPDIND2,0                                                        
         MVC   OVBYTE,BYTE         SAVE UPDATE CHECK FLAG                       
         GOTO1 ASETACT             SET UP ACCOUNT EXTRA DETAILS                 
         BE    UPD04                                                            
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         LA    R1,MRKSCRH                                                       
         B     UPDATEX                                                          
                                                                                
UPD04    CLI   OVBYTE,ACTICHUP     TEST CHECK FOR UPDATE                        
         BNE   UPD12                                                            
         LA    R1,INPMRKH          CHECK LIVE UPDATE IS OK                      
         ST    R1,FVADDR                                                        
         ZAP   OVHALF,TOTITEM      SET TOTAL ITEMS                              
         ZAP   OVDUB,TOTMRK        SET TOTAL INVOICES MARKED                    
         ZAP   DUB,CHQAMT                                                       
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    UPD06                                                            
         USING SCUTOTS,R1                                                       
         L     R1,ASCUTOTS                                                      
         CP    STOMRK,CHQAMS       TEST BATCH TOTAL MATCHES CHEQUE              
         BE    UPD06                                                            
         DROP  R1                                                               
         MVC   FVMSGNO,=AL2(AE$SCTDB)                                           
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'CURTCUR),S.CURTCUR                                      
         LTR   RB,RB               EXIT CC NOT EQUAL FOR ROUT                   
         B     EXIT                                                             
                                                                                
UPD06    TM    SAFCIND1,SAFCI1SC   TEST SINGLE CURRENCY ACTION                  
         BZ    *+16                                                             
         ZAP   OVDUB,CTOMRK        USE CURRENCY TOTALS                          
         ZAP   DUB,CHQAMC                                                       
*&&                                                                             
         CP    DUB,OVDUB           TEST BATCH TOTAL MATCHES CHEQUE              
         BE    UPD08                                                            
         MVC   FVMSGNO,=AL2(EABATTOT)                                           
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE CURRENCY ACTION                  
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(AE$CUTDB)   CURRENCY TOTALS DO NOT BALANCE          
*&&                                                                             
         LTR   RB,RB               EXIT CC NOT EQUAL FOR ROUT                   
         B     EXIT                                                             
                                                                                
UPD08    ICM   R1,3,OVRNUM         READ ALL TSAR RECORDS - CHECK TOTALS         
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         GOTO1 ATSARGET,OVRNUM                                                  
         BNE   UPD10                                                            
         TM    TSARINDS,TSARMKQ                                                 
         BZ    UPD08                                                            
         SP    OVHALF,PONE         CLEAR DOWN MARKED INVOICES                   
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE CURRENCY ACTION                  
         BO    *+14                                                             
         SP    OVDUB,TSARAMNT                                                   
         B     *+10                                                             
         SP    OVDUB,TSARAFCA                                                   
         TM    TSARIND2,TSARGRSS   TEST TAKEN GROSS                             
         BO    UPD08                                                            
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE CURRENCY ACTION                  
         BZ    *+14                                                             
         AP    OVDUB,TSARFDIC                                                   
         B     UPD08                                                            
         AP    OVDUB,TSARFDIS                                                   
         B     UPD08                                                            
*&&                                                                             
*&&US                                                                           
         SP    OVDUB,TSARAMNT                                                   
         B     UPD08                                                            
*&&                                                                             
                                                                                
UPD10    L     R1,ATSARBLK         TEST EOF FROM TSAR                           
         TM    TSERRS-TSARD(R1),TSEEOF                                          
         BO    *+6                                                              
         DC    H'0'                                                             
         CP    OVDUB,PZERO         TEST CHEQUE TOTAL                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    OVHALF,PZERO        TEST ALL MARKED INVOICES ACCOUNTED           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 ABATMAN,BATCHKQ     CHECK BATCH DETAILS ARE OK                   
         B     EXIT                CURSOR SET, IF ERROR FOUND                   
                                                                                
UPD12    ZAP   ADDITEM,PZERO       CLEAR BATCH ITEM COUNT/CASH                  
         ZAP   ADDCASH,PZERO                                                    
*&&UK                                                                           
         ZAP   OVROUND,CHQAMT      CALCULATE OVERALL ROUNDING ERROR             
         SP    OVROUND,TOTMRK                                                   
         SP    OVROUND,TOTEXDF                                                  
                                                                                
         ZAP   OVROUNDS,PZERO      CLEAR OVERALL ROUNDING ERROR (2ND)           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    UPD14                                                            
         USING SCUTOTS,R1                                                       
         L     R1,ASCUTOTS                                                      
         ZAP   OVROUNDS,CHQAMS     CALC. OVERALL ROUNDING ERROR (2ND)           
         SP    OVROUNDS,STOMRK                                                  
         SP    OVROUNDS,STOEXDF                                                 
         DROP  R1                                                               
*&&                                                                             
UPD14    OC    PRTSUB,PRTSUB                                                    
         BZ    UPD18                                                            
         LH    R1,=Y(AS$CNDAB)                                                  
         CP    CHQAMT,PZERO        TEST NEGATIVE PAYMENT                        
         BNL   *+8                                                              
         LH    R1,=Y(AS$CRDAB)                                                  
         GOTO1 ABLDNAR,(R1)                                                     
         MVC   CHQNARR,SPACES                                                   
         IC    R1,OVWORK                                                        
         STC   R1,CHQNARRL         SET EXECUTE L'NARRATIVE                      
         EX    R1,*+4                                                           
         MVC   CHQNARR(0),OVWORK+1 EXTRACT NARRATIVE                            
                                                                                
         GOTO1 ABATMAN,BATCHKQ     SET BATCH DETAILS FOR HEADING                
                                                                                
         GOTO1 APRTINI             INITIALISE AND PRINT FRONT PAGE              
         MVC   REPH5+L'DISLLINE+1(L'LC@PAID),LC@PAID                            
         LA    R1,REPH5+L'DISLLINE+1+L'LC@PAID-1                                
         CLI   0(R1),C' '          SEEK FIRST NON-BLANK                         
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
         MVC   REPH8,REPH5         COLUMN HEADINGS IN HEAD 8                    
         MVC   REPH9,REPH6                        AND HEAD 9                    
         MVC   REPH6,REPH4         SUPPLIER DETAILS IN HEAD 6                   
         MVC   REPH4,SPACES                                                     
         LA    R1,REPH4                                                         
         ICM   R1,8,=AL1(100)                                                   
         GOTO1 ABLDDSC             BATCH DETAILS IN HEAD 4                      
         LH    R1,=Y(AS$CNDMC)                                                  
         CP    CHQAMT,PZERO        TEST NEGATIVE PAYMENT                        
         BNL   *+8                                                              
         LH    R1,=Y(AS$CRDAB)                                                  
         GOTO1 ABLDNAR,(R1)                                                     
         MVC   REPH5,SPACES        NARRATIVE IN HEAD 5                          
         IC    R1,OVWORK                                                        
         EX    R1,*+4                                                           
         MVC   REPH5(0),OVWORK+1   EXTRACT NARRATIVE                            
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC                                                
         BZ    UPD18                                                            
         MVC   REPHA,REPH9         INSERT LINE AFTER BATCH DETAILS              
         MVC   REPH9,REPH8         INSERT LINE AFTER BATCH DETAILS              
         MVC   REPH8,REPH7                                                      
         MVC   REPH7,REPH6                                                      
         MVC   REPH6,REPH5                                                      
         MVC   REPH5,SPACES                                                     
         MVI   REPH5,FF                                                         
         MVC   REPH5+1(L'LC@BATTS),LC@BATTS                                     
         LA    R2,REPH5+L'LC@BATTS                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   3(L'CURTCUR,R2),COMPCURT+(CURTCUR-CURTABD)                       
         CURED TOTMRK,(13,L'CURTCUR+4(R2)),COMPCURT,ALIGN=LEFT                  
         AR    R2,R0                                                            
         MVC   L'CURTCUR+6(L'CURTCUR,R2),FORECURT+(CURTCUR-CURTABD)             
         CURED CTOMRK,(13,2*L'CURTCUR+7(R2)),FORECURT,ALIGN=LEFT                
         AR    R2,R0                                                            
         MVC   2*L'CURTCUR+9(L'LC@EXCHR,R2),LC@EXCHR                            
         LA    R2,2*L'CURTCUR+L'LC@EXCHR+10(R2)                                 
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         GOTO1 AEDTRAT,DMCB,ATLXRATE,(11,2(R2))                                 
         CLI   13(R2),C' '                                                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   16(L'LC@EXDF,R2),LC@EXDF                                         
         LA    R2,L'LC@EXDF+16(R2)                                              
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         ZAP   DUB,TOTEXDF                                                      
         AP    DUB,OVROUND                                                      
         CURED DUB,(13,2(R2)),COMPCURT,ALIGN=LEFT,MINUS=YES                     
                                                                                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    UPD16                                                            
         LA    R2,2(R2)            R2=A(EXCHANGE DIFFERENCE AMOUNT)             
         AR    R2,R0               ADD SIGNIFICANT LENGTH                       
         USING SCUTOTS,R1                                                       
         L     R1,ASCUTOTS                                                      
         ZAP   DUB,STOEXDF                                                      
         AP    DUB,OVROUNDS                                                     
         DROP  R1                                                               
         CURED DUB,(13,0(R2)),SCNDCURT,ALIGN=LEFT,MINUS=YES,CURSYMB=YES         
                                                                                
UPD16    DS    0H                                                               
         MVC   REPH5(L'REPH5-1),REPH5+1  REMOVE INITIAL FF                      
*&&                                                                             
UPD18    GOTO1 ABATMAN,BATADDQ     LIVE - CHECK/ADD BATCH RECORD                
         BNE   EXIT                                                             
                                                                                
         LA    R1,CTRYTAB                                                       
         USING CTRYTABD,R1                                                      
UPD20    CLI   CTRYCTRY,0          TEST NO COUNTRY MATCH                        
         BNE   *+14                                                             
         OC    UPDIND1,PSTDFLT     SET DEFAULT POSTING INDICATORS               
         B     UPD22                                                            
         CLC   CTRYCTRY,AGYCTRY                                                 
         BNE   *+14                                                             
         OC    UPDIND1,CTRYPSTI    SET COUNTRY POSTING INDICATORS               
         B     UPD22                                                            
         LA    R1,CTRYTABL(R1)                                                  
         B     UPD20                                                            
         DROP  R1                                                               
                                                                                
UPD22    ICM   R2,15,AICOTAB       TEST/SET A(INTERCOMPANY TABLE)               
         BZ    *+10                                                             
         GOTO1 ABLDICO             BUILD INTERCOMPANY POSTING TABLE             
         LA    R0,IOAMAX           R0=IO AREA COUNT                             
         L     R1,AIOAS            R1=A(1ST IOAREA IN GWS)                      
         LA    RF,AIOSAVE1         RF=A(1ST A(IOSAVE) IN OVRWORK)               
UPD24    ST    R1,0(RF)            SAVE A(IOSAVE)                               
         LA    RE,IOBUFF-IOAS(R1)  RE=A(IO BUFFER)                              
         ST    RE,4(RF)            SAVE A(IOBUFF)                               
         LA    RF,8(RF)            RF=NEXT A(IOSAVE)                            
         LA    R1,IOALQ(R1)        R1=NEXT IOAREA                               
         BCT   R0,UPD24                                                         
         USING TRNBLK,RF                                                        
         LA    RF,TRNBLOCK         RF=A(ADDTRN BLOCK)                           
         MVC   TRNCTRY,AGYCTRY     COUNTRY                                      
         MVC   TRNCOMF,ACOM        A(COMFACS)                                   
         MVC   TRNCPYS1,COMPSTAT                                                
         MVC   TRNCPYS2,COMPSTA2                                                
         MVC   TRNCPYS3,COMPSTA3                                                
         MVC   TRNCPYS4,COMPSTA4                                                
         MVC   TRNCPYS5,COMPSTA5                                                
         MVC   TRNCPYS6,COMPSTA6                                                
         MVC   TRNCPYS7,COMPSTA7                                                
         MVC   TRNCPYS8,COMPSTA8                                                
         MVC   TRNCPYS9,COMPSTA9                                                
         MVC   TRNCPYSA,COMPSTAA                                                
         MVC   TRNGLMOA,COMPGMOA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                                                               
         MVC   TRNACC,AOVIOB3      A(FOR ACCOUNT I/O)                           
         MVC   TRNBMOS,SBATMONP    PWOS BATCH MONTH                             
         MVC   TRNPUSER,TWAUSRID   SET USER-ID FROM TWA                         
         MVI   TRNINDS1,TRNIVDAT   DON'T CHECK TRNKDATE/TRNDATE                 
*&&UK                                                                           
         MVC   TRNTOBA,VTOBACCO    PASS A(TOBACCO)                              
         MVC   TRNCCCUR,C.CURTCUR                                               
         MVC   TRNCCURS,S.CURTCUR                                               
*&&                                                                             
         DROP  RF                                                               
                                                                                
         LA    R1,INPMRKH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,OVRNUM                                                      
*&&UK*&& ZAP   SAVROUND,OVROUND    INITIALISE ROUNDING ERROR                    
*&&UK*&& ZAP   SAVROUNS,OVROUNDS                                                
UPD26    GOTO1 ATSARGET,OVRNUM                                                  
         BNE   UPD30                                                            
         TM    TSARINDS,TSARMKQ                                                 
         BZ    UPD28                                                            
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARIND2,TSARGRSS                                                
         BNO   *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'AC4GROSS),AC4GROSS                          
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPD28                                                            
         GOTO1 VREPORT,REPD        PRINT IT                                     
UPD28    ICM   R1,3,OVRNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         B     UPD26                                                            
                                                                                
UPD30    LA    R1,1                                                             
         STCM  R1,3,OVRNUM                                                      
*&&UK                                                                           
         ZAP   DUB,SAVROUND                                                     
         BZ    UPD34                                                            
         CURED DUB,(L'MANCAM,WORK+1),COMPCURT,ALIGN=LEFT,FLOAT=-                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY ALLOCATION           
         BZ    UPD32                                                            
         STC   R0,BYTE             SAVE SIGNIFICANT LENGTH                      
         LA    R2,WORK+1                                                        
         AR    R2,R0                                                            
         ZAP   DUB,SAVROUNS                                                     
         CURED DUB,(L'MANCAM,1(R2)),SCNDCURT,ALIGN=LEFT,FLOAT=-,       X        
               CURSYMB=YES                                                      
         SR    R2,R2                                                            
         IC    R2,BYTE             GET LENGTH OF FIRST EDIT                     
         LA    R2,1(R2)            PLUS SPACE                                   
         AR    R2,R0               ADD LENGTH OF THIS EDIT                      
         LR    R0,R2               SET OVERALL LENGTH                           
UPD32    LA    R1,DMCB             WARNING, UNABLE TO ROUND OFF EX. DIF         
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMTYP,GTMSCR                                                    
*        MVI   GTMAXL,L'REPP1      MAXIMUM LENGTH                               
         MVI   GTMAXL,100          MAXIMUM LENGTH OF GETTXT MOVE                
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         AH    R0,=H'1'                                                         
         STC   R0,WORK                                                          
         LA    R0,WORK                                                          
         STCM  R0,7,GTASUBST                                                    
         LA    R0,REPP1                                                         
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVC   GTMSGNO,=AL2(AS$UNEXD)                                           
         GOTO1 VGETTXT,(R1)                                                     
         DROP  R1                                                               
         CLI   XACTION,ACTDRFT     TEST DRAFT/LIVE UPDATE                       
         BE    *+6                                                              
         DC    H'0'                ROUNDING ERROR ON FULL UPDATE                
         GOTO1 VREPORT,REPD        MUST BE PRINTING IF DRAFT                    
                                                                                
UPD34    ZAP   SAVROUND,OVROUND    INITIALISE ROUNDING ERROR                    
         ZAP   SAVROUNS,OVROUNDS                                                
*&&                                                                             
         GOTO1 AREPINI             PRINT POSTINGS HEADING                       
         GOTO1 ASTDEL              SET UP STANDARD ELEMENTS                     
                                                                                
         USING JARAYD,R1                                                        
         SAM31 ,                                                                
         L     R1,AJARAY           INIT TABLE                                   
         MVI   JARDFIL,JARDEOT                                                  
         SAM24 ,                                                                
         DROP  R1                                                               
                                                                                
UPD36    GOTO1 ATSARGET,OVRNUM                                                  
         BE    UPD38                                                            
         L     RF,ATSARBLK                                                      
         USING TSARD,RF                                                         
         TM    TSERRS,TSEEOF                                                    
         BO    UPD58                                                            
         DC    H'0'                                                             
         DROP  RF                                                               
                                                                                
UPD38    TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    UPD56                                                            
         TM    TSARINDS,TSARMKQ    TEST IF SELECTED FOR CHEQUE                  
         BZ    UPD56                                                            
                                                                                
         OC    TSARDADR,TSARDADR   TEST NO FILE TRANSACTION                     
         BNZ   UPD40                                                            
         GOTO1 ABLDNEW             BUILD A TRANSACTION FROM SCRATCH             
         B     UPD46               GO STRAIGHT TO POST NEW TRANSACTION          
                                                                                
UPD40    MVC   IODA,TSARDADR       READ CREDIT INTO IOBUFF1                     
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    *+8                 NO - GET FOR UPDATE                          
         LA    R1,IOGETRUP+IOACCMST+IO1Q                                        
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 ASETELAD,AIOBUFF1   SET A(CREDIT TRANSACTION ELEMENTS)           
         MVC   OVOLDAFC,OLDAFCEL   SAVE ANY OLD MEMBER CURRENCY AFCEL           
         CLI   AMPYEL,FF           TEST MULTIPLE MPYELS - SEE SETELAD           
         BE    *+12                                                             
         CLI   AMPYEL,MPYLN2Q      TEST SINGLE MPYEL OF CORRECT LENGTH          
         BNL   UPD42                                                            
         GOTO1 ASETMPY,AIOBUFF1    NO - DELETE MPYEL(S), ADD FRESH ONE          
         GOTO1 ASETELAD,AIOBUFF1   RESET A(CREDIT TRANSACTION ELEMENTS)         
                                                                                
         USING TRNELD,R2                                                        
UPD42    ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRNSTAT,TSARSTA     TEST SOMEONE AMENDING ELSEWHERE              
         BNE   UPDABCAN                                                         
         NI    TRNSTAT,FF-(TRNSHOLD)   CLEAR HELD                               
                                                                                
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   TRSSTAT,TSARSSTA    TEST SOMEONE AMENDING ELSEWHERE              
         BNE   UPDABCAN                                                         
                                                                                
UPD43    TM    COMPSTAD,CPYSAP2J   Pay2Job checks (after potential              
         JZ    UPD43X              PROMOTE)                                     
CRED     USING TRNRECD,R1                                                       
         L     R1,AIOBUFF1         Applicable scenario?                         
         CLI   CRED.TRNRSTYP,TRNTINV            TYPE-01 FOR US & UK             
         JE    UPD43A                                                           
*&&US*&& CLI   CRED.TRNRSTYP,TRNTMEPY           TYPE-10 FOR US                  
*&&US*&& JE    UPD43A                                                           
*&&US*&& CLI   CRED.TRNRSTYP,TRNTMUBL           TYPE-46 FOR US                  
*&&US*&& JE    UPD43A                                                           
*&&US*&& CLI   CRED.TRNRSTYP,61                 TYPE-61 FOR US                  
*&&US*&& JNE   UPD43X                                                           
*&&UK*&& CLI   CRED.TRNRSTYP,TRNTNBIN           TYPE-21 FOR Uk                  
*&&UK*&& JE    UPD43A                                                           
*&&UK*&& CLI   CRED.TRNRSTYP,72                 TYPE-72 FOR Uk                  
*&&UK*&& JNE   UPD43X                                                           
UPD43A   CLI   CRED.TRNKLDG,C'V'                                                
         JE    UPD43B                                                           
*&&US                                                                           
         CLI   CRED.TRNKLDG,C'W'                                                
         JE    UPD43B                                                           
         CLI   CRED.TRNKLDG,C'X'                                                
         JE    UPD43B                                                           
         CLI   CRED.TRNKLDG,C'Y'                                                
         JNE   UPD43X                                                           
*&&                                                                             
*&&UK                                                                           
         CLI   CRED.TRNKLDG,C'X'                                                
         JNE   UPD43X                                                           
*&&                                                                             
         DROP  CRED                                                             
                                                                                
         USING JARAYD,R1                                                        
UPD43B   DS    0H                                                               
         SAM31 ,                                                                
         L     R1,AJARAY           Put to table                                 
         LHI   R0,JARAY#                                                        
UPD43C   CLI   JARDFIL,JARDEOT                                                  
         JE    UPD43D                                                           
         AHI   R1,JARAYLQ                                                       
         JCT   R0,UPD43C                                                        
         SAM24 ,                                                                
         DC    H'0'                                                             
***      DOP2J call disabled here (just to be in sync with DOP2M code)          
***      L     R1,AJARAY                                                        
                                                                                
UPD43D   MVI   JARDFIL,JARDFMQ     ADD TO TABLE (MUST BE ACCMST)                
         MVC   JARDPDA,IODA                                                     
         XC    JARDERR,JARDERR                                                  
         MVC   JARDATE,CHQDATB                                                  
         MVC   JARPAYR,CHQNUM                                                   
         MVI   JARPREV,C'N'        no reversals (required as passing            
         AHI   R1,JARAYLQ          credits)                                     
         MVI   JARDFIL,JARDEOT                                                  
         SAM24 ,                                                                
         DROP  R1                                                               
                                                                                
UPD43X   CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD44                                                            
         GOTO1 AEXTRSL             YES - EXTEND IT                              
         GOTO1 ASETELAD,AIOBUFF1   REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD44    MVI   TRSMARK,TRSMCMQ     SET MARKER TYPE/ACTION                       
         MVC   TRSUDAT,TODAYC      SET USED DATE                                
         MVC   TRSUMOS,SBATMONP    SET USED MOS                                 
         NI    TRSSTAT,FF-TRSSVOID CLEAR VOID STATUS, IF SET                    
         XC    TRSVOID,TRSVOID     CLEAR SAVED USED DATE, IF SET                
         OI    TRSSTAT2,TRSSMCHQ   SET PAID BY MARKER MANUAL CHEQUE             
                                                                                
         USING MPYELD,R2                                                        
         ICM   R2,15,AMPYEL        R2=A(MANUAL PAYMENT ELEMENT)                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MPYNO,CHQNUM                                                     
         MVC   MPYDTE,CHQDATB                                                   
         MVC   MPYBNK,BANKACC+(TRNKUNT-TRNKCULA)                                
         ZAP   MPYAMNT,CHQAMT                                                   
                                                                                
         TM    TSARIND2,TSARGRSS   TEST TAKEN GROSS                             
         BZ    UPD46                                                            
         USING SCIELD,R2                                                        
         ICM   R2,15,ASCICDSC      R2=A(LIVE DISCOUNT ELEMENT)                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   SCITYPE,SCITNOCD    SET DISCOUNT NOT TAKEN                       
         XC    ASCICDSC,ASCICDSC   CLEAR A(LIVE DISCOUNT SCIEL)                 
                                                                                
UPD46    DS    0H                                                               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+14                                                             
         ZAP   DUB,CHQAMS          SET SECOND CURRENCY CHEQUE                   
         B     UPD48                                                            
         TM    COMPSTA7,CPYSSCNV   TEST CONVERTED TO OCAELS                     
         BZ    UPD50                                                            
         MVC   WORK(L'CURTCUR),C.CURTCUR  PRIMARY INTO SECONDARY                
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         ZAP   DUB,CHQAMT          USE PRIMARY CURRENCY CHEQUE                  
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         ZAP   DUB,12(8,R1)        TO SET SECONDARY CURRENCY CHEQUE             
UPD48    LA    R1,OVOCAL           UPDATE/CREATE CREDIT OCAEL                   
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QMPYAMNT   SECONDARY CURRENCY CHEQUE AMOUNT             
         MVI   OCANSEQN,1          CAN ONLY BE ONE (SEE ABOVE)                  
         ZAP   OCANCASH(L'MPYAMNT),DUB                                          
         LA    R1,OCANTR1L+L'MPYAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAACNV',WORK),AIOBUFF1,ACOM,OVOCAL,0,0         
         MVI   OVOCAL,0            RESET A(OCA VALUES LIST)                     
         LA    R1,OVOCAL                                                        
         ST    R1,AOCANXT          RESET A(NEXT)                                
         GOTO1 ASETELAD,AIOBUFF1   RESET A(ELEMENTS)                            
*&&                                                                             
UPD50    GOTO1 AADDOFF             ADD TO OFFICE TABLE                          
         BNE   UPDABEND                                                         
         ICM   RF,15,ATRNEL                                                     
         ZAP   OVIAMNT,TRNAMNT-TRNELD(L'TRNAMNT,RF)                             
*&&UK*&& AP    OVIAMNT,TMPEXDF                                                  
         TM    TSARIND3,TSARDIFF   TEST PENNY DIFFERENCE                        
         BO    UPD56               NO POSTING TO VENDOR                         
         GOTO1 APSTINV,AIOBUFF1    BLD & POST INVOICE LEVEL IN IOBUFF2          
         BNE   UPDABEND                                                         
         OC    TSARDADR,TSARDADR   TEST NO FILE TRANSACTION                     
         BZ    UPD56               THAT'S ALL                                   
                                                                                
         USING MPYELD,R2                                                        
         ICM   R2,15,AMPYEL        R2=A(CR MANUAL PAYMENT ELEMENT)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MPYSUB,OVPAYSBR     SET NEW DR KEY SUBREF IN CR MPYEL            
*&&UK                                                                           
         USING AFCELD,R2                                                        
         ICM   R2,15,AAFCEL        R2=A(CR AFCEL)                               
         BZ    *+16                                                             
         TM    SAFCIND1,SAFCI1SC   TEST NOT DEALING IN CURRENCY                 
         BO    *+8                                                              
         OI    AFCXSTA2,AFCXSMEM   CHANGE TO MEMO CURRENCY                      
                                                                                
         OC    OVOLDAFC,OVOLDAFC   TEST OLD MEMBER CURRENCY AFCEL               
         BZ    UPD52                                                            
         LA    R2,OVOLDAFC         RE-ADD OLD AFCEL AS MEMO                     
         OI    AFCXSTA2,AFCXSMEM   CHANGE TO MEMO CURRENCY                      
         GOTO1 VHELLO,DMCB,(C'P',OVACCMST),AIOBUFF1,AFCELD,            X        
               =C'ADD=END',0                                                    
         CLI   12(R1),0                                                         
         BNE   UPDABEND                                                         
*&&                                                                             
UPD52    DS    0H                                                               
         L     R2,AIOBUFF1         R2=A(CR DATA RECORD)                         
         USING TRNRECD,R2                                                       
         OI    TRNRSTA2,TRNSUSED                                                
         MVC   TRNRSUSE,SBATMONP                                                
         XC    IODA,IODA           FORCE REFRESH OF IODA/IOWORK                 
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD54               YES - GET NEXT TSAR RECORD                   
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
         MVC   BYTE,TRNRSTAT            SAVE ORIGINAL STATUS                    
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   *+12                                                             
         NI    TRNRSTAT,FF-TRNSARCH     CLEAR ACCARC INDICATOR                  
         LA    R1,IOADFR+IOACCMST+IO1Q  RE-ADD ACCARC RECORD TO ACCMST          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPD54    MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    *+8                                                              
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNRSTA),TRNRSTA                         
         TM    BYTE,TRNSARCH       TEST RECORD PROMOTED TO ACCMST               
         BNO   *+14                                                             
         DROP  R2                                                               
         L     R2,AIOSAVE          R2=A(SAVED DATA RECORD VALUES)               
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD56                                                            
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
UPD56    ICM   R1,3,OVRNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         B     UPD36                                                            
                                                                                
UPD58    DS    0H                                                               
*&&UK*&& GOTO1 AAPPBCO             APPORTION BANK CHARGES TO OFFICES            
         GOTO1 ATOTOFF             CREATE SINGLE MERGED OFFICE ENTRY            
         B     UPD60                                                            
         EJECT                                                                  
***********************************************************************         
* POST CHEQUE(S)/DISCOUNT/DISCOUNT TAX ADJUSTMENT/BANK CHARGES        *         
***********************************************************************         
                                                                                
UPD60    GOTO1 APSTOFF             POST TRANSACTIONS BY OFFICE                  
         BNE   UPDABEND            ERROR - MUST ABEND                           
         CLI   AGYCTRY,CTRYGER     TEST GERMANY                                 
         BNE   UPD66                                                            
         GOTO1 APSTCOS             POST COSTING BY CLIENT/OFFICE                
         BNE   UPDABEND            ERROR - MUST ABEND                           
UPD66    OC    AICOTAB,AICOTAB     TEST POSTING INTERCOMPANY                    
         BZ    UPD68                                                            
         GOTO1 APSTICO             POST INTERCOMPANY                            
                                                                                
UPD68    TM    UPDIND2,UPDIADDQ    TEST ANYTHING POSTED                         
         BO    UPD70                                                            
                                                                                
         GOTO1 ABATMAN,BATUPDQ     DELETE BATCH RECORD                          
                                                                                
         LA    R1,MRKACTH          STICK CURSOR HERE ON ERROR                   
         MVC   FVMSGNO,=AL2(EANOTUPD)  NOTHING TO UPDATE                        
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPDATEX                                                          
         MVC   FVMSGNO,=AL2(EANOTUPR)  NOTHING TO UPDATE, REPORT                
         B     UPDATEX                                                          
                                                                                
         USING TRNBLK,RF                                                        
UPD70    LA    RF,TRNBLOCK         LAST TIME CALL FOR ADDTRN                    
         OI    TRNINDS,TRNILAST                                                 
         OI    TRNINDS2,TRNIUPDG   UPDATING GL TOO                              
         MVC   TRNREC,AIOBUFF2     A(TRANSACTION RECORD)                        
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    UPD71                                                            
         GOTO1 VADDTRN,TRNBLK                                                   
         DROP  RF                                                               
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 ABATMAN,BATUPDQ     UPDATE BATCH RECORD                          
                                                                                
         NI    TWAMODE2,FF-TWAM2CHG                                             
         NI    TWAMODE3,FF-TWAM3WRM                                             
                                                                                
UPD71    BAS   RE,DOP2J            PROCESS PAY TO JOBS/INVOICES                 
         JE    UPD72                                                            
         MVC   FVMSGNO,=AL2(AE$PNFUS)                                           
         J     UPDABEND                                                         
                                                                                
UPD72    MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPD74                                                            
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         GOTO1 APRTCLO                                                          
UPD74    MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         LA    R1,MRKACTH                                                       
                                                                                
UPDATEX  ST    R1,FVADDR           SET FIELD ADDRESS                            
         XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         B     EXIT                                                             
                                                                                
UPDABCAN MVI   FVOMTYP,GTMINF      UPDATE CANCELLED                             
         MVC   FVMSGNO,=AL2(AI$UPCAN)                                           
UPDABEND LA    R2,PARM             DEFINE GETTXT CONTROL BLOCK                  
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMTYP,FVOMTYP      SET MESSAGE TYPE                             
         MVC   GTMSGNO,FVMSGNO     ERROR SET BY UPDATE ROUTINE                  
         CLI   GTMSGNO,X'FF'       TEST HOB INDICATES GENERAL MESSAGE           
         BNE   *+12                                                             
         MVI   GTMSGNO,0           CLEAR HOB                                    
         MVI   GTMSYS,X'FF'        SET GENERAL SYSTEM                           
         XC    MRKMSG,MRKMSG       CLEAR MESSAGE FIELD                          
         CLC   FVXTRA,SPACES       TEST EXTRA TEXT APPENDED                     
         BNH   UPDABE02                                                         
         LA    R0,FVXTRA                                                        
         STCM  R0,7,GTATXT                                                      
         LA    RF,FVXTRA+L'FVXTRA-1                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R0                                                            
         LA    RF,1(RF)                                                         
         STC   RF,GTLTXT                                                        
                                                                                
UPDABE02 GOTO1 VGETTXT,GETTXTD                                                  
         OI    MRKMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MRKSCRH+(FVOIND-FVIHDR),FVOCUR                                   
         MVC   MRKACT,SACTNAME     RESTORE REAL ACTION NAME                     
         MVC   XACTNAME,SACTNAME   RESTORE REAL ACTION VALUES                   
         MVC   XACTION,SACTION                                                  
         MVC   XACTINDS,SACTINDS                                                
         DC    H'0',C'$ABEND'                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DO PAY 2 JOBS IF REQUIRED                                           *         
***********************************************************************         
         USING JARAYD,R3                                                        
DOP2J    NTR1                                                                   
         SAM31 ,                                                                
         L     R3,AJARAY           ANYTHING IN THE TABLE?                       
         CLI   JARDFIL,JARDEOT                                                  
         JE    DOP2JY                                                           
                                                                                
         USING JPYDETD,R2                                                       
         LA    R2,ELEMT                                                         
         XC    JPYDETD(JPYDLNQ),JPYDETD                                         
         MVI   JPYDUPD,C'N'                                                     
         CLI   XACTION,ACTDRFT                                                  
         JE    DOP2J2                                                           
         MVI   JPYDUPD,C'Y'                                                     
                                                                                
DOP2J2   STCM  R3,B'1111',JPYDARY                                               
                                                                                
         MVC   JPYDCLL,PRODALEN                                                 
         LLC   RE,PRODALEN                                                      
         LLC   RF,PRODBLEN                                                      
         SR    RF,RE                                                            
         STC   RF,JPYDPRL                                                       
         LLC   RE,PRODBLEN                                                      
         LLC   RF,PRODCLEN                                                      
         SR    RF,RE                                                            
         STC   RF,JPYDJOL                                                       
                                                                                
         MVC   JPYDCOM,ACOM                                                     
*&&UK*&& MVC   JPYDPRO,VPROMOTE                                                 
                                                                                
         MVI   JPYDTYP,JPYDTJQ+JPYDRIQ                                          
                                                                                
         SAM24 ,                                                                
         L     RF,=V(PAY2JOBC)                                                  
         A     RF,OVRELO                                                        
         GOTO1 (RF),JPYDETD                                                     
         JNE   *+2                                                              
         SAM31 ,                                                                
***      JNE   DOP2JN              (ignore any errors for now)                  
                                                                                
         L     R3,AJARAY                                                        
         MVI   JARDFIL,JARDEOT                                                  
                                                                                
DOP2JY   DS    0H                                                               
         SAM24 ,                                                                
         CR    RB,RB                                                            
         J     DOP2JX                                                           
DOP2JN   LTR   RB,RB                                                            
DOP2JX   XIT1                                                                   
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
* QUIT                                                                *         
***********************************************************************         
                                                                                
QUIT     XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    TWAMODE2,FF-TWAM2CHG                                             
         B     EXIT                                                             
                                                                                
***********************************************************************         
* GENERAL EXIT                                                        *         
***********************************************************************         
                                                                                
EXIT     LH    RF,=Y(SOVRWRK2-SAVED)                                            
         LA    RF,SAVED(RF)                                                     
         USING SOVRWRK2,RF                                                      
         MVC   SACHQAMT,CHQAMT     CHEQUE AMOUNT                                
         MVC   SACHQAMC,CHQAMC     FOREIGN AMOUNT                               
*&&UK*&& MVC   SAOVRATE,MANRAT     EXCHANGE RATE                                
         DROP  RF                                                               
         XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
                                                                                
OVLIDC   DS    0X                  OVLIDS MUST MAP TO OVLIDC EXACTLY            
LDGLIDC  DS    0X                  VALID SUPPLIER LEDGERS IN UNIT S             
*&&UK*&& DC    C'FTVX'                                                          
*&&US*&& DC    C'PQSTUVWXY'                                                     
         DC    AL1(EOT)                                                         
LDGLIDCL EQU   *-LDGLIDC                                                        
                                                                                
BNKLIDC  DC    C'CB'               VALID BANK LEDGERS IN UNIT S                 
         DC    AL1(EOT)                                                         
BNKLIDCL EQU   *-BNKLIDC                                                        
                                                                                
CTRYTDC  DC    AL1(CTRYUSA)        COUNTRY POSTING RULES                        
         DC    AL1(UPDIBANK)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(CTRYCAN)                                                     
         DC    AL1(UPDIBANK)                                                    
         DC    AL1(0,0)                                                         
         DC    AL1(CTRYGER)                                                     
         DC    AL1(UPDIBANK+UPDICDSC+UPDIDTAX+UPDIEXDF+UPDIBCHA)                
         DC    AL1(0,0)                                                         
         DC    AL1(EOT)                                                         
CTRYTDCL EQU   *-CTRYTDC                                                        
                                                                                
         DC    CL(L'TRNKULC)'SJ999'                                             
         DC    CL8'ACCMST'                                                      
         DC    AL1(UPDIBANK+UPDICDSC+UPDIEXDF+UPDIBCHA)                         
         DC    PL2'200'            MAXIMUM INVOICES FOR ONE CHEQUE              
         DC    CL(L'CURTCUR)'EUR'  EURO CURRENCY CODE                           
         DC    CL(L'CURTCUR)'GBP'  GBP CURRENCY CODE                            
                                                                                
OVLIDCL  EQU   *-OVLIDC                                                         
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES - ONE                                              *         
***********************************************************************         
                                                                                
         DS    0F                                                               
OVROU1   NMOD1 0,**OVR1**,RA,R9                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     ADDSPA                                                           
         B     ADDTRN                                                           
*&&UK*&& B     APPBCO                                                           
         B     BATMAN                                                           
         B     BLDDSC                                                           
         B     BLDICO                                                           
         B     BLDNAR                                                           
*&&UK*&& B     CALCRAT                                                          
*&&UK*&& B     MAXMIN                                                           
         B     MRKALL                                                           
         B     PSTCOS                                                           
         B     PSTOFF                                                           
         B     PSTINV                                                           
         B     PSTICO                                                           
*&&UK*&& B     RATLIM                                                           
         B     RECFLT                                                           
         B     SETACT                                                           
         B     SETKEY                                                           
         B     SETMPY                                                           
         B     STDEL                                                            
         B     TOTOFF                                                           
*&&UK*&& B     VERRAT                                                           
                                                                                
OVROU1L  MVI   DUB,0                                                            
         B     *+8                                                              
OVROU1E  MVI   DUB,1                                                            
         B     *+8                                                              
OVROU1H  MVI   DUB,2                                                            
         CLI   DUB,1                                                            
OVROU1X  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* ADD SPECIAL PAYMENT ACCOUNT ELEMENTS IF NECCESARY                   *         
* NTRY - P1=(PSTACT,A(IOBUFF2)), P2=A(OFFTAB ENTRY), P3=A(TRAN ELEM)  *         
***********************************************************************         
                                                                                
ADDSPA   L     R2,0(R1)            ADDRESS TRANSACTION RECORD                   
         USING TRNRECD,R2                                                       
         L     R3,4(R1)            ADDRESS OFFICE TABLE ENTRY                   
         USING OFFTABD,R3                                                       
         L     R4,8(R1)            ADDRESS TRANSACTION ELEMENT                  
         USING TRNELD,R4                                                        
         MVC   OVBYTE,0(R1)                                                     
         LA    R5,PSTTAB           SEARCH OFFICE POSTING TABLE                  
         USING PSTTABD,R5                                                       
ADDSPA02 IC    RF,PSTACT                                                        
         N     RF,=AL4(FF-PSTLEVEL)                                             
         EX    RF,*+8                                                           
         BZ    ADDSPA12                                                         
         TM    OVBYTE,0                                                         
         CLI   PSTSPAT,0           TEST SPAEL REQUIRED                          
         BE    ADDSPA12                                                         
         MVC   LARFADDR,PSTAMNT    TEST AN AMOUNT TO BE POSTED                  
         EX    0,LARF                                                           
         LTR   RF,RF                                                            
         BZ    ADDSPA12                                                         
         AH    RF,PSTAMOF                                                       
         CP    0(L'OFFCDSC,RF),PZERO                                            
         BE    ADDSPA12                                                         
         MVC   LARFADDR,PSTACC     TEST ACCOUNT EXITS                           
         EX    0,LARF                                                           
         USING ACCXD,RF                                                         
         OC    ACCX,ACCX                                                        
         BNZ   *+14                                                             
         MVC   FVMSGNO,PSTSERR                                                  
         B     OVROU1H             ERROR WILL FORCE ABEND                       
                                                                                
         XR    R1,R1                                                            
         LA    RE,TRNRFST          SEE IF RELAVENT SPA ALREADY EXISTS           
         USING SPAELD,RE                                                        
ADDSPA04 CLI   SPAEL,EOT                                                        
         BE    ADDSPA08                                                         
         CLI   SPAEL,SPAELQ                                                     
         BNE   ADDSPA06                                                         
         CLC   SPATYPE,PSTSPAT                                                  
         BNE   ADDSPA06                                                         
                                                                                
         MVC   SPAAULA,ACCX+(ACTKULA-ACTKCULA)    CHANGE THIS SPA               
         ICM   R1,1,ACCXOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    ADDSPA12                                                         
         LA    R1,SPAAACT-1(R1)                                                 
*&&US                                                                           
         CLC   SPAAULA(L'BANKUL),BANKACC+1                                      
         BE    *+10                                                             
*&&                                                                             
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         B     ADDSPA12                                                         
                                                                                
ADDSPA06 IC    R1,SPALN                                                         
         AR    RE,R1                                                            
         B     ADDSPA04                                                         
         DROP  RE                                                               
                                                                                
ADDSPA08 XR    RE,RE               ADD SPAEL TO END                             
         ICM   RE,3,TRNRLEN        TAKE CURRENT RECORD LENGTH                   
         LA    RE,TRNRECD(RE)                                                   
         BCTR  RE,0                RE=A(EOR)                                    
         USING SPAELD,RE                                                        
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ        BUILD SPECIAL POSTING A/C ELEMENT            
         MVI   SPALN,SPALNQ                                                     
         MVC   SPATYPE,PSTSPAT     ACCOUNT TYPE                                 
         MVC   SPAAULA,ACCX+(ACTKULA-ACTKCULA)                                  
         XR    R1,R1                                                            
         ICM   R1,1,ACCXOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    ADDSPA10                                                         
         LA    R1,SPAAACT-1(R1)                                                 
*&&US                                                                           
         CLC   SPAAULA(L'BANKUL),BANKACC+1                                      
         BE    *+10                                                             
*&&                                                                             
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         DROP  RE,RF                                                            
ADDSPA10 LA    RE,SPALNQ(RE)       BUMP TO NEXT ELEMENT                         
         MVI   0(RE),0             SET NEW EOR                                  
         LA    RE,1(RE)                                                         
         SR    RE,R2                                                            
         STCM  RE,3,TRNRLEN        SET NEW RECORD LENGTH                        
                                                                                
ADDSPA12 LA    R5,PSTTABL(R5)                                                   
         CLI   PSTACT,EOT                                                       
         BNE   ADDSPA02                                                         
ADDSPAX  B     OVROU1E                                                          
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ADD TRANSACTION CALLING EMU                                         *         
* NTRY R1 - A(TRNCACNM), TRNBLK HOLDS TRANSACTION                     *         
***********************************************************************         
ADDTRN   LR    R3,R1                                                            
         OI    UPDIND2,UPDIADDQ    SET RECORD(S) ADDED VIA ADDTRN               
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    ADDTRNX                                                          
         LA    RF,TRNBLOCK                                                      
         USING TRNBLK,RF                                                        
         MVC   TRNREC,AIOBUFF2     A(TRANSACTION RECORD)                        
         ICM   RE,3,TRNBSEQN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TRNBSEQN       UPDATE NUMBER OF RECORDS ADDED               
         MVC   TRNCACNM,0(R3)      SET CONTRA A/C NAME                          
         OI    TRNINDS,TRNICONV    SET CONVERTED RECORD                         
         OI    TRNINDS2,TRNIADDG   ADD GL POSTING                               
         GOTO1 VADDTRN,TRNBLK      ADD TRANSACTION                              
         BNE   ADDTRNE                                                          
         LA    RF,TRNBLOCK                                                      
         NI    TRNINDS,FF-TRNICONV RESET CONVERTED RECORD FLAG                  
         DROP  RF                                                               
         AP    ADDITEM,PONE        ITEM COUNT FOR ADDED RECORDS                 
*&&UK                                                                           
         L     RF,AIOBUFF2         FIND AND DELETE OCAEL                        
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         USING OCAELD,RF                                                        
         SR    R0,R0                                                            
ADDTRN02 IC    R0,OCALN                                                         
         AR    RF,R0                                                            
         CLI   OCAEL,0             DID NOT HAVE AN OCAEL                        
         BE    ADDTRNX                                                          
         CLI   OCAEL,OCAELQ        LOCATED OCAEL                                
         BNE   ADDTRN02                                                         
         GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),AIOBUFF2,ACOM,0,OCAELD,0            
         DROP  RF                                                               
*&&                                                                             
ADDTRNX  B     OVROU1E                                                          
                                                                                
ADDTRNE  OI    TWAMODE3,TWAM3UWD                                                
         XC    FVMSGNO,FVMSGNO                                                  
         L     RF,AIOBUFF2                                                      
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'TRNKCULA),TRNKCULA-TRNRECD(RF)                          
         LA    RF,TRNBLOCK                                                      
         USING TRNBLK,RF                                                        
         CLI   TRNERRS,TRNEACCI    TEST INVALID ACCOUNT                         
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         CLI   TRNERRS,TRNEACOL     TEST ACCOUNT CLOSED OR LOCKED               
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$LOCKD)                                           
         DROP  RF                                                               
         OC    FVMSGNO,FVMSGNO     TEST ACCEPTABLE ERROR                        
         BNZ   *+6                                                              
         DC    H'0'                ADDTRN ERROR                                 
         L     RD,SAVERD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  RETURN TO THE ROOT                           
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* APPORTION BANK CHARGES BY OFFICE                                    *         
***********************************************************************         
                                                                                
APPBCO   CP    BCHAAMT,PZERO       TEST BANK CHARGES TO BE POSTED               
         BE    APPBCOX                                                          
         LA    R0,OFFTABN          APPORTION BANK CHARGES BY OFFICE             
         L     R2,AOFFTAB                                                       
         USING OFFTABD,R2                                                       
         XR    R3,R3                                                            
         ZAP   DUB,BCHAAMT                                                      
         ZAP   OVDUB1,BCHAAM2                                                   
         ZAP   DUB2,PZERO                                                       
         ZAP   OVDUB,CHQAMT                                                     
APPBCO02 CLI   OFFTABD,EOT         TEST EOT                                     
         BE    APPBCO04                                                         
         ZAP   PKWK16A,OFFCHEQ                                                  
         BNM   *+10                                                             
         MP    PKWK16A,PONENEG                                                  
         CP    DUB2,PKWK16A        TEST HIGH OFFICE ABSOLUTE AMOUNT             
         BH    *+12                                                             
         ZAP   DUB2,PKWK16A        SAVE HIGH OFFICE AMOUNT                      
         LR    R3,R2               R3=A(HIGHEST OFFICE ENTRY)                   
         ZAP   PKWK16A,BCHAAMT     TAKE BANK CHARGES AMOUNT                     
         MP    PKWK16A,OFFCHEQ     MULTIPLY BY OFFICE AMOUNT                    
*&&UK                                                                           
         LA    RE,7                                                             
         XR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         SR    RE,RF                                                            
*&&                                                                             
*&&US*&& LA    RE,5                                                             
         SRP   PKWK16A,0(RE),0     SHIFT LEFT 7 LESS DECIMAL POINT              
         DP    PKWK16A,OVDUB       DIVIDE BY CHEQUE AMOUNT                      
         LCR   RE,RE               REVERSE NUMBER OF PLACES                     
         SRP   PKWK16A(8),64(RE),5 SHIFT RIGHT 7-DECP                           
         ZAP   OFFBCHA,PKWK16A(8)  FIRST 8 BYTES ARE QUOTIENT                   
         ZAP   OVDUB2,OFFBCHA                                                   
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,SMANBCHC   FROM CURRENCY                                
         MVC   EURKCUTO,SMANCURC   TO CURRENCY                                  
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),OVDUB2,OVDUB2,0,0               
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   OFFBCHAC,OVDUB2                                                  
         SP    DUB,OFFBCHA         REDUCE AMOUNT LEFT                           
         SP    OVDUB1,OFFBCHAC     REDUCE AMOUNT LEFT                           
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,APPBCO02                                                      
                                                                                
APPBCO04 LTR   R3,R3               TEST ANY OFFICE POSTINGS                     
         BZ    APPBCOX                                                          
         CP    DUB,PZERO           ACCOMMODATE ANY PENNY DIFFERENCES            
         BE    *+10                                                             
         AP    OFFBCHA-OFFTABD(L'OFFBCHA,R3),DUB                                
         CP    OVDUB1,PZERO        ACCOMMODATE ANY CENT DIFFERENCES             
         BE    *+10                                                             
         AP    OFFBCHAC-OFFTABD(L'OFFBCHAC,R3),OVDUB1                           
APPBCOX  B     OVROU1E                                                          
         DROP R2                                                                
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* BATCH MANAGEMENT ROUTINES                                           *         
***********************************************************************         
                                                                                
BATMAN   SLL   R1,2                ENTRY, R1=ACTION NUMBER                      
         B     *+0(R1)                                                          
         B     BATCHK              1 - CHECK BATCH UPDATE IS VALID              
         B     BATADD              2 - CHECK AND (RE)ADD BATCH RECORD           
         B     BATUPD              3 - UPDATE BATCH RECORD                      
                                                                                
BATCHK   LA    R1,MRKOPTH          TEST USER HAS GIVEN BATCH DATA               
         ST    R1,FVADDR                                                        
         OC    OBATMON,OBATMON     TEST OPTIONS BATCH MONTH                     
         BNZ   BATCHK02                                                         
         MVC   OBATMONP,BATMONP    (SET PWOS BATCH MONTH)                       
         OC    OBATMON,BATMON      TEST/SET HEADER MONTH IN OPTION              
         BNZ   BATCHK02                                                         
         MVC   FVMSGNO,=AL2(EAMISBMO)                                           
         B     OVROU1H             BATCH MONTH MISSING - USE OPTION             
BATCHK02 DS    0H                                                               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    BATCHK04                                                         
         TM    COMPSTA9,CPYS2CNV   TEST FILE NOT CONVERTED                      
         BO    BATCHK04                                                         
         CLC   COMPSCMO,OBATMONP   TEST AFTER SECOND CURRENCY START             
         BNH   BATCHK04                                                         
         MVC   FVMSGNO,=AL2(AE$SCCNV)                                           
         B     OVROU1H             SECONDARY CURRENCY NOT VALID                 
*&&                                                                             
BATCHK04 OC    OBATREF,OBATREF     TEST OPTIONS BATCH REFERENCE                 
         BNZ   BATCHK06                                                         
         OC    OBATREF,BATREF      TEST/SET HEADER REFERENCE IN OPTION          
         BNZ   BATCHK06                                                         
         MVC   FVMSGNO,=AL2(EAMISBRF)                                           
         B     OVROU1H             BATCH REF MISSING - USE OPTION               
                                                                                
BATCHK06 OC    OBATNAM,OBATNAM     TEST OPTIONS BATCH NAME                      
         BNZ   BATCHK08                                                         
         OC    OBATNAM,BATNAM      TEST/SET HEADER NAME IN OPTION               
         BNZ   BATCHK08                                                         
         MVC   FVMSGNO,=AL2(EAMISBNA)                                           
         B     OVROU1H             BATCH NAME MISSING - USE OPTION              
                                                                                
BATCHK08 MVC   SBATMON,OBATMON     SAVE BATCH VALUES (OPTIONS GET LOST)         
         MVC   SBATREF,OBATREF                                                  
         MVC   SBATNAM,OBATNAM                                                  
         MVC   SBATMONP,OBATMONP                                                
         B     OVROU1E             CC EQU - ROOT LOADS CONFIRM SCREEN           
         EJECT                                                                  
BATADD   LA    R2,KEY              CHECK BATCH RECORD DOESN'T EXIST             
         USING BATRECD,R2                                                       
         MVC   BATKEY,SPACES                                                    
         XC    BATKEY(BATKEND),BATKEY                                           
         MVI   BATKTYP,BATKTYPQ                                                 
         MVC   BATKCPY,COMPANY                                                  
         MVC   BATKOFF,TWAUSRID                                                 
         MVI   BATKGRUP,TBAGGENQ   GENERAL ACCOUNTING                           
         MVI   BATKTYPE,BT36       BT36                                         
         MVC   BATKDATE,TODAYP     DATE                                         
         MVC   BATKREF,SBATMON     YMRRRR (SBATMON+SBATREF)                     
         MVC   SBATKEY,BATKEY      SAVE IT FOR LATER                            
         LA    R1,IOREAD+IOACCDIR+IO3Q                                          
         CLI   XACTION,ACTDRFT     TEST DRAFT (NO UPDATE)                       
         BE    *+8                                                              
         LA    R1,IORDUPD+IOACCDIR+IO3Q                                         
         GOTO1 AIOEXEC                                                          
         MVC   BAERROR,IOERROR     SAVE ERROR RETURN BYTE                       
         BNE   BATADD2                                                          
         LA    R1,MRKOPTH          CURSOR TO OPTIONS FIELD                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EABATDUP)                                           
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'SBATREF),SBATREF BUILD REF/MMMYY                        
         LA    R1,FVXTRA+L'SBATREF-1                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
         LA    RF,2(R1)                                                         
         MVC   WORK(L'SBATMONP),SBATMONP                                        
         MVI   WORK+L'SBATMONP,X'01'     SET A DAY                              
         GOTO1 VDATCON,DMCB,(1,WORK),(9,(RF))                                   
         B     OVROU1H                   FOUND AND NOT DELETED - ERROR          
                                                                                
BATADD2  TM    BAERROR,IOEALL-IOERNF-IOEDEL  ALLOW DELETED/NOT FOUND            
         BZ    *+6                                                              
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         TM    BAERROR,IOEDEL      TEST RECORD FOUND BUT DELETED                
         BZ    BATADD4                                                          
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    BATADD4             SKIP GETREC (TRANSLATES TO DMREAD)           
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BNE   BATADD3                                                          
         GOTO1 AIOEXEC,IOGET+IORDEL+IOACCMST+IO3Q                               
         B     BATADD3A                                                         
BATADD3  GOTO1 AIOEXEC,IOGETRUP+IORDEL+IOACCMST+IO3Q                            
BATADD3A TM    BAERROR,IOEALL-IOEDEL                                            
         BZ    BATADD4                                                          
         DC    H'0'                                                             
                                                                                
BATADD4  L     R2,AIOBUFF          ADD/RE-ADD BATCH RECORD                      
         XC    BATRECD(256),BATRECD                                             
         MVC   BATKEY,SBATKEY                                                   
         MVI   BATRSTA,BATSNOK+BATSRECV                                         
         LA    RF,BATRFST                                                       
         USING BTHELD,RF                                                        
         MVI   BTHEL,BTHELQ                                                     
         MVI   BTHLN,BTHLNQ                                                     
         MVC   BTHNAME,SBATNAM     TAKE USER'S BATCH NAME                       
         ZAP   BTHCASH,PZERO                                                    
         ZAP   BTHITEM,PZERO                                                    
         LA    RF,BTHLNQ+1(RF)                                                  
         DROP  RF                                                               
         SR    RF,R2                                                            
         STCM  RF,3,BATRLEN                                                     
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    OVROU1E                                                          
         LA    R1,IOADDREC+IOACCMST+IO3Q  ADD RECORD                            
         TM    BAERROR,IOEDEL      TEST RECORD FOUND BUT DELETED                
         BZ    *+8                                                              
         LA    R1,IOPUT+IOACCMST+IO3Q  PUT RECORD BACK                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    OVROU1E                                                          
         MVC   KEY,BATRECD         EXTRACT KEY FROM DATA RECORD KEY             
         MVC   KEY+(BATKSTA-BATRECD)(L'BATKSTA),BATRSTA                         
         DROP  R2                                                               
         TM    BAERROR,IOEDEL      TEST REVIVING DELETED RECORD                 
         BZ    OVROU1E                                                          
         L     R2,AIOSAVE          R2=A(SAVED DATA RECORD VALUES)               
         MVC   KEY+(BATKDA-BATRECD)(L'BATKDA),0(R2)                             
         GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO3Q                                    
         BE    OVROU1E                                                          
         DC    H'0'                                                             
         EJECT                                                                  
         USING BATRECD,R2                                                       
BATUPD   LA    R2,KEY              RE-READ BATCH RECORD                         
         MVC   BATKEY,SBATKEY                                                   
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    OVROU1E                                                          
         LA    R1,IORDUPD+IOACCDIR+IO3Q                                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                DIE ON OTHER ERROR                           
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    BATUPD02            SKIP GETREC (TRANSLATES TO DMREAD)           
         GOTO1 AIOEXEC,IOGETRUP+IOACCMST+IO3Q                                   
         BE    BATUPD02                                                         
         DC    H'0'                                                             
                                                                                
BATUPD02 L     R2,AIOBUFF          AMEND AND UPDATE BATCH RECORD                
         MVI   BATRSTA,BATSUPD+BATSRECV   BATCH UPDATED                         
         TM    UPDIND2,UPDIADDQ    TEST ANYTHING ADDED                          
         BO    *+12                                                             
         MVI   BATRSTA,BATSDELT+BATSRECV  BATCH DELETED                         
         B     BATUPD04                                                         
         LA    RF,BATRFST                                                       
         USING BTHELD,RF                                                        
         CLI   BTHEL,BTHELQ        TEST RECORD OK                               
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   BTHCASH,TOTMRK      NET INVOICE TOTAL                            
*&&UK*&& AP    BTHCASH,TOTDSC      ADD BACK DISCOUNT                            
         ZAP   BTHITEM,ADDITEM     ITEMS ADDED VIA ADDTRN                       
         DROP  RF                                                               
                                                                                
BATUPD04 LA    R1,IOPUT+IOACCMST+IO3Q  PUT RECORD BACK                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                DIE ON ERROR                                 
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    OVROU1E                                                          
         MVC   KEY+(BATKSTA-BATRECD)(L'BATKSTA),BATRSTA                         
         DROP  R2                                                               
         L     R2,AIOSAVE          R2=A(SAVED DATA RECORD VALUES)               
         MVC   KEY+(BATKDA-BATRECD)(L'BATKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO3Q  WRITE RECORD                           
         GOTO1 AIOEXEC                                                          
         BE    OVROU1E                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD BATCH DETAILS LINE                                            *         
* NTRY - R1  BYTE  0    L'OUTPUT                                      *         
*            BYTES 1-3  A(OUTPUT)                                     *         
***********************************************************************         
                                                                                
BLDDSC   ST    R1,OVFULL           BUILD SUBSTITUTION BLOCK                     
         XC    WORK,WORK                                                        
                                                                                
         MVI   WORK,L'SBATREF+1    &1 - BATCH REFERENCE                         
         OC    WORK+1(L'SBATREF),SBATREF                                        
         LA    R2,WORK+L'SBATREF+1                                              
         BNZ   BLDDSC02                                                         
         MVI   WORK,2                                                           
         MVI   WORK+1,C'?'         BATCH REFERENCE UNKNOWN                      
         LA    R2,WORK+2                                                        
                                                                                
BLDDSC02 XC    TEMP,TEMP           &2 - BATCH MONTH                             
         OC    TEMP(L'SBATMONP),SBATMONP                                        
         BNZ   BLDDSC04                                                         
         MVI   0(R2),2                                                          
         MVI   1(R2),C'?'          BATCH MONTH UNKNOWN                          
         LA    R2,2(R2)                                                         
         B     BLDDSC06                                                         
BLDDSC04 MVI   TEMP+L'SBATMONP,01                                               
         MVI   0(R2),9                                                          
         MVC   1(8,R2),SPACES                                                   
         GOTO1 VDATCON,DMCB,(1,TEMP),(9,1(R2))                                  
         LA    R2,9(R2)                                                         
                                                                                
BLDDSC06 ZAP   OVDUB,PONE          &3 - ITEMS ADDED                             
         CURED OVDUB,(3,1(R2)),0,ALIGN=LEFT                                     
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                  &4 - ITEM COUNT CONTROL                      
         CURED OVDUB,(3,1(R2)),0,ALIGN=LEFT                                     
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         ZAP   OVDUB,TOTMRK        &5 - CASH TOTAL SO FAR                       
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC                                                
         BZ    *+10                                                             
         ZAP   OVDUB,CTOMRK        USE CURRENCY                                 
*&&                                                                             
         CURED OVDUB,(13,1(R2)),2,ALIGN=LEFT,MINUS=YES                          
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         ZAP   OVDUB,CHQAMT        &6 - CASH CONTROL TOTAL                      
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC                                                
         BZ    *+10                                                             
         ZAP   OVDUB,CHQAMC        USE CURRENCY                                 
*&&                                                                             
         CURED OVDUB,(13,1(R2)),2,ALIGN=LEFT,MINUS=YES                          
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         MVI   0(R2),0             SET END OF SUBSTITUTION BLOCK                
                                                                                
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         L     R0,OVFULL                                                        
         STCM  R0,8,GTMAXL         MAXIMUM LENGTH                               
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,WORK                                                          
         STCM  R0,7,GTASUBST                                                    
         MVC   GTMSGNO,=AL2(AS$BATRM)                                           
         GOTO1 VGETTXT,(R1)                                                     
BLDDSCX  B     OVROU1E                                                          
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* READ BANK ACCOUNT AND BUILD INTERCOMPANY TABLE IF NECESSARY         *         
* NTRY - R2=A(INTERCOMPANY TABLE)                                     *         
***********************************************************************         
                                                                                
         USING ICOTABD,R2                                                       
BLDICO   LA    R1,KEY              CLEAR KEY                                    
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,BANKACC                                                 
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XR    R0,R0                                                            
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST                                                       
         USING ICPELD,R1                                                        
BLDICO02 CLI   ICPEL,0                                                          
         BE    BLDICOX                                                          
         CLI   ICPEL,ICPELQ                                                     
         BE    BLDICO04                                                         
         IC    R0,ICPLN                                                         
         AR    R1,R0                                                            
         B     BLDICO02                                                         
                                                                                
BLDICO04 LA    R0,ICOTABN          MAXIMUM TABLE ENTRIES                        
         LA    R1,ICPSUBEL                                                      
         USING ICPSUBEL,R1                                                      
         XR    RF,RF                                                            
BLDICO06 ICM   RF,1,ICPSLN         TEST/SET SUB-ELEMENT LENGTH                  
         BZ    BLDICO10            EOR                                          
         ZAP   ICOAMNT,PZERO       PRESET AMOUNT TO ZERO                        
         ZAP   ICOAMNTS,PZERO      PRESET AMOUNT TO ZERO (2ND)                  
         MVC   ICOOFFC,ICPSOFF     SET OFFICE CODE                              
         MVC   ICOFACT,SPACES                                                   
         SH    RF,=Y((ICPSACC-ICPSUBEL)+1)                                      
         EX    RF,*+4                                                           
         MVC   ICOFACT(0),ICPSACC  SET FROM ACCOUNT                             
         IC    RF,ICPSLN           RESET SUB-ELEMENT LENGTH                     
         AR    R1,RF                                                            
         LA    R2,ICOTABL(R2)                                                   
         BCT   R0,BLDICO06                                                      
         DC    H'0'                INTERCOMPANY TABLE FULL                      
                                                                                
BLDICO10 L     R2,AICOTAB          NOW READ FROM INTERCOMPANY ACCOUNTS          
         LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES       SET COMPANY IN KEY                           
         MVC   ACTKCPY,COMPANY                                                  
                                                                                
BLDICO12 CLI   ICOTABD,0           TEST EOT                                     
         BE    BLDICO24                                                         
         MVC   ICOFNAM,SPACES      CLEAR FROM ACCOUNT NAME                      
         MVC   ICOTNAM,SPACES      CLEAR TO ACCOUNT NAME                        
         MVC   ACTKUNT(L'ICOTACT),ICOFACT                                       
         XR    R4,R4               SET FIRST TIME FOR MINI-LOOP                 
                                                                                
BLDICO14 LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    R0,R0                                                            
         B     *+10                                                             
BLDICO16 IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         CLI   NAMEL,0             TEST EOR                                     
         BE    BLDICO20                                                         
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   BLDICO18                                                         
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RF,ICOFNAM          FROM ACCOUNT NAME                            
         LTR   R4,R4               TEST SECOND TIME                             
         BZ    *+8                                                              
         LA    RF,ICOTNAM          TO ACCOUNT NAME                              
         EX    RE,*+8                                                           
         B     BLDICO16                                                         
         MVC   0(0,RF),NAMEREC                                                  
                                                                                
BLDICO18 LTR   R4,R4               TEST FIRST TIME                              
         BNZ   BLDICO16                                                         
         USING APTELD,R1                                                        
         CLI   APTEL,APTELQ        TEST ACCOUNT POINTER ELEMENT                 
         BNE   BLDICO16                                                         
         TM    APTSTAT,APTSINTL    TEST INTERCOMPANY POINTER ACCOUNT            
         BZ    BLDICO16                                                         
         MVC   ICOTACT,APTACCU     EXTRACT U/L/ACCOUNT                          
         B     BLDICO16                                                         
                                                                                
BLDICO20 LTR   R4,R4               TEST FIRST TIME                              
         BNZ   BLDICO22                                                         
         MVC   ACTKUNT(L'ICOTACT),ICOTACT                                       
         LA    R4,1                                                             
         B     BLDICO14            RETURN TO SET TO ACCOUNT VALUES              
                                                                                
BLDICO22 CLC   ICOFNAM,SPACES      TEST FROM ACCOUNT NAME                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   ICOTNAM,SPACES      TEST TO ACCOUNT NAME                         
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ICOTABL(R2)                                                   
         B     BLDICO12            RETURN TO READ/SET NEXT TABLE ENTRY          
                                                                                
BLDICO24 DS    0H                                                               
                                                                                
BLDICOX  B     OVROU1X                                                          
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD DYNAMIC CHEQUE NARRATIVE                           *         
* NTRY - R1=MESSAGE NUMBER                                            *         
***********************************************************************         
                                                                                
BLDNAR   STH   R1,OVHALF           SAVE MESSAGE NUMBER                          
         MVC   OVWORK,SPACES       CLEAR NARRATIVE OUTPUT AREA                  
         LA    R2,WORK             BUILD SUBSTITUTION STRING                    
         USING NARBLKD,R2                                                       
         MVC   NARBLKD(NARBLKL),SPACES                                          
         MVI   NARNUML,NARNUMLQ                                                 
         MVC   NARNUM,CHQNUM                                                    
         MVI   NARDATL,NARDATLQ                                                 
         MVC   NARDAT,SPACES                                                    
         GOTO1 VDATCON,DMCB,(1,CHQDATP),(17,NARDAT)                             
         MVI   NARAMTL,NARAMTLQ                                                 
         CURED CHQAMT,(13,NARAMT),2,ALIGN=LEFT                                  
         MVI   NARBNKL,NARBNKLQ                                                 
         MVC   NARBNK,BANKACC+(TRNKUNT-TRNKCULA)                                
         MVI   NARBLKD+NARBLKL,0                                                
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,OVHALF                                                   
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GTMAXL,L'CHQNARR                                                 
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,WORK                                                          
         STCM  R0,7,GTASUBST                                                    
         LA    R0,OVWORK+1         BUILD MESSAGE IN OVWORK+1                    
         STCM  R0,7,GTAOUT                                                      
         GOTO1 VGETTXT,(R1)                                                     
         LA    R1,DMCB                                                          
         IC    RF,GTMAXL                                                        
         BCTR  RF,0                                                             
         STC   RF,OVWORK           SET EXECUTE L'NARRATIVE IN OVWORK            
BLDNARX  B     OVROU1X                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* CALCULATE EXCHANGE RATE                                             *         
***********************************************************************         
                                                                                
CALCRAT  LR    R3,R1                                                            
         L     R2,0(R3)                                                         
         USING AFCAMNT,R2                                                       
         ZAP   PKWK16A,AFCAMNT         CURRENCY AMOUNT                          
         BZ    CALCRATL                                                         
         L     R2,4(R3)                                                         
         USING TRNAMNT,R2                                                       
         ZAP   DUB,TRNAMNT             AGENCY AMOUNT                            
         BZ    CALCRATL                                                         
         L     R2,8(R3)                                                         
         USING CURTABD,R2                                                       
         LA    RE,11                                                            
         XR    RF,RF                                                            
         IC    RF,CURTDECP                                                      
         DROP  R2                                                               
         SR    RE,RF                                                            
         SRP   PKWK16A,0(RE),0                                                  
         LA    RE,5                                                             
         XR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         SR    RE,RF                                                            
         SRP   DUB,0(RE),0                                                      
         BO    CALCRATH            ERROR IF SHIFT OVERFLOW                      
         ZAP   DUB2,DUB            TAKE DIVISOR                                 
         LA    R1,1                ESTABLISH 2+ NO. OF LEADING ZEROES           
         SRP   DUB2,1,0            SHIFT DIVISOR UNTIL OVERFLOW                 
         BO    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-14                                                             
         SRA   R1,1                DIVIDE BY TWO                                
         EX    R1,*+8                                                           
         BNZ   CALCRATH                                                         
         OC    PKWK16A(0),PKWK16A                                               
         DP    PKWK16A,DUB                                                      
         SRP   PKWK16A(8),64-1,5                                                
         LM    R0,R1,PKWK16A       FIRST 8 BYTES IS QUOTIENT                    
         SRDL  R0,4                                                             
         STM   R0,R1,PKWK16A       LOSE PACKED SIGN                             
         OC    PKWK16A(3),PKWK16A  ENSURE RATE WILL FIT                         
         BNZ   CALCRATH                                                         
         MVC   0(L'ATLXRATE,R3),PKWK16A+3                                       
CALCRATX B     OVROU1E                                                          
CALCRATL B     OVROU1L                                                          
CALCRATH B     OVROU1H                                                          
         EJECT                                                                  
***********************************************************************         
* MAXIMUM AND MINIUM EXCHANGE RATES                                   *         
* NTRY - R1 - A(CURRENCY TABLE ENTRY)                                 *         
* XIT  - CC HIGH - NO CURRENCY RECORD  EXRMIN - MIN  EXRMAX - MAX     *         
***********************************************************************         
                                                                                
MAXMIN   L     R3,0(R1)                                                         
         GOTO1 VBLDCUR,DMCB,(R3),(R3),ACOM                                      
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            RF=A(CURRENCY RECORD)                        
         LA    RF,GCFIRST(RF)                                                   
         USING GCREL,RF                                                         
         CLI   GCREL,GCRELQ        TEST CURRENCY ELEMENT                        
         BNE   MAXMINH                                                          
         MVC   EXRMIN,GCRMNEXC                                                  
         MVC   EXRMAX,GCRMXEXC                                                  
         DROP  RF                                                               
MAXMINX  B     OVROU1E                                                          
MAXMINH  B     OVROU1H                                                          
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* ROUTINE TO SELECT/DESELECT ALL TRANSACTIONS                         *         
***********************************************************************         
                                                                                
MRKALL   LA    R1,MRKOPTH          SET CURSOR TO OPTIONS FIELD                  
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
MRKALL02 STH   R1,HALF                                                          
         GOTO1 ATSARGET,HALF                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY ONLY TRANSACTION                
         BO    MRKALL12                                                         
         GOTO1 AFILTER             FILTER TRANSACTION                           
         BNE   MRKALL12            NOT REQUIRED - GET NEXT                      
         CLC   OPTALL,AC@YES       TEST IF USER IS MARKING                      
         BNE   MRKALL06                                                         
         TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BO    MRKALL12                                                         
         CP    TOTITEM,MAXINVS                                                  
         BL    MRKALL03                                                         
         MVC   FVMSGNO,=AL2(AE$MXINV)                                           
         MVC   FVXTRA,SPACES                                                    
         CURED MAXINVS,(((2*L'MAXINVS)-1),FVXTRA),0,ALIGN=LEFT                  
         B     OVROU1H             MAXIMUM ITEMS EXCEEDED                       
                                                                                
MRKALL03 AP    TOTITEM,PONE                                                     
         OI    TSARINDS,TSARMKQ                                                 
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
         AP    REPMRK,TSARAMNT     ADD TO MARKED THIS SESSION                   
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         AP    CTOMRK,TSARAFCA     ADD TO MARKED (AFC)                          
         SP    CTOBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         AP    CREMRK,TSARAFCA     ADD TO MARKED THIS SESSION (AFC)             
                                                                                
         AP    TOTEXDF,TMPEXDF     ADD EXCHANGE DIFFERENCE                      
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         SP    TOTMRK,TSARFDIS     MINUS DISCOUNT                               
         AP    TOTDSC,TSARFDIS     ADD TO DISCOUNT                              
         AP    TOTBAL,TSARFDIS     ADD TO BALANCE                               
         SP    REPMRK,TSARFDIS     MINUS DISCOUNT                               
         SP    CTOMRK,TSARFDIC     MINUS DISCOUNT (AFC)                         
         AP    CTODSC,TSARFDIC     ADD TO DISCOUNT (AFC)                        
         AP    CTOBAL,TSARFDIC     ADD TO BALANCE (AFC)                         
         SP    CREMRK,TSARFDIC     MINUS DISCOUNT (AFC)                         
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    MRKALL04                                                         
         USING SCUTOTS,R1                                                       
         L     R1,ASCUTOTS                                                      
         AP    STOMRK,TSARSCUA     ADD TO MARKED (2ND)                          
         SP    STOBAL,TSARSCUA     SUBTRACT FROM BALANCE (2ND)                  
         AP    SREMRK,TSARSCUA     ADD TO MARKED THIS SESSION (2ND)             
         SP    STOMRK,TSARSCUD     MINUS DISCOUNT (2ND)                         
         AP    STODSC,TSARSCUD     ADD TO DISCOUNT (2ND)                        
         AP    STOBAL,TSARSCUD     ADD TO BALANCE (2ND)                         
         SP    SREMRK,TSARSCUD     MINUS DISCOUNT (2ND)                         
         AP    STOEXDF,TMPEXDFS    ADD EXCHANGE DIFFERENCE (2ND)                
*&&                                                                             
MRKALL04 B     MRKALL10                                                         
                                                                                
MRKALL06 TM    TSARINDS,TSARMKQ    WAS RECORD ALREADY MARKED?                   
         BZ    MRKALL12                                                         
         SP    TOTITEM,PONE                                                     
         BNM   *+6                                                              
         DC    H'0'                CANNOT UNMARK MORE THAN YOU MARKED           
         NI    TSARINDS,FF-TSARMKQ                                              
                                                                                
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
         SP    REPMRK,TSARAMNT     MUST HAVE BEEN ADDED TO MARKED               
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         SP    TOTEXDF,TMPEXDF     REMOVE EXCHANGE DIFFERENCE                   
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         SP    CTOMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
         AP    CTOBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         SP    CREMRK,TSARAFCA     MUST HAVE BEEN ADDED TO MARKED (AFC)         
         AP    TOTMRK,TSARFDIS     PLUS DISCOUNT                                
         SP    TOTBAL,TSARFDIS     SUBTRACT FROM BALANCE                        
         AP    REPMRK,TSARFDIS     ADDED TO MARKED                              
         SP    TOTDSC,TSARFDIS     SUBTRACT FROM DISCOUNT                       
         AP    CTOMRK,TSARFDIC     PLUS DISCOUNT (AFC)                          
         SP    CTOBAL,TSARFDIC     SUBTRACT FROM BALANCE (AFC)                  
         AP    CREMRK,TSARFDIC     ADDED TO MARKED (AFC)                        
         SP    CTODSC,TSARFDIC     SUBTRACT FROM DISCOUNT (AFC)                 
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    MRKALL08                                                         
         L     R1,ASCUTOTS                                                      
         SP    STOMRK,TSARSCUA     SUBTRACT FROM MARKED (2ND)                   
         AP    STOBAL,TSARSCUA     ADD TO BALANCE (2ND)                         
         SP    SREMRK,TSARSCUA     MUST HAVE BEEN ADDED TO MARKED (2ND)         
         AP    STOMRK,TSARSCUD     PLUS DISCOUNT (2ND)                          
         SP    STOBAL,TSARSCUD     SUBTRACT FROM BALANCE (2ND)                  
         AP    SREMRK,TSARSCUD     ADDED TO MARKED (2ND)                        
         SP    STODSC,TSARSCUD     SUBTRACT FROM DISCOUNT (2ND)                 
         SP    STOEXDF,TMPEXDFS    REMOVE EXCHANGE DIFFERENCE (2ND)             
         DROP  R1                                                               
*&&                                                                             
MRKALL08 B     MRKALL10            THIS SESSION, SO SUBTRACT IT                 
                                                                                
MRKALL10 L     R1,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         MVI   TSACTN-TSARD(R1),TSAPUT                                          
         GOTO1 VTSAR,(R1)                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
                                                                                
MRKALL12 LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         CH    R1,DISMAX                                                        
         BNH   MRKALL02                                                         
         B     OVROU1E                                                          
         EJECT                                                                  
***********************************************************************         
* MAKE COSTING POSTINGS FROM OFFICE EXTRA TABLE                       *         
***********************************************************************         
                                                                                
C        USING ACTRECD,KEY                                                      
PSTCOS   MVI   COSTOPIK,0          ESTABLISH COSTING OFFPOS                     
         MVC   C.ACTKEY,SPACES     READ COSTING LEDGER                          
         MVC   C.ACTKCPY,COMPANY                                                
         MVI   C.ACTKUNT,C'1'                                                   
         MVI   C.ACTKLDG,C'C'                                                   
         MVC   WORK(L'ACTKULA),C.ACTKULA                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING LDGELD,R1                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTCOS02 IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         CLI   LDGEL,0             TEST EOR                                     
         BE    PSTCOSE1                                                         
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   PSTCOS02                                                         
         CLI   LDGOPOS,LDGOKEY     TEST OFFICE IN KEY                           
         BH    *+10                                                             
         MVC   COSTOPIK,LDGOPOS    SET OFFICE POSITION                          
                                                                                
         MVI   CINCOPIK,0          SET COSTING INCOME OFFICE POSITION           
         MVC   C.ACTKEY,SPACES     READ COSTING INCOME LEDGER                   
         MVC   C.ACTKCPY,COMPANY                                                
         MVI   C.ACTKUNT,C'1'                                                   
         MVI   C.ACTKLDG,C'2'                                                   
         MVC   WORK(L'ACTKULA),C.ACTKULA                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING LDGELD,R1                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTCOS04 IC    R0,LDGLN                                                         
         AR    R1,R0                                                            
         CLI   LDGEL,0             TEST EOR                                     
         BE    PSTCOSE1                                                         
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   PSTCOS04                                                         
         CLI   LDGOPOS,LDGOKEY     TEST OFFICE IN KEY                           
         BH    *+10                                                             
         MVC   CINCOPIK,LDGOPOS    SET OFFICE POSITION                          
                                                                                
         L     R2,AOFXTAB          R2=A(OFFICE EXTRA TABLE)                     
         USING OFFTABD,R2                                                       
         B     *+8                                                              
PSTCOS06 LA    R2,OFXTABL(R2)                                                   
         OC    OFFCAIN,OFFCAIN     TEST FURTHER ACCOUNT                         
         BZ    PSTCOS38                                                         
         TM    OFFIND1,OFFI1XPC    TEST KNOWN PRODUCTION CLIENT                 
         BZ    PSTCOS06                                                         
         CLI   OFFCOSG,C' '        TEST KNOWN COSTING GROUP                     
         BNH   PSTCOS06                                                         
         ZAP   OVDUB,OFFCDSC       ESTABLISH NET CASH DISCOUNT                  
         AP    OVDUB,OFFDTAX       ADD NEGATIVE TAX ADJUSTMENT                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTCOS08                                                         
         ZAP   OVDUB1,OFFCDSCS     ESTABLISH NET CASH DISCOUNT (2ND)            
         AP    OVDUB1,OFFDTAXS     ADD NEGATIVE TAX ADJUSTMENT (2ND)            
         BNZ   PSTCOS12            TEST SECONDARY CURRENCY NON-ZERO             
*&&                                                                             
PSTCOS08 CP    OVDUB,PZERO         TEST ZERO DISCOUNT POSTING                   
         BNE   PSTCOS12                                                         
                                                                                
         ZAP   OVDUB,OFFDIFF       YES - TRY NET DIFFERENCE                     
         AP    OVDUB,OFFDIFT       ADD NEGATIVE TAX ADJUSTMENT                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTCOS10                                                         
         ZAP   OVDUB1,OFFDIFFS     ESTABLISH NET DIFFERENCE (2ND)               
         AP    OVDUB1,OFFDIFTS     ADD NEGATIVE TAX ADJUSTMENT (2ND)            
         BNZ   PSTCOS12            TEST SECONDARY CURRENCY NON-ZERO             
*&&                                                                             
PSTCOS10 CP    OVDUB,PZERO         TEST ZERO DIFFERENCE POSTING                 
         BE    PSTCOS06            NO COSTING VALUE TO POST                     
                                                                                
PSTCOS12 MVC   C.ACTKEY,SPACES     READ COSTING CLIENT FOR NAME                 
         MVC   C.ACTKCPY,COMPANY                                                
         MVI   C.ACTKUNT,C'1'                                                   
         MVI   C.ACTKLDG,C'C'                                                   
         MVC   C.ACTKACT,OFFPPCA                                                
         MVC   WORK(L'ACTKULA),C.ACTKULA                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTCOS22 IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         CLI   NAMEL,0             TEST EOR                                     
         BE    PSTCOSE2                                                         
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   PSTCOS22                                                         
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    PSTCOSE2                                                         
         EX    R1,*+4                                                           
         MVC   COSCLIN(0),NAMEREC                                               
                                                                                
         MVC   C.ACTKEY,SPACES     READ INCOME COSTING GROUP                    
         MVC   C.ACTKCPY,COMPANY                                                
         MVI   C.ACTKUNT,C'1'                                                   
         MVI   C.ACTKLDG,C'2'                                                   
         MVC   C.ACTKACT(L'OFFCOSG),OFFCOSG                                     
         MVC   WORK(L'ACTKULA),C.ACTKULA                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTCOSE1            ERROR WILL FORCE ABEND                       
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTCOS28 IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         CLI   NAMEL,0             TEST EOR                                     
         BE    PSTCOSE2                                                         
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   PSTCOS28                                                         
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    PSTCOSE2                                                         
         EX    R1,*+4                                                           
         MVC   COSGRPN(0),NAMEREC                                               
         DROP  R1,C                                                             
*&&UK                                                                           
C        USING CURTABD,COMPCURT                                                 
*&&                                                                             
         L     R3,AIOBUFF2         R3=A(IOBUFF2 FOR TRANSACTION)                
         USING TRNRECD,R3                                                       
         LA    R4,TRNRFST          R4=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R4                                                        
PSTCOS32 MVC   POSTCPY,COMPANY     COSTING CLIENT                               
         MVI   POSTUNT,C'1'                                                     
         MVI   POSTLDG,C'C'                                                     
         MVC   POSTACT,OFFPPCA                                                  
         MVC   POSTCCPY,COMPANY    COSTING GROUP                                
         MVI   POSTCUNT,C'1'                                                    
         MVI   POSTCLDG,C'2'                                                    
         MVC   POSTCACT,SPACES                                                  
         MVC   POSTCACT(L'OFFCOSG),OFFCOSG                                      
         MVI   POSTSTAT,TRNSDR     DEBIT COSTING/INCOME                         
         MVC   POSTOFFC(1),OFFOFFC                                              
         MVI   POSTOFFC+1,C' '                                                  
         MVC   POSTCACN,COSGRPN    TO ACCOUNT NAME IS CONTRA NAME               
                                                                                
PSTCOS34 MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,POSTCULA                                                
         MVC   TRNKCULC,POSTCULC                                                
         MVC   TRNKDATE,CHQDATP                                                 
         MVC   TRNKREF,CHQNUM                                                   
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         MVC   TRNELD(L'OVTRNEL),OVTRNEL                                        
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVC   TRNSUB,TRNKSBR                                                   
         MVC   TRNSTAT,POSTSTAT                                                 
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         OI    TRNSTAT,TRNS2NDC    SET SECOND CURRENCY POSTING                  
*&&                                                                             
         ZAP   TRNAMNT,OVDUB       FOR OFXTAB NET AMOUNT                        
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+10                                                             
         AP    ADDCASH,TRNAMNT     ACCUMULATE TOTAL DEBITS                      
         MVC   TRNOFFC,POSTOFFC                                                 
         LA    RF,TRNELD+L'OVTRNEL RF=A(NEXT ELEMENT)                           
                                                                                
         USING TRSELD,RF                                                        
         MVC   TRSELD(L'OVTRSEL),OVTRSEL                                        
         LA    RF,L'OVTRSEL(RF)    RF=A(NEXT ELEMENT)                           
                                                                                
         USING TIDELD,RF                                                        
         MVC   TIDELD(L'OVTIDEL),OVTIDEL                                        
         LA    RF,L'OVTIDEL(RF)    RF=A(NEXT ELEMENT)                           
                                                                                
         USING PIDELD,RF                                                        
         MVC   PIDELD(L'OVPIDEL),OVPIDEL                                        
         LA    RF,L'OVPIDEL(RF)    RF=A(NEXT ELEMENT)                           
         DROP  RF                                                               
                                                                                
         MVI   0(RF),0             SET EOR                                      
         LA    RF,1(RF)                                                         
         SR    RF,R3                                                            
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTCOS36                                                         
         LA    R1,OVOCAL           CREATE OCAEL                                 
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRNAMNT   SECONDARY CURRENCY AMOUNT                    
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'TRNAMNT),OVDUB1                                       
         LA    R1,OCANTR1L+L'TRNAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAACNV',WORK),AIOBUFF2,ACOM,OVOCAL,0,0         
         MVI   OVOCAL,0            RESET A(OCA VALUES LIST)                     
         LA    R1,OVOCAL                                                        
         ST    R1,AOCANXT          RESET A(NEXT)                                
*&&                                                                             
PSTCOS36 GOTO1 AADDTRN,POSTCACN                                                 
         GOTO1 AREPTRN,AIOBUFF2             REPORT TRANSACTION POSTING          
                                                                                
         L     R3,AIOBUFF2         R3=A(TRANSACTION RECORD)                     
         XC    POSTCULA,POSTCULC   SWAP ACCOUNT WITH CONTRA                     
         XC    POSTCULC,POSTCULA                                                
         XC    POSTCULA,POSTCULC                                                
         CLI   POSTLDG,C'C'        TEST POST TO COSTING/INCOME                  
         BE    PSTCOS06                                                         
         MVI   POSTSTAT,0          CREDIT                                       
         MVC   POSTCACN,COSCLIN                                                 
         B     PSTCOS34            INCOME/COSTING POSTING                       
                                                                                
PSTCOS38 DS    0H                                                               
                                                                                
PSTCOSX  B     OVROU1X                                                          
                                                                                
PSTCOSE1 MVC   FVMSGNO,=AL2(AE$ACTNF)                                           
         B     *+10                                                             
PSTCOSE2 MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA,WORK                                                      
         B     OVROU1H                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* POST AT OFFICE LEVEL FROM OFFICE TABLE.                             *         
* IF CHEQUES ARE NOT SPLIT BY OFFICE, ONE SET OF BANK POSTINGS IS     *         
* MADE WITH AN OFFICE OF SPACES (U.K.) OR DEFAULT BANK OFFICE (U.S.)  *         
* IF INCOME IS NOT SPLIT BY OFFICE, ONE SET OF INCOME POSTINGS IS     *         
* MADE WITH AN OFFICE OF SPACES (U.K.) OR DEFAULT BANK OFFICE (U.S.)  *         
***********************************************************************         
                                                                                
PSTOFF   L     R2,AIOBUFF2         R2=A(IOBUFF2 FOR TRANSACTION)                
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES       BUILD TRANSACTION KEY                        
         MVC   TRNKCULC,ACCOUNT                                                 
         MVC   TRNKDATE,CHQDATP                                                 
         MVC   TRNKREF,CHQNUM                                                   
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         MVI   OVOCAL,0            SET NO OCA VALUES LIST                       
         LA    R1,OVOCAL                                                        
         ST    R1,AOCANXT          SAVE START POSITION IN LIST                  
                                                                                
         LA    R5,PSTTAB           LOOP THROUGH POSTING TABLE                   
         USING PSTTABD,R5                                                       
PSTOFF02 MVC   OVBYTE,PSTACT       TEST OFFICE LEVEL POSTING                    
         NI    OVBYTE,PSTLEVEL                                                  
         CLI   OVBYTE,PSTLOFF                                                   
         BNE   PSTOFF94                                                         
         IC    RF,PSTACT           TEST POSTING ACTION                          
         N     RF,=AL4(FF-PSTLEVEL)                                             
         EX    RF,*+8                                                           
         BZ    PSTOFF94                                                         
         TM    UPDIND1,0                                                        
         MVC   LARFADDR,PSTACC     EXTRACT ACCOUNT                              
         EX    0,LARF                                                           
         MVC   TRNKCULA,0(RF)                                                   
         MVC   LARFADDR,PSTCNTR    EXTRACT CONTRA                               
         EX    0,LARF                                                           
         MVC   TRNKCULC,0(RF)                                                   
                                                                                
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         LA    R4,TRNRFST          R4=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R4                                                        
         MVC   TRNELD(L'OVTRNEL),OVTRNEL                                        
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVI   TRNSUB,0                                                         
         LA    RF,L'OVTRNEL(R4)    RF=A(NEXT ELEMENT)                           
         TM    PSTIND2,PST2DNAR    TEST ADDING DIFFERENCE NARRATIVE             
         BZ    PSTOFF04                                                         
         LH    R1,=Y(SOVRWRK2-SAVED)                                            
         LA    R1,SAVED(R1)                                                     
         USING SOVRWRK2,R1                                                      
         OC    SDIFNARL,SDIFNARL   TEST ANY NARRATIVE TO ADD                    
         BZ    PSTOFF04                                                         
         SR    RF,RF                                                            
         IC    RF,SDIFNARL                                                      
         BCTR  RF,0                                                             
         EX    RF,*+4              APPEND DIFFERENCE NARRATIVE                  
         MVC   TRNNARR(0),SDIFNAR                                               
         DROP  R1                                                               
         LA    RF,TRNLN1Q+1(RF)    ADD L'FIXED PORTION +1                       
         STC   RF,TRNLN                                                         
         LA    RF,TRNELD(RF)       RF=A(NEXT ELEMENT)                           
                                                                                
         USING TRSELD,RF                                                        
PSTOFF04 MVC   TRSELD(TRSLNQ),OVTRSEL                                           
         XC    TRSUDAT,TRSUDAT     RESET USED DATE                              
         XC    TRSUMOS,TRSUMOS     RESET USED MOS                               
         LA    RF,TRSLNQ(RF)       RF=A(NEXT ELEMENT)                           
                                                                                
         USING TIDELD,RF                                                        
         MVC   TIDELD(TIDLNQ),OVTIDEL                                           
         LA    RF,TIDLNQ(RF)       RF=A(NEXT ELEMENT)                           
                                                                                
         USING PIDELD,RF                                                        
         OC    STWAPAS#,STWAPAS#   ATTACH A PERSON ID ELEMENT IF THE            
         BZ    *+14                USER LOGGED ON WITH A PASSWORD               
         MVC   PIDELD(PIDLNQ),OVPIDEL                                           
         LA    RF,PIDLNQ(RF)       RF=A(NEXT ELEMENT)                           
                                                                                
         XC    OSCIGLEV,OSCIGLEV   CLEAR SAVED ADDRESS                          
         TM    PSTELE,PSTEDSC      SCIEL FOR CHEQUE/DISC/DIFF TAX ADJ.          
         BZ    PSTOFF10                                                         
         USING SCIELD,RF           BUILD SUBSIDIARY CASH ELEMENT                
         XC    SCIELD(SCILN1Q),SCIELD                                           
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         TM    PSTACT,PSTDTAX      TEST DISCOUNT TAX ADJUSTMENT                 
         BO    PSTOFF06                                                         
         TM    CHQIND1,CHQISBNK    TEST BANK SPLIT BY OFFICE                    
         BZ    PSTOFF10                                                         
         L     R3,AOFFTAB          R3=A(OFFICE TABLE)                           
         OC    OFFTABL(OFFTABL,R3),OFFTABL(R3)                                  
         BZ    PSTOFF10            TEST AT LEAST TWO ENTRIES                    
         MVI   SCITYPE,SCITCHQT    TOTAL CHEQUE SCIEL                           
         ZAP   SCIAMNT,CHQAMT                                                   
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTOFF08                                                         
         USING OCANTRY,R1                                                       
         L     R1,AOCANXT          BUILD/AUGMENT OCA VALUES LIST                
         MVI   OCANTYPE,QSCITCHQT                                               
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'SCIAMNT),CHQAMS                                       
         LA    R1,OCANTR1L+L'SCIAMNT(R1)                                        
         MVI   OCANTRY,0           SET END OF LIST                              
         ST    R1,AOCANXT          SAVE A(NEXT)                                 
         DROP  R1                                                               
*&&                                                                             
         B     PSTOFF08                                                         
                                                                                
PSTOFF06 MVI   SCITYPE,SCITGLEV    GROSS LESS VAT                               
         ST    RF,OSCIGLEV         SAVE A(SCIEL) - AMOUNT FILLED LATER          
PSTOFF08 LA    RF,SCILN1Q(RF)      RF=A(NEXT ELEMENT)                           
                                                                                
PSTOFF10 DS    0H                                                               
*&&UK                                                                           
         XC    OSCIBCHA,OSCIBCHA   CLEAR SAVED ADDRESS                          
         TM    PSTELE2,PSTEBCH     TEST SCIEL FOR BANK CHARGE                   
         BZ    PSTOFF12                                                         
         USING SCIELD,RF           BUILD SUBSIDIARY CASH ELEMENT                
         XC    SCIELD(SCILN1Q),SCIELD                                           
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITBCHA    BANK CHARGE POSTING                          
         ST    RF,OSCIBCHA         SAVE A(SCIEL)                                
         LA    RF,SCILN1Q(RF)      RF=A(NEXT ELEMENT)                           
*&&                                                                             
PSTOFF12 TM    PSTELE,PSTESPA      TEST SPAEL NEEDED                            
         BZ    PSTOFF14                                                         
         CLC   FACTOR,SPACES       TEST FACTORING COMPANY PRESENT               
         BNH   PSTOFF14                                                         
         USING SPAELD,RF           ADD FACTORING ELEMENT TO RECORD              
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATFACC                                                 
         MVC   SPAAULA,FACTOR                                                   
         LA    RF,SPALNQ(RF)       RF=A(NEXT ELEMENT)                           
                                                                                
PSTOFF14 XC    OAFCEL,OAFCEL                                                    
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST DEALING IN CURRENCY                     
         BZ    PSTOFF16                                                         
         TM    PSTELE,PSTEAFC      TEST ADDING AFCEL                            
         BZ    PSTOFF16                                                         
         TM    PSTACT,PSTBCHA      ONLY AFCEL ON FOREIGN BANK CHARGES           
         BZ    *+12                                                             
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BZ    PSTOFF16                                                         
         USING AFCELD,RF           BUILD AFC ELEMENT                            
         XC    AFCEL(AFCLNQ),AFCEL                                              
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         OC    AFCCURR,FORECURT+(CURTCUR-CURTABD) COPY & TEST CUR CODE          
         BZ    PSTOFF16                                                         
         MVC   OVAFCXR,ATLXRATE    COPY AND TEST EXCHANGE RULE                  
         OC    AFCX,ATLX                                                        
         BZ    PSTOFF16                                                         
         NI    AFCXSTA2,FF-AFCXSMEM                                             
         TM    PSTIND1,PST1MEM     TEST MEMO TYPE AFC                           
         BZ    *+8                                                              
         OI    AFCXSTA2,AFCXSMEM   MEMO ONLY (NO ACCOUNTING VALUE)              
         ST    RF,OAFCEL           SAVE A(AFCEL)                                
         LA    RF,AFCLNQ(RF)       RF=A(NEXT ELEMENT)                           
                                                                                
PSTOFF16 TM    PSTELE,PSTEFFT      TEST ADDING FFTEL                            
         BZ    PSTOFF18                                                         
         USING FFTELD,RF                                                        
         LA    R1,FFTLN1Q+L'FFTDLEN+L'CURTCUR                                   
         EX    R1,*+4                                                           
         XC    FFTELD(0),FFTELD                                                 
         MVI   FFTEL,FFTELQ                                                     
         STC   R1,FFTLN                                                         
         MVI   FFTTYPE,FFTTACUR                                                 
         MVI   FFTDLEN,L'CURTCUR                                                
         MVC   FFTDATA(L'CURTCUR),FORECURT+(CURTCUR-CURTABD)                    
         AR    RF,R1               RF=A(NEXT ELEMENT)                           
*&&                                                                             
PSTOFF18 TM    PSTELE2,PSTETRXA+PSTETRXD                                        
         BZ    PSTOFF20                                                         
         USING TRXELD,RF                                                        
         XC    TRXELD(TRXLN1Q),TRXELD                                           
         MVI   TRXEL,TRXELQ                                                     
         MVI   TRXLN,TRXLN1Q                                                    
         MVI   TRXSTA3,TRXSADVC    SET TRANSACTION IS ADVANCE                   
         TM    PSTELE2,PSTETRXA                                                 
         BO    *+8                                                              
         MVI   TRXSTA3,TRXSDIFF    SET TRANSACTION IS DIFFERENCE                
         LA    RF,TRXLN1Q(RF)      RF=A(NEXT ELEMENT)                           
                                                                                
PSTOFF20 DS    0H                  ADD NEXT ELEMENT HERE                        
         DROP  RF                                                               
         MVI   0(RF),0             SET EOR                                      
         LA    RF,1(RF)                                                         
         SR    RF,R2                                                            
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
                                                                                
         MVC   POSTCACN,ACCNAME    DEFAULT CONTRA NAME IS VENDOR NAME           
                                                                                
         USING OFFTABD,R3                                                       
         L     R3,AOFFTAB          R3=A(OFFICE TABLE)                           
         TM    PSTACT,PSTBANK+PSTBCHA                                           
         BZ    PSTOFF22                                                         
         TM    CHQIND1,CHQISBNK    TEST POSTING TO BANK BY OFFICE               
         BO    PSTOFF26                                                         
         B     PSTOFF24                                                         
                                                                                
PSTOFF22 TM    PSTACT,PSTCDSC+PSTDTAX                                           
         BNZ   *+6                                                              
         DC    H'0'                UNKNOWN OFFICE LEVEL POSTING                 
         TM    CHQIND1,CHQISPAL    TEST POSTING TO P&L BY OFFICE                
         BO    PSTOFF26                                                         
                                                                                
PSTOFF24 L     R3,AOFFTABT         USE OFFICE TOTAL ENTRY                       
*&&UK                                                                           
         MVC   TRNOFFC,OFFICE      SET INVOICE OFFICE OR SPACES                 
         TM    COMPSTA4,CPYSICPY   TEST MAKING INTERCOMPANY POSTINGS            
         BZ    *+10                                                             
         MVC   TRNOFFC,BANKOF      SET BANK A/C OFFICE                          
*&&                                                                             
*&&US*&& MVC   TRNOFFC,BANKOF      SET BANK OFFICE OR SPACES                    
                                                                                
PSTOFF26 LA    RF,OFFTABD          LOCATE OFFICE AMOUNT                         
         AH    RF,PSTAMOF          SET POSTING AMOUNT                           
         ZAP   TRNAMNT,0(L'OFFCHEQ,RF)                                          
         XC    OVIAOCA,OVIAOCA                                                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTOFF30                                                         
         ZAP   OVIAOCA,PZERO                                                    
         LA    R1,OFFCHEQS         CHEQUE AMOUNT (SECONDARY)                    
         TM    PSTACT,PSTBANK      TEST BANK POSTING                            
         BO    PSTOFF28                                                         
         LA    R1,OFFCDSCS         DISCOUNT AMOUNT (SECONDARY)                  
         TM    PSTELE2,PSTETRXD    TEST DIFFERENCE POSTING                      
         BZ    *+8                                                              
         LA    R1,OFFDIFFS         DIFFERENCE AMOUNT (SECONDARY)                
         TM    PSTACT,PSTCDSC      TEST INCOME POSTING                          
         BO    PSTOFF28                                                         
         LA    R1,OFFDTAXS         DISCOUNT TAX ADJUSTMENT (SECONDARY)          
         TM    PSTELE2,PSTETRXD    TEST DIFFERENCE POSTING                      
         BZ    *+8                                                              
         LA    R1,OFFDIFTS         DIFFERENCE TAX ADJ (SECONDARY)               
PSTOFF28 ZAP   OVIAOCA,0(L'OFFCHEQS,R1)                                         
         BNZ   PSTOFF32            NON-ZERO 2ND CURRENCY ALWAYS POST            
*&&                                                                             
PSTOFF30 CP    TRNAMNT,PZERO                                                    
         BNE   PSTOFF32            NON-ZERO                                     
         CLI   AGYCTRY,CTRYGER     ZERO - UNLESS GERMANY                        
         BNE   PSTOFF78            NOTHING TO POST                              
         TM    PSTIND2,PST2ZERO    TEST ZERO POSTING ACCEPTABLE                 
         BZ    PSTOFF78            NOTHING TO POST                              
PSTOFF32 MVC   TRNSTAT,PSTSTAT                                                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTOFF36                                                         
         TM    PSTACT,PSTBCHA      UNLESS BANK CHARGES                          
         BNZ   *+8                                                              
         OI    TRNSTAT,TRNS2NDC    SET SECOND CURRENCY POSTING                  
*&&                                                                             
PSTOFF36 TM    PSTIND1,PST1SEI     TEST SWAP TYPE/SIGN FOR SE/SI                
         BZ    PSTOFF38                                                         
         CLI   TRNKLDG,C'E'                                                     
         BE    PSTOFF38                                                         
         XI    TRNSTAT,TRNSDR      SI/SQ - SWAP TYPE DR/CR                      
         MP    TRNAMNT,PONENEG     SI/SQ - SWAP SIGN +/-                        
         OC    OVIAOCA,OVIAOCA     TEST SECONDARY CURRENCY AMOUNT               
         BZ    PSTOFF38                                                         
         MP    OVIAOCA,PONENEG                                                  
                                                                                
PSTOFF38 TM    PSTIND1,PST1NEG     TEST NEGATE AMOUNT                           
         BZ    PSTOFF40                                                         
         MP    TRNAMNT,PONENEG                                                  
         OC    OVIAOCA,OVIAOCA     TEST SECONDARY CURRENCY AMOUNT               
         BZ    PSTOFF42                                                         
         MP    OVIAOCA,PONENEG                                                  
                                                                                
PSTOFF40 OC    OVIAOCA,OVIAOCA     TEST SECONDARY CURRENCY AMOUNT               
         BZ    PSTOFF42                                                         
*&&UK                                                                           
         USING OCANTRY,R1                                                       
         L     R1,AOCANXT          BUILD/AUGMENT OCA VALUES LIST                
         MVI   OCANTYPE,QTRNAMNT                                                
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'TRNAMNT),OVIAOCA                                      
         LA    R1,OCANTR1L+L'TRNAMNT(R1)                                        
         MVI   OCANTRY,0           SET END OF LIST                              
         ST    R1,AOCANXT          SAVE A(NEXT)                                 
         DROP  R1                                                               
*&&                                                                             
PSTOFF42 DS    0H                                                               
*&&UK                                                                           
         ICM   RF,15,OAFCEL        RF=A(AFCEL)                                  
         BZ    PSTOFF44                                                         
         USING AFCELD,RF                                                        
         MVC   AFCXRATE,OVAFCXR                                                 
         ZAP   DUB,TRNAMNT                                                      
         LA    RE,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RE                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,AFCCURR                                                 
         MVC   EURKRULE,AFCX                                                    
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RE                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,OAFCEL        RE & RF CORRUPTED BY GOTO1                   
         ZAP   AFCAMNT,DUB                                                      
         TM    PSTELE2,PSTEAFCC    TEST CALCULATE RATE                          
         BZ    PSTOFF44                                                         
         LA    RE,OFFTABD          LOCATE OFFICE AMOUNT                         
         AH    RE,PSTAMOF                                                       
         ZAP   AFCAMNT,L'OFFCHEQ(L'OFFCHEQC,RE)                                 
         GOTO1 ACALCRAT,DMCB,AFCAMNT,TRNAMNT,FORECURT                           
         BNE   PSTOFF44                                                         
         ICM   RF,15,OAFCEL        RE & RF CORRUPTED BY GOTO1                   
         MVC   AFCXRATE,0(R1)                                                   
         DROP  RF                                                               
*&&                                                                             
         USING SCIELD,RE                                                        
PSTOFF44 DS    0H                                                               
*&&UK                                                                           
         ICM   RE,15,OSCIBCHA      RE=A(SCIEL FOR BANK CHARGE)                  
         BZ    *+10                                                             
         ZAP   SCIAMNT,OFFBCHA                                                  
*&&                                                                             
         ICM   RE,15,OSCIGLEV      RE=A(SCIEL FOR DISC/DIFF TAX ADJ.)           
         BZ    PSTOFF58                                                         
         ZAP   SCIAMNT,OFFCDSC     UPDATE GROSS LESS TAX BUCKET                 
         AP    SCIAMNT,OFFDTAX     ADD NEGATIVE TAX FOR NET                     
         TM    PSTELE2,PSTETRXD    TEST DIFFERENCE POSTING                      
         BZ    PSTOFF46                                                         
         ZAP   SCIAMNT,OFFDIFF     UPDATE GROSS LESS TAX BUCKET                 
         AP    SCIAMNT,OFFDIFT     ADD NEGATIVE TAX FOR NET                     
                                                                                
PSTOFF46 SR    R1,R1                                                            
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT                                                
         BZ    PSTOFF48                                                         
         USING OCANTRY,R1                                                       
         L     R1,AOCANXT                                                       
         MVI   OCANTYPE,QSCITGLEV  GROSS LESS VAT                               
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'SCIAMNT),OFFCDSCS                                     
         AP    OCANCASH(L'SCIAMNT),OFFDTAXS                                     
         TM    PSTELE2,PSTETRXD    TEST DIFFERENCE POSTING                      
         BZ    PSTOFF48                                                         
         ZAP   OCANCASH(L'SCIAMNT),OFFDIFFS                                     
         AP    OCANCASH(L'SCIAMNT),OFFDIFTS                                     
*&&                                                                             
PSTOFF48 TM    OFFIND1,OFFI1OXO    TEST OFFICE EXTRA TABLE IN USE               
         BZ    PSTOFF56                                                         
         L     RF,AOFXTAB          SUBTRACT OFXTAB DISCOUNT/DIFFERENCE          
OFX      USING OFFTABD,RF                                                       
         LA    R0,OFXTABN                                                       
PSTOFF50 OC    OFX.OFFTAXA,OFX.OFFTAXA TEST FURTHER ACCOUNT                     
         BZ    PSTOFF56                                                         
         TM    CHQIND1,CHQISPAL    TEST POSTING TO P&L BY OFFICE                
         BZ    *+14                                                             
         CLC   OFX.OFFOFFC,OFFOFFC TEST OFFICE MATCHES                          
         BNE   PSTOFF54                                                         
         TM    PSTELE2,PSTETRXD    TEST DIFFERENCE POSTING                      
         BO    PSTOFF52                                                         
         SP    SCIAMNT,OFX.OFFCDSC ADJUST GROSS LESS TAX BUCKET                 
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   ADJUST SECONDARY CURRENCY?                   
         BZ    *+10                                                             
         SP    OCANCASH(L'SCIAMNT),OFX.OFFCDSCS                                 
*&&                                                                             
         B     PSTOFF54                                                         
PSTOFF52 SP    SCIAMNT,OFX.OFFDIFF ADJUST GROSS LESS TAX BUCKET                 
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   ADJUST SECONDARY CURRENCY?                   
         BZ    *+10                                                             
         SP    OCANCASH(L'SCIAMNT),OFX.OFFDIFFS                                 
*&&                                                                             
PSTOFF54 LA    RF,OFXTABL(RF)      NEXT OFXTAB ENTRY                            
         BCT   R0,PSTOFF50                                                      
         DROP  OFX                                                              
PSTOFF56 MP    SCIAMNT,PONENEG                                                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT                                                
         BZ    PSTOFF58                                                         
         MP    OCANCASH(L'SCIAMNT),PONENEG                                      
         LA    R1,OCANTR1L+L'SCIAMNT(R1)                                        
         MVI   OCANTRY,0           SET END OF LIST                              
         ST    R1,AOCANXT          SAVE A(NEXT)                                 
         DROP  R1                                                               
*&&                                                                             
         DROP  RE                                                               
                                                                                
PSTOFF58 TM    TRNSTAT,TRNSDR      ACCUMULATE TOTAL DEBITS                      
         BZ    *+10                                                             
         AP    ADDCASH,TRNAMNT                                                  
         C     R3,AOFFTABT         TEST USING OFFICE TOTALS                     
         BE    PSTOFF60                                                         
         TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFFICE EXTRA TABLE         
         BZ    *+12                                                             
         TM    CHQIND1,CHQISPAL    TEST POSTING TO P&L BY OFFICE                
         BZ    PSTOFF60                                                         
         MVC   TRNOFFC,OFFOFFC     POSTING BY OFFICE                            
PSTOFF60 MVC   TRNKOFF,SPACES      SET KEY OFFICE TO SPACES                     
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    PSTOFF62            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    PSTOFF62            YES - OFFICE NOT IN KEY                      
         MVC   TRNKOFF,TRNOFFC     SET OFFICE IN KEY                            
                                                                                
PSTOFF62 XR    R1,R1               TEST/SET OFFICE POSITION IN KEY              
         USING ACCXD,RF                                                         
         MVC   LARFADDR,PSTACC     LOAD OFFICE POSITION IN KEY                  
         EX    0,LARF                                                           
         CLI   ACCXOPIK,0                                                       
         BE    PSTOFF64                                                         
         IC    R1,ACCXOPIK                                                      
         LA    R1,TRNKACT-1(R1)                                                 
*&&US                                                                           
         CLC   TRNKULA(L'BANKUL),BANKACC+1                                      
         BE    *+10                                                             
*&&                                                                             
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         DROP  RF                                                               
                                                                                
PSTOFF64 TM    PSTIND1,PST1OPIK    TEST CONTRA OFFICES                          
         BZ    PSTOFF66                                                         
         XR    R1,R1               TEST/SET OFFICE POSITION IN KEY              
         USING ACCXD,RF                                                         
         MVC   LARFADDR,PSTCNTR    LOAD OFFICE POSITION IN KEY                  
         EX    0,LARF                                                           
         CLI   ACCXOPIK,0                                                       
         BE    PSTOFF66                                                         
         IC    R1,ACCXOPIK                                                      
         LA    R1,TRNKCACT-1(R1)                                                
*&&US                                                                           
         CLC   TRNKULC(L'BANKUL),BANKACC+1                                      
         BE    *+10                                                             
*&&                                                                             
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         DROP  RF                                                               
                                                                                
PSTOFF66 TM    PSTIND1,PST1SPA     TEST INSERTING SPAELS                        
         BZ    PSTOFF68                                                         
*&&UK*&& ZAP   TMPEXDF,OFFEXDF     NON-ZERO IF EXCHANGE DIFFERENCES             
*&&UK*&& ZAP   TMPEXDFS,OFFEXDFS                                                
         GOTO1 AADDSPA,PARM,(UPDIND1,TRNRECD),OFFTABD,TRNELD                    
         BNE   OVROU1X                                                          
                                                                                
PSTOFF68 TM    PSTELE2,PSTETRP     TEST TRANSACTION POSTING ELEMENTS            
         BZ    PSTOFF70                                                         
         GOTO1 AADDTRP,PARM,TRNRECD,OFFTABD,TRNELD                              
                                                                                
PSTOFF70 TM    PSTELE2,PSTEAPE     TEST CREATING APEEL                          
         BZ    PSTOFF72                                                         
         GOTO1 AADDAPE,PARM,TRNRECD,OFFTABD,TRNELD,PSTTABD                      
                                                                                
PSTOFF72 TM    PSTIND2,PST2DISC    TEST MAIN DISCOUNT POSTING                   
         BZ    PSTOFF74                                                         
         GOTO1 AADDSCG,PARM,TRNRECD,OFFTABD                                     
                                                                                
PSTOFF74 DS    0H                                                               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTOFF76                                                         
         CLI   OVOCAL,0            TEST ANY OCA VALUES TO ADD/UPDATE            
         BE    PSTOFF76                                                         
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAACNV',WORK),AIOBUFF2,ACOM,OVOCAL,0,0         
         MVI   OVOCAL,0            RESET A(OCA VALUES LIST)                     
         LA    R1,OVOCAL                                                        
         ST    R1,AOCANXT          RESET A(NEXT)                                
*&&                                                                             
                                                                                
PSTOFF76 GOTO1 AADDTRN,POSTCACN                                                 
         GOTO1 AREPTRN,AIOBUFF2    REPORT TRANSACTION POSTING                   
                                                                                
PSTOFF78 TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFFICE EXTRA TABLE         
         BO    PSTOFF82                                                         
         C     R3,AOFFTABT         TEST OFFICE TABLE MERGED                     
         BE    PSTOFF80                                                         
         LA    R3,OFFTABL(R3)      R3=A(NEXT OFFICE ENTRY)                      
         CLI   OFFTABD,EOT         TEST EOT                                     
         BE    PSTOFF80                                                         
         OC    OFFOFFC,OFFOFFC     TEST ANY FURTHER OFFICE                      
         BZ    PSTOFF80                                                         
         B     PSTOFF26                                                         
                                                                                
PSTOFF80 TM    PSTACT,PSTDTAX+PSTCDSC  TEST DISCOUNT/TAX ADJUSTMENT             
         BZ    PSTOFF94                                                         
         L     R3,AOFXTAB          PROCESS OFFICE EXTRA TABLE                   
         B     *+8                                                              
PSTOFF82 LA    R3,OFXTABL(R3)      NEXT OFXTAB ENTRY                            
         OC    OFFTABD(OFXTABL),OFFTABD                                         
         BZ    PSTOFF94            NO FURTHER ENTRY                             
         TM    PSTIND2,PST2DISC    TEST DISCOUNT                                
         BZ    *+12                                                             
         TM    OFFIND1,OFFI1XPC    TEST PRODUCTION CONTRA A/C                   
         BZ    PSTOFF82                                                         
         MVC   LARFADDR,PSTACC     (RE)SET C/U/L/ACCOUNT                        
         EX    0,LARF                                                           
         MVC   TRNKCULA,0(RF)                                                   
         MVC   LARFADDR,PSTCNTR    (RE)SET C/U/L/CONTRA ACCOUNT                 
         EX    0,LARF                                                           
         MVC   TRNKCULC,0(RF)                                                   
         TM    PSTIND2,PST2DTAT    TEST POSTING TO TAX LEDGER                   
         BZ    PSTOFF84                                                         
         MVC   TRNKCPY,COMPANY     SET COMPANY                                  
         MVC   TRNKULA,OFFTAXA     SET TAX U/L/ACCOUNT                          
         B     PSTOFF26                                                         
PSTOFF84 TM    OFFIND1,OFFI1XPC    TEST PRODUCTION CONTRA A/C                   
         BZ    PSTOFF26                                                         
         MVC   TRNKULC,OFFCAIN     SET CONTRA U/L/ACCOUNT                       
K        USING ACTRECD,KEY                                                      
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCULA,TRNKCULA READ THIS ACCOUNT                            
         MVC   WORK(L'ACTKULA),K.ACTKULA                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTOFFE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTOFFE1            ERROR WILL FORCE ABEND                       
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         SR    RF,RF               RF=A(PPREL) OR 0                             
         SR    R0,R0                                                            
         B     *+10                                                             
PSTOFF86 IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         CLI   NAMEL,0             TEST EOR                                     
         BE    PSTOFFE2                                                         
         CLI   NAMEL,PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BNE   *+10                                                             
         LR    RF,R1               RF=A(PPREL)                                  
         B     PSTOFF86                                                         
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   PSTOFF86                                                         
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    RE,*+4                                                           
         MVC   POSTCACN(0),RECNAME SET CONTRA ACCOUNT NAME                      
         DROP  R1                                                               
         LTR   RF,RF               TEST A(PPREL)                                
         BNZ   PSTOFF90                                                         
         MVC   K.ACTKEY,SPACES     IF NO PRODUCT PPREL                          
         SR    R1,R1                                                            
         IC    R1,PRODALEN         READ CLIENT FOR PPREL                        
         LA    R1,(TRNKCACT-TRNKCULC)-1(R1)                                     
         EX    R1,*+4                                                           
         MVC   K.ACTKCULA(0),TRNKCULC                                           
         MVC   WORK(L'ACTKULA),K.ACTKULA                                        
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTOFFE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTOFFE1            ERROR WILL FORCE ABEND                       
         L     RF,AIOBUFF                                                       
         LA    RF,ACTRFST-ACTRECD(RF)                                           
         USING PPRELD,RF                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTOFF88 IC    R0,PPRLN                                                         
         AR    RF,R0                                                            
         CLI   PPREL,0             TEST EOR                                     
         BE    PSTOFFE2            ERROR IF NO PPREL                            
         CLI   PPREL,PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BNE   PSTOFF88                                                         
PSTOFF90 MVC   OFFPPCA,PPRCOSTA    EXTRACT COSTING ACCOUNT                      
         DROP  RF                                                               
         MVC   WORK,SPACES                                                      
         MVC   OFFCOSG,CDSCCOST    DEFAULT DISCOUNT A/C COSTING BYTE            
         SR    R1,R1                                                            
         ICM   R1,1,CDSCOPIK       TEST OFFICE IN KEY                           
         BZ    PSTOFF26                                                         
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCULA,CDSCACC  READ INCOME ACCOUNT FOR COSTING BYTE         
         LA    R1,K.ACTKACT-1(R1)                                               
         MVC   0(1,R1),OFFOFFC                                                  
         MVC   WORK(L'ACTKULA),K.ACTKULA                                        
         DROP  K                                                                
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO1Q                                     
         BNE   PSTOFFE1            ERROR WILL FORCE ABEND                       
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         BNE   PSTOFFE1            ERROR WILL FORCE ABEND                       
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING RSTELD,R1                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTOFF92 IC    R0,RSTLN                                                         
         AR    R1,R0                                                            
         CLI   RSTEL,0             TEST EOR                                     
         BE    PSTOFFE2            ERROR WILL ABEND                             
         CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BNE   PSTOFF92                                                         
         MVC   OFFCOSG,RSTCOSTG                                                 
         B     PSTOFF26                                                         
         DROP  R1                                                               
                                                                                
PSTOFF94 LA    R5,PSTTABL(R5)      NEXT POSTING TABLE ENTRY                     
         CLI   PSTACT,EOT          TEST EOT                                     
         BNE   PSTOFF02            PROCESS NEXT POSTING                         
         B     OVROU1E                                                          
                                                                                
PSTOFFE1 MVC   FVMSGNO,=AL2(AE$ACTNF)                                           
         B     *+10                                                             
PSTOFFE2 MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA(L'ACTKULA),WORK                                           
         B     OVROU1H                                                          
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
***********************************************************************         
* POST AT INVOICE LEVEL                                               *         
* NTRY - R1=A(IOBUFF CONTAINING CREDIT (WHICH MAY NOT BE IOBUFF2))    *         
*        SETELAD HAS SET A(CREDIT TRANSACTION ELEMENTS)               *         
***********************************************************************         
                                                                                
PSTINV   L     R3,0(R1)            R3=A(CREDIT TRANSACTION)                     
         MVC   OVCNTRA,TRNKCULC-TRNKEY(R3)                                      
         L     R2,AIOBUFF2                                                      
         USING TRNRECD,R2                                                       
         LA    R5,PSTTAB                                                        
         USING PSTTABD,R5                                                       
                                                                                
         XC    EXDFSJ,EXDFSJ       CLEAR SJ EXCH DIFF ACCOUNT                   
         TM    COMPSTA9,CPYSXDSJ   TEST POST EXCH DIFF TO JOB                   
         BZ    *+10                                                             
         MVC   EXDF,EXDFACC        RESTORE DEFAULT EXCH DIFF A/C                
                                                                                
         XC    CPJWORK,CPJWORK                                                  
         ICM   RF,15,ASOREL        EXTRACT SOURCE FROM SOREL                    
         BZ    PSTINV02                                                         
         USING SORELD,RF                                                        
         MVC   CPJACT,SORAULA                                                   
         CLI   SORLN,SORAL2Q                                                    
         BL    *+10                                                             
         MVC   CPJWORK,SORAWRK                                                  
         B     PSTINV04                                                         
                                                                                
PSTINV02 ICM   RF,15,ACPJEL        ELSE TRY CPJEL                               
         BNZ   *+14                                                             
         MVC   CPJACT,OVCNTRA+(TRNKULA-TRNKCULA)  DEFAULT                       
         B     PSTINV04                                                         
         USING CPJEL,RF                                                         
         MVC   CPJACT,SPACES                                                    
         MVI   CPJACT,C'S'         UNIT                                         
         MVC   CPJACT+1(1),CPJTYPE LEDGER                                       
         CLI   CPJTYPE,CPJTOTH     TEST 'OTHER' SOURCE                          
         BNE   *+14                                                             
         MVC   CPJACT,CPJOULA                                                   
         B     PSTINV04                                                         
         CLI   CPJTYPE,CPJTEXP     TEST EXPENSE SOURCE                          
         BNE   *+14                                                             
         MVC   CPJACT+L'ACTKUNT+L'ACTKLDG(L'CPJEXP),CPJEXP                      
         B     PSTINV04                                                         
         MVC   CPJACT+L'ACTKUNT+L'ACTKLDG(L'CPJCLI),CPJCLI                      
         LA    RE,CPJACT+(TRNKACT-TRNKULA)+L'CPJCLI-1                           
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         BCT   RE,*-8                                                           
         MVC   1(2,RE),CPJPRO                                                   
         MVC   3(L'CPJJOB,RE),CPJJOB                                            
         CLI   CPJLN,CPJCPJLQ                                                   
         BNH   PSTINV04                                                         
         MVC   CPJWORK,CPJWRK                                                   
         DROP  RF                                                               
                                                                                
PSTINV04 LTR   RF,RF               TEST SOURCE ELEMENT FOUND                    
         BZ    PSTINV06                                                         
         TM    COMPSTA9,CPYSXDSJ                                                
         BZ    PSTINV06                                                         
         OC    CPJACT,CPJACT                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   PRODUL,CPJACT       TEST SJ SOURCE                               
         BNE   PSTINV06            ONLY OVERRIDE IF SJ SOURCE                   
         MVC   EXDF(L'COMPANY),COMPANY                                          
         MVC   EXDF+L'COMPANY(L'CPJACT),CPJACT                                  
                                                                                
PSTINV06 MVC   TRNKEY,0(R3)        SET CONSTANTS                                
         XC    TRNRSTA,TRNRSTA                                                  
         XC    TRNRLNK,TRNRLNK                                                  
         LA    R4,TRNRFST                                                       
         USING TRNELD,R4                                                        
         XR    RF,RF                                                            
         OC    TSARDADR,TSARDADR   TEST NEW INVOICE NOT ON FILE                 
         BZ    PSTINV08                                                         
         IC    RF,TRNKSBR                                                       
         LA    RF,1(RF)            BUMP KEY SUB-REFERENCE BY ONE                
         CH    RF,=H'256'                                                       
         BL    *+6                                                              
         DC    H'0'                MAXIMUM KEY SUB-REFERENCE REACHED            
         STC   RF,TRNKSBR                                                       
         MVC   TRNELD(L'OVTRNEL),OVTRNEL                                        
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         STC   RF,TRNSUB                                                        
         ICM   RF,15,ATRNEL                                                     
         MVC   TRNOFFC,TRNOFFC-TRNELD(RF)                                       
         B     PSTINV10                                                         
                                                                                
PSTINV08 ICM   R1,15,ATRNEL        R1=A(NEW TRNEL)                              
         IC    RF,TRNLN-TRNELD(R1)                                              
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TRNELD(0),0(R1)                                                  
                                                                                
PSTINV10 CLI   PSTACT,EOT          LOOP FOR EACH TABLE ENTRY                    
         BE    PSTINVX                                                          
         MVC   OVBYTE,PSTACT       TEST INVOICE LEVEL POSTING                   
         NI    OVBYTE,PSTLEVEL                                                  
         CLI   OVBYTE,PSTLINV                                                   
         BNE   PSTINV70                                                         
         TM    PSTIND2,PST2INEW    TEST NEW INVOICE POSTINGS                    
         BZ    PSTINV12                                                         
         OC    TSARDADR,TSARDADR   YES - DON'T WANT REAL INVOICE                
         BZ    PSTINV14                                                         
         B     PSTINV70                                                         
PSTINV12 OC    TSARDADR,TSARDADR   NO - DON'T WANT NEW INVOICE                  
         BZ    PSTINV70                                                         
                                                                                
PSTINV14 LA    R4,TRNRFST                                                       
                                                                                
         MVC   POSTCACN,SPACES     PRESUME NO CONTRA NAME TO UPDATE             
                                                                                
         MVC   LARFADDR,PSTACC     EXTRACT ACCOUNT                              
         EX    0,LARF                                                           
         MVC   TRNKCULA,0(RF)                                                   
         MVC   LARFADDR,PSTCNTR    EXTRACT CONTRA                               
         EX    0,LARF                                                           
         MVC   TRNKCULC,0(RF)                                                   
         XC    OVIAOCA,OVIAOCA     CLEAR OCAEL AMOUNT                           
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTINV16                                                         
         TM    PSTACT,PSTSUPP      TEST VENDOR POSTING                          
         BZ    PSTINV15                                                         
         ZAP   OVIAOCA,TSARSCUA    TAKE AMOUNT FOR OCAEL                        
         AP    OVIAOCA,TMPEXDFS    ADJUST FOR EXCHANGE DIFFERENCE               
         B     PSTINV16                                                         
PSTINV15 TM    PSTACT,PSTEXDF      TEST EXCHANGE DIFFERENCE POSTING             
         BZ    PSTINV16                                                         
         ZAP   OVIAOCA,TMPEXDFS    TAKE EXCHANGE DIFFERENCE FOR OCAEL           
*&&                                                                             
PSTINV16 MVC   LARFADDR,PSTAMNT    GET AMOUNT FOR TRANSACTION                   
         EX    0,LARF                                                           
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO AMOUNT IN TABLE                           
         AH    RF,PSTAMOF                                                       
         ZAP   TRNAMNT,0(L'TRNAMNT,RF)                                          
         BNZ   PSTINV18                                                         
         OC    OVIAOCA,OVIAOCA     TEST OCAEL AMOUNT SET                        
         BZ    PSTINV70            NOTHING TO POST                              
         CP    OVIAOCA,PZERO       TEST OCAEL AMOUNT ZERO                       
         BE    PSTINV70            NOTHING TO POST                              
                                                                                
PSTINV18 TM    PSTACT,PSTEXDF      TEST EXCHANGE DIFFERENCE POSTING             
         BZ    PSTINV24                                                         
         CLC   TRNKCULA,EXDF       TEST POSTING TO EXCH DIFF A/C                
         BNE   PSTINV24                                                         
         CLC   PRODUL,TRNKUNT      TEST PRODUCTION ACCOUNT                      
         BNE   PSTINV20                                                         
         CLC   TRNKCULA,EXDFSJ     TEST ALREADY READ THIS ACCOUNT               
         BE    PSTINV20                                                         
                                                                                
         L     R0,AOVIOS1          SAVE CR INVOICE TRANSACTION                  
         L     RE,AIOSAVE1                                                      
         LA    R1,IOALQ                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R0,AOVIOS2          SAVE DR CHEQUE TRANSACTION                   
         L     RE,AIOSAVE2                                                      
         LA    R1,IOALQ                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
K        USING ACTRECD,KEY                                                      
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCULA,TRNKCULA                                              
         MVI   GETIND,GETIABLQ+GETINLOK+GETINCLO+GETINOSV                       
         GOTO1 AGETACC,0                                                        
         BNE   PSTINVER            ABEND - NO NEED TO RESTORE IOAS              
         MVC   EXDFSJ,K.ACTKULA    SAVE SJ EXCH DIFF A/C AFTER GETACC           
                                                                                
         L     R0,AIOSAVE1         RESTORE CR INVOICE TRANSACTION               
         L     RE,AOVIOS1                                                       
         LA    R1,IOALQ                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R0,AIOSAVE2         RESTORE DR CHEQUE TRANSACTION                
         L     RE,AOVIOS2                                                       
         LA    R1,IOALQ                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
         USING ACTRECD,R1                                                       
PSTINV20 CLC   TRNKCULC,SUPPACC    TEST CONTRA A/C IS SUPPLIER                  
         BNE   *+14                                                             
         MVC   POSTCACN,ACCNAME    JUST NEED THE NAME                           
         B     PSTINV24                                                         
         MVC   K.ACTKEY,SPACES                                                  
         MVC   K.ACTKCULA,TRNKCULC                                              
         MVC   FVXTRA(L'TRNKULC),TRNKULC                                        
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         GOTO1 AIOEXEC,IOREAD+IOACCDIR+IO3Q                                     
         BNE   PSTINVER                                                         
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO3Q                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOBUFF3         ACCOUNT IN IO3                               
         LA    RF,ACTRFST                                                       
         USING NAMELD,RF                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
PSTINV22 IC    R0,NAMLN                                                         
         AR    RF,R0                                                            
         CLI   NAMEL,0                                                          
         BE    PSTINVER                                                         
         CLI   NAMEL,NAMELQ                                                     
         BNE   PSTINV22                                                         
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         BM    PSTINVER                                                         
         EX    RE,*+4                                                           
         MVC   POSTCACN(0),NAMEREC                                              
                                                                                
         DROP  R1,RF,K                                                          
                                                                                
PSTINV24 MVC   TRNSTAT,PSTSTAT     STATUS FROM TABLE                            
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         OI    TRNSTAT,TRNS2NDC    SET SECOND CURRENCY POSTING                  
*&&                                                                             
         TM    PSTIND1,PST1NEG     TEST AMOUNT NEGATED                          
         BZ    PSTINV26                                                         
         MP    TRNAMNT,PONENEG                                                  
         OC    OVIAOCA,OVIAOCA                                                  
         BZ    PSTINV26                                                         
         MP    OVIAOCA,PONENEG                                                  
PSTINV26 NI    TRNBLOCK+(TRNINDS-TRNBLK),FF-(TRNIDUCN)                          
         TM    PSTIND1,PST1DUCN    TEST DON'T UPDATE CONTRA NAMES               
         BZ    *+8                                                              
         OI    TRNBLOCK+(TRNINDS-TRNBLK),TRNIDUCN                               
         TM    PSTIND1,PST1CSRF    TEST CLEAR SUB REFERENCE                     
         BZ    *+12                                                             
         MVI   TRNKSBR,0                                                        
         MVI   TRNSUB,0                                                         
         TM    PSTIND1,PST1SEI     TEST SWAP TYPE/SIGN FOR SE/SI                
         BZ    PSTINV30                                                         
         CLI   TRNKLDG,C'E'        TEST EXPENSE                                 
         BE    PSTINV30                                                         
         CLI   TRNKLDG,C'J'        TEST JOB (EXCHDIFF=SJ)                       
         BNE   PSTINV28                                                         
         MVC   TRNKWORK,CPJWORK    SET WORK CODE                                
         MVC   TRNOFFC,CPJWORK                                                  
         B     PSTINV32                                                         
                                                                                
PSTINV28 XI    TRNSTAT,TRNSDR      SI/SQ - SWAP TYPE DR/CR                      
         MP    TRNAMNT,PONENEG     SI/SQ - SWAP SIGN +/-                        
                                                                                
         USING ACCXD,RF                                                         
PSTINV30 MVC   LARFADDR,PSTACC     GET OFFICE POSITION FOR ACCOUNT              
         EX    0,LARF                                                           
         CLI   ACCXOPIK,0                                                       
         BE    PSTINV32                                                         
         XR    R1,R1                                                            
         IC    R1,ACCXOPIK                                                      
         LA    R1,TRNKACT-1(R1)                                                 
*&&US                                                                           
         CLC   TRNKULA(L'BANKUL),BANKACC+1                                      
         BE    *+10                                                             
*&&                                                                             
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         DROP  RF                                                               
                                                                                
PSTINV32 TM    TRNSTAT,TRNSDR                                                   
         BZ    *+10                                                             
         AP    ADDCASH,TRNAMNT     ACCUMULATE TOTAL DEBITS                      
                                                                                
         XR    R1,R1                                                            
         IC    R1,TRNLN            TRNLN1Q / VARIABLE IF PST1INEW               
         XR    RF,RF                                                            
         TM    PSTIND1,PST1NAR     TEST ADDING CHEQUE NARRATIVE                 
         BZ    PSTINV34                                                         
         IC    RF,CHQNARRL                                                      
         EX    RF,*+4                                                           
         MVC   TRNNARR(0),CHQNARR                                               
         LA    RF,1(RF)                                                         
PSTINV34 AR    RF,R1                                                            
         STC   RF,TRNLN                                                         
         DROP  R4                                                               
                                                                                
         AR    R4,RF               R4=A(NEXT ELEMENT)                           
         USING TRSELD,R4           COPY TRANSACTION STATUS ELEMENT              
         OC    TSARDADR,TSARDADR                                                
         BZ    PSTINV36                                                         
         MVC   TRSELD(L'OVTRSEL),OVTRSEL                                        
         CLI   TRNKLDG,C'J'        TEST JOB (EXCHDIFF=SJ)                       
         BNE   *+16                                                             
         XC    TRSUDAT,TRSUDAT                                                  
         XC    TRSUMOS,TRSUMOS                                                  
         LA    R4,L'OVTRSEL(R4)    R4=A(NEXT ELEMENT)                           
         B     PSTINV38                                                         
                                                                                
PSTINV36 ICM   R1,15,ATRSEL        R1=A(NEW TRSEL)                              
         XR    RF,RF                                                            
         IC    RF,TRSLN-TRSELD(R1)                                              
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TRSELD(0),0(R1)                                                  
         LA    R4,1(RF,R4)                                                      
                                                                                
         USING TIDELD,R4                                                        
PSTINV38 MVC   TIDELD(L'OVTIDEL),OVTIDEL                                        
         LA    R4,L'OVTIDEL(R4)    R4=A(NEXT ELEMENT)                           
                                                                                
         OC    STWAPAS#,STWAPAS#   ATTACH A PERSON ID ELEMENT IF THE            
         BZ    *+14                USER LOGGED ON WITH A PASSWORD               
         USING PIDELD,R4                                                        
         MVC   PIDELD(L'OVPIDEL),OVPIDEL                                        
         LA    R4,L'OVPIDEL(R4)    R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV40 TM    PSTELE,PSTEMPY      TEST ADDING MPYEL                            
         BZ    PSTINV42                                                         
         USING MPYELD,R4           BUILD MANUAL PAYMENT ELEMENT                 
         ICM   RF,15,AMPYEL        RF=A(CR MANUAL PAYMENT ELEMENT)              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MPYELD(MPYLN2Q),0(RF)   COPY ORIGINAL MPYEL                      
         MVI   MPYLN,MPYLN2Q                                                    
         MVC   MPYSUB,TRNKSBR-TRNRECD(R3)                                       
         LA    R4,MPYLN2Q(R4)      R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV42 TM    PSTELE,PSTESPA      TEST ADDING SPAEL                            
         BZ    PSTINV44                                                         
         CLC   FACTOR,SPACES       TEST FACTORING COMPANY PRESENT               
         BNH   PSTINV44                                                         
         USING SPAELD,R4           COPY FACTORING ELEMENT TO RECORD             
         MVC   SPAELD(L'OVSPAEL),OVSPAEL                                        
         LA    R4,L'OVSPAEL(R4)    R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV44 TM    PSTELE,PSTEAFC      TEST ADDING AFCEL                            
         BZ    PSTINV48                                                         
*&&UK                                                                           
         ICM   RF,15,AAFCEL        RF=A(CREDIT AFC ELEMENT) OR 0                
         BZ    PSTINV48                                                         
         USING AFCELD,R4           BUILD AFC ELEMENT                            
         MVC   AFCELD(AFCLNQ),0(RF)                                             
         TM    PSTIND1,PST1NEG     TEST AMOUNT NEGATED                          
         BZ    *+10                                                             
         MP    AFCAMNT,PONENEG     NEGATE CURRENCY AMOUNT TOO                   
         GOTO1 ACALCRAT,DMCB,AFCAMNT,TRNRFST+(TRNAMNT-TRNELD),FORECURT          
         BNE   PSTINV48                                                         
         MVC   AFCXRATE,0(R1)                                                   
         TM    SAFCIND1,SAFCI1SC   TEST ALWAYS MEMO                             
         BZ    *+12                                                             
         TM    PSTIND1,PST1MEM     TEST MEMO TYPE AFC                           
         BZ    PSTINV46                                                         
         USING TRNELD,RE                                                        
         LA    RE,TRNRFST          A(FIRST ELEMENT) - TRANS ELEMENT             
         ZAP   DUB,TRNAMNT                                                      
         LA    RE,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RE                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,AFCCURR                                                 
         MVC   EURKRULE,AFCX                                                    
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RE                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   AFCAMNT,DUB                                                      
         OI    AFCXSTA2,AFCXSMEM   MEMO ONLY (NO ACCOUNTING VALUE)              
         DROP  R4                                                               
PSTINV46 LA    R4,AFCLNQ(R4)       R4=A(NEXT ELEMENT)                           
*&&                                                                             
PSTINV48 TM    PSTELE,PSTEDUE      TEST ADDING DUEEL                            
         BZ    PSTINV50                                                         
         USING DUEELD,R4           BUILD DUE DATE ELEMENT                       
         ICM   RF,15,ADUEEL        RF=A(CREDIT DUE DATE ELEMENT) OR 0           
         BZ    PSTINV50                                                         
         XR    R1,R1                                                            
         ICM   R1,1,DUELN-DUEELD(RF)                                            
         BZ    PSTINV50                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DUEELD(0),0(RF)                                                  
         DROP  R4                                                               
         LA    R4,1(R1,R4)         R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV50 DS    0H                                                               
*&&UK                                                                           
         TM    PSTELE,PSTEFFT      TEST ADDING FFTEL                            
         BZ    PSTINV52                                                         
         USING FFTELD,R4                                                        
         LA    R1,FFTLN1Q+L'FFTDLEN+L'CURTCUR                                   
         EX    R1,*+4                                                           
         XC    FFTELD(0),FFTELD                                                 
         MVI   FFTEL,FFTELQ                                                     
         STC   R1,FFTLN                                                         
         MVI   FFTTYPE,FFTTACUR                                                 
         MVI   FFTDLEN,L'CURTCUR                                                
         MVC   FFTDATA(L'CURTCUR),FORECURT+(CURTCUR-CURTABD)                    
         DROP  R4                                                               
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
*&&                                                                             
PSTINV52 TM    PSTELE2,PSTEFFTK    TEST ADDING FFTEL FOR KEY REFERENCE          
         BZ    PSTINV54                                                         
         USING FFTELD,R4                                                        
         LA    R1,FFTLN1Q+L'FFTDLEN+L'TRNKREF                                   
         EX    R1,*+4                                                           
         XC    FFTELD(0),FFTELD                                                 
         MVI   FFTEL,FFTELQ                                                     
         STC   R1,FFTLN                                                         
         MVI   FFTTYPE,FFTTKREF                                                 
         MVI   FFTDLEN,L'TRNKREF                                                
         MVC   FFTDATA(L'TRNKREF),TRNKREF                                       
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
                                                                                
         CLC   TSARAAES,SPACES                                                  
         BNH   PSTINV53                                                         
         XC    FFTEL(12),FFTEL                                                  
         MVI   FFTEL,FFTELQ              X'DB'                                  
         MVI   FFTTYPE,FFTTESTN          ESTIMATE NUMBER (122)                  
         MVI   FFTSEQ,0                                                         
         MVC   FFTCESTN,SPACES     INIT AS SPACES                               
         MVC   FFTOESTN,SPACES     INIT AS SPACES                               
         MVC   FFTOESTN(3),TSARAAES                                             
         MVI   FFTDLEN,12                                                       
         LA    R1,FFTESLNQ               SET LENGTH                             
         STC   R1,FFTLN                  LENGTH OF ELEMENT                      
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV53 CLC   TSARFINV,SPACES                                                  
         BNH   PSTINV54                                                         
         XC    FFTEL(12),FFTEL                                                  
         MVI   FFTEL,FFTELQ              X'DB'                                  
         MVI   FFTTYPE,FFTTINVN          SUPPLIER INVOICE NUMBER (44)           
         MVI   FFTSEQ,0                                                         
         MVC   FFTDATA(L'TSARFINV),TSARFINV                                     
         LHI   R1,L'TSARFINV                                                    
         LA    RE,TSARFINV+L'TSARFINV-1                                         
PSTI53A1 CLI   0(RE),C' '                LAST NON-SPACE                         
         BH    PSTI53A2                                                         
         AHI   RE,-1                                                            
         BCT   R1,PSTI53A1                                                      
                                                                                
PSTI53A2 STC   R1,FFTDLEN                LENGTH OF TEXT                         
         AHI   R1,FFTDATA-FFTELD                                                
         STC   R1,FFTLN                  LENGTH OF ELEMENT                      
                                                                                
         DROP  R4                                                               
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV54 DS    0H                                                               
*&&UK                                                                           
         CLI   AGYCTRY,CTRYHOL     TEST HOLLAND                                 
         BNE   PSTINV56                                                         
         CLC   EXPUL,TRNKULA       TEST EXPENSE ACCOUNT                         
         BNE   PSTINV56                                                         
         USING FFTELD,R4           ADD TAX INDICATOR 1                          
         LA    R1,FFTLN1Q+L'FFTDLEN+1                                           
         EX    R1,*+4                                                           
         XC    FFTELD(0),FFTELD                                                 
         MVI   FFTEL,FFTELQ                                                     
         STC   R1,FFTLN                                                         
         MVI   FFTTYPE,FFTTTAXI                                                 
         MVI   FFTDLEN,1                                                        
         MVI   FFTDATA,C'1'                                                     
         DROP  R4                                                               
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
*&&                                                                             
PSTINV56 TM    PSTELE,PSTESOR      TEST ADDING SOREL                            
         BZ    PSTINV57                                                         
         USING SORELD,R4                                                        
         XC    SORELD(SORALNQ),SORELD                                           
         MVI   SOREL,SORELQ                                                     
         MVI   SORLN,SORALNQ                                                    
         MVI   SORSYS,SORSACC                                                   
         MVC   SORAULA,CPJACT                                                   
         OC    CPJWORK,CPJWORK                                                  
         BZ    *+14                                                             
         MVI   SORLN,SORAL2Q                                                    
         MVC   SORAWRK,CPJWORK                                                  
         SR    R1,R1                                                            
         IC    R1,SORLN                                                         
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
         DROP  R4                                                               
                                                                                
PSTINV57 OC    TSARFMMD,TSARFMMD   MEDIA MOS                                    
         BZ    PSTINV58                                                         
         USING GDAELD,R4                                                        
         XC    GDAELD(GDALNQ),GDAELD                                            
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDAMMOS     MEDIA MOS (PAYABLES)                         
         MVC   GDADATE,TSARFMMD                                                 
         LLC   R1,GDALN                                                         
         AR    R4,R1               R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV58 TM    PSTELE,PSTETRX      TEST TRXEL FOR EXCHANGE DIFFERENCE           
         BO    *+12                                                             
         TM    PSTELE2,PSTETRXA    TEST TRXEL FOR ADVANCE                       
         BZ    PSTINV62                                                         
         USING TRXELD,R4                                                        
         XC    TRXELD(TRXLN1Q),TRXELD                                           
         MVI   TRXEL,TRXELQ                                                     
         MVI   TRXLN,TRXLN1Q                                                    
         TM    PSTELE,PSTETRX                                                   
         BZ    PSTINV60                                                         
         MVI   TRXSTA1,TRXSXDIF    SET EXCHANGE DIFFERENCE                      
         CLI   EXDF+(ACTKLDG-ACTRECD),C'J'                                      
         BNE   PSTINV60                                                         
         OI    TRXSTA1,TRXSXDSJ    YES - SET FLAG                               
                                                                                
PSTINV60 TM    PSTELE2,PSTETRXA                                                 
         BZ    *+8                                                              
         MVI   TRXSTA3,TRXSADVC    SET ADVANCE                                  
         DROP  R4                                                               
         LA    R4,TRXLN1Q(R4)      R4=A(NEXT ELEMENT)                           
                                                                                
PSTINV62 MVI   0(R4),0             SET EOR                                      
         LA    R4,1(R4)                                                         
         SR    R4,R2                                                            
         STCM  R4,3,TRNRLEN        SET RECORD LENGTH                            
                                                                                
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         MVC   FVXTRA(L'TRNKULC),TRNKULC                                        
         CLC   POSTCACN,SPACES     TEST CONTRA NAME                             
         BNE   *+12                                                             
         TM    PSTIND1,PST1DUCN    OK IF NOT UPDATING CONTRA NAMES              
         BZ    PSTINVER                                                         
                                                                                
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTINV66                                                         
         OC    OVIAOCA,OVIAOCA     TEST SECONDARY CURRENCY TRNAMNT              
         BZ    PSTINV66                                                         
         LA    R1,OVOCAL           CREATE DEBIT OCAEL                           
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRNAMNT   SECONDARY CURRENCY DEBIT AMOUNT              
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'TRNAMNT),OVIAOCA                                      
         LA    R1,OCANTR1L+L'TRNAMNT(R1)                                        
         TM    PSTELE,PSTEMPY      TEST ADDING MPYEL                            
         BZ    PSTINV64                                                         
         MVI   OCANTYPE,QMPYAMNT   SECONDARY CURRENCY CHEQUE AMOUNT             
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'MPYAMNT),CHQAMS                                       
         LA    R1,OCANTR1L+L'MPYAMNT(R1)                                        
PSTINV64 MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAACNV',WORK),AIOBUFF2,ACOM,OVOCAL,0,0         
         MVI   OVOCAL,0            RESET A(OCA VALUES LIST)                     
         LA    R1,OVOCAL                                                        
         ST    R1,AOCANXT          RESET A(NEXT)                                
*&&                                                                             
PSTINV66 GOTO1 AADDTRN,POSTCACN                                                 
         GOTO1 AREPTRN,AIOBUFF2    REPORT TRANSACTION POSTING                   
                                                                                
         CLI   PSTACT,PSTSUPP+PSTLINV                                           
         BNE   *+10                                                             
         MVC   OVPAYSBR,TRNKSBR                                                 
                                                                                
PSTINV70 LA    R5,PSTTABL(R5)      NEXT POST INVOICE TABLE ENTRY                
         B     PSTINV10                                                         
                                                                                
PSTINVX  MVC   FVXTRA,SPACES       CLEAR ERROR PRESETS                          
         XC    FVMSGNO,FVMSGNO                                                  
         TM    COMPSTA9,CPYSXDSJ   TEST POST EXCH DIFF TO JOB                   
         BZ    *+10                                                             
         MVC   EXDF,EXDFACC        RESTORE DEFAULT EXCH DIFF A/C                
         B     OVROU1E                                                          
                                                                                
PSTINVER B     OVROU1H             ERROR WILL FORCE ABEND                       
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* POST TO INTERCOMPANY ACCOUNTS FROM INTERCOMPANY TABLE               *         
***********************************************************************         
                                                                                
PSTICO   MVI   POSTOPIK,0          OFFICE NOT POSITIONED IN KEY                 
         L     R2,AIOBUFF2         R2=A(IOBUFF2 FOR TRANSACTION)                
         USING TRNRECD,R2                                                       
         L     R3,AICOTAB          R3=A(INTERCOMPANY TABLE)                     
         USING ICOTABD,R3                                                       
         LA    R4,TRNRFST          R4=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R4                                                        
         LA    R0,ICOTABN          R0=INTERCOMPANY TABLE COUNT                  
                                                                                
PSTICO02 CLI   ICOTABD,0           TEST EOT                                     
         BE    PSTICO10                                                         
         CP    ICOAMNTS,PZERO      TEST ANYTHING FOR THIS OFFICE (2ND)          
         BNE   *+14                                                             
         CP    ICOAMNT,PZERO       TEST ANYTHING FOR THIS OFFICE                
         BE    PSTICO08                                                         
         MVC   POSTCPY,COMPANY     FROM INTERCOMPANY ACCOUNT                    
         MVC   POSTULA,ICOFACT                                                  
         MVC   POSTCCPY,COMPANY    TO INTERCOMPANY ACCOUNT                      
         MVC   POSTULC,ICOTACT                                                  
         MVI   POSTSTAT,TRNSDR     DEBIT FIRST INTERCOMPANY                     
         MVC   POSTOFFC(1),POSTACT OFFICE FROM ACCOUNT BYTE ONE                 
         MVI   POSTOFFC+1,C' '                                                  
         MVC   POSTCACN,ICOTNAM    TO ACCOUNT NAME IS CONTRA NAME               
                                                                                
PSTICO04 MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,POSTCULA                                                
         MVC   TRNKCULC,POSTCULC                                                
         MVC   TRNKDATE,CHQDATP                                                 
         MVC   TRNKREF,CHQNUM                                                   
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         MVC   TRNELD(L'OVTRNEL),OVTRNEL                                        
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVC   TRNSUB,TRNKSBR                                                   
         MVC   TRNSTAT,POSTSTAT                                                 
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         OI    TRNSTAT,TRNS2NDC    SET SECOND CURRENCY POSTING                  
*&&                                                                             
         ZAP   TRNAMNT,ICOAMNT     FOR TABLE AMOUNT                             
         TM    TRNSTAT,TRNSDR                                                   
         BZ    *+10                                                             
         AP    ADDCASH,TRNAMNT     ACCUMULATE TOTAL DEBITS                      
         MVC   TRNOFFC,POSTOFFC                                                 
         LA    RF,TRNELD+L'OVTRNEL RF=A(NEXT ELEMENT)                           
                                                                                
         USING TRSELD,RF                                                        
         MVC   TRSELD(L'OVTRSEL),OVTRSEL                                        
         LA    RF,L'OVTRSEL(RF)    RF=A(NEXT ELEMENT)                           
                                                                                
         USING TIDELD,RF                                                        
         MVC   TIDELD(L'OVTIDEL),OVTIDEL                                        
         LA    RF,L'OVTIDEL(RF)    RF=A(NEXT ELEMENT)                           
                                                                                
         USING PIDELD,RF                                                        
         MVC   PIDELD(L'OVPIDEL),OVPIDEL                                        
         LA    RF,L'OVPIDEL(RF)    RF=A(NEXT ELEMENT)                           
         DROP  RF                                                               
                                                                                
         MVI   0(RF),0             SET EOR                                      
         LA    RF,1(RF)                                                         
         SR    RF,R2                                                            
         STCM  RF,3,TRNRLEN        SET RECORD LENGTH                            
                                                                                
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PSTICO06                                                         
         LA    R1,OVOCAL           CREATE OCAEL                                 
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRNAMNT   SECONDARY CURRENCY AMOUNT                    
         MVI   OCANSEQN,1                                                       
         ZAP   OCANCASH(L'TRNAMNT),ICOAMNTS                                     
         LA    R1,OCANTR1L+L'TRNAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAACNV',WORK),AIOBUFF2,ACOM,OVOCAL,0,0         
         MVI   OVOCAL,0            RESET A(OCA VALUES LIST)                     
         LA    R1,OVOCAL                                                        
         ST    R1,AOCANXT          RESET A(NEXT)                                
*&&                                                                             
PSTICO06 GOTO1 AADDTRN,POSTCACN                                                 
         GOTO1 AREPTRN,AIOBUFF2             REPORT TRANSACTION POSTING          
                                                                                
         L     R2,AIOBUFF2         R2=A(TRANSACTION RECORD)                     
         XC    POSTCULA,POSTCULC   SWAP ACCOUNT WITH CONTRA                     
         XC    POSTCULC,POSTCULA                                                
         XC    POSTCULA,POSTCULC                                                
         CLC   ICOFACT,POSTULA     TEST FIRST INTERCOMPANY ACCOUNT              
         BE    PSTICO08                                                         
         MVI   POSTSTAT,0          CREDIT SECOND INTERCOMPANY                   
         MVC   POSTOFFC,ICOOFFC    OFFICE FROM INTERCOMPANY TABLE               
         MVC   POSTCACN,ICOFNAM    FROM ACCOUNT NAME IS CONTRA NAME             
         B     PSTICO04            SECOND INTERCOMPANY POSTING                  
                                                                                
PSTICO08 LA    R3,ICOTABL(R3)      NEXT ENTRY IN ICOTAB                         
         BCT   R0,PSTICO02         UP TO ICOTABN ENTRIES                        
                                                                                
PSTICO10 DS    0H                                                               
                                                                                
PSTICOX  B     OVROU1X                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* CHECK WITHIN EXCHANGE RATE LIMITS                                   *         
* NTRY P1 - A(FOREIGN CURRENCY)  P2 - A(AGENCY CURRENCY)  P3 - (RATE) *         
* XIT  CC HIGH - OUTSIDE LIMITS   EXRMIN - MIN,  EXRMAX - MAX                   
***********************************************************************         
                                                                                
RATLIM   LR    R3,R1                                                            
         GOTO1 AMAXMIN,(R3)                                                     
         BH    RATLIMH                                                          
         CLI   AGYCTRY,CTRYGBR     TEST NO UK                                   
         BE    RATLIM02                                                         
                                                                                
         ZAP   PKWK16A,PZERO       DIVIDE BY AGENCY CURRENCY                    
         MVO   PKWK16A,EXRMIN                                                   
         SRP   PKWK16A,10,0                                                     
         ZAP   PKWK16B,PZERO                                                    
         MVO   PKWK16B,EXRMAX                                                   
         SRP   PKWK16B,10,0                                                     
         GOTO1 AMAXMIN,4(R3)                                                    
         BH    RATLIMH                                                          
         ZAP   OVIAMNT,PZERO                                                    
         MVO   OVIAMNT,EXRMAX                                                   
         DP    PKWK16A,OVIAMNT                                                  
         ZAP   OVIAMNT,PZERO                                                    
         MVO   OVIAMNT,EXRMIN                                                   
         DP    PKWK16B,OVIAMNT                                                  
         MVC   EXRMIN,PKWK16A+(L'PKWK16A-L'OVIAMNT)-8                           
         MVC   EXRMAX,PKWK16B+(L'PKWK16B-L'OVIAMNT)-8                           
                                                                                
RATLIM02 L     R3,8(R3)            TEST RATE WITHIN LIMITS                      
         CLC   EXRMIN,0(R3)                                                     
         BH    RATLIMH                                                          
         CLC   EXRMAX,0(R3)                                                     
         BL    RATLIMH                                                          
RATLIMX  B     OVROU1E                                                          
RATLIMH  B     OVROU1H                                                          
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                          *         
***********************************************************************         
                                                                                
         USING TRNELD,R2                                                        
RECFLT   ICM   R2,15,ATRNEL        TEST THIS IS A TRANSACTION                   
         BZ    OVROU1H                                                          
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   OVROU1X                                                          
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    OVROU1H                                                          
                                                                                
RECFLT02 CLC   OFFICE,SPACES       TEST OFFICE FILTER SET                       
         BNH   RECFLT04                                                         
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   OVROU1X             WRONG OFFICE                                 
         CLC   TRNOFFC(0),OFFICE                                                
                                                                                
RECFLT04 GOTO1 ABLDSRC             BUILD SOURCE A/C IN SRCWORK                  
                                                                                
         OC    SRCACC,SRCACC       TEST FILTER FOR SOURCE A/C                   
         BZ    RECFLT06                                                         
         IC    RF,SRCACCXL                                                      
         EX    RF,*+8                                                           
         BNE   OVROU1X             SOURCE ACCOUNT DOES NOT MATCH                
         CLC   SRCWORK(0),SRCACC                                                
                                                                                
         USING TRSELD,R2                                                        
RECFLT06 ICM   R2,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE                           
         BL    OVROU1H                                                          
         CLC   TRSDATE,ADAEND                                                   
         BH    OVROU1H                                                          
                                                                                
         B     OVROU1E                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET UP EXTRA ACCOUNT DETAILS                                       
***********************************************************************         
                                                                                
SETACT   LA    R5,SETOTAB                                                       
         USING SETOTABD,R5                                                      
SETACT02 CLI   SETOACC,EOT                                                      
         BE    SETACTX                                                          
         MVC   LAREADDR,SETOACCX                                                
         EX    0,LARE                                                           
         LR    R3,RE                                                            
         USING ACCXD,R3                                                         
         MVC   LAREADDR,SETOACC                                                 
         EX    0,LARE                                                           
         MVC   ACCX,0(RE)                                                       
         OC    ACCX,ACCX                                                        
         BZ    SETACT06                                                         
         LA    R2,KEY              R2=A(KEY)                                    
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY(LDGKEND),ACCX                                            
         GOTO1 AGETLDG,0                                                        
         BE    *+6                                                              
         DC    H'0'                DON'T ALLOW ANY ERROR                        
         ICM   R1,15,RECALDGT      GET LEDGER ENTRY                             
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING LEDGTABD,R1                                                      
         CLI   LEDGTOFF,L'ACTKACT  TEST OFFICE WITHIN KEY                       
         BH    *+10                                                             
         MVC   ACCXOPIK,LEDGTOFF   SAVE OFFICE POSITION IN KEY                  
                                                                                
         USING ACTRECD,R2                                                       
         MVC   ACTKCULA,ACCX                                                    
*&&US                                                                           
         CLC   SETOACC,=S(BANKACC)                                              
         BE    *+10                                                             
*&&                                                                             
         CLC   SETOACC,=S(SUPPACC) DON'T DO OFFICE SUBSTITUTION ON              
         BNE   *+8                 VENDOR ACCOUNT                               
         MVI   ACCXOPIK,0                                                       
         CLI   ACCXOPIK,0                                                       
         BE    SETACT04                                                         
         CLI   TWAACCS,C'*'        TEST LIMIT ACCESS LOGON                      
         BNE   SETACT04                                                         
         SR    R1,R1               INSERT OFFICE INTO ACCOUNT KEY               
         IC    R1,ACCXOPIK                                                      
         LA    R1,ACTKACT-1(R1)                                                 
         MVC   0(1,R1),TWAACCS+1                                                
                                                                                
SETACT04 GOTO1 AGETACC,0                                                        
         BNE   SETACTE             EXIT WITH ERROR                              
         MVC   ACCXNAME,RECNAME                                                 
                                                                                
         ICM   R1,15,RECRSTEL      ** COSTING BYTE IS NOT USED **               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   ACCXCOST,RSTCOSTG-RSTELD(R1)                                     
                                                                                
SETACT06 LA    R5,SETOTABL(R5)                                                  
         B     SETACT02                                                         
                                                                                
SETACTX  B     OVROU1E                                                          
                                                                                
SETACTE  B     OVROU1H                                                          
                                                                                
         DS    0H                                                               
SETOTAB  DC    S(BANKACC),S(BANK)                                               
         DC    S(SUPPACC),S(SUPP)                                               
*&&UK*&& DC    S(EXDFACC),S(EXDF)                                               
         DC    S(BCHAACC),S(BCHA)                                               
         DC    S(CDSCACC),S(CDSC)                                               
         DC    S(DTAXACC),S(DTAX)                                               
         DC    AL1(EOT)                                                         
         DROP  R1,R2,R3,R5                                                      
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
*        R2=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
SETKEY   STC   R1,WORK                                                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY02                                                         
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                SUPPLIER MISSING                             
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY02 TM    WORK,SETOFF         SET OFFICE                                   
         BZ    SETKEY04                                                         
         MVC   TRNKOFF,SPACES                                                   
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    SETKEY04            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    SETKEY04            YES - OFFICE NOT IN KEY                      
         MVC   TRNKOFF,OFFICE                                                   
                                                                                
SETKEY04 TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY06                                                         
         MVC   TRNKCULC,SPACES                                                  
         MVI   TRNKCULC+L'TRNKCULC-1,X'41'                                      
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY06                                                         
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY06 TM    WORK,SETSDT         SET MIN TRANSACTION DATE                     
         BZ    SETKEY08                                                         
         MVC   TRNKDATE,PERSTA                                                  
                                                                                
SETKEY08 TM    WORK,SETREF         SET REFERENCE (BILL NUMBER)                  
         BZ    SETKEY10                                                         
         MVC   TRNKREF,SPACES                                                   
         MVI   TRNKREF+L'TRNKREF-1,X'41'                                        
         OC    REFSTA,REFSTA                                                    
         BZ    SETKEY10                                                         
         MVC   TRNKREF,REFSTA                                                   
                                                                                
SETKEY10 TM    WORK,NXTOFF         BUMP OFFICE (NEW OFFICES ONLY)               
         BZ    SETKEY12                                                         
         IC    RE,TRNKOFF+(L'TRNKOFF-1)                                         
         LA    RE,1(RE)                                                         
         STC   RE,TRNKOFF+(L'TRNKOFF-1)                                         
                                                                                
SETKEY12 TM    WORK,NXTCON         BUMP CONTRA                                  
         BZ    SETKEY14                                                         
         IC    RE,TRNKCACT+(L'TRNKCACT-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKCACT+(L'TRNKCACT-1)                                       
                                                                                
SETKEY14 TM    WORK,NXTSDT         BUMP DATE                                    
         BZ    SETKEY16                                                         
         IC    RE,TRNKDATE+(L'TRNKDATE-1)                                       
         LA    RE,1(RE)                                                         
         STC   RE,TRNKDATE+(L'TRNKDATE-1)                                       
                                                                                
SETKEY16 DS    0H                                                               
                                                                                
SETKEYX  MVI   TRNKSBR,0           ALWAYS CLEAR SUB-REFERENCE                   
         B     OVROU1X                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE ALL MANUAL PAYMENT ELEMENTS AND ADD A BLANK ONE              *         
* NTRY - R1=A(A(TRANSACTION RECORD))                                  *         
***********************************************************************         
                                                                                
SETMPY   L     R2,0(R1)            R2=A(TRANSACTION RECORD)                     
         LA    RF,TRNRFST-TRNRECD                                               
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    SETMPY08                                                         
*&&US                                                                           
         GOTO1 VHELLO,DMCB,(C'D',OVACCMST),('MPYELQ',(R2)),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*&&                                                                             
*&&UK                                                                           
         USING MPYELD,R3                                                        
         LA    R3,TRNRFST-TRNRECD(R2)                                           
SETMPY02 CLI   MPYEL,0                                                          
         BE    SETMPY06                                                         
         CLI   MPYEL,MPYELQ                                                     
         BE    SETMPY04                                                         
         SR    R1,R1                                                            
         IC    R1,MPYLN                                                         
         AR    R3,R1                                                            
         B     SETMPY02                                                         
SETMPY04 GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),(R2),ACOM,0,MPYELD,0                
         B     SETMPY02                                                         
         DROP  R3                                                               
*&&                                                                             
SETMPY06 LA    RF,ELEMT            BUILD NEW ELEMENT                            
         USING MPYELD,RF                                                        
         XC    MPYELD(MPYLN2Q),MPYELD                                           
         MVI   MPYEL,MPYELQ        SET ELEMENT CODE                             
         MVI   MPYLN,MPYLN2Q       SET LENGTH                                   
         GOTO1 VHELLO,DMCB,(C'P',OVACCMST),(R2),MPYELD                          
         DROP  RF                                                               
         CLI   12(R1),0                                                         
         BE    OVROU1X                                                          
         DC    H'0'                DIE ON ANY ERROR                             
                                                                                
         USING MPYELD,R2                                                        
SETMPY08 AR    R2,RF               DRAFT A MPYEL ONTO END                       
         CLI   MPYEL,MPYELQ                                                     
         BNE   *+8                                                              
         MVI   MPYEL,FF                                                         
         IC    RF,MPYLN                                                         
         CLI   MPYEL,0                                                          
         BNE   SETMPY08                                                         
         XC    MPYELD(MPYLN2Q+1),MPYELD                                         
         MVI   MPYEL,MPYELQ        SET ELEMENT CODE                             
         MVI   MPYLN,MPYLN2Q       SET LENGTH                                   
         DROP  R2                                                               
         L     R2,0(R1)                                                         
         ICM   R1,3,TRNRLEN-TRNRECD(R2)                                         
         LA    R1,MPYLN2Q(R1)                                                   
         STCM  R1,3,TRNRLEN-TRNRECD(R2)                                         
         B     OVROU1X                                                          
         EJECT                                                                  
***********************************************************************         
* STANDARD ELEMENTS - TRNEL, TRSEL, SPAEL, PIDEL AND TIDEL            *         
***********************************************************************         
                                                                                
STDEL    LA    RF,OVTRNEL                                                       
         USING TRNELD,RF                                                        
         XC    TRNELD(TRNLN1Q),TRNELD                                           
         MVI   TRNEL,TRNELQ        BUILD TRANSACTION ELEMENT                    
         MVI   TRNTYPE,BT36                                                     
         MVC   TRNMOS,SBATMON                                                   
         MVC   TRNBREF,SBATREF                                                  
         MVI   TRNNARR,C' '                                                     
         MVI   TRNLN,TRNLN1Q+1                                                  
                                                                                
         LA    RF,OVTRSEL                                                       
         USING TRSELD,RF           BUILD TRSEL IN OVTRSEL                       
         XC    TRSELD(TRSLNQ),TRSELD                                            
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVI   TRSMARK,TRSMCMQ     SET MARKER ACTION                            
         MVI   TRSSTAT3,TRSSMRK1   SET PROGRAM IS MARKER MANUAL CHEQUE          
         MVC   TRSUDAT,TODAYC      SET USED DATE                                
         MVC   TRSUMOS,SBATMONP    SET USED MOS                                 
                                                                                
         USING SPAELD,RF           BUILD FACTORING COMPANY IN OVSPAEL           
         LA    RF,OVSPAEL                                                       
         XC    SPAELD(SPALNQ),SPAELD                                            
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATFACC                                                 
         MVC   SPAAULA,FACTOR                                                   
                                                                                
         LA    RF,OVTIDEL                                                       
         USING TIDELD,RF                                                        
         MVI   TIDEL,TIDELQ        ATTACH A TERMINAL ID ELEMENT                 
         MVI   TIDLN,TIDLNQ                                                     
         L     RE,AUTL                                                          
         MVC   TID,TSYM-UTLD(RE)                                                
                                                                                
         LA    RF,OVPIDEL                                                       
         USING PIDELD,RF                                                        
         XC    PIDELD(PIDLNQ),PIDELD                                            
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,STWAPAS#                                                   
         DROP  RF                                                               
                                                                                
STDELX   B     OVROU1X                                                          
         EJECT                                                                  
***********************************************************************         
* TOTAL OFFICE VALUES INTO TOTALS ELEMENT                             *         
***********************************************************************         
                                                                                
TOTOFF   L     R2,AOFFTAB          R2=A(OFFICE TABLE)                           
         L     R3,AOFFTABT         R3=A(OFFICE TOTAL ENTRY)                     
         XR    R1,R1                                                            
         ZAP   OVDUB2,PZERO                                                     
         USING OFFTABD,R2                                                       
*&&US                                                                           
         OC    OFFCHEQ,OFFCHEQ     MAKE DAMN SURE THESE HAVE PACKED             
         BNZ   *+10                NUMBERS IN THEM                              
         ZAP   OFFCHEQ,PZERO                                                    
         OC    OFFCHEQC,OFFCHEQC                                                
         BNZ   *+10                                                             
         ZAP   OFFCHEQC,PZERO                                                   
         OC    OFFCDSC,OFFCDSC                                                  
         BNZ   *+10                                                             
         ZAP   OFFCDSC,PZERO                                                    
         OC    OFFCDSCC,OFFCDSCC                                                
         BNZ   *+10                                                             
         ZAP   OFFCDSCC,PZERO                                                   
         OC    OFFDTAX,OFFDTAX                                                  
         BNZ   *+10                                                             
         ZAP   OFFDTAX,PZERO                                                    
         OC    OFFBCHA,OFFBCHA                                                  
         BNZ   *+10                                                             
         ZAP   OFFBCHA,PZERO                                                    
         OC    OFFBCHAC,OFFBCHAC                                                
         BNZ   *+10                                                             
         ZAP   OFFBCHAC,PZERO                                                   
         OC    OFFEXDF,OFFEXDF                                                  
         BNZ   *+10                                                             
         ZAP   OFFEXDF,PZERO                                                    
         OC    OFFDIFF,OFFDIFF                                                  
         BNZ   *+10                                                             
         ZAP   OFFDIFF,PZERO                                                    
         OC    OFFDIFFC,OFFDIFFC                                                
         BNZ   *+10                                                             
         ZAP   OFFDIFFC,PZERO                                                   
         OC    OFFDIFT,OFFDIFT                                                  
         BNZ   *+10                                                             
         ZAP   OFFDIFT,PZERO                                                    
         OC    OFFADVC,OFFADVC                                                  
         BNZ   *+10                                                             
         ZAP   OFFADVC,PZERO                                                    
         OC    OFFADVCC,OFFADVCC                                                
         BNZ   *+10                                                             
         ZAP   OFFADVCC,PZERO                                                   
         OC    OFFADEX,OFFADEX                                                  
         BNZ   *+10                                                             
         ZAP   OFFADEX,PZERO                                                    
*&&                                                                             
                                                                                
         PUSH  USING                                                            
TOTAL    USING OFFTABD,R3                                                       
         ZAP   TOTAL.OFFCHEQ,PZERO                                              
         ZAP   TOTAL.OFFCHEQC,PZERO                                             
         ZAP   TOTAL.OFFCHEQS,PZERO                                             
         ZAP   TOTAL.OFFCDSC,PZERO                                              
         ZAP   TOTAL.OFFCDSCC,PZERO                                             
         ZAP   TOTAL.OFFCDSCS,PZERO                                             
         ZAP   TOTAL.OFFDTAX,PZERO                                              
         ZAP   TOTAL.OFFDTAXS,PZERO                                             
         ZAP   TOTAL.OFFBCHA,PZERO                                              
         ZAP   TOTAL.OFFBCHAC,PZERO                                             
         ZAP   TOTAL.OFFEXDF,PZERO                                              
         ZAP   TOTAL.OFFEXDFS,PZERO                                             
         ZAP   TOTAL.OFFDIFF,PZERO                                              
         ZAP   TOTAL.OFFDIFFC,PZERO                                             
         ZAP   TOTAL.OFFDIFFS,PZERO                                             
         ZAP   TOTAL.OFFDIFT,PZERO                                              
         ZAP   TOTAL.OFFDIFTS,PZERO                                             
         ZAP   TOTAL.OFFADVC,PZERO                                              
         ZAP   TOTAL.OFFADVCC,PZERO                                             
         ZAP   TOTAL.OFFADVCS,PZERO                                             
         ZAP   TOTAL.OFFADEX,PZERO                                              
         ZAP   TOTAL.OFFADEXS,PZERO                                             
         LA    R0,OFFTABN                                                       
TOTOFF02 AP    TOTAL.OFFCHEQ,OFFCHEQ                                            
         AP    TOTAL.OFFCHEQC,OFFCHEQC                                          
         AP    TOTAL.OFFCHEQS,OFFCHEQS                                          
         AP    TOTAL.OFFCDSC,OFFCDSC                                            
         AP    TOTAL.OFFCDSCC,OFFCDSCC                                          
         AP    TOTAL.OFFCDSCS,OFFCDSCS                                          
         AP    TOTAL.OFFDTAX,OFFDTAX                                            
         AP    TOTAL.OFFDTAXS,OFFDTAXS                                          
         AP    TOTAL.OFFBCHA,OFFBCHA                                            
         AP    TOTAL.OFFBCHAC,OFFBCHAC                                          
         CP    TOTAL.OFFEXDF,=P'99999999'                                       
         BE    *+16                                                             
         AP    TOTAL.OFFEXDF,OFFEXDF                                            
         AP    TOTAL.OFFEXDFS,OFFEXDFS                                          
         AP    TOTAL.OFFDIFF,OFFDIFF                                            
         AP    TOTAL.OFFDIFFC,OFFDIFFC                                          
         AP    TOTAL.OFFDIFFS,OFFDIFFS                                          
         AP    TOTAL.OFFDIFT,OFFDIFT                                            
         AP    TOTAL.OFFDIFTS,OFFDIFTS                                          
         AP    TOTAL.OFFADVC,OFFADVC                                            
         AP    TOTAL.OFFADVCC,OFFADVCC                                          
         AP    TOTAL.OFFADVCS,OFFADVCS                                          
         AP    TOTAL.OFFADEX,OFFADEX                                            
         AP    TOTAL.OFFADEXS,OFFADEXS                                          
         TM    OFFIND1,OFFI1OXO    TEST OFFICE EXTRA TABLE IN USE               
         BZ    *+8                                                              
         OI    TOTAL.OFFIND1,OFFI1OXO                                           
         LA    R2,OFFTABL(R2)      RF=A(NEXT OFFICE TABLE ENTRY)                
         OC    OFFOFFC,OFFOFFC     TEST ANY FURTHER OFFICE                      
         BZ    *+8                                                              
         BCT   R0,TOTOFF02                                                      
         POP   USING                                                            
TOTOFFX  B     OVROU1E                                                          
         DROP  R2                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VERIFY EXCHANGE RATE                                                *         
***********************************************************************         
                                                                                
VERRAT   OI    MANRATH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    MANRATYH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    ATLXRATE,ATLXRATE   VERIFY EXCHANGE RATE RANGE                   
         BZ    VERRATE                                                          
         TM    MANRATH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VERRAT02            NO NEED TO VERIFY PROTECTED RATE             
         TM    SSIND1,SSI1XSYS                                                  
         BO    VERRAT02            NO NEED TO VERIFY SYSTEM RATE                
         XR    RE,RE                                                            
         IC    RE,PRSTCURT+(CURTDECP-CURTABD)                                   
         XR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         SR    RE,RF                                                            
         STC   RE,ATLXSHFT         SET SHIFT VALUE                              
         GOTO1 ARATLIM,PARM,PRSTCURT,COMPCURT,ATLXRATE                          
         BNH   VERRAT02                                                         
         CLC   GBP,PRSTCURT+(CURTCUR-CURTABD)                                   
         BE    VERRAT02            CANNOT CHECK STERLING RANGE                  
                                                                                
         OI    OVBYTE,INVEXCHQ     EXCH RATE NOT IN VALID RANGE                 
         MVC   WORK,SPACES                                                      
         LA    R0,L'EXRMIN                                                      
         GOTO1 AEDTRAT,DMCB,EXRMIN,((R0),WORK)                                  
         LA    RF,WORK+(L'EXRMIN*2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'-'                                                       
         LA    RF,2(RF)                                                         
         LA    R0,L'EXRMIN                                                      
         GOTO1 AEDTRAT,DMCB,EXRMAX,((R0),(RF))                                  
         LA    RF,WORK+(L'EXRMIN*4)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         LA    R0,WORK                                                          
         SR    RF,R0                                                            
         SRA   RF,4                TEST APPENDAGE IS NOT > 16 CHARS             
         BNZ   *+10                                                             
         MVC   FVXTRA,WORK                                                      
                                                                                
VERRAT02 GOTO1 AEDTRAT,DMCB,ATLXRATE,(L'MANRAT,MANRAT)                          
         LA    R1,MANRAT+L'MANRAT                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
                                                                                
         MVC   2(L'CURTCUR,R1),PRSTCURT+(CURTCUR-CURTABD)                       
*        TM    SSIND1,SSI1XSYS     IF SYSTEM RATE                               
*        BNO   VERRAT04                                                         
         LR    R0,R1               MAY BE TRULY EURO                            
         GOTO1 ATSTEUR,PRSTCURT+(CURTCUR-CURTABD)                               
         LR    R1,R0                                                            
         BNE   VERRAT04                                                         
         MVC   2(L'CURTCUR,R1),EURO                                             
         OI    ATLXSTAT,QFROMEUR                                                
VERRAT04 MVI   2+L'CURTCUR(R1),C'-'                                             
         MVC   L'CURTCUR+3(L'CURTCUR,R1),COMPCURT+(CURTCUR-CURTABD)             
*        TM    SSIND1,SSI1XSYS     IF SYSTEM RATE                               
*        BNO   VERRAT06                                                         
         LR    R0,R1               MAY BE TRULY EURO                            
         GOTO1 ATSTEUR,COMPCURT+(CURTCUR-CURTABD)                               
         LR    R1,R0                                                            
         BNE   VERRAT06                                                         
         MVC   L'CURTCUR+3(L'CURTCUR,R1),EURO                                   
         OI    ATLXSTAT,QINTOEUR                                                
VERRAT06 OI    SAFCIND1,SAFCI1ON+SAFCIALT  DEFAULT TO ALTERNATE TOTALS          
         TM    OVBYTE,INVEXCHQ                                                  
         BZ    VERRATX                                                          
VERRATE  LA    RE,MANRATH                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$EXRNV) 'EXCH RATE NOT IN VALID RANGE'            
         B     OVROU1H                                                          
VERRATX  OI    MANRATH+(FVIIND-FVIHDR),FVIVAL                                   
         B     OVROU1E                                                          
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* POSTING TABLE.  PST**** TYPE + PSTL*** LEVEL (INV/OFF), TRNSTAT     *         
*                 PST1*** INDIC. (NAR/NEG/SEI/OPIK), PSTE*** ELEMENTS *         
*                 SPAT*** SPATYPE ASSOC., 0, AMNT OFFSET, SPA ERR.MSG *         
*                 S(C/U/L/ACCOUNT), S(C/U/L/CONTRA), S(AMOUNT)        *         
***********************************************************************         
         DS    0H                    ENSURE ON HALF WORD BOUNDARY               
         USING OFFTABD,R3                                                       
PSTTAB   DC    AL1(PSTSUPP+PSTLINV)  CHEQUE DEBIT - VENDOR                      
         DC    AL1(TRNSDR)                                                      
         DC    AL1(PST1NAR+PST1DUCN)                                            
         DC    AL1(PSTESPA+PSTEMPY+PSTEDUE+PSTEAFC)                             
         DC    AL1(PSTEFFTK,0),AL2(0,0)                                         
         DC    S(SUPP,OVCNTRA,OVIAMNT)                                          
         DC    AL1(0,0)                                                         
*&&UK                                                                           
         DC    AL1(PSTEXDF+PSTLINV)  EXCHANGE DIFFERENCE - VENDOR               
         DC    AL1(TRNSAUTH)                                                    
         DC    AL1(PST1CSRF+PST1DUCN)                                           
         DC    AL1(PSTEMPY+PSTESOR+PSTEFFT+PSTETRX)                             
         DC    AL1(0,0),AL2(0,0)                                                
         DC    S(SUPP,OVCNTRA,TMPEXDF)                                          
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PSTEXDF+PSTLINV)  EXCHANGE DIFFERENCE - EXPENSE              
         DC    AL1(TRNSDR+TRNSAUTH)                                             
         DC    AL1(PST1OPIK+PST1SEI+PST1CSRF)                                   
         DC    AL1(PSTEMPY+PSTEFFT+PSTETRX)                                     
         DC    AL1(0,SPATEXDF),AL2(0,AE$EXDAM)                                  
         DC    S(EXDF,SUPP,TMPEXDF)                                             
         DC    AL1(0,0)                                                         
*&&                                                                             
         DC    AL1(PSTSUPP+PSTLINV)  ADVANCE - VENDOR                           
         DC    AL1(TRNSAUTH)                                                    
         DC    AL1(PST1NEG+PST1DUCN)                                            
         DC    AL1(PSTEAFC)                                                     
         DC    AL1(PSTETRXA,0),AL2(0,0)                                         
         DC    S(SUPP,OVCNTRA,OVIAMNT)                                          
         DC    AL1(PST2INEW,0)                                                  
*&&UK                                                                           
         DC    AL1(PSTEXDF+PSTLINV)  ADVANCE EXCHANGE DIFF - VENDOR             
         DC    AL1(TRNSAUTH)                                                    
         DC    AL1(PST1CSRF+PST1DUCN)                                           
         DC    AL1(PSTESOR+PSTEFFT+PSTETRX)                                     
         DC    AL1(PSTETRXA,0),AL2(0,0)                                         
         DC    S(SUPP,OVCNTRA,TMPEXDF)                                          
         DC    AL1(PST2INEW,0)                                                  
                                                                                
         DC    AL1(PSTEXDF+PSTLINV)  ADVANCE EXCHANGE DIFF - EXPENSE            
         DC    AL1(TRNSDR+TRNSAUTH)                                             
         DC    AL1(PST1OPIK+PST1SEI+PST1CSRF)                                   
         DC    AL1(PSTESOR+PSTEFFT+PSTETRX)                                     
         DC    AL1(PSTETRXA,SPATEXDF),AL2(0,AE$EXDAM)                           
         DC    S(EXDF,SUPP,TMPEXDF)                                             
         DC    AL1(PST2INEW,0)                                                  
*&&                                                                             
         DC    AL1(PSTBANK+PSTLOFF)  CHEQUE CREDIT(S) - BANK                    
         DC    AL1(0)                                                           
         DC    AL1(PST1SPA)                                                     
         DC    AL1(PSTEDSC+PSTESPA+PSTEAFC)                                     
         DC    AL1(PSTEBCH+PSTEAFCC+PSTETRP,0),AL2(OFFCHEQ-OFFTABD,0)           
         DC    S(BANK,SUPP,OFFTABD)                                             
         DC    AL1(PST2ZERO,0)                                                  
                                                                                
         DC    AL1(PSTCDSC+PSTLOFF)  DISCOUNT - DISCOUNT                        
         DC    AL1(TRNSDR)                                                      
         DC    AL1(PST1SEI+PST1NEG)                                             
         DC    AL1(PSTEFFT)                                                     
         DC    AL1(PSTEAPE,SPATCSHD),AL2(OFFCDSC-OFFTABD,AE$DACCM)              
         DC    S(CDSC,SUPP,OFFTABD)                                             
         DC    AL1(PST2DISC,0)                                                  
*&&UK                                                                           
         DC    AL1(PSTDTAX+PSTLOFF)  DISCOUNT TAX ADJUSTMENT - DISCOUNT         
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PSTEFFT)                                                     
         DC    AL1(PSTEAPE,0),AL2(OFFDTAX-OFFTABD,0)                            
         DC    S(CDSC,SUPP,OFFTABD)                                             
         DC    AL1(PST2DTAD,0)                                                  
                                                                                
         DC    AL1(PSTDTAX+PSTLOFF)  DISCOUNT TAX ADJUSTMENT - TAX              
         DC    AL1(TRNSDR)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PSTEDSC+PSTEFFT)                                             
         DC    AL1(0,0),AL2(OFFDTAX-OFFTABD,0)                                  
         DC    S(DTAX,SUPP,OFFTABD)                                             
         DC    AL1(PST2DTAT,0)                                                  
                                                                                
         DC    AL1(PSTBCHA+PSTLOFF)  BANK CHARGES - BANK CHARGE                 
         DC    AL1(TRNSDR)                                                      
         DC    AL1(PST1SEI)                                                     
         DC    AL1(PSTEFFT)                                                     
         DC    AL1(0,SPATBCHA),AL2(OFFBCHA-OFFTABD,AE$BCHAM)                    
         DC    S(BCHA,SUPP,OFFTABD)                                             
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(PSTBCHA+PSTLOFF)  BANK CHARGES - BANK                        
         DC    AL1(0)                                                           
         DC    AL1(PST1OPIK)         SET CONTRA OPIK'S                          
         DC    AL1(PSTEDSC+PSTESPA+PSTEAFC)                                     
         DC    AL1(PSTEAFCC,0),AL2(OFFBCHA-OFFTABD,0)                           
         DC    S(BANK,BCHA,OFFTABD)                                             
         DC    AL1(0,0)                                                         
*&&                                                                             
         DC    AL1(PSTCDSC+PSTLOFF)  DIFFERENCE - EXPENSE                       
         DC    AL1(TRNSDR)                                                      
         DC    AL1(PST1SEI+PST1NEG)                                             
         DC    AL1(PSTEFFT)                                                     
         DC    AL1(PSTETRXD+PSTEAPE,0),AL2(OFFDIFF-OFFTABD,AE$DACCM)            
         DC    S(CDSC,SUPP,OFFTABD)                                             
         DC    AL1(PST2DNAR+PST2DISC,0)                                         
*&&UK                                                                           
         DC    AL1(PSTDTAX+PSTLOFF)  DIFFERENCE TAX ADJUST - EXPENSE            
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(PSTEFFT)                                                     
         DC    AL1(PSTETRXD+PSTEAPE,0),AL2(OFFDIFT-OFFTABD,0)                   
         DC    S(CDSC,SUPP,OFFTABD)                                             
         DC    AL1(PST2DNAR+PST2DTAD,0)                                         
                                                                                
         DC    AL1(PSTDTAX+PSTLOFF)  DIFFERENCE TAX ADJUST - TAX                
         DC    AL1(TRNSDR)                                                      
         DC    AL1(0)                                                           
         DC    AL1(PSTEDSC+PSTEFFT)                                             
         DC    AL1(PSTETRXD,0),AL2(OFFDIFT-OFFTABD,0)                           
         DC    S(DTAX,SUPP,OFFTABD)                                             
         DC    AL1(PST2DNAR+PST2DTAT,0)                                         
                                                                                
*&&                                                                             
         DC    AL1(EOT)                                                         
         DROP  R3                                                               
         EJECT                                                                  
EXPUL    DC    C'SE'               SUBSIDIARY UNIT, EXPENSE LEDGER              
         LTORG                                                                  
                                                                                
BATCHKQ  EQU   1                                                                
BATADDQ  EQU   2                                                                
BATUPDQ  EQU   3                                                                
                                                                                
SUPNDSP  EQU   19                  DISP. TO SUPPLIER NAME FROM CODE             
BNKNDSP  EQU   19                  DISP. TO BANK NAME FROM CODE                 
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES - TWO                                              *         
***********************************************************************         
                                                                                
         DS    0F                                                               
OVROU2   NMOD1 0,**OVR2**,RA,R9                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         LH    R5,=Y(SOVRWRK2-SAVED)                                            
         LA    R5,SAVED(R5)                                                     
         USING SOVRWRK2,R5                                                      
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     ADDOFF                                                           
         B     PAYWOI                                                           
         B     DIFFER                                                           
         B     BLDNEW                                                           
         B     ADDTRP                                                           
         B     ADDAPE                                                           
         B     ADDSCG                                                           
         B     VALPWI                                                           
         B     VALDIF                                                           
                                                                                
OVROU2L  MVI   DUB,0                                                            
         B     *+8                                                              
OVROU2E  MVI   DUB,1                                                            
         B     *+8                                                              
OVROU2H  MVI   DUB,2                                                            
         CLI   DUB,1                                                            
OVROU2X  XIT1  ,                                                                
         EJECT                                                                  
*&&UK                                                                           
C        USING CURTABD,COMPCURT                                                 
*&&                                                                             
***********************************************************************         
* ADD/CHANGE PAYMENT WITHOUT INVOICE                                  *         
***********************************************************************         
                                                                                
PAYWOI   LR    R0,R1               SAVE CALL INDICATOR                          
         SR    R2,R2                                                            
*&&UK                                                                           
         USING SCUTOTS,R2                                                       
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         L     R2,ASCUTOTS                                                      
*&&                                                                             
         OC    PAYRNUM,PAYRNUM     TEST PAYMENT TSAR RECORD EXISTS              
         BZ    PAYWOI04                                                         
         CLI   XTSRCNT,1           ONLY RELIABLE IF SINGLE EXTRA RECORD         
         BE    *+10                                                             
         MVC   PAYRNUM,=H'1'       ELSE READ THROUGH TO FIND IT                 
PAYWOI02 GOTO1 ATSARGET,PAYRNUM    GET TSAR RECORD                              
         BE    *+6                                                              
         DC    H'0'                TROUBLE                                      
         TM    TSARIND3,TSARPWOI   TEST THIS IS THE RECORD                      
         BO    PAYWOI04                                                         
         ICM   R1,3,PAYRNUM                                                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,PAYRNUM                                                     
         B     PAYWOI02                                                         
*                                                                               
PAYWOI04 LTR   R0,R0               TEST FIRST TIME                              
         BZ    PAYWOI40                                                         
         MVC   PWIBANK,BANKACC+(ACTKACT-ACTRECD)                                
         MVC   PWIBANN,SBNKNAME                                                 
         MVC   PWISUPP,SUPPACC+(ACTKUNT-ACTRECD)                                
         MVC   PWISUPN,ACCNAME                                                  
         MVC   PWICHQN,CHQNUM                                                   
         GOTO1 VDATCON,DMCB,(1,CHQDATP),(17,PWICHQD)                            
*&&UK*&& CURED CHQAMT,(L'PWICHQA,PWICHQA),COMPCURT,ALIGN=LEFT,FLOAT=-           
*&&US*&& CURED CHQAMT,(L'PWICHQA,PWICHQA),2,ALIGN=LEFT,FLOAT=-                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BO    PAYWOI06                                                         
         XC    PWICHST,PWICHST     CONCEAL SECONDARY CURRENCY                   
         OI    PWICHSTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PWIAMST,PWIAMST                                                  
         OI    PWIAMSTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWIAMSH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    PWIAMSH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,PWIAMTH                                                       
         B     PAYWOI08                                                         
                                                                                
PAYWOI06 MVC   PWICHAC,C.CURTCUR                                                
         MVC   PWICHSC,S.CURTCUR                                                
         MVC   PWIAMTC,C.CURTCUR                                                
         MVC   PWIAMSC,S.CURTCUR                                                
         CURED CHQAMS,(L'PWICHS,PWICHS),SCNDCURT,ALIGN=LEFT,FLOAT=-             
                                                                                
PAYWOI08 TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BO    PAYWOI10                                                         
*&&                                                                             
         XC    PWICU1T,PWICU1T     CLEAR ALL CURRENCY FIELDS                    
         OI    PWICU1TH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PWIRATT,PWIRATT                                                  
         OI    PWIRATTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    PWICU2T,PWICU2T                                                  
         OI    PWICU2TH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PWIAMCH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    PWIAMCH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,PWIAMTH                                                       
         B     PAYWOI12                                                         
*&&UK                                                                           
PAYWOI10 DS    0H                                                               
         CURED CHQAMC,(L'PWICHQC,PWICHQC),FORECURT,ALIGN=LEFT,FLOAT=-           
         MVC   PWICUC1,FORECURT+(CURTCUR-CURTABD)                               
         MVC   PWICUC2,FORECURT+(CURTCUR-CURTABD)                               
         GOTO1 AEDTRAT,DMCB,ATLXRATE,(L'PWIRAT,PWIRAT)                          
         LA    R1,PWIRAT+L'PWIRAT                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'CURTCUR,R1),FORECURT+(CURTCUR-CURTABD)                       
         MVI   2+L'CURTCUR(R1),C'-'                                             
         MVC   L'CURTCUR+3(L'CURTCUR,R1),COMPCURT+(CURTCUR-CURTABD)             
         LA    R1,PWIAMCH                                                       
*&&                                                                             
PAYWOI12 DS    0H                                                               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         LA    R1,PWIAMSH          SET THAT FIELD FOR INPUT                     
*&&                                                                             
         ST    R1,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         OC    PAYRNUM,PAYRNUM     TEST PAYMENT EXISTS                          
         BNZ   PAYWOI22                                                         
                                                                                
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI14                                                         
         CP    STOBAL,PZERO        TEST SECONDARY CURRENCY BALANCE              
         BE    PAYWOI20                                                         
         MVC   WORK(L'CURTCUR),S.CURTCUR  SET PRIMARY FROM SECONDARY            
         MVC   WORK+L'CURTCUR(L'CURTCUR),C.CURTCUR                              
         ZAP   DUB,STOBAL          USE SECONDARY CURRENCY BALANCE               
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         ZAP   DUB,12(8,R1)        TO SET PRIMARY CURRENCY BALANCE              
         B     PAYWOI16                                                         
*&&                                                                             
PAYWOI14 ZAP   DUB,TOTBAL                                                       
         BZ    PAYWOI20                                                         
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,TOTEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
*&&                                                                             
PAYWOI16 DS    0H                                                               
*&&UK*&& CURED DUB,(L'PWIAMT,PWIAMT),COMPCURT,ALIGN=LEFT,FLOAT=-                
*&&US*&& CURED DUB,(L'PWIAMT,PWIAMT),2,ALIGN=LEFT,FLOAT=-                       
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI18                                                         
         ZAP   DUB,STOBAL                                                       
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,STOEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
         CURED DUB,(L'DIFAMS,DIFAMS),SCNDCURT,ALIGN=LEFT,FLOAT=-                
                                                                                
PAYWOI18 TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    PAYWOI20                                                         
         CURED CTOBAL,(L'PWIAMC,PWIAMC),FORECURT,ALIGN=LEFT,FLOAT=-             
*&&                                                                             
PAYWOI20 MVC   PWIOFF,OFFICE                                                    
         CLI   TWAACCS,C'*'        TEST SINGLE OFFICE LIMIT ACCESS              
         BNE   *+14                                                             
         MVC   PWIOFF(1),TWAACCS+1                                              
         MVI   PWIOFF+1,C' '                                                    
         MVCDD PWINAR,AC#PAYWI,LL                                               
         B     OVROU2E                                                          
                                                                                
PAYWOI22 MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         ZAP   DUB,TOTBAL          USE CURRENT BALANCE                          
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,TOTEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
                                                                                
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI24                                                         
         MVC   WORK(L'CURTCUR),S.CURTCUR  SET PRIMARY FROM SECONDARY            
         MVC   WORK+L'CURTCUR(L'CURTCUR),C.CURTCUR                              
         ZAP   DUB,PAYAMS          USE SECONDARY CURRENCY VALUE                 
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         ZAP   DUB,12(8,R1)        TO SET PRIMARY CURRENCY VALUE                
*&&                                                                             
PAYWOI24 CP    DUB,PZERO           BUT IF BALANCE IS ZERO                       
         BE    *+12                                                             
         TM    TSARINDS,TSARMKQ    OR IF THE RECORD IS MARKED                   
         BZ    *+10                                                             
         ZAP   DUB,TSARAMNT        USE RECORD AMOUNT                            
*&&UK*&& CURED DUB,(L'PWIAMT,PWIAMT),COMPCURT,ALIGN=LEFT,FLOAT=-                
*&&US*&& CURED DUB,(L'PWIAMT,PWIAMT),2,ALIGN=LEFT,FLOAT=-                       
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI26                                                         
         ZAP   DUB,STOBAL          AIM TO ELIMINATE CURRENT BALANCE             
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,STOEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
         CP    DUB,PZERO           BUT IF BALANCE IS ZERO                       
         BE    *+12                                                             
         TM    TSARINDS,TSARMKQ    OR IF THE RECORD IS MARKED                   
         BZ    *+10                                                             
         ZAP   DUB,TSARSCUA        USE RECORD AMOUNT                            
         CURED DUB,(L'DIFAMS,DIFAMS),SCNDCURT,ALIGN=LEFT,FLOAT=-                
                                                                                
PAYWOI26 TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    PAYWOI28                                                         
         ZAP   DUB,CTOBAL          USE CURRENT BALANCE                          
         BZ    *+12                BUT IF BALANCE IS ZERO                       
         TM    TSARINDS,TSARMKQ    OR IF THE RECORD IS MARKED                   
         BZ    *+10                                                             
         ZAP   DUB,TSARAFCA        USE RECORD AMOUNT                            
         CURED DUB,(L'PWIAMC,PWIAMC),FORECURT,ALIGN=LEFT,FLOAT=-                
*&&                                                                             
PAYWOI28 MVC   PWIOFF,TSAROFF                                                   
         MVC   PWINAR,SPAYNAR                                                   
         B     OVROU2E                                                          
                                                                                
PAYWOI40 L     RF,AINP             TEST QUIT PAYMENT WITHOUT UPDATING           
         CLI   TIOBAID-TIOBD(RF),PFK02                                          
         BE    PAYWOI92                                                         
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI42                                                         
         OI    PWIAMSH+(FVOIND-FVIHDR),FVOXMT                                   
         ZAP   PAYAMS,PZERO                                                     
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,PWIAMSH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,SCNDCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   PAYAMS,4(8,R1)                                                   
         CURED PAYAMS,(L'PWIAMS,PWIAMS),SCNDCURT,ALIGN=LEFT,FLOAT=-             
                                                                                
         MVC   WORK(L'CURTCUR),S.CURTCUR  SET PRIMARY FROM SECONDARY            
         MVC   WORK+L'CURTCUR(L'CURTCUR),C.CURTCUR                              
         ZAP   DUB,PAYAMS          USE SECONDARY CURRENCY VALUE                 
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         ZAP   DUB,12(8,R1)        TO SET PRIMARY CURRENCY VALUE                
         CURED DUB,(L'PWIAMT,PWIAMT),COMPCURT,ALIGN=LEFT,FLOAT=-                
         OI    PWIAMTH+(FVIIND-FVIHDR),FVITHIS                                  
         STC   R0,PWIAMTH+(FVILEN-FVIHDR)                                       
*&&                                                                             
PAYWOI42 OI    PWIAMTH+(FVOIND-FVIHDR),FVOXMT                                   
         TM    PWIAMCH+(FVATRB-FVIHDR),FVAPROT                                  
         BZ    PAYWOI46                                                         
         ZAP   PAYAMT,PZERO                                                     
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,PWIAMTH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         SR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   PAYAMT,4(8,R1)                                                   
         BNZ   PAYWOI44                                                         
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    OVROU2H                                                          
         LA    R1,PWIAMSH          SECONDARY CURRENCY AMOUNT                    
         ST    R1,FVADDR                                                        
*&&                                                                             
         B     OVROU2H                                                          
                                                                                
PAYWOI44 DS    0H                                                               
*&&UK*&& CURED PAYAMT,(L'PWIAMT,PWIAMT),COMPCURT,ALIGN=LEFT,FLOAT=-             
*&&US*&& CURED PAYAMT,(L'PWIAMT,PWIAMT),2,ALIGN=LEFT,FLOAT=-                    
         B     PAYWOI60                                                         
                                                                                
PAYWOI46 OI    PWIAMCH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,PWIAMCH                                                       
         ST    R1,FVADDR                                                        
         TM    PWIAMTH+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    PAYWOI48                                                         
         TM    PWIAMCH+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    PAYWOI48                                                         
         CLI   PWIAMTH+(FVILEN-FVIHDR),0                                        
         BE    PAYWOI48                                                         
         CLI   PWIAMCH+(FVILEN-FVIHDR),0                                        
         BE    PAYWOI48                                                         
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     OVROU2H                                                          
PAYWOI48 CLI   PWIAMTH+(FVILEN-FVIHDR),0                                        
         BNE   PAYWOI50                                                         
         CLI   PWIAMCH+(FVILEN-FVIHDR),0                                        
         BNE   PAYWOI50                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     OVROU2H                                                          
                                                                                
PAYWOI50 CLI   PWIAMTH+(FVILEN-FVIHDR),0                                        
         BE    PAYWOI54                                                         
         TM    PWIAMTH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    *+12                                                             
         CLI   PWIAMCH+(FVILEN-FVIHDR),0                                        
         BNE   PAYWOI54                                                         
         ZAP   PAYAMT,PZERO        PAYMENT AMOUNT - AGENCY                      
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,PWIAMTH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         SR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   PAYAMT,4(8,R1)                                                   
         BNZ   PAYWOI52                                                         
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    OVROU2H                                                          
         LA    R1,PWIAMSH          SECONDARY CURRENCY AMOUNT                    
         ST    R1,FVADDR                                                        
*&&                                                                             
         B     OVROU2H                                                          
                                                                                
PAYWOI52 DS    0H                                                               
*&&UK*&& CURED PAYAMT,(L'PWIAMT,PWIAMT),COMPCURT,ALIGN=LEFT,FLOAT=-             
*&&US*&& CURED PAYAMT,(L'PWIAMT,PWIAMT),2,ALIGN=LEFT,FLOAT=-                    
         ZAP   DUB,PAYAMT                                                       
*&&UK                                                                           
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,FORECURT+(CURTCUR-CURTABD)                              
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PAYAMC,DUB                                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     OVROU2H                                                          
         CURED PAYAMC,(L'PWIAMC,PWIAMC),FORECURT,ALIGN=LEFT,FLOAT=-             
*&&                                                                             
         B     PAYWOI60                                                         
                                                                                
PAYWOI54 ZAP   PAYAMC,PZERO        PAYMENT AMOUNT - LOCAL                       
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,PWIAMCH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         SR    RF,RF                                                            
         IC    RF,FORECURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   PAYAMC,4(8,R1)                                                   
         BZ    OVROU2H                                                          
*&&UK*&& CURED PAYAMC,(L'PWIAMC,PWIAMC),FORECURT,ALIGN=LEFT,FLOAT=-             
*&&US*&& CURED PAYAMC,(L'PWIAMC,PWIAMC),2,ALIGN=LEFT,FLOAT=-                    
         ZAP   DUB,PAYAMC                                                       
*&&UK                                                                           
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,FORECURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('INVERTQ+APPLYQ',EURKBLKD),DUB,DUB,0,0             
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   PAYAMT,DUB                                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     OVROU2H                                                          
         CURED PAYAMT,(L'PWIAMT,PWIAMT),COMPCURT,ALIGN=LEFT,FLOAT=-             
*&&                                                                             
PAYWOI60 OI    PWIOFFH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    PWIOFFN,PWIOFFN                                                  
         OI    PWIOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   PAYOFF,SPACES       CLEAR PAYMENT OFFICE                         
         MVC   PAYOFF,OFFICE       SAVE INVOICE OFFICE                          
         IC    R0,OFFICEXL         SAVE INVOICE OFFICE X'LENGTH                 
         TM    COMPSTAT,CPYSOROE   TEST OFFICE AGENCY                           
         BO    *+12                                                             
         CLI   TWAACCS,0           TEST LIMIT ACCESS LOGON                      
         BE    *+8                                                              
         MVI   FVMINL,1            COMPULSORY FIELD                             
         GOTO1 AVALOFF,PWIOFFH                                                  
         STC   R0,OFFICEXL         RESTORE INVOICE OFFICE X'LENGTH              
         BE    PAYWOI62            PAY OFFICE PRESENT                           
         MVC   OFFICE,PAYOFF       RESTORE INVOICE OFFICE                       
         BH    OVROU2E             ERROR                                        
         MVC   PAYOFF,SPACES       CLEAR PAYMENT OFFICE                         
         B     PAYWOI64            NOT REQUIRED - NOT INPUT                     
PAYWOI62 XC    PAYOFF,OFFICE       SAVE/RESTORE PAY/INVOICE OFFICES             
         XC    OFFICE,PAYOFF                                                    
         XC    PAYOFF,OFFICE                                                    
         MVC   PWIOFFN,RECNAME                                                  
         OI    PWIOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    PAYOFF,SPACES       SET ANYTHING < C' ' TO C' '                  
                                                                                
PAYWOI64 MVC   PAYNAR,SPACES                                                    
         XC    PAYNARL,PAYNARL                                                  
         GOTO1 AFLDVAL,PWINARH                                                  
         BNE   PAYWOI66                                                         
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   PAYNAR(0),FVIFLD                                                 
         MVC   PAYNARL,FVILEN                                                   
                                                                                
PAYWOI66 MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,PWIOFFH                                                       
         ST    R1,FVADDR                                                        
         L     RF,AINP                                                          
         CLI   TIOBAID-TIOBD(RF),PFK05                                          
         BNE   OVROU2E             REMAIN IN PAYMENT SCREEN                     
                                                                                
         OC    PAYRNUM,PAYRNUM     TEST RECORD EXISTS                           
         BZ    PAYWOI80                                                         
         TM    TSARINDS,TSARMKQ    TEST STILL MARKED                            
         BZ    PAYWOI80                                                         
         SP    TOTITEM,PONE        UNMARK TOTALS                                
         BNM   *+6                                                              
         DC    H'0'                CANNOT UNMARK MORE THAN YOU MARKED           
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
         SP    REPMRK,TSARAMNT     MUST HAVE BEEN ADDED TO MARKED               
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         SP    TOTEXDF,TMPEXDF     REMOVE EXCHANGE DIFFERENCES                  
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         SP    CTOMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
         AP    CTOBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         SP    CREMRK,TSARAFCA     MUST HAVE BEEN ADDED TO MARKED (AFC)         
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI80                                                         
         SP    STOMRK,TSARSCUA     SUBTRACT FROM MARKED                         
         AP    STOBAL,TSARSCUA     ADD TO BALANCE                               
         SP    SREMRK,TSARSCUA     MUST HAVE BEEN ADDED TO MARKED               
         SP    STOEXDF,TMPEXDFS    REMOVE EXCHANGE DIFFERENCES                  
*&&                                                                             
                                                                                
PAYWOI80 MVI   TSARLEN+1,TSARCML   ADD/UPDATE PAYMENT AND RETURN                
         XC    TSARDADR,TSARDADR   NO DISK ADDRESS                              
         MVC   TSARCON,BANKACC                                                  
         MVC   TSARDAT,CHQDATP                                                  
         MVC   TSARREF,CHQNUM                                                   
         MVI   TSARSBR,0                                                        
         MVC   TSARMOS,SBATMONP                                                 
         MVI   TSARRSTA,0                                                       
                                                                                
         MVI   TSARINDS,TSARMKQ    MARK FOR INPUT SCREEN                        
         MVC   TSAROFF,PAYOFF                                                   
         MVC   TSARBAT,SBATMON                                                  
         MVI   TSARBTY,BT36                                                     
         XC    TSARVAR,TSARVAR                                                  
         ZAP   TSARAMNT,PAYAMT                                                  
         XC    TSARFSAC,TSARFSAC                                                
         XC    TSARFWRK,TSARFWRK                                                
         XC    TSARFOTH,TSARFOTH                                                
         XC    TSARFDUE,TSARFDUE                                                
         ZAP   TSARFDIS,PZERO                                                   
*&&UK*&& ZAP   TSARFDIC,PZERO                                                   
         MVI   TSARIND2,0                                                       
         MVC   TSARADAT,TODAYC                                                  
         MVI   TSARSSTA,0                                                       
*&&US*&& XC    TSARFINV,TSARFINV                                                
*&&UK                                                                           
         ZAP   TSARAFCA,PZERO                                                   
         XC    TSARAFCC,TSARAFCC                                                
         XC    TSARAFCX,TSARAFCX                                                
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    PAYWOI82                                                         
         ZAP   TSARAFCA,PAYAMC                                                  
         MVC   TSARAFCC,FORECURT+(CURTCUR-CURTABD)                              
         MVC   TSARAFCX,ATLX                                                    
PAYWOI82 OC    S.CURTCUR,S.CURTCUR                                              
         BZ    PAYWOI84                                                         
         ZAP   TSARSCUA,PZERO                                                   
         ZAP   TSARSCUD,PZERO                                                   
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI84                                                         
         ZAP   TSARSCUA,PAYAMS     SET SECONDARY CURRENCY PAYMENT               
*&&                                                                             
PAYWOI84 MVI   TSARIND3,TSARPWOI   PAYMENT WITHOUT INVOICE                      
         MVC   SPAYNARL,PAYNARL    SAVE NARRATIVE NOW                           
         MVC   SPAYNAR,PAYNAR                                                   
         MVC   TSARDNAR,=Y(SPAYNARL-SAVED)                                      
         AP    TOTITEM,PONE        UPDATE VARIOUS TOTALS                        
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
         AP    REPMRK,TSARAMNT     ADD TO MARKED THIS SESSION                   
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         AP    TOTEXDF,TMPEXDF     ADD EXCHANGE DIFFERENCES                     
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         AP    CTOMRK,TSARAFCA     ADD TO MARKED (AFC)                          
         SP    CTOBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         AP    CREMRK,TSARAFCA     ADD TO MARKED THIS SESSION (AFC)             
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    PAYWOI86                                                         
         AP    STOMRK,TSARSCUA     ADD TO MARKED                                
         SP    STOBAL,TSARSCUA     SUBTRACT FROM BALANCE                        
         AP    SREMRK,TSARSCUA     ADD TO MARKED THIS SESSION                   
         AP    STOEXDF,TMPEXDFS    ADD EXCHANGE DIFFERENCES                     
*&&                                                                             
PAYWOI86 OC    PAYRNUM,PAYRNUM                                                  
         BNZ   PAYWOI90                                                         
         GOTO1 ATSARADD            ADD NEW TSAR RECORD                          
         BE    PAYWOI88                                                         
         L     R1,ATSARBLK                                                      
         TM    TSERRS-TSARD(R1),TSEEOF                                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AE$TMTRN)                                           
         MVI   FVOMTYP,GTMERR                                                   
         B     OVROU2X                                                          
                                                                                
PAYWOI88 OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         L     R1,ATSARBLK                                                      
         MVC   PAYRNUM,TSRNUM-TSARD(R1)                                         
         IC    R1,XTSRCNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,XTSRCNT                                                       
         B     PAYWOI92                                                         
                                                                                
PAYWOI90 L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
PAYWOI92 NI    TWAMODE3,FF-(TWAM3ASC)                                           
         GOTO1 AOVRSCR,CMSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
         GOTO1 ADISPLAY                                                         
         GOTO1 ABLDTOT,INPTOTH                                                  
         B     OVROU2E             RETURN TO INPUT SCREEN                       
         EJECT                                                                  
***********************************************************************         
* ADD/CHANGE DIFFERENCE                                               *         
***********************************************************************         
                                                                                
DIFFER   LR    R0,R1               SAVE CALL INDICATOR                          
         SR    R2,R2                                                            
*&&UK                                                                           
         USING SCUTOTS,R2                                                       
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         L     R2,ASCUTOTS                                                      
*&&                                                                             
         OC    DIFFRNUM,DIFFRNUM   TEST DIFFERENCE TSAR RECORD EXISTS           
         BZ    DIFFER04                                                         
         CLI   XTSRCNT,1           ONLY RELIABLE IF SINGLE EXTRA RECORD         
         BE    *+10                                                             
         MVC   DIFFRNUM,=H'1'      ELSE READ THROUGH TO FIND IT                 
DIFFER02 GOTO1 ATSARGET,DIFFRNUM   GET TSAR RECORD                              
         BE    *+6                                                              
         DC    H'0'                TROUBLE                                      
         TM    TSARIND3,TSARDIFF   TEST THIS IS THE RECORD                      
         BO    DIFFER04                                                         
         ICM   R1,3,DIFFRNUM                                                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,DIFFRNUM                                                    
         B     DIFFER02                                                         
*                                                                               
DIFFER04 LTR   R0,R0               TEST FIRST TIME                              
         BZ    DIFFER40                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CDSCACC),CDSCACC                                           
         MVI   GETIND,GETIABLQ+GETINLOK+GETINOSV                                
         GOTO1 AGETACC,0                                                        
         BNE   OVROU2H                                                          
         MVC   DIFFNAME,RECNAME                                                 
         MVC   DIFACCT,CDSCACC+(ACTKACT-ACTRECD)                                
         MVC   DIFACCN,DIFFNAME                                                 
         MVC   DIFSUPP,SUPPACC+(ACTKUNT-ACTRECD)                                
         MVC   DIFSUPN,ACCNAME                                                  
         MVC   DIFCHQN,CHQNUM                                                   
         GOTO1 VDATCON,DMCB,(1,CHQDATP),(17,DIFCHQD)                            
*&&UK*&& CURED CHQAMT,(L'DIFCHQA,DIFCHQA),COMPCURT,ALIGN=LEFT,FLOAT=-           
*&&US*&& CURED CHQAMT,(L'DIFCHQA,DIFCHQA),2,ALIGN=LEFT,FLOAT=-                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BO    DIFFER06                                                         
         XC    DIFCHST,DIFCHST     CONCEAL SECONDARY CURRENCY                   
         OI    DIFCHSTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    DIFAMST,DIFAMST                                                  
         OI    DIFAMSTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    DIFAMSH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    DIFAMSH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,DIFAMTH                                                       
         B     DIFFER08                                                         
                                                                                
DIFFER06 MVC   DIFCHAC,C.CURTCUR                                                
         MVC   DIFCHSC,S.CURTCUR                                                
         MVC   DIFAMTC,C.CURTCUR                                                
         MVC   DIFAMSC,S.CURTCUR                                                
         CURED CHQAMS,(L'DIFCHS,DIFCHS),SCNDCURT,ALIGN=LEFT,FLOAT=-             
                                                                                
DIFFER08 TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BO    DIFFER10                                                         
*&&                                                                             
         XC    DIFCU1T,DIFCU1T     CLEAR ALL CURRENCY FIELDS                    
         OI    DIFCU1TH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    DIFRATT,DIFRATT                                                  
         OI    DIFRATTH+(FVOIND-FVIHDR),FVOXMT                                  
         XC    DIFCU2T,DIFCU2T                                                  
         OI    DIFCU2TH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    DIFAMCH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    DIFAMCH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,DIFAMTH                                                       
         B     DIFFER12                                                         
*&&UK                                                                           
DIFFER10 DS    0H                                                               
         CURED CHQAMC,(L'DIFCHQC,DIFCHQC),FORECURT,ALIGN=LEFT,FLOAT=-           
         MVC   DIFCUC1,FORECURT+(CURTCUR-CURTABD)                               
         MVC   DIFCUC2,FORECURT+(CURTCUR-CURTABD)                               
         GOTO1 AEDTRAT,DMCB,ATLXRATE,(L'DIFRAT,DIFRAT)                          
         LA    R1,DIFRAT+L'DIFRAT                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   2(L'CURTCUR,R1),FORECURT+(CURTCUR-CURTABD)                       
         MVI   2+L'CURTCUR(R1),C'-'                                             
         MVC   L'CURTCUR+3(L'CURTCUR,R1),COMPCURT+(CURTCUR-CURTABD)             
         LA    R1,DIFAMCH                                                       
*&&                                                                             
DIFFER12 DS    0H                                                               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    *+8                                                              
         LA    R1,DIFAMSH          SET THAT FIELD FOR INPUT                     
*&&                                                                             
         ST    R1,FVADDR                                                        
         MVI   FVOMTYP,GTMINF                                                   
         OC    DIFFRNUM,DIFFRNUM   TEST DIFFERENCE EXISTS                       
         BNZ   DIFFER20                                                         
                                                                                
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         ZAP   DUB,TOTBAL          AIM TO ELIMINATE CURRENT BALANCE             
         BNZ   DIFFER14                                                         
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER14                                                         
         CP    STOBAL,PZERO        TEST SECONDARY CURRENCY BALANCE              
         BE    DIFFER18                                                         
*&&                                                                             
DIFFER14 DS    0H                                                               
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,TOTEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
*&&                                                                             
*&&UK*&& CURED DUB,(L'DIFAMT,DIFAMT),COMPCURT,ALIGN=LEFT,FLOAT=-                
*&&US*&& CURED DUB,(L'DIFAMT,DIFAMT),2,ALIGN=LEFT,FLOAT=-                       
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER16                                                         
         ZAP   DUB,STOBAL          AIM TO ELIMINATE CURRENT BALANCE             
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,STOEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
         CURED DUB,(L'DIFAMS,DIFAMS),SCNDCURT,ALIGN=LEFT,FLOAT=-                
                                                                                
DIFFER16 TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    DIFFER18                                                         
         CURED CTOBAL,(L'DIFAMC,DIFAMC),FORECURT,ALIGN=LEFT,FLOAT=-             
*&&                                                                             
DIFFER18 MVC   DIFOFF,OFFICE                                                    
         CLI   TWAACCS,C'*'        TEST SINGLE OFFICE LIMIT ACCESS              
         BNE   *+14                                                             
         MVC   DIFOFF(1),TWAACCS+1                                              
         MVI   DIFOFF+1,C' '                                                    
         MVCDD DIFNAR,AC#DFRNC,LL                                               
         B     OVROU2E                                                          
                                                                                
DIFFER20 MVC   FVMSGNO,=AL2(AI$RDECH)                                           
         ZAP   DUB,TOTBAL          AIM TO ELIMINATE CURRENT BALANCE             
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,TOTEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BNZ   *+14                ZERO OK IN ONE OF PRIMARY/SECONDARY          
*&&                                                                             
         CP    DUB,PZERO           BUT IF BALANCE IS ZERO                       
         BE    *+12                                                             
         TM    TSARINDS,TSARMKQ    OR IF THE RECORD IS MARKED                   
         BZ    *+10                                                             
         ZAP   DUB,TSARAMNT        USE RECORD AMOUNT                            
*&&UK*&& CURED DUB,(L'DIFAMT,DIFAMT),COMPCURT,ALIGN=LEFT,FLOAT=-                
*&&US*&& CURED DUB,(L'DIFAMT,DIFAMT),2,ALIGN=LEFT,FLOAT=-                       
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER22                                                         
         ZAP   DUB,STOBAL          AIM TO ELIMINATE CURRENT BALANCE             
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    *+10                                                             
         SP    DUB,STOEXDF         ADJUST FOR EXCHANGE DIFFERENCE               
         TM    TSARINDS,TSARMKQ    BUT IF THE RECORD IS MARKED                  
         BZ    *+10                                                             
         ZAP   DUB,TSARSCUA        USE RECORD AMOUNT                            
         CURED DUB,(L'DIFAMS,DIFAMS),SCNDCURT,ALIGN=LEFT,FLOAT=-                
                                                                                
DIFFER22 TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    DIFFER24                                                         
         ZAP   DUB,CTOBAL          AIM TO ELIMINATE CURRENT BALANCE             
         BZ    *+12                BUT IF BALANCE IS ZERO                       
         TM    TSARINDS,TSARMKQ    OR IF THE RECORD IS MARKED                   
         BZ    *+10                                                             
         ZAP   DUB,TSARAFCA        USE RECORD AMOUNT                            
         CURED DUB,(L'DIFAMC,DIFAMC),FORECURT,ALIGN=LEFT,FLOAT=-                
*&&                                                                             
DIFFER24 MVC   DIFOFF,TSAROFF                                                   
         MVC   DIFNAR,SDIFNAR                                                   
         B     OVROU2E                                                          
                                                                                
DIFFER40 L     RF,AINP             TEST QUIT DIFFERENCE W/O UPDATING            
         CLI   TIOBAID-TIOBD(RF),PFK02                                          
         BE    DIFFER92                                                         
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER42                                                         
         OI    DIFAMSH+(FVOIND-FVIHDR),FVOXMT                                   
         ZAP   DIFFAMS,PZERO                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,DIFAMSH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,SCNDCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   DIFFAMS,4(8,R1)                                                  
         CURED DIFFAMS,(L'DIFAMS,DIFAMS),SCNDCURT,ALIGN=LEFT,FLOAT=-            
                                                                                
         TM    DIFAMTH+(FVIIND-FVIHDR),FVITHIS  IF PRIMARY TOUCHED              
         BO    *+12                                                             
         TM    DIFAMSH+(FVIIND-FVIHDR),FVITHIS  OR SECONDARY TOUCHED            
         BZ    DIFFER42                                                         
         MVC   WORK(L'CURTCUR),S.CURTCUR  SET PRIMARY FROM SECONDARY            
         MVC   WORK+L'CURTCUR(L'CURTCUR),C.CURTCUR                              
         ZAP   DUB,DIFFAMS         USE SECONDARY CURRENCY VALUE                 
         GOTO1 VCASHVAL,DMCB,(X'80',DUB),(X'28',0),WORK,0,0,0                   
         ZAP   DUB,12(8,R1)        TO SET PRIMARY CURRENCY VALUE                
         CURED DUB,(L'DIFAMT,DIFAMT),COMPCURT,ALIGN=LEFT,FLOAT=-                
         OI    DIFAMTH+(FVIIND-FVIHDR),FVITHIS                                  
         STC   R0,DIFAMTH+(FVILEN-FVIHDR)                                       
*&&                                                                             
DIFFER42 OI    DIFAMTH+(FVOIND-FVIHDR),FVOXMT                                   
         TM    DIFAMCH+(FVATRB-FVIHDR),FVAPROT                                  
         BZ    DIFFER46                                                         
         ZAP   DIFFAMT,PZERO                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,DIFAMTH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         SR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   DIFFAMT,4(8,R1)                                                  
         BNZ   DIFFER44            NON-ZERO DIFFAMT                             
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    OVROU2H                                                          
         CP    DIFFAMS,PZERO       AND IF THAT'S ZERO - ERROR                   
         BNE   DIFFER44                                                         
         LA    R1,DIFAMSH          SECONDARY CURRENCY AMOUNT                    
         ST    R1,FVADDR                                                        
*&&                                                                             
         B     OVROU2H                                                          
                                                                                
DIFFER44 DS    0H                  ONE OF PRIMARY/SECONDARY NON-ZERO            
*&&UK*&& CURED DIFFAMT,(L'DIFAMT,DIFAMT),COMPCURT,ALIGN=LEFT,FLOAT=-            
*&&US*&& CURED DIFFAMT,(L'DIFAMT,DIFAMT),2,ALIGN=LEFT,FLOAT=-                   
         SR    RE,RE                                                            
         ICM   RE,1,PROFMXDF                                                    
         BZ    DIFFER60                                                         
         CVD   RE,DUB1                                                          
         ZAP   DUB2,DIFFAMT                                                     
         NI    DUB2+7,X'FE'        FORCE AMOUNT POSITIVE                        
         CP    DUB2,DUB1                                                        
         BNH   DIFFER60                                                         
         MVC   FVMSGNO,=AL2(AE$AMTHI)                                           
         B     OVROU2H             AMOUNT TOO HIGH                              
                                                                                
DIFFER46 OI    DIFAMCH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R1,DIFAMCH                                                       
         ST    R1,FVADDR                                                        
         TM    DIFAMTH+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    DIFFER48                                                         
         TM    DIFAMCH+(FVIIND-FVIHDR),FVITHIS                                  
         BZ    DIFFER48                                                         
         CLI   DIFAMTH+(FVILEN-FVIHDR),0                                        
         BE    DIFFER48                                                         
         CLI   DIFAMCH+(FVILEN-FVIHDR),0                                        
         BE    DIFFER48                                                         
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     OVROU2H                                                          
DIFFER48 CLI   DIFAMTH+(FVILEN-FVIHDR),0                                        
         BNE   DIFFER50                                                         
         CLI   DIFAMCH+(FVILEN-FVIHDR),0                                        
         BNE   DIFFER50                                                         
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     OVROU2H                                                          
                                                                                
DIFFER50 CLI   DIFAMTH+(FVILEN-FVIHDR),0                                        
         BE    DIFFER56                                                         
         TM    DIFAMTH+(FVIIND-FVIHDR),FVITHIS                                  
         BO    *+12                                                             
         CLI   DIFAMCH+(FVILEN-FVIHDR),0                                        
         BNE   DIFFER56                                                         
         ZAP   DIFFAMT,PZERO       DIFFERENCE AMOUNT - AGENCY                   
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,DIFAMTH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         SR    RF,RF                                                            
         IC    RF,COMPCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   DIFFAMT,4(8,R1)                                                  
         BNZ   DIFFER52            NON-ZERO DIFFAMT                             
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    OVROU2H                                                          
         CP    DIFFAMS,PZERO       AND IF THAT'S ZERO - ERROR                   
         BNE   DIFFER52                                                         
         LA    R1,DIFAMSH          SECONDARY CURRENCY AMOUNT                    
         ST    R1,FVADDR                                                        
*&&                                                                             
         B     OVROU2H                                                          
                                                                                
DIFFER52 DS    0H                  ONE OF PRIMARY/SECONDARY NON-ZERO            
*&&UK*&& CURED DIFFAMT,(L'DIFAMT,DIFAMT),COMPCURT,ALIGN=LEFT,FLOAT=-            
*&&US*&& CURED DIFFAMT,(L'DIFAMT,DIFAMT),2,ALIGN=LEFT,FLOAT=-                   
         SR    RE,RE                                                            
         ICM   RE,1,PROFMXDF                                                    
         BZ    DIFFER54                                                         
         CVD   RE,DUB1                                                          
         ZAP   DUB2,DIFFAMT                                                     
         NI    DUB2+7,X'FE'        FORCE AMOUNT POSITIVE                        
         CP    DUB2,DUB1                                                        
         BNH   DIFFER54                                                         
         MVC   FVMSGNO,=AL2(AE$AMTHI)                                           
         B     OVROU2H             AMOUNT TOO HIGH                              
DIFFER54 ZAP   DUB,DIFFAMT                                                      
*&&UK                                                                           
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,FORECURT+(CURTCUR-CURTABD)                              
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DIFFAMC,DUB                                                      
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     OVROU2H                                                          
         CURED DIFFAMC,(L'DIFAMC,DIFAMC),FORECURT,ALIGN=LEFT,FLOAT=-            
*&&                                                                             
         B     DIFFER60                                                         
                                                                                
DIFFER56 ZAP   DIFFAMC,PZERO       DIFFERENCE AMOUNT - LOCAL                    
         MVI   FVMINL,1                                                         
         GOTO1 AFLDVAL,DIFAMCH                                                  
         BNE   OVROU2X                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         SR    RF,RF                                                            
         IC    RF,FORECURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),FVIFLD),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   OVROU2X             INVALID AMOUNT                               
         ZAP   DIFFAMC,4(8,R1)                                                  
         BZ    OVROU2H                                                          
*&&UK*&& CURED DIFFAMC,(L'DIFAMC,DIFAMC),FORECURT,ALIGN=LEFT,FLOAT=-            
*&&US*&& CURED DIFFAMC,(L'DIFAMC,DIFAMC),2,ALIGN=LEFT,FLOAT=-                   
         ZAP   DUB,DIFFAMC                                                      
*&&UK                                                                           
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,FORECURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKRULE,ATLX       EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('INVERTQ+APPLYQ',EURKBLKD),DUB,DUB,0,0             
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   DIFFAMT,DUB                                                      
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     OVROU2H                                                          
         CURED DIFFAMT,(L'DIFAMT,DIFAMT),COMPCURT,ALIGN=LEFT,FLOAT=-            
         SR    RE,RE                                                            
         ICM   RE,1,PROFMXDF                                                    
         BZ    DIFFER60                                                         
         CVD   RE,DUB1                                                          
         ZAP   DUB2,DIFFAMT                                                     
         NI    DUB2+7,X'FE'        FORCE AMOUNT POSITIVE                        
         CP    DUB2,DUB1                                                        
         BNH   DIFFER60                                                         
         MVC   FVMSGNO,=AL2(AE$AMTHI)                                           
         B     OVROU2H             AMOUNT TOO HIGH                              
*&&                                                                             
DIFFER60 OI    DIFOFFH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    DIFOFFN,DIFOFFN                                                  
         OI    DIFOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         MVC   DIFFOFF,SPACES      CLEAR DIFFERENCE OFFICE                      
         MVC   DIFFOFF,OFFICE      SAVE INVOICE OFFICE                          
         IC    R0,OFFICEXL         SAVE INVOICE OFFICE X'LENGTH                 
         TM    COMPSTAT,CPYSOROE   TEST OFFICE AGENCY                           
         BO    *+12                                                             
         CLI   TWAACCS,0           TEST LIMIT ACCESS LOGON                      
         BE    *+8                                                              
         MVI   FVMINL,1            COMPULSORY FIELD                             
         GOTO1 AVALOFF,DIFOFFH                                                  
         STC   R0,OFFICEXL         RESTORE INVOICE OFFICE X'LENGTH              
         BE    DIFFER62            DIFFERENCE OFFICE PRESENT                    
         MVC   OFFICE,DIFFOFF      RESTORE INVOICE OFFICE                       
         BH    OVROU2E             ERROR                                        
         MVC   DIFFOFF,SPACES      CLEAR DIFFERENCE OFFICE                      
         B     DIFFER64            NOT REQUIRED - NOT INPUT                     
DIFFER62 XC    DIFFOFF,OFFICE      SAVE/RESTORE DIFF/INVOICE OFFICES            
         XC    OFFICE,DIFFOFF                                                   
         XC    DIFFOFF,OFFICE                                                   
         MVC   DIFOFFN,RECNAME                                                  
         OI    DIFOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         OC    DIFFOFF,SPACES      SET ANYTHING < C' ' TO C' '                  
                                                                                
                                                                                
DIFFER64 MVC   DIFFNAR,SPACES                                                   
         XC    DIFFNARL,DIFFNARL                                                
         GOTO1 AFLDVAL,DIFNARH                                                  
         BNE   DIFFER66                                                         
         IC    R1,FVXLEN                                                        
         EX    R1,*+4                                                           
         MVC   DIFFNAR(0),FVIFLD                                                
         MVC   DIFFNARL,FVILEN                                                  
                                                                                
DIFFER66 MVC   FVMSGNO,=AL2(AI$ACTOK)                                           
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,DIFOFFH                                                       
         ST    R1,FVADDR                                                        
         L     RF,AINP                                                          
         CLI   TIOBAID-TIOBD(RF),PFK05                                          
         BNE   OVROU2E             REMAIN IN DIFFERENCE SCREEN                  
                                                                                
         OC    DIFFRNUM,DIFFRNUM   TEST RECORD EXISTS                           
         BZ    DIFFER80                                                         
         TM    TSARINDS,TSARMKQ    TEST STILL MARKED                            
         BZ    DIFFER80                                                         
         SP    TOTITEM,PONE        UNMARK TOTALS                                
         BNM   *+6                                                              
         DC    H'0'                CANNOT UNMARK MORE THAN YOU MARKED           
         SP    TOTMRK,TSARAMNT     SUBTRACT FROM MARKED                         
         AP    TOTBAL,TSARAMNT     ADD TO BALANCE                               
         SP    REPMRK,TSARAMNT     MUST HAVE BEEN ADDED TO MARKED               
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         SP    TOTEXDF,TMPEXDF     REMOVE EXCHANGE DIFFERENCES                  
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         SP    CTOMRK,TSARAFCA     SUBTRACT FROM MARKED (AFC)                   
         AP    CTOBAL,TSARAFCA     ADD TO BALANCE (AFC)                         
         SP    CREMRK,TSARAFCA     MUST HAVE BEEN ADDED TO MARKED (AFC)         
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER80                                                         
         SP    STOMRK,TSARSCUA     SUBTRACT FROM MARKED                         
         AP    STOBAL,TSARSCUA     ADD TO BALANCE                               
         SP    SREMRK,TSARSCUA     MUST HAVE BEEN ADDED TO MARKED               
         SP    STOEXDF,TMPEXDFS    REMOVE EXCHANGE DIFFERENCES                  
*&&                                                                             
DIFFER80 MVI   TSARLEN+1,TSARCML   ADD/UPDATE DIFFERENCE AND RETURN             
         XC    TSARDADR,TSARDADR   NO DISK ADDRESS                              
         MVC   TSARCON,CDSCACC                                                  
         MVC   TSARDAT,CHQDATP                                                  
         MVC   TSARREF,CHQNUM                                                   
         MVI   TSARSBR,0                                                        
         MVC   TSARMOS,SBATMONP                                                 
         MVI   TSARRSTA,0                                                       
                                                                                
         MVI   TSARINDS,TSARMKQ    MARK FOR INPUT SCREEN                        
         MVC   TSAROFF,DIFFOFF                                                  
         MVC   TSARBAT,SBATMON                                                  
         MVI   TSARBTY,BT36                                                     
         XC    TSARVAR,TSARVAR                                                  
         ZAP   TSARAMNT,DIFFAMT                                                 
         XC    TSARFSAC,TSARFSAC                                                
         XC    TSARFWRK,TSARFWRK                                                
         XC    TSARFOTH,TSARFOTH                                                
         XC    TSARFDUE,TSARFDUE                                                
         ZAP   TSARFDIS,PZERO                                                   
*&&UK*&& ZAP   TSARFDIC,PZERO                                                   
         MVI   TSARIND2,0                                                       
         MVC   TSARADAT,TODAYC                                                  
         MVI   TSARSSTA,0                                                       
*&&US*&& XC    TSARFINV,TSARFINV                                                
*&&UK                                                                           
         ZAP   TSARAFCA,PZERO                                                   
         XC    TSARAFCC,TSARAFCC                                                
         XC    TSARAFCX,TSARAFCX                                                
         TM    SAFCIND1,SAFCI1SC   TEST USING SINGLE FOREIGN CURRENCY           
         BZ    DIFFER82                                                         
         ZAP   TSARAFCA,DIFFAMC                                                 
         MVC   TSARAFCC,FORECURT+(CURTCUR-CURTABD)                              
         MVC   TSARAFCX,ATLX                                                    
DIFFER82 OC    S.CURTCUR,S.CURTCUR                                              
         BZ    DIFFER84                                                         
         ZAP   TSARSCUA,PZERO                                                   
         ZAP   TSARSCUD,PZERO                                                   
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER84                                                         
         ZAP   TSARSCUA,DIFFAMS    SET SECONDARY CURRENCY DIFFERENCE            
*&&                                                                             
DIFFER84 MVI   TSARIND3,TSARDIFF   DIFFERENCE                                   
         MVC   SDIFNARL,DIFFNARL   SAVE NARRATIVE NOW                           
         MVC   SDIFNAR,DIFFNAR                                                  
         MVC   TSARDNAR,=Y(SDIFNARL-SAVED)                                      
         AP    TOTITEM,PONE        UPDATE VARIOUS TOTALS                        
         AP    TOTMRK,TSARAMNT     ADD TO MARKED                                
         SP    TOTBAL,TSARAMNT     SUBTRACT FROM BALANCE                        
         AP    REPMRK,TSARAMNT     ADD TO MARKED THIS SESSION                   
*&&UK                                                                           
         ZAP   SAVROUND,PZERO                                                   
         ZAP   SAVROUNS,PZERO                                                   
         GOTO1 AEXCHDF                                                          
         AP    TOTEXDF,TMPEXDF     ADD EXCHANGE DIFFERENCES                     
         ZAP   CTOEXDF,TOTEXDF     KEEP IN LINE                                 
         AP    CTOMRK,TSARAFCA     ADD TO MARKED (AFC)                          
         SP    CTOBAL,TSARAFCA     SUBTRACT FROM BALANCE (AFC)                  
         AP    CREMRK,TSARAFCA     ADD TO MARKED THIS SESSION (AFC)             
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    DIFFER86                                                         
         AP    STOMRK,TSARSCUA     ADD TO MARKED                                
         SP    STOBAL,TSARSCUA     SUBTRACT FROM BALANCE                        
         AP    SREMRK,TSARSCUA     ADD TO MARKED THIS SESSION                   
         AP    STOEXDF,TMPEXDFS    ADD EXCHANGE DIFFERENCES                     
*&&                                                                             
DIFFER86 OC    DIFFRNUM,DIFFRNUM                                                
         BNZ   DIFFER90                                                         
         GOTO1 ATSARADD            ADD NEW TSAR RECORD                          
         BE    DIFFER88                                                         
         L     R1,ATSARBLK                                                      
         TM    TSERRS-TSARD(R1),TSEEOF                                          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(AE$TMTRN)                                           
         MVI   FVOMTYP,GTMERR                                                   
         B     OVROU2X                                                          
                                                                                
DIFFER88 OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE BIT                         
         L     R1,ATSARBLK                                                      
         MVC   DIFFRNUM,TSRNUM-TSARD(R1)                                        
         IC    R1,XTSRCNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,XTSRCNT                                                       
         B     DIFFER92                                                         
                                                                                
DIFFER90 L     RF,ATSARBLK         PUT CHANGED RECORD BACK TO TSAR              
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DIFFER92 NI    TWAMODE3,FF-(TWAM3ASC)                                           
         GOTO1 AOVRSCR,CMSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    DISIND,DISIRST      SET START FROM BEGINNING                     
         GOTO1 ABLDBAL,INPACCH     BUILD ACCOUNT CODE/NAME/BALANCE LINE         
         GOTO1 ADISPLAY                                                         
         GOTO1 ABLDTOT,INPTOTH                                                  
         B     OVROU2E             RETURN TO INPUT SCREEN                       
         EJECT                                                                  
***********************************************************************         
* BUILD A NEW TRANSACTION IN IOBUFF1                                  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
BLDNEW   L     R2,AIOBUFF1         R2=A(TRANSACTION RECORD)                     
                                                                                
         MVC   TRNKEY,SPACES       BUILD TRANSACTION KEY                        
         MVC   TRNKCULA,SUPPACC                                                 
         MVC   TRNKCULC,TSARCON                                                 
         MVC   TRNKDATE,TSARDAT                                                 
         MVC   TRNKREF,TSARREF                                                  
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         LA    R4,TRNRFST          R4=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R4                                                        
         MVC   TRNELD(L'OVTRNEL),OVTRNEL                                        
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVC   TRNSUB,TRNKSBR                                                   
         ZAP   TRNAMNT,PZERO                                                    
         TM    TSARIND3,TSARDIFF   CARRY DIFFERENCE IN A SCIEL                  
         BO    *+10                                                             
         ZAP   TRNAMNT,TSARAMNT    PAYAMNT                                      
         MVC   TRNOFFC,TSAROFF                                                  
         SR    R1,R1                                                            
         TM    TSARIND3,TSARPWOI   TEST PAYMENT WITHOUT INVOICE                 
         BZ    BLDNEW02                                                         
         ICM   R1,1,SPAYNARL                                                    
         BZ    BLDNEW06                                                         
         LA    RF,SPAYNAR                                                       
         B     BLDNEW04                                                         
BLDNEW02 TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BO    *+6                                                              
         DC    H'0'                UNKNOWN EXTRA RECORD                         
         ICM   R1,1,SDIFNARL                                                    
         BZ    BLDNEW06                                                         
         LA    RF,SDIFNAR                                                       
BLDNEW04 BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   TRNNARR(0),0(RF)                                                 
         LA    R1,TRNLN1Q+1(R1)    ADD FIXED PORTION +1                         
         STC   R1,TRNLN                                                         
BLDNEW06 SR    R0,R0                                                            
         IC    R0,TRNLN                                                         
         AR    R4,R0               R4=A(NEXT ELEMENT)                           
                                                                                
         USING SCIELD,R4                                                        
         TM    TSARIND3,TSARDIFF   IF DIFFERENCE POSTING                        
         BZ    BLDNEW08                                                         
         XC    SCIELD(SCILN1Q),SCIELD                                           
         MVI   SCIEL,SCIELQ        BUILD SUBSIDIARY CASH ELEMENT                
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,PZERO                                                    
         SP    SCIAMNT,TSARAMNT    DIFFAMT                                      
         SR    R0,R0                                                            
         IC    R0,SCILN                                                         
         AR    R4,R0               R4=A(NEXT ELEMENT)                           
                                                                                
         USING TRSELD,R4                                                        
BLDNEW08 MVC   TRSELD(TRSLNQ),OVTRSEL                                           
         XC    TRSUDAT,TRSUDAT     RESET USED DATE                              
         XC    TRSUMOS,TRSUMOS     RESET USED MOS                               
         SR    R0,R0                                                            
         IC    R0,TRSLN                                                         
         AR    R4,R0               R4=A(NEXT ELEMENT)                           
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC   TEST CURRENCY VALUES ESTABLISHED             
         BZ    BLDNEW10                                                         
         USING AFCELD,R4                                                        
         XC    AFCELD(AFCLNQ),AFCELD                                            
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         MVC   AFCCURR,TSARAFCC                                                 
         MVC   AFCX,TSARAFCX                                                    
         ZAP   AFCAMNT,TSARAFCA    PAYAMC/DIFFAMC                               
         TM    TSARIND3,TSARDIFF                                                
         BZ    *+10                                                             
         MP    AFCAMNT,PONENEG     REVERSE AMOUNT                               
         SR    R0,R0                                                            
         IC    R0,AFCLN                                                         
         AR    R4,R0               R4=A(NEXT ELEMENT)                           
*&&                                                                             
BLDNEW10 MVI   0(R4),0             SET EOR                                      
         LA    R4,1(R4)                                                         
         SR    R4,R2                                                            
         STCM  R4,3,TRNRLEN        SET RECORD LENGTH                            
         GOTO1 ASETELAD,AIOBUFF1                                                
         B     OVROU2E                                                          
                                                                                
         DROP  R2,R4                                                            
         EJECT                                                                  
                                                                                
         DROP  R5                  CANNOT ADDRESS SOVRWRK                       
                                                                                
***********************************************************************         
* INSERT ANY FURTHER OVROU2 ROUTINES ABOVE THIS POINT                 *         
* R5 REUSED IN ADDTRP - SOVRWRK NOT ADDRESSABLE BEYOND THIS POINT     *         
***********************************************************************         
                                                                                
***********************************************************************         
* ADD TRANSACTION POINTER ELEMENTS IF NECESSARY                       *         
* NTRY - P1=A(IOBUFF2), P2=A(OFFTAB ENTRY), P3=A(TRNEL)               *         
***********************************************************************         
                                                                                
ADDTRP   LM    R2,R4,0(R1)                                                      
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         USING OFFTABD,R3          R3=A(OFFTAB ENTRY)                           
         USING TRNELD,R4           R4=A(TRNEL)                                  
*&&US                                                                           
         GOTO1 VHELLO,DMCB,(C'D',OVACCMST),('TRPELQ',TRNRECD),0,0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*&&UK                                                                           
         LA    R5,TRNRFST-TRNRECD(R2)                                           
         USING TRPELD,R5                                                        
ADDTRP02 CLI   TRPEL,0                                                          
         BE    ADDTRP06                                                         
         CLI   TRPEL,TRPELQ                                                     
         BE    ADDTRP04                                                         
         SR    R1,R1                                                            
         IC    R1,TRPLN                                                         
         AR    R5,R1                                                            
         B     ADDTRP02                                                         
ADDTRP04 GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),TRNRECD,ACOM,0,TRPELD,0             
         B     ADDTRP02                                                         
         DROP  R5                                                               
*&&                                                                             
ADDTRP06 MVI   OVBYTE,0                                                         
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN                                                     
         LA    RF,TRNRECD(RF)                                                   
         BCTR  RF,0                                                             
         CLI   0(RF),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRPELD,RF                                                        
         CP    OFFADVC,PZERO       TEST ADVANCE                                 
         BE    ADDTRP14                                                         
         MVI   TRPEL,TRPELQ                                                     
         MVI   TRPSTAT,0           ALWAYS A CREDIT                              
         MVI   TRPTYPE,TRPTADV     ADVANCE PAYMENT                              
         ZAP   TRPAMNT,OFFADVC     ADVANCE AMOUNT                               
         MP    TRPAMNT,PONENEG     REVERSE SIGN                                 
         SP    TRPAMNT,OFFADEX     MINUS NEGATIVE EXCHANGE DIFF                 
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDTRP08                                                         
         IC    R1,OVBYTE           BUMP OCAEL SEQUENCE NUMBER                   
         LA    R1,1(R1)                                                         
         STC   R1,OVBYTE                                                        
         L     R1,AOCANXT          GET A(NEXT OCA VALUE)                        
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRPAMNT                                                
         MVC   OCANSEQN,OVBYTE                                                  
         ZAP   OCANCASH(L'TRPAMNT),OFFADVCS  ADVANCE AMOUNT                     
         MP    OCANCASH(L'TRPAMNT),PONENEG   REVERSE SIGN                       
         SP    OCANCASH(L'TRPAMNT),OFFADEXS  MINUS NEGATIVE EXCH DIFF           
         LA    R1,OCANTR1L+L'TRPAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         ST    R1,AOCANXT          SET A(NEXT OCA VALUE)                        
*&&                                                                             
ADDTRP08 MVC   TRPUL,SUPP+(ACTKUNT-ACTRECD)                                     
         LA    R1,SUPP+(ACTKACT-ACTRECD)                                        
         LA    RE,SUPP+(L'SUPP-1)                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1               RE=EXECUTE L'ACCOUNT                         
         EX    RE,*+4                                                           
         MVC   TRPACT(0),0(R1)                                                  
         SR    R1,R1                                                            
         ICM   R1,1,SUPPOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    *+14                                                             
         LA    R1,TRPACT-1(R1)                                                  
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         LA    RE,TRPLN1Q+1(RE)                                                 
         STC   RE,TRPLN            SET L'ELEMENT                                
         AR    RF,RE               RF=A(NEXT ELEMENT)                           
         CP    OFFADEX,PZERO       TEST ADVANCE EXCHANGE DIFFERENCE             
         BE    ADDTRP14                                                         
         MVI   TRPEL,TRPELQ                                                     
         MVI   TRPSTAT,0           VENDOR -CR                                   
         MVI   TRPTYPE,TRPTAXV     ADVANCE EXCHANGE DIFFERENCE - VENDOR         
         ZAP   TRPAMNT,OFFADEX     ADVANCE EXCHANGE DIFFERENCE AMOUNT           
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDTRP10                                                         
         IC    R1,OVBYTE           BUMP OCAEL SEQUENCE NUMBER                   
         LA    R1,1(R1)                                                         
         STC   R1,OVBYTE                                                        
         L     R1,AOCANXT          GET A(NEXT OCA VALUE)                        
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRPAMNT                                                
         MVC   OCANSEQN,OVBYTE                                                  
         ZAP   OCANCASH(L'TRPAMNT),OFFADEXS  ADVANCE EX DIFF AMOUNT             
         LA    R1,OCANTR1L+L'TRPAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         ST    R1,AOCANXT          SET A(NEXT OCA VALUE)                        
*&&                                                                             
ADDTRP10 MVC   TRPUL,SUPP+(ACTKUNT-ACTRECD)                                     
         LA    R1,SUPP+(ACTKACT-ACTRECD)                                        
         LA    RE,SUPP+(L'SUPP-1)                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1               RE=EXECUTE L'ACCOUNT                         
         EX    RE,*+4                                                           
         MVC   TRPACT(0),0(R1)                                                  
         SR    R1,R1                                                            
         ICM   R1,1,SUPPOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    *+14                                                             
         LA    R1,TRPACT-1(R1)                                                  
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         LA    RE,TRPLN1Q+1(RE)                                                 
         STC   RE,TRPLN            SET L'ELEMENT                                
         AR    RF,RE               RF=A(NEXT ELEMENT)                           
         MVI   TRPEL,TRPELQ                                                     
         MVI   TRPTYPE,TRPTAXX     ADVANCE EXCHANGE DIFFERENCE - EXDF           
         ZAP   TRPAMNT,OFFADEX     ADVANCE EXCHANGE DIFFERENCE                  
         MVC   TRPUL,EXDF+(ACTKUNT-ACTRECD)                                     
         MVI   TRPSTAT,0           EXCHANGE DIFFERENCE A/C CREDIT               
         CLI   TRPLDG,C'E'         TEST POSTING TO SE                           
         BE    *+14                                                             
         MP    TRPAMNT,PONENEG     NO - REVERSE SIGN TO POST +CR                
         B     *+8                                                              
         MVI   TRPSTAT,TRPSDR      SE POST -DR                                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDTRP12                                                         
         IC    R1,OVBYTE           BUMP OCAEL SEQUENCE NUMBER                   
         LA    R1,1(R1)                                                         
         STC   R1,OVBYTE                                                        
         L     R1,AOCANXT          GET A(NEXT OCAEL VALUE)                      
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRPAMNT                                                
         MVC   OCANSEQN,OVBYTE                                                  
         ZAP   OCANCASH(L'TRPAMNT),OFFADEXS  ADVANCE EX DIFF AMOUNT             
         CLI   TRPLDG,C'E'         TEST POSTING TO SE                           
         BE    *+10                                                             
         MP    OCANCASH(L'TRPAMNT),PONENEG  NO - REV SIGN TO POST +CR           
         LA    R1,OCANTR1L+L'TRPAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         ST    R1,AOCANXT                                                       
*&&                                                                             
ADDTRP12 LA    R1,EXDF+(ACTKACT-ACTRECD)                                        
         LA    RE,EXDF+(L'EXDF-1)                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1               RE=EXECUTE L'ACCOUNT                         
         EX    RE,*+4                                                           
         MVC   TRPACT(0),0(R1)                                                  
         SR    R1,R1                                                            
         ICM   R1,1,EXDFOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    *+14                                                             
         LA    R1,TRPACT-1(R1)                                                  
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         LA    RE,TRPLN1Q+1(RE)                                                 
         STC   RE,TRPLN            SET L'ELEMENT                                
         AR    RF,RE               RF=A(NEXT ELEMENT)                           
*                                                                               
ADDTRP14 CP    OFFDIFF,PZERO       TEST DIFFERENCE                              
         BNE   *+14                                                             
         CP    OFFDIFFS,PZERO      TEST DIFFERENCE IN SECOND CURRENCY           
         BE    ADDTRP24            SKIP DIFFERENCE & TAX ADJUSTMENT             
         MVI   TRPEL,TRPELQ                                                     
         MVI   TRPTYPE,TRPTDIF     DIFFERENCE                                   
         ZAP   TRPAMNT,OFFDIFF     FOR AMOUNT OF THE DIFFERENCE                 
         MVC   TRPUL,CDSC+(ACTKUNT-ACTRECD)                                     
         MVI   TRPSTAT,0           ASSUME A CREDIT                              
         CLI   TRPLDG,C'E'         TEST POSTING TO SE                           
         BNE   *+14                                                             
         MVI   TRPSTAT,TRPSDR      SE POST MINUS DEBIT                          
         MP    TRPAMNT,PONENEG                                                  
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDTRP16                                                         
         IC    R1,OVBYTE           BUMP OCAEL SEQUENCE NUMBER                   
         LA    R1,1(R1)                                                         
         STC   R1,OVBYTE                                                        
         L     R1,AOCANXT          GET A(NEXT OCAEL VALUE)                      
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRPAMNT                                                
         MVC   OCANSEQN,OVBYTE                                                  
         ZAP   OCANCASH(L'TRPAMNT),OFFDIFFS  DIFFERENCE AMOUNT                  
         CLI   TRPLDG,C'E'         TEST POSTING TO SE                           
         BNE   *+10                                                             
         MP    OCANCASH(L'TRPAMNT),PONENEG  SE POST MINUS DEBIT                 
         LA    R1,OCANTR1L+L'TRPAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         ST    R1,AOCANXT                                                       
*&&                                                                             
ADDTRP16 LA    R1,CDSC+(ACTKACT-ACTRECD)                                        
         LA    RE,CDSC+(L'CDSC-1)                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1               RE=EXECUTE L'ACCOUNT                         
         EX    RE,*+4                                                           
         MVC   TRPACT(0),0(R1)                                                  
         SR    R1,R1                                                            
         ICM   R1,1,CDSCOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    *+14                                                             
         LA    R1,TRPACT-1(R1)                                                  
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         LA    RE,TRPLN1Q+1(RE)                                                 
         STC   RE,TRPLN            SET L'ELEMENT                                
         AR    RF,RE               RF=A(NEXT ELEMENT)                           
                                                                                
ADDTRP18 CP    OFFDIFT,PZERO       TEST DIFFERENCE TAX ADJUSTMENT               
         BNE   *+14                                                             
         CP    OFFDIFTS,PZERO      TEST DIFFERENCE TAX ADJ 2ND CURRENCY         
         BE    ADDTRP24                                                         
         MVI   TRPEL,TRPELQ                                                     
         MVI   TRPSTAT,0           ALWAYS A CREDIT                              
         MVI   TRPTYPE,TRPTDTD     DIFFERENCE TAX - DIFFERENCE A/C              
         ZAP   TRPAMNT,OFFDIFT     FOR AMOUNT OF THE DIFF TAX ADJ               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDTRP20                                                         
         IC    R1,OVBYTE           BUMP OCAEL SEQUENCE NUMBER                   
         LA    R1,1(R1)                                                         
         STC   R1,OVBYTE                                                        
         L     R1,AOCANXT          GET A(NEXT OCAEL VALUE)                      
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRPAMNT                                                
         MVC   OCANSEQN,OVBYTE                                                  
         ZAP   OCANCASH(L'TRPAMNT),OFFDIFTS  DIFFERENCE TAX ADJ                 
         LA    R1,OCANTR1L+L'TRPAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         ST    R1,AOCANXT                                                       
*&&                                                                             
ADDTRP20 MVC   TRPUL,CDSC+(ACTKUNT-ACTRECD)                                     
         LA    R1,CDSC+(ACTKACT-ACTRECD)                                        
         LA    RE,CDSC+(L'CDSC-1)                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1               RE=EXECUTE L'ACCOUNT                         
         EX    RE,*+4                                                           
         MVC   TRPACT(0),0(R1)                                                  
         SR    R1,R1                                                            
         ICM   R1,1,CDSCOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    *+14                                                             
         LA    R1,TRPACT-1(R1)                                                  
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         LA    RE,TRPLN1Q+1(RE)                                                 
         STC   RE,TRPLN            SET L'ELEMENT                                
         AR    RF,RE               RF=A(NEXT ELEMENT)                           
                                                                                
         MVI   TRPEL,TRPELQ                                                     
         MVI   TRPSTAT,TRPSDR      ALWAYS A DEBIT                               
         MVI   TRPTYPE,TRPTDTT     DIFFERENCE TAX - TAX A/C                     
         ZAP   TRPAMNT,OFFDIFT     FOR AMOUNT OF THE DIFF TAX ADJ               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDTRP22                                                         
         IC    R1,OVBYTE           BUMP OCAEL SEQUENCE NUMBER                   
         LA    R1,1(R1)                                                         
         STC   R1,OVBYTE                                                        
         L     R1,AOCANXT          GET A(NEXT OCAEL VALUE)                      
         USING OCANTRY,R1          BUILD LIST OF OCA VALUES TO SET              
         MVI   OCANTYPE,QTRPAMNT                                                
         MVC   OCANSEQN,OVBYTE                                                  
         ZAP   OCANCASH(L'TRPAMNT),OFFDIFTS  DIFFERENCE TAX ADJ                 
         LA    R1,OCANTR1L+L'TRPAMNT(R1)                                        
         MVI   OCANTRY,0           END OF LIST                                  
         DROP  R1                                                               
         ST    R1,AOCANXT                                                       
*&&                                                                             
ADDTRP22 MVC   TRPUL,DTAX+(ACTKUNT-ACTRECD)                                     
         LA    R1,DTAX+(ACTKACT-ACTRECD)                                        
         LA    RE,DTAX+(L'DTAX-1)                                               
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1               RE=EXECUTE L'ACCOUNT                         
         EX    RE,*+4                                                           
         MVC   TRPACT(0),0(R1)                                                  
         SR    R1,R1                                                            
         ICM   R1,1,DTAXOPIK       R1=OFFICE POSITION IN KEY, OR 0              
         BZ    *+14                                                             
         LA    R1,TRPACT-1(R1)                                                  
         MVC   0(1,R1),TRNOFFC     INSERT OFFICE INTO KEY ACCOUNT               
         LA    RE,TRPLN1Q+1(RE)                                                 
         STC   RE,TRPLN            SET L'ELEMENT                                
         AR    RF,RE               RF=A(NEXT ELEMENT)                           
                                                                                
ADDTRP24 DS    0H                                                               
                                                                                
         MVI   0(RF),0             SET NEW EOR                                  
         LA    RF,1(RF)                                                         
         SR    RF,R2                                                            
         STCM  RF,3,TRNRLEN        ADJUST RECORD LENGTH                         
                                                                                
         B     OVROU2E                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ADD ANALYSIS POINTER ELEMENTS IF NECESSARY                          *         
* NTRY - P1=A(IOBUFF2)                                                *         
*        P2=A(OFFTAB ENTRY)                                           *         
*        P3=A(TRNEL)                                                  *         
*        P4=A(PSTTAB ENTRY)                                           *         
***********************************************************************         
                                                                                
ADDAPE   LM    R2,R5,0(R1)                                                      
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         USING OFFTABD,R3          R3=A(OFFTAB ENTRY)                           
         USING TRNELD,R4           R4=A(TRNEL)                                  
         USING PSTTABD,R5          R5=A(POSTING TABLE)                          
         CLI   AGYCTRY,CTRYGER     TEST GERMANY                                 
         BNE   ADDAPE16                                                         
         CLI   TRNKLDG,C'I'        TEST POSTING TO INCOME LEDGER                
         BNE   ADDAPE16                                                         
         GOTO1 VHELLO,DMCB,(C'D',OVACCMST),('APEELQ',TRNRECD),0,0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFXTAB                     
         BO    ADDAPE02                                                         
         TM    PSTIND2,PST2DISC    TEST MAIN DISCOUNT/DIFFERENCE                
         BZ    *+12                                                             
         TM    OFFIND1,OFFI1OXO    IN OFFTAB TEST OFXTAB IN USE                 
         BO    ADDAPE02                                                         
         OC    DTAX,DTAX           TEST TAX (VAT) ACCOUNT PRESENT               
         BZ    ADDAPE16                                                         
         CP    OFFDTAX,PZERO       TEST ANY GENERAL TAX ADJUSTMENT              
         BE    ADDAPE16                                                         
ADDAPE02 LA    RF,TRNRFST                                                       
         USING APEELD,RF                                                        
         SR    R0,R0                                                            
         IC    R0,APELN                                                         
         AR    RF,R0                                                            
         CLI   APEEL,0                                                          
         BNE   *-10                                                             
         MVI   APEEL,APEELQ        BUILD APEEL ON RECORD                        
         MVI   APENUM,0            CLEAR COUNT OF SUB-ELEMENTS                  
         MVI   APELN,APELN1Q       FIXED PORTION LENGTH                         
         TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFXTAB                     
         BO    ADDAPE04                                                         
         OC    DTAX,DTAX           TEST MAIN TAX ACCOUNT                        
         BZ    ADDAPE08                                                         
         CP    OFFDTAX,PZERO       TEST ANY MAIN ACCOUNT TAX ADJUSTMENT         
         BE    ADDAPE08                                                         
ADDAPE04 IC    R1,APENUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,APENUM                                                        
         MVI   APENSTAT,APENSDR                                                 
         LA    R1,DTAX+L'ACTKCPY   MAIN DISCOUNT TAX ACCOUNT                    
         LA    RE,DTAX+L'DTAX-1                                                 
         TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFXTAB                     
         BZ    ADDAPE06                                                         
         LA    R1,OFFTAXA          SPECIFIC DISCOUNT TAX ACCOUNT                
         LA    RE,OFFTAXA+L'OFFTAXA-1                                           
ADDAPE06 CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R1                                                            
         EX    RE,*+4                                                           
         MVC   APENACT(0),0(R1)                                                 
         SR    R1,R1                                                            
         ICM   R1,1,DTAXOPIK                                                    
         BZ    *+14                                                             
         LA    R1,APENACT+1(R1)                                                 
         MVC   0(1,R1),TRNOFFC     SET OFFICE IN KEY                            
         LA    RE,APELN2Q+1(RE)                                                 
         STC   RE,APENLEN                                                       
         LA    RE,APELN1Q(RE)                                                   
         STC   RE,APELN                                                         
         TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFXTAB - DONE              
         BO    ADDAPE14                                                         
         TM    PSTIND2,PST2DISC    TEST MAIN DISCOUNT/DIFFERENCE                
         BZ    ADDAPE14                                                         
         TM    OFFIND1,OFFI1OXO    TEST OFFTAB - OFXTAB IN USE                  
         BZ    ADDAPE14                                                         
                                                                                
ADDAPE08 L     R3,AOFXTAB          PROCESS OFFICE EXTRA TABLE                   
ADDAPE10 CLI   OFFTABD,EOT         TEST EOT                                     
         BE    ADDAPE12                                                         
         OC    OFFTAXA,OFFTAXA     TEST FURTHER ENTRY                           
         BZ    ADDAPE12                                                         
         TM    CHQIND1,CHQISPAL    TEST POSTING TO P&L BY OFFICE                
         BZ    *+14                                                             
         CLC   OFFOFFC,TRNOFFC     TEST OFFICE MATCHES                          
         BNE   ADDAPE10                                                         
         IC    R1,APENUM                                                        
         LA    R1,1(R1)                                                         
         STC   R1,APENUM                                                        
         SR    R1,R1                                                            
         IC    R1,APELN            TAKE CURRENT L'ELEMENT                       
         LA    R1,APEELD(R1)                                                    
         USING APENTRY,R1                                                       
         MVI   APENSTAT,APENSDR                                                 
         LA    RE,OFFTAXA+L'OFFTAXA-1                                           
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         LA    R0,OFFTAXA                                                       
         SR    RE,R0                                                            
         EX    RE,*+4                                                           
         MVC   APENACT(0),OFFTAXA                                               
         LA    RE,APELN2Q+1(RE)                                                 
         STC   RE,APENLEN          SET  L'THIS ENTRY                            
         DROP  R1                                                               
         SR    R1,R1                                                            
         IC    R1,APELN            TAKE CURRENT L'ELEMENT                       
         AR    RE,R1                                                            
         STC   RE,APELN            AND ADJUST IT                                
         LA    R3,OFXTABL(R3)                                                   
         B     ADDAPE10                                                         
ADDAPE12 SR    RE,RE                                                            
         IC    RE,APELN            TAKE FINAL L'ELEMENT                         
         DROP  RF                                                               
                                                                                
ADDAPE14 AR    RF,RE               BUMP RF                                      
         MVI   0(RF),0                                                          
         LA    RF,1(RF)                                                         
         LA    RE,TRNRECD                                                       
         SR    RF,RE                                                            
         STCM  RF,3,TRNRLEN        UPDATE RECORD LENGTH                         
                                                                                
ADDAPE16 B     OVROU2E                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ADD SPAEL FOR COSTING ACCOUNT/GROUP TO INCOME POSTING (GERMANY)     *         
* NTRY - P1=A(IOBUFF2)                                                *         
*        P2=A(OFFTAB ENTRY)                                           *         
***********************************************************************         
                                                                                
ADDSCG   LM    R2,R3,0(R1)                                                      
         USING TRNRECD,R2          R2=A(TRANSACTION RECORD)                     
         USING OFFTABD,R3          R3=A(OFFTAB ENTRY)                           
         CLI   AGYCTRY,CTRYGER     TEST GERMANY                                 
         BNE   ADDSCGX                                                          
         CLI   TRNKLDG,C'I'        TEST POSTING TO INCOME LEDGER                
         BNE   ADDSCGX                                                          
         GOTO1 VHELLO,DMCB,(C'D',OVACCMST),('SPAELQ',TRNRECD),         X        
               (L'SPATYPE,=AL1(SPATCOSG)),0                                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    OFFIND1,OFFI1OFX    TEST POSTING FROM OFFICE EXTRA TABLE         
         BZ    ADDSCGX                                                          
         OC    OFFPPCA,OFFPPCA     TEST NEED COSTING POINTER                    
         BZ    ADDSCGX                                                          
         OC    OFFCAIN,OFFCAIN     TEST FURTHER ACCOUNT                         
         BZ    ADDSCGX                                                          
         TM    OFFIND1,OFFI1XPC    TEST KNOWN PRODUCTION CLIENT                 
         BZ    ADDSCGX                                                          
         CLI   OFFCOSG,C' '        TEST KNOWN COSTING GROUP                     
         BNH   ADDSCGX                                                          
         ZAP   OVDUB,OFFCDSC       ESTABLISH NET CASH DISCOUNT                  
         SP    OVDUB,OFFDTAX                                                    
         BNZ   ADDSCG02                                                         
         ZAP   OVDUB,OFFDIFF       OR ESTABLISH NET DIFFERENCE                  
         SP    OVDUB,OFFDIFT                                                    
         BZ    ADDSCGX                                                          
ADDSCG02 SR    R1,R1               ADD SPAEL TO END                             
         ICM   R1,3,TRNRLEN        TAKE CURRENT RECORD LENGTH                   
         LA    R1,TRNRECD(R1)                                                   
         BCTR  R1,0                R1=A(EOR)                                    
         USING SPAELD,R1           BUILD COSTING A/C & GROUP POINTER            
         XC    SPAEL(SPALNQ),SPAEL                                              
         MVI   SPAEL,SPAELQ                                                     
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATCOSG                                                 
         MVC   SPACGRP,OFFCOSG                                                  
         MVI   SPAAUNT,C'1'                                                     
         MVI   SPAALDG,C'C'                                                     
         MVC   SPAAACT,OFFPPCA                                                  
         DROP  R1                                                               
         LA    R1,SPALNQ(R1)       BUMP TO NEXT ELEMENT                         
         MVI   0(R1),0             SET NEW EOR                                  
         LA    R1,1(R1)                                                         
         SR    R1,R2                                                            
         STCM  R1,3,TRNRLEN        SET NEW RECORD LENGTH                        
ADDSCGX  B     OVROU2E                                                          
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE PAYMENT WITHOUT INVOICE                                    *         
***********************************************************************         
                                                                                
VALPWI   LA    R1,0                INDICATE NOT FIRST TIME CALL                 
         CLI   TWASCROV,CMSCR3                                                  
         BE    VALPWI10                                                         
         CLI   PROFDFAV,C'B'       TEST ADVANCES & DIFFERENCES VALID            
         BE    VALPWI02                                                         
         CLI   PROFDFAV,C'A'       TEST ADVANCES VALID                          
         BE    VALPWI02                                                         
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     OVROU2X             INVALID ACTION                               
VALPWI02 CP    TOTITEM,MAXINVS     TEST WE CAN PAY ANOTHER INVOICE              
         BL    VALPWI04                                                         
         MVC   FVMSGNO,=AL2(AE$MXINV)                                           
         MVC   FVXTRA,SPACES                                                    
         CURED MAXINVS,(((2*L'MAXINVS)-1),FVXTRA),0,ALIGN=LEFT                  
         B     OVROU2X             MAXIMUM INVOICES MARKED                      
         USING TSARD,R2                                                         
VALPWI04 L     R2,ATSARBLK                                                      
         MVI   TSARLEN+1,TSARCML   ADD DUMMY PAYMENT                            
         MVI   TSARKEY,FF                                                       
         MVC   TSARKEY+1(L'TSARKEY-2),TSARKEY                                   
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    VALPWI06                                                         
         MVC   FVMSGNO,=AL2(AE$NOSPC)                                           
         TM    TSERRS,TSEEOF       TEST OUT OF SPACE                            
         BO    OVROU2X             MAXIMUM ITEMS EXCEEDED                       
         DC    H'0'                ONLY TSAR EOF OK                             
VALPWI06 MVI   TSACTN,TSADEL       NOW DELETE EXTRA RECORD                      
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         GOTO1 AOVRSCR,CMSCR3                                                   
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'PWIPFK     MAXIMUM LENGTH                               
         LA    RF,PWIPFK                                                        
         STCM  RF,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$PFCM1)                                           
         DROP  R1                                                               
         GOTO1 VGETTXT,(R1)                                                     
         OI    TWAMODE3,TWAM3ASC   APPLICATION SCREEN PROCESSING                
         LA    R1,1                INDICATE FIRST TIME CALL                     
VALPWI10 GOTO1 APAYWOI,(R1)                                                     
         B     OVROU2X                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE DIFFERENCE                                                 *         
***********************************************************************         
                                                                                
VALDIF   LA    R1,0                INDICATE NOT FIRST TIME CALL                 
         CLI   TWASCROV,CMSCR4                                                  
         BE    VALDIF10                                                         
         CLI   PROFDFAV,C'B'       TEST DIFFERENCES & ADVANCES VALID            
         BE    VALDIF02                                                         
         CLI   PROFDFAV,C'D'       TEST DIFFERENCES VALID                       
         BE    VALDIF02                                                         
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     OVROU2X             INVALID ACTION                               
VALDIF02 CP    TOTITEM,MAXINVS     TEST WE CAN PAY A DIFFERENCE                 
         BL    VALDIF04                                                         
         MVC   FVMSGNO,=AL2(AE$MXINV)                                           
         MVC   FVXTRA,SPACES                                                    
         CURED MAXINVS,(((2*L'MAXINVS)-1),FVXTRA),0,ALIGN=LEFT                  
         B     OVROU2X             MAXIMUM INVOICES MARKED                      
         USING TSARD,R2                                                         
VALDIF04 L     R2,ATSARBLK                                                      
         MVI   TSARLEN+1,TSARCML   ADD DUMMY DIFFERENCE                         
         MVI   TSARKEY,FF                                                       
         MVC   TSARKEY+1(L'TSARKEY-2),TSARKEY                                   
         MVI   TSACTN,TSAADD                                                    
         GOTO1 VTSAR,TSARD                                                      
         BE    VALDIF06                                                         
         MVC   FVMSGNO,=AL2(AE$NOSPC)                                           
         TM    TSERRS,TSEEOF       TEST OUT OF SPACE                            
         BO    OVROU2X             MAXIMUM ITEMS EXCEEDED                       
         DC    H'0'                ONLY TSAR EOF OK                             
VALDIF06 MVI   TSACTN,TSADEL       NOW DELETE EXTRA RECORD                      
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
         GOTO1 AOVRSCR,CMSCR4                                                   
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'DIFPFK     MAXIMUM LENGTH                               
         LA    RF,PWIPFK                                                        
         STCM  RF,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$PFCM1)                                           
         DROP  R1                                                               
         GOTO1 VGETTXT,(R1)                                                     
         OI    TWAMODE3,TWAM3ASC   APPLICATION SCREEN PROCESSING                
         LA    R1,1                INDICATE FIRST TIME CALL                     
VALDIF10 GOTO1 ADIFFER,(R1)                                                     
         B     OVROU2X                                                          
         EJECT                                                                  
***********************************************************************         
* ADD INVOICE VALUES TO OFFICE TABLE                                  *         
* NTRY - SETELAD HAS SET A(CREDIT TRANSACTION ELEMENTS)               *         
***********************************************************************         
                                                                                
ADDOFF   ICM   R4,15,ATRNEL        R4=A(TRANSACTION ELEMENT)                    
         USING TRNELD,R4                                                        
         ICM   R3,15,ASCICDSC      R3=A(SUBSIDIARY CASH DISCOUNT) OR 0          
         USING SCIELD,R3                                                        
         L     R2,AOFFTAB          R2=A(OFFICE TABLE)                           
         USING OFFTABD,R2                                                       
*&&UK                                                                           
         ICM   R5,15,AAFCEL        R4=A(FOREIGN CURRENCY ELEMENT)               
         USING AFCELD,R5                                                        
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BO    ADDOFF02            YES - LEAVE SECONDARY VALUES INTACT          
         ZAP   TSARSCUA,PZERO      CLEAR SECONDARY CURRENCY TRNAMNT             
         ZAP   TSARSCUD,PZERO      CLEAR SECONDARY CURRENCY SCIAMNT             
*&&                                                                             
                                                                                
ADDOFF02 LA    R0,OFFTABN          R0=OFFICE TABLE ENTRY COUNT                  
ADDOFF04 OC    OFFTABD(OFFTABL),OFFTABD  TEST EMPTY SLOT                        
         BNZ   ADDOFF06                                                         
         ZAP   OFFCHEQ,PZERO       CLEAR OFFICE CHEQUE                          
         ZAP   OFFCHEQC,PZERO      CLEAR OFFICE CHEQUE IN CURRENCY              
         ZAP   OFFCHEQS,PZERO      CLEAR OFFICE CHEQUE 2ND CURRENCY             
         ZAP   OFFCDSC,PZERO       CLEAR OFFICE DISCOUNT                        
         ZAP   OFFCDSCC,PZERO      CLEAR OFFICE DISCOUNT IN CURRENCY            
         ZAP   OFFCDSCS,PZERO      CLEAR OFFICE DISCOUNT 2ND CURRENCY           
         ZAP   OFFDTAX,PZERO       CLEAR OFFICE DISCOUNT TAX ADJUSTMENT         
         ZAP   OFFDTAXS,PZERO      CLEAR OFFICE DISCOUNT TAX ADJ 2ND            
         ZAP   OFFEXDF,PZERO       CLEAR OFFICE EXCHANGE DIFFERENCE             
         ZAP   OFFEXDFS,PZERO      CLEAR OFFICE EX DIFF 2ND CURRENCY            
         ZAP   OFFBCHA,PZERO       CLEAR OFFICE BANK CHARGES                    
         ZAP   OFFBCHAC,PZERO      CLEAR OFFICE CHARGES IN CURRENCY             
         ZAP   OFFDIFF,PZERO       CLEAR OFFICE DIFFERENCE                      
         ZAP   OFFDIFFC,PZERO      CLEAR OFFICE DIFFERENCE IN CURRENCY          
         ZAP   OFFDIFFS,PZERO      CLEAR OFFICE DIFFERENCE 2ND CURRENCY         
         ZAP   OFFDIFT,PZERO       CLEAR OFFICE DIFFERENCE TAX ADJUST           
         ZAP   OFFDIFTS,PZERO      CLEAR OFFICE DIFFERENCE TAX ADJ 2ND          
         ZAP   OFFADVC,PZERO       CLEAR OFFICE ADVANCE                         
         ZAP   OFFADVCC,PZERO      CLEAR OFFICE ADVANCE IN CURRENCY             
         ZAP   OFFADVCS,PZERO      CLEAR OFFICE ADVANCE 2ND CURRENCY            
         ZAP   OFFADEX,PZERO       CLEAR OFFICE ADVANCE EXCHANGE DIFF           
         ZAP   OFFADEXS,PZERO      CLEAR OFFICE ADVANCE EX DIFF 2ND CUR         
*&&UK                                                                           
         MVC   OFFOFFC,OFFICE      SET INVOICE OFFICE OR SPACES                 
         TM    COMPSTA4,CPYSICPY   TEST MAKING INTERCOMPANY POSTINGS            
         BZ    *+10                                                             
         MVC   OFFOFFC,BANKOF      SET BANK A/C OFFICE                          
*&&                                                                             
*&&US*&& MVC   OFFOFFC,BANKOF      SET BANK OFFICE OR SPACES                    
         TM    CHQIND1,CHQISPAL+CHQISBNK                                        
         BZ    ADDOFF08                                                         
         MVC   OFFOFFC,TRNOFFC     SET OFFICE                                   
         B     ADDOFF08                                                         
                                                                                
ADDOFF06 TM    CHQIND1,CHQISPAL+CHQISBNK                                        
         BZ    ADDOFF08                                                         
         CLC   OFFOFFC,TRNOFFC     TEST OFFICE MATCHES                          
         BE    ADDOFF08                                                         
         LA    R2,OFFTABL(R2)                                                   
         BCT   R0,ADDOFF04                                                      
         DC    H'0'                OFFICE TABLE FULL                            
                                                                                
ADDOFF08 TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BO    ADDOFF10            DON'T ADJUST CHEQUE HERE                     
         AP    OFFCHEQ,TRNAMNT     ADD TO OFFICE CHEQUE                         
*&&UK*&& AP    OFFCHEQS,TSARSCUA   ADD TO OFFICE CHEQUE 2ND CURRENCY            
         TM    TSARIND3,TSARPWOI   TEST ADVANCE                                 
         BZ    ADDOFF10                                                         
         AP    OFFADVC,TRNAMNT     ADD TO OFFICE ADVANCE                        
*&&UK*&& AP    OFFADVCS,TSARSCUA   ADD TO OFFICE ADVANCE 2ND CURRENCY           
                                                                                
ADDOFF10 DS    0H                                                               
*&&UK                                                                           
         LTR   R5,R5               TEST IN CURRENCY                             
         BZ    ADDOFF14                                                         
         TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BZ    ADDOFF12                                                         
         AP    OFFDIFFC,AFCAMNT    ADD TO OFFICE DIFFERENCE (CURRENCY)          
         SP    OFFCHEQC,AFCAMNT    TAKE FROM OFFICE CHEQUE (CURRENCY)           
         B     ADDOFF14                                                         
ADDOFF12 AP    OFFCHEQC,AFCAMNT    ADD TO OFFICE CHEQUE (CURRENCY)              
         TM    TSARIND3,TSARPWOI   TEST ADVANCE                                 
         BZ    *+10                                                             
         AP    OFFADVCC,AFCAMNT    ADD TO OFFICE ADVANCE (CURRENCY)             
                                                                                
ADDOFF14 LTR   R3,R3               TEST DISCOUNT FOUND                          
         BZ    ADDOFF48                                                         
         TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BZ    ADDOFF16                                                         
         AP    OFFDIFF,SCIAMNT     ADD TO DIFFERENCE                            
         SP    OFFDIFFS,TSARSCUA   ADD TO -VE DIFFERENCE 2ND CURRENCY           
         SP    OFFCHEQ,SCIAMNT     AND TAKE FROM OFFICE CHEQUE                  
         AP    OFFCHEQS,TSARSCUA   AND ADD TO -VE OFFICE CHEQUE 2ND CUR         
         B     ADDOFF18            CURRENCY HANDLED ALREADY                     
                                                                                
ADDOFF16 AP    OFFCDSC,SCIAMNT     ADD TO DISCOUNT                              
         AP    OFFCDSCS,TSARSCUD   ADD TO DISCOUNT 2ND CURRENCY                 
         SP    OFFCHEQ,SCIAMNT     AND TAKE FROM OFFICE CHEQUE                  
         SP    OFFCHEQS,TSARSCUD   AND TAKE FROM OFFICE CHEQUE 2ND CUR          
         LTR   R5,R5               TEST SINGLE CURRENCY IN USE                  
         BZ    ADDOFF18                                                         
         ZAP   DUB,SCIAMNT                                                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,COMPCURT+(CURTCUR-CURTABD)                              
         MVC   EURKCUTO,AFCCURR                                                 
         MVC   EURKRULE,AFCX                                                    
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    OFFCDSCC,DUB        ADD TO DISCOUNT (CURRENCY)                   
         SP    OFFCHEQC,DUB        TAKE FROM OFFICE CHEQUE (CURRENCY)           
         DROP  R5                                                               
                                                                                
ADDOFF18 TM    UPDIND1,UPDIDTAX    TEST MAKING DISCOUNT TAX ADJUSTMENT          
         BZ    ADDOFF48                                                         
         LH    RF,=H'1600'         DEFAULT RATE                                 
         ICM   R1,15,ARAVEL        TEST/SET RATE OF VAT ELEMENT                 
         USING RATELD,R1                                                        
         BZ    ADDOFF20                                                         
         ICM   RF,3,RATRATE                                                     
         BNZ   ADDOFF20                                                         
         ZAP   PL13,PZERO          ZERO RATED TAX - ZERO ADJUSTMENT             
         ZAP   PL13S,PZERO                                                      
         B     ADDOFF22            AND SKIP TO BUILD OFXTAB ENTRY               
                                                                                
ADDOFF20 ZAP   PL13,SCIAMNT        CALCULATE DISCOUNT TAX ADJUSTMENT            
         MP    PL13,=P'10000'                                                   
         AH    RF,=H'10000'                                                     
         CVD   RF,OVDUB                                                         
         DP    PL13,OVDUB                                                       
         ZAP   OVDUB,PL13(5)                                                    
         ZAP   PL13,OVDUB                                                       
         LH    RF,=H'1600'         DEFAULT RATE                                 
         ICM   R1,15,ARAVEL        TEST/SET RATE OF VAT ELEMENT                 
         BZ    *+8                                                              
         ICM   RF,3,RATRATE                                                     
         DROP  R1                                                               
         CVD   RF,OVDUB                                                         
         MP    PL13,OVDUB                                                       
         SRP   PL13,64-4,5         ROUND THE RESULT                             
                                                                                
         LH    RF,=H'1600'         DEFAULT RATE                                 
         ICM   R1,15,ARAVEL        TEST/SET RATE OF VAT ELEMENT                 
         USING RATELD,R1                                                        
         BZ    *+8                                                              
         ICM   RF,3,RATRATE        RATE MUST BE SET (SEE ABOVE)                 
         ZAP   PL13S,TSARSCUD      CALCULATE DISCOUNT TAX ADJ 2ND CUR           
         TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BZ    *+16                                                             
         ZAP   PL13S,PZERO                                                      
         SP    PL13S,TSARSCUA      CALCULATE DIFFERENCE TAX ADJ 2ND CUR         
         MP    PL13S,=P'10000'                                                  
         AH    RF,=H'10000'                                                     
         CVD   RF,OVDUB                                                         
         DP    PL13S,OVDUB                                                      
         ZAP   OVDUB,PL13S(5)                                                   
         ZAP   PL13S,OVDUB                                                      
         LH    RF,=H'1600'         DEFAULT RATE                                 
         ICM   R1,15,ARAVEL        TEST/SET RATE OF VAT ELEMENT                 
         BZ    *+8                                                              
         ICM   RF,3,RATRATE                                                     
         DROP  R1                                                               
         CVD   RF,OVDUB                                                         
         MP    PL13S,OVDUB                                                      
         SRP   PL13S,64-4,5        ROUND THE RESULT                             
                                                                                
ADDOFF22 CLI   AGYCTRY,CTRYGER     TEST GERMANY                                 
         BNE   ADDOFF44                                                         
         MVC   WORK(L'ACTKULA),DTAX+L'ACTKCPY                                   
         MVC   TEMP(L'ACTKULA),SUPP+L'ACTKCPY                                   
         MVI   BYTE,0                                                           
         USING SPAELD,R1                                                        
         ICM   R1,15,ASPAITAX      TEST/SET A(INPUT TAX ACCOUNT)                
         BZ    *+14                                                             
         MVC   WORK(L'ACTKULA),SPAAULA                                          
         OI    BYTE,OFFI1XTA       SET SPECIFIC TAX ACCOUNT                     
         USING SORELD,RF                                                        
         ICM   RF,15,ASORPROD      TEST/SET A(ACC PRODUCTION SOURCE)            
         BZ    ADDOFF30                                                         
         OI    BYTE,OFFI1XPC       SET SPECIFIC PRODUCTION CONTRA A/C           
         MVC   TEMP,SPACES                                                      
         IC    RE,PRODBLEN                                                      
         LA    RE,L'PRODUL-1(RE)                                                
         EX    RE,*+4                                                           
         MVC   TEMP(0),SORAULA                                                  
         SP    OFFCDSC,SCIAMNT     ADJUST MAIN OFFICE DISCOUNT                  
         SP    OFFCDSCS,TSARSCUD   ADJUST MAIN OFFICE DISCOUNT 2ND CUR          
         B     ADDOFF32            PROCEED WHETHER OR NO TAX A/C                
ADDOFF30 LTR   R1,R1               TEST A(INPUT TAX ACCOUNT)                    
         BZ    ADDOFF44                                                         
ADDOFF32 OI    OFFIND1,OFFI1OXO    SET OFFICE EXTRA TABLE IN USE                
         L     RE,AOFXTAB                                                       
OFX      USING OFFTABD,RE                                                       
         LA    R0,OFXTABN                                                       
ADDOFF34 OC    OFX.OFFTABD(OFXTABL),OFX.OFFTABD TEST EMPTY ENTRY                
         BNZ   ADDOFF36                                                         
         MVC   OFX.OFFOFFC,SPACES  CLEAR OFFICE                                 
         TM    CHQIND1,CHQISPAL    TEST POSTING TO P&L BY OFFICE                
         BZ    *+10                                                             
         MVC   OFX.OFFOFFC,OFFOFFC SET OFFICE (OR SPACES)                       
         MVC   OFX.OFFIND1,BYTE    SET OVERRIDE ACCOUNT INDICATORS              
         OI    OFX.OFFIND1,OFFI1OFX                                             
         ZAP   OFX.OFFCDSC,PZERO   CLEAR DISCOUNT                               
         ZAP   OFX.OFFCDSCS,PZERO  CLEAR DISCOUNT 2ND CURRNECY                  
         ZAP   OFX.OFFDTAX,PZERO   CLEAR DISCOUNT TAX ADJUSTMENT                
         ZAP   OFX.OFFDTAXS,PZERO  CLEAR DISCOUNT TAX ADJ 2ND CURRENCY          
         ZAP   OFX.OFFDIFF,PZERO   CLEAR DIFFERENCE                             
         ZAP   OFX.OFFDIFFS,PZERO  CLEAR DIFFERENCE 2ND CURRENCY                
         ZAP   OFX.OFFDIFT,PZERO   CLEAR DIFFERENCE TAX ADJUSTMENT              
         ZAP   OFX.OFFDIFTS,PZERO  CLEAR DIFFERENCE TAX ADJ 2ND CURR            
         MVC   OFX.OFFTAXA,WORK    SET TAX ACCOUNT                              
         MVC   OFX.OFFCAIN,TEMP    SET CLIENT/PRODUCT INCOME CONTRA             
         B     ADDOFF38                                                         
ADDOFF36 TM    CHQIND1,CHQISPAL    TEST POSTING TO P&L BY OFFICE                
         BZ    *+14                                                             
         CLC   OFX.OFFOFFC,OFFOFFC TEST OFFICE MATCHES                          
         BNE   ADDOFF42                                                         
         CLC   OFX.OFFTAXA,WORK    TEST TAX ACCOUNT MATCHES                     
         BNE   ADDOFF42                                                         
         CLC   OFX.OFFCAIN,TEMP    TEST INCOME CONTRA A/C MATCHES               
         BNE   ADDOFF42                                                         
ADDOFF38 TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BO    ADDOFF40                                                         
         AP    OFX.OFFCDSC,SCIAMNT SET DISCOUNT BASIS                           
         SP    OFX.OFFDTAX,PL13    NEGATIVE TAX ADJUSTMENT TOTAL                
         AP    OFX.OFFCDSCS,TSARSCUD 2ND CURRENCY DISCOUNT BASIS                
         SP    OFX.OFFDTAXS,PL13S    2ND CURRENCY NEGATIVE TAX ADJ              
         B     ADDOFF48                                                         
ADDOFF40 AP    OFX.OFFDIFF,SCIAMNT SET DIFFERENCE BASIS                         
         SP    OFX.OFFDIFT,PL13    NEGATIVE TAX ADJUSTMENT TOTAL                
         SP    OFX.OFFDIFFS,TSARSCUA 2ND CURRENCY DIFFERENCE BASIS              
         SP    OFX.OFFDIFTS,PL13S    2ND CURRENCY NEGATIVE TAX ADJ              
         B     ADDOFF48                                                         
ADDOFF42 LA    RE,OFXTABL(RE)      NEXT OFXTAB ENTRY                            
         BCT   R0,ADDOFF34                                                      
         DC    H'0'                OFXTAB FULL                                  
         DROP  R1,OFX                                                           
                                                                                
ADDOFF44 TM    TSARIND3,TSARDIFF   TEST DIFFERENCE                              
         BO    ADDOFF46                                                         
         SP    OFFDTAX,PL13        NEGATIVE TAX ADJUSTMENT TOTAL                
         SP    OFFDTAXS,PL13S      2ND CURRENCY NEGATIVE TAX ADJ                
         B     ADDOFF48                                                         
ADDOFF46 SP    OFFDIFT,PL13        NEGATIVE TAX ADJUSTMENT TOTAL                
         SP    OFFDIFTS,PL13S      2ND CURRENCY NEGATIVE TAX ADJ                
         B     ADDOFF48                                                         
                                                                                
ADDOFF48 ZAP   TMPEXDF,PZERO       KEEP EXCHANGE DIFF. FOR -CR                  
         ZAP   TMPEXDFS,PZERO      KEEP EXCHANGE DIFF. FOR -CR (2ND)            
         TM    SAFCIND1,SAFCI1SC   TEST SINGLE CURRENCY ALLOCATION              
         BZ    ADDOFF50                                                         
         TM    SAFCIND1,SAFCI1AL                                                
         BO    ADDOFF50                                                         
         GOTO1 AEXCHDF                                                          
         BNE   ADDOFF50                                                         
         AP    OFFCHEQ,TMPEXDF     CALCULATE REAL VALUE OF CHEQUE               
         ZAP   OFFEXDF,=P'99999999' INDICATE EXCHANGE DIFFERENCE POSTED         
         AP    OFFCHEQS,TMPEXDFS   CALCULATE REAL VALUE OF CHEQUE               
         TM    TSARIND3,TSARDIFF   TEST DIFFERENCE POSTING                      
         BZ    ADDOFF49                                                         
         SP    OFFDIFF,TMPEXDF     ADJUST DIFFERENCE                            
         SP    OFFDIFFS,TMPEXDFS   ADJUST DIFFERENCE                            
         B     ADDOFF50                                                         
ADDOFF49 TM    TSARIND3,TSARPWOI   TEST ADVANCE POSTING                         
         BZ    ADDOFF50                                                         
         AP    OFFADEX,TMPEXDF     SET EXCHANGE DIFFERENCE                      
         AP    OFFADEXS,TMPEXDFS   SET EXCHANGE DIFFERENCE (2ND)                
         B     ADDOFF50                                                         
*&&                                                                             
                                                                                
         USING ICOTABD,R1                                                       
ADDOFF50 ICM   R1,15,AICOTAB       POST INTO ICOTAB IF NECESSARY                
         BZ    ADDOFF56                                                         
         CLC   BANKOF,TRNOFFC      TEST THIS IS THE BANK OFFICE                 
         BE    ADDOFF56                                                         
         B     *+8                                                              
ADDOFF52 LA    R1,ICOTABL(R1)      NO - NEXT ENTRY                              
         CLI   ICOTABD,0           TEST EOT                                     
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IPTRM)                                           
         B     OVROU2H                                                          
         CLC   ICOOFFC,TRNOFFC     TEST OFFICE MATCHES                          
         BNE   ADDOFF52                                                         
         AP    ICOAMNT,TRNAMNT     ADD TRANSACTION AMOUNT                       
         LTR   R3,R3                                                            
         BZ    *+10                                                             
         SP    ICOAMNT,SCIAMNT     MINUS DISCOUNT                               
*&&UK                                                                           
         TM    SCURIND1,SCURIOMT   TEST SECONDARY CURRENCY CHEQUE               
         BZ    ADDOFF54                                                         
         AP    ICOAMNTS,TSARSCUA   ADD TRANSACTION AMOUNT (2ND)                 
         LTR   R3,R3                                                            
         BZ    ADDOFF54                                                         
         SP    ICOAMNTS,TSARSCUD   MINUS DISCOUNT (2ND)                         
*&&                                                                             
ADDOFF54 DS    0H                                                               
                                                                                
         DROP  R1                                                               
                                                                                
ADDOFF56 DS    0H                                                               
         B     OVROU2E                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OVRWRKD  DSECT                                                                  
QINTOEUR EQU   X'08'               EXCHANGE RATE INTO EURO (EUREKA)             
QFROMEUR EQU   X'04'               EXCHANGE RATE FROM EURO (EUREKA)             
OVDUB    DS    D                                                                
OVDUB1   DS    D                                                                
OVDUB2   DS    D                                                                
OVFULL   DS    F                                                                
OSCIGLEV DS    A                   A(SCIEL FOR DISC/DIFF TAX ADJ.)              
OSCIBCHA DS    A                   A(SCIEL FOR BANK CHARGES)                    
OAFCEL   DS    A                   A(AFCEL)                                     
OVHALF   DS    H                                                                
OVBYTE   DS    XL1                                                              
CHQAMNTQ EQU   X'80'                                                            
CURAMNTQ EQU   X'40'                                                            
EXCHRATQ EQU   X'20'                                                            
INVEXCHQ EQU   X'10'                                                            
OVCHAR   DS    XL1                                                              
OVRELO   DS    A                                                                
                                                                                
AJARAY   DS    A                   JARAY                                        
AOFFTAB  DS    A                   OFFICE TABLE                                 
AOFFTABT DS    A                   OFFICE TABLE TOTAL ENTRY                     
AICOTAB  DS    A                   INTERCOMPANY TABLE                           
AOFXTAB  DS    A                   OFFICE EXTRA TABLE                           
AOVIOS1  DS    A                   OVERLAY IO AREA 1 (SAVE)                     
AOVIOB1  DS    A                                     (BUFFER)                   
AOVIOS2  DS    A                   OVERLAY IO AREA 2 (SAVE)                     
AOVIOB2  DS    A                                     (BUFFER)                   
AOVIOS3  DS    A                   OVERLAY IO AREA 3 (SAVE)                     
AOVIOB3  DS    A                                     (BUFFER)                   
OVIOALQ  EQU   3*IOALQ             NUMBER OF OVERLAY IO AREAS                   
*                                                                               
OVROUT1  DS    0A                                                               
AADDSPA  DS    A                                                                
AADDTRN  DS    A                                                                
*&&UK                                                                           
AAPPBCO  DS    A                                                                
*&&                                                                             
ABATMAN  DS    A                                                                
ABLDDSC  DS    A                                                                
ABLDICO  DS    A                                                                
ABLDNAR  DS    A                                                                
*&&UK                                                                           
ACALCRAT DS    A                                                                
AMAXMIN  DS    A                                                                
*&&                                                                             
AMRKALL  DS    A                                                                
APSTCOS  DS    A                                                                
APSTOFF  DS    A                                                                
APSTINV  DS    A                                                                
APSTICO  DS    A                                                                
*&&UK                                                                           
ARATLIM  DS    A                                                                
*&&                                                                             
ARECFLT  DS    A                                                                
ASETACT  DS    A                                                                
ASETKEY  DS    A                                                                
ASETMPY  DS    A                                                                
ASTDEL   DS    A                                                                
ATOTOFF  DS    A                                                                
*&&UK                                                                           
AVERRAT  DS    A                                                                
*&&                                                                             
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
                                                                                
OVROUT2  DS    0A                                                               
AADDOFF  DS    A                                                                
APAYWOI  DS    A                                                                
ADIFFER  DS    A                                                                
ABLDNEW  DS    A                                                                
AADDTRP  DS    A                                                                
AADDAPE  DS    A                                                                
AADDSCG  DS    A                                                                
AVALPWI  DS    A                                                                
AVALDIF  DS    A                                                                
OVROUT2N EQU   (*-OVROUT2)/L'OVROUT2                                            
                                                                                
AOCANXT  DS    A                   A(POSITION FOR NEXT OCA VALUE)               
                                                                                
SINDS    DS    XL1                                                              
SLCVAL   EQU   X'80'                                                            
SLCNZ    EQU   X'40'                                                            
SFCVAL   EQU   X'20'                                                            
SFCNZ    EQU   X'10'                                                            
SEXVAL   EQU   X'08'                                                            
SEXNZ    EQU   X'04'                                                            
*&&UK                                                                           
OVRATE   DS    XL(L'MANRAT)                                                     
OVAFCXR  DS    XL(L'AFCXRATE)                                                   
OVCURT   DS    XL(L'PRSTCURT)                                                   
*&&                                                                             
OVIAMNT  DS    PL(L'TRNAMNT)                                                    
OVIAOCA  DS    PL(L'OCANCASH)                                                   
OVCNTRA  DS    XL(L'ACTKCULA)      CONTRA C/U/L/ACCOUNT                         
OVPAYSBR DS    XL(L'TRNKSBR)                                                    
OVROUND  DS    PL6                                                              
OVROUNDS DS    PL6                                                              
EXRMIN   DS    CL(L'GCRMNEXC)      MIN VALID EXCH RATE (CURR RECORD)            
EXRMAX   DS    CL(L'GCRMXEXC)      MAX VALID EXCH RATE (CURR RECORD)            
                                                                                
OVRNUM   DS    XL(L'TSRNUM)        TSAR RECORD NUMBER                           
                                                                                
OVLIDS   DS    0X                  OVLIDC MUST MAP TO OVLIDS EXACTLY            
LDGLIST  DS    XL(LDGLIDCL)        VALID SUPPLIER LEDGERS IN UNIT S             
BNKLIST  DS    XL(BNKLIDCL)        VALID BANK LEDGERS IN UNIT S                 
CTRYTAB  DS    XL(CTRYTDCL)                                                     
DUMCON   DS    CL(L'TRNKULC)                                                    
OVACCMST DS    CL8                                                              
PSTDFLT  DS    AL1                                                              
MAXINVS  DS    PL2                 MAXIMUM INVOICES FOR ONE CHEQUE              
EURO     DS    CL(L'CURTCUR)       EURO CURRENCY CODE                           
GBP      DS    CL(L'CURTCUR)       GBP CURRENCY CODE                            
OVLIDSL  EQU   *-OVLIDS                                                         
                                                                                
OVWORK   DS    XL255               WORK AREA                                    
         ORG   OVWORK                                                           
OVTRNAM  DS    PL(L'TRNAMNT)       TEMPORARY TRNAMNT                            
OVAFCAM  DS    PL(L'AFCAMNT)       TEMPORARY AFCAMNT                            
OVTRNAS  DS    PL(L'TRNAMNT)       TEMPORARY TRNAMNT SECOND CURRENCY            
         ORG                                                                    
OVTRNEL  DS    XL(TRNLN1Q+1)       STANDARD MARKER TRNEL                        
OVTRSEL  DS    XL(TRSLNQ)          STANDARD MARKER TRSEL                        
OVTIDEL  DS    XL(TIDLNQ)          STANDARD MARKER TIDEL                        
OVPIDEL  DS    XL(PIDLNQ)          STANDARD MARKER PIDEL                        
OVSPAEL  DS    XL(SPALNQ)          STANDARD MARKER SPAEL                        
                                                                                
OVOLDAFC DS    XL(L'OLDAFCEL)      OLD MEMBER CURRENCY AFCEL                    
                                                                                
STRNEL   DS    XL255               TRNEL SAVED WITHIN PSTOFF ROUTINE            
                                                                                
*UPDIND1  DS    XL1                 UPDATE INDICATOR1                           
*UPDIBANK EQU   X'80'               POST TO BANK                                
*UPDICDSC EQU   X'40'               POST TO DISCOUNT                            
*UPDIDTAX EQU   X'20'               POST TO DISCOUNT TAX ADJUSTMENT             
*UPDIEXDF EQU   X'10'               POST TO EXCHANGE DIFFERENCE                 
*UPDIBCHA EQU   X'08'               POST TO BANK CHARGE                         
                                                                                
*UPDIND2  DS    XL1                 UPDATE INDICATOR2                           
*UPDIADDQ EQU   X'80'               ITEM(S) ADDED VIA ADDTRN                    
*UPDIMRGE EQU   X'40'               MERGE OFFICE TABLE INTO ONE POSTING         
                                                                                
CPJACT   DS    CL(L'TRNKULA)       CLIENT/PRODUCT/JOB UNIT LEDGER & ACC         
CPJWORK  DS    CL(L'TRNKWORK)      PRODUCTION WORKCODE                          
PL13     DS    PL13                                                             
PL13S    DS    PL13                FOR SECONDARY CURRENCY                       
BAERROR  DS    XL(L'IOERROR)       IOEXEC ERROR RETURN BYTE                     
                                                                                
EXDFSJ   DS    CL(L'TRNKULA)       SJ EXCH DIFF A/C AFTER GETACC                
                                                                                
POSTCULA DS    0CL(L'TRNKCULA)     POSTING ACCOUNT                              
POSTCPY  DS    CL(L'TRNKCPY)       COMPANY                                      
POSTULA  DS    0CL(L'TRNKCULA-L'TRNKCPY)                                        
POSTUNT  DS    CL(L'TRNKUNT)       UNIT                                         
POSTLDG  DS    CL(L'TRNKLDG)       LEDGER                                       
POSTACT  DS    CL(L'TRNKACT)       ACCOUNT                                      
                                                                                
POSTCULC DS    0CL(L'TRNKCULC)     POSTING CONTRA ACCOUNT                       
POSTCCPY DS    CL(L'TRNKCCPY)      CONTRA COMPANY                               
POSTULC  DS    0CL(L'TRNKCULC-L'TRNKCCPY)                                       
POSTCUNT DS    CL(L'TRNKCUNT)      CONTRA UNIT                                  
POSTCLDG DS    CL(L'TRNKCLDG)      CONTRA LEDGER                                
POSTCACT DS    CL(L'TRNKCACT)      CONTRA ACCOUNT                               
                                                                                
POSTCACN DS    CL(L'NAMEREC)       POSTING CONTRA ACCOUNT NAME                  
POSTSTAT DS    XL(L'TRNSTAT)       POSTING STATUS                               
POSTOFFC DS    XL(L'TRNOFFC)       POSTING OFFICE                               
POSTOPIK DS    XL1                 POSITION TO DISPLACE OFFICE INTO KEY         
                                                                                
COSTOPIK DS    XL1                 COSTING KEY OFFICE POSITION                  
CINCOPIK DS    XL1                 COSTING INCOME KEY OFFICE POSTITION          
COSCLIN  DS    CL36                COSTING CLIENT NAME                          
COSGRPN  DS    CL36                COSTING GROUP ACCOUNT NAME                   
                                                                                
TRNBLOCK DS    XL(TRNBLKL)         ADDTRN BLOCK                                 
                                                                                
OVOCAL   DS    XL255               OCA LIST AREA                                
                                                                                
NARBLKD  DSECT                                                                  
NARNUML  DS    XL1                                                              
NARNUM   DS    CL(L'CHQNUM)                                                     
NARNUMLQ EQU   *-NARNUML                                                        
NARDATL  DS    XL1                                                              
NARDAT   DS    CL10                                                             
NARDATLQ EQU   *-NARDATL                                                        
NARAMTL  DS    XL1                                                              
NARAMT   DS    CL13                                                             
NARAMTLQ EQU   *-NARAMTL                                                        
NARBNKL  DS    XL1                                                              
NARBNK   DS    CL14                                                             
NARBNKLQ EQU   *-NARBNKL                                                        
NARBLKL  EQU   *-NARBLKD                                                        
                                                                                
OFFTABD  DSECT                     ** OFFICE AND OFFICE EXTRA TABLE **          
OFFOFFC  DS    CL(L'TRNOFFC)       OFFICE                                       
OFFIND1  DS    XL1                 INDICATOR - 1                                
OFFI1OXO EQU   X'80'               OFXTAB IN USE (OFFTAB ONLY)                  
OFFI1OFX EQU   X'40'               OFXTAB ENTRY (OFXTAB ONLY)                   
OFFI1XTA EQU   X'20'               OFXTAB ENTRY - TAX ACCOUNT                   
OFFI1XPC EQU   X'10'               OFXTAB ENTRY - PRODUCTION CONTRA A/C         
OFFCDSC  DS    PL6                 OFFICE DISCOUNT                              
OFFCDSCC DS    PL6                 OFFICE DISCOUNT IN CURRENCY                  
OFFCDSCS DS    PL6                 OFFICE DISCOUNT SECONDARY CURRENCY           
OFFDTAX  DS    PL6                 OFFICE DISCOUNT TAX ADJUSTMENT               
OFFDTAXS DS    PL6                 OFFICE DISCOUNT TAX ADJ SECONDARY            
OFFDIFF  DS    PL6                 OFFICE DIFFERENCE                            
OFFDIFFC DS    PL6                 OFFICE DIFFERENCE IN CURRENCY                
OFFDIFFS DS    PL6                 OFFICE DIFFERENCE SECONDARY CURRENCY         
OFFDIFT  DS    PL6                 OFFICE DIFFERENCE TAX ADJUSTMENT             
OFFDIFTS DS    PL6                 OFFICE DIFFERENCE TAX ADJ SECONDARY          
OFFCOMMX DS    0X                  ** END OF COMMON AREA **                     
         ORG   OFFCOMMX            ** DEFINED FOR OFFTAB ONLY **                
OFFCHEQ  DS    PL6                 OFFICE CHEQUE                                
OFFCHEQC DS    PL6                 OFFICE CHEQUE IN CURRENCY                    
OFFCHEQS DS    PL6                 OFFICE CHEQUE SECONDARY CURRENCY             
OFFEXDF  DS    PL6                 OFFICE EXCH DIFFERENCE                       
OFFEXDFS DS    PL6                 OFFICE EXCH DIFFERENCE 2ND CURRENCY          
OFFBCHA  DS    PL6                 OFFICE BANK CHARGES                          
OFFBCHAC DS    PL6                 OFFICE BANK CHARGES IN CURRENCY              
OFFADVC  DS    PL6                 OFFICE ADVANCE                               
OFFADVCC DS    PL6                 OFFICE ADVANCE IN CURRENCY                   
OFFADVCS DS    PL6                 OFFICE ADVANCE SECONDARY CURRENCY            
OFFADEX  DS    PL6                 OFFICE ADVANCE EXCHANGE DIFFERENCE           
OFFADEXS DS    PL6                 OFFICE ADVANCE EX DIFF 2ND CURRENCY          
OFFTABL  EQU   *-OFFTABD                                                        
OFFTABN  EQU   48                                                               
OFFLTAB  EQU   (OFFTABN*OFFTABL)+1 NEED EOT MARKER                              
         ORG   OFFCOMMX            ** DEFINED FOR OFXTAB ONLY **                
OFFTAXA  DS    CL(L'SPAAULA)       OFFICE TAX ACCOUNT                           
OFFCAIN  DS    CL(L'SORAULA)       OFFICE CONTRA ACCOUNT FOR INCOME             
OFFPPCA  DS    CL(L'PPRCOSTA)      OFFICE COSTING ACCOUNT                       
OFFCOSG  DS    CL(L'RSTCOSTG)      OFFICE COSTING GROUP                         
OFXTABL  EQU   *-OFFTABD                                                        
OFXTABN  EQU   48                                                               
OFXLTAB  EQU   (OFXTABN*OFXTABL)+1 NEED EOT MARKER                              
                                                                                
ICOTABD  DSECT                     ** INTERCOMPANY TABLE **                     
ICOOFFC  DS    CL(L'TRNOFFC)       OFFICE                                       
ICOAMNT  DS    PL8                 AMOUNT                                       
ICOAMNTS DS    PL8                 AMOUNT (SECONDARY CURRENCY)                  
ICOFACT  DS    CL(L'ACTKCULA-1)    FROM U/L/ACCOUNT                             
ICOTACT  DS    CL(L'ACTKCULA-1)    TO U/L/ACCOUNT                               
ICOFNAM  DS    CL(L'NAMEREC)       FROM A/C NAME                                
ICOTNAM  DS    CL(L'NAMEREC)       TO A/C NAME                                  
ICOTABL  EQU   *-ICOTABD                                                        
ICOTABN  EQU   24                                                               
ICOLTAB  EQU   (ICOTABN*ICOTABL)+1 NEED EOT MARKER                              
                                                                                
* ACQUIRED STORAGE COMPRISES                                                    
* OFFICE TOTAL ENTRY (OFFTABL)                                                  
* OFFICE TABLE       (OFFLTAB)                                                  
* INTERCOMPANY TABLE (ICOLTAB)                                                  
* OFFICE EXTRA TABLE (OFXLTAB)                                                  
* THREE I/O AREAS    (OVIOALQ)                                                  
STORAGEL EQU   ((OFFTABL+OFFLTAB+ICOLTAB+OFXLTAB+OVIOALQ+1)/8+1)*8              
                                                                                
CTRYTABD DSECT                     ** COUNTRY VARIABLES TABLE **                
CTRYCTRY DS    XL(L'AGYCTRY)       COUNTRY CODE                                 
CTRYPSTI DS    XL(L'UPDIND1)       POSTING RULE                                 
         DS    XL2                 N/D                                          
CTRYTABL EQU   *-CTRYTABD                                                       
                                                                                
PSTTABD  DSECT                                                                  
PSTACT   DS    XL1                 POSTING ACTION (BITS X'F8'>UPDIND1)          
PSTBANK  EQU   X'80'               -POST TO BANK                                
PSTSUPP  EQU   X'80'               -POST TO SUPPLIER                            
PSTCDSC  EQU   X'40'               -POST TO DISCOUNT                            
PSTDTAX  EQU   X'20'               -POST TO DISCOUNT TAX ADJUSTMENT             
PSTEXDF  EQU   X'10'               -POST TO EXCHANGE DIFFERENCE                 
PSTBCHA  EQU   X'08'               -POST TO BANK CHARGE                         
PSTLEVEL EQU   X'03'               LEVEL OF POSTING                             
PSTLINV  EQU   0                   -INVOICE                                     
PSTLOFF  EQU   1                   -OFFICE                                      
PSTLICO  EQU   2                   -INTER-COMPANY                               
PSTSTAT  DS    XL1                 TRANSACTION STATUS                           
PSTIND1  DS    XL1                 POSTING INDICATOR - 2                        
PST1NEG  EQU   X'80'               -NEGATE TRANSACTION AMOUNT                   
PST1SEI  EQU   X'40'               -SWAP TYPE/SIGN (SE/SI)                      
PST1NAR  EQU   X'20'               -ADD CHEQUE NARATIVE                         
PST1OPIK EQU   X'10'               -POST BY OFFICE (CONTRA'S IF OFFLEV)         
PST1SPA  EQU   X'08'               -ADD SPA DETAILS                             
PST1DUCN EQU   X'04'               -DON'T UPDATE CONTRA NAMES                   
PST1CSRF EQU   X'02'               -CLEAR SUB REFERENCE                         
PST1MEM  EQU   X'01'               -MEMO TYPE AFC IF PSTEAFC                    
PSTELE   DS    XL1                 POST ELEMENTS                                
PSTEAFC  EQU   X'80'               -AFCEL                                       
PSTEDUE  EQU   X'40'               -DUEEL                                       
PSTEFFT  EQU   X'20'               -FFTEL FOR ASSOCIATED CURRENCY               
PSTEMPY  EQU   X'10'               -MPYEL                                       
PSTEDSC  EQU   X'08'               -SCIEL FOR DISCOUNT                          
PSTESOR  EQU   X'04'               -SOREL                                       
PSTESPA  EQU   X'02'               -SPAEL                                       
PSTETRX  EQU   X'01'               -TRXEL                                       
PSTELE2  DS    XL1                 MORE POST ELEMENTS                           
PSTEBCH  EQU   X'80'               -SCIEL FOR BANK CHARGE                       
PSTEAFCC EQU   X'40'               -CALCULATED AFC RATE                         
PSTEFFTK EQU   X'20'               -FFTEL FOR KEY REFERENCE                     
PSTETRP  EQU   X'10'               -TRPEL(S)                                    
PSTETRXA EQU   X'08'               -TRXEL FOR ADVANCE                           
PSTETRXD EQU   X'04'               -TRXEL FOR MINOR DIFFERENCE                  
PSTEAPE  EQU   X'02'               -APEEL FOR GERMAN SI POSTINGS                
PSTSPAT  DS    XL1                 ASSOCIATED SPA TYPE                          
PSTAMOF  DS    H                   OFFSET TO AMOUNT FIELD                       
PSTSERR  DS    AL2                 SPA NOT FOUND ERROR MESSAGE                  
PSTACC   DS    S                   A(C/U/L/ACCOUNT CODE) - SEE ACCXD            
PSTCNTR  DS    S                   A(C/U/L/CNTRA ACC CODE)                      
PSTAMNT  DS    S                   AMOUNT (LESS OFFSET)                         
PSTIND2  DS    XL1                 POSTING INDICATOR - 2                        
PST2INEW EQU   X'80'               -INVOICE NEW POSTING                         
PST2DNAR EQU   X'40'               -ADD DIFFERENCE NARRATIVE                    
PST2DISC EQU   X'20'               -MAIN DISCOUNT/DIFFERENCE POSTING            
PST2DTAD EQU   X'10'               -TAX ADJUSTMENT TO DISC/DIFF                 
PST2DTAT EQU   X'08'               -TAX ADJUSTMENT TO TAX LEDGER                
PST2ZERO EQU   X'04'               -ALLOW ZERO POSTING (GERMANY)                
PSTIND3  DS    XL1                 POSTING INDICATOR - 3                        
PSTTABL  EQU   *-PSTTABD                                                        
         EJECT                                                                  
SETOTABD DSECT                                                                  
SETOACC  DS    S                                                                
SETOACCX DS    S                                                                
SETOTABL EQU   *-SETOTABD                                                       
         EJECT                                                                  
* ACMRKWRK                                                                      
       ++INCLUDE ACMRKWRK                                                       
WORKD    DSECT                                                                  
         ORG   TEMP                                                             
TEMPAM1  DS    PL6                 TEMPORARY PL6                                
TEMPAM2  DS    PL6                 TEMPORARY PL6                                
                                                                                
SAVED    DSECT                                                                  
         ORG   SOVRWRK                                                          
BANKBAL  DS    XL(L'RECBAL)        BANK ACCOUNT BALANCE                         
BANKOF   DS    XL(L'OFFICE)        BANK ACCOUNT OFFICE                          
BANKOFXL DS    XL(L'OFFICEXL)      EXECUTE L'BANK ACCOUNT OFFICE                
BANKPOFF DS    XL(L'OCNPOFF)       BANK POSTING OFFICE IN OCNEL                 
BANKIND1 DS    XL(L'ACCIND1)       BANK A/C INDICATOR - 1                       
                                                                                
CHQNARRL DS    XL1                 EXECUTE L'CHEQUE NARRATIVE                   
CHQNARR  DS    CL100               CHEQUE NARRATIVE                             
                                                                                
CHQIND1  DS    XL1                 CHEQUE INDICATOR - 1                         
CHQISBNK EQU   X'80'               SPLIT BANK BY OFFICE                         
CHQISPAL EQU   X'40'               SPLIT ACCOUNT BY OFFICE                      
                                                                                
TOTITEM  DS    PL2                 MARKED INVOICES                              
ADDITEM  DS    PL2                 BATCH ITEMS (ADDTRN ADDED RECORDS)           
ADDCASH  DS    PL6                 BATCH CASH POSTED (DEBITS)                   
                                                                                
SBATKEY  DS    XL(L'BATKEY)        SAVED RECORD KEY                             
                                                                                
SBATMON  DS    XL(L'OBATMON)       SAVED BATCH MONTH                            
SBATREF  DS    XL(L'OBATREF)       SAVED BATCH REFERENCE                        
SBATNAM  DS    XL(L'OBATNAM)       SAVED BATCH NAME                             
SBATMONP DS    XL(L'OBATMONP)      SAVED BATCH MONTH (PWOS)                     
                                                                                
SMANCURC DS    CL(L'CURTCUR)       SAVED BANK CURRENCY CODE                     
SMANBCHC DS    CL(L'CURTCUR)       SAVED BANK CHARGES CURRENCY CODE             
                                                                                
         ORG   SOVRWRK2                                                         
*&&UK                                                                           
SAOVRATE DS    XL(L'MANRAT)                                                     
*&&                                                                             
SACHQAMT DS    PL(L'CHQAMT)                                                     
SACHQAMC DS    PL(L'CHQAMC)                                                     
                                                                                
XTSRCNT  DS    XL1                 EXTRA TSAR RECORD COUNT                      
                                                                                
PAYRNUM  DS    XL(L'TSRNUM)        PAYMENT TSAR RECORD NUMBER                   
PAYAMT   DS    PL6                 PAYMENT AMOUNT - AGENCY                      
PAYAMC   DS    PL6                 PAYMENT AMOUNT - LOCAL                       
PAYAMS   DS    PL6                 PAYMENT AMOUNT - SECONDARY                   
PAYOFF   DS    CL2                 PAYMENT OFFICE                               
PAYNARL  DS    XL1                 CURRENT L'PAYMENT NARRATIVE                  
PAYNAR   DS    XL(L'PWINAR)        CURRENT PAYMENT NARRATIVE                    
SPAYNARL DS    XL1                 SAVED L'PAYMENT NARRATIVE                    
SPAYNAR  DS    XL(L'PWINAR)        SAVED PAYMENT NARRATIVE                      
                                                                                
DIFFRNUM DS    XL(L'TSRNUM)        DIFFERENCE TSAR RECORD NUMBER                
DIFFAMT  DS    PL6                 DIFFERENCE AMOUNT - AGENCY                   
DIFFAMC  DS    PL6                 DIFFERENCE AMOUNT - LOCAL                    
DIFFAMS  DS    PL6                 DIFFERENCE AMOUNT - SECONDARY                
DIFFOFF  DS    CL2                 DIFFERENCE OFFICE                            
DIFFNARL DS    XL1                 L'DIFFERENCE NARRATIVE                       
DIFFNAR  DS    XL(L'DIFNAR)        DIFFERENCE NARRATIVE                         
DIFFNAME DS    CL(L'NAMEREC)       DIFFERENCE A/C NAME                          
SDIFNARL DS    XL1                 SAVED L'DIFFERENCE NARRATIVE                 
SDIFNAR  DS    XL(L'DIFNAR)        SAVED DIFFERENCE NARRATIVE                   
SBNKNAME DS    XL(L'NAMEREC)       SAVED BANK A/C NAME                          
                                                                                
         ORG   TOTALS                                                           
TOTCRS   DS    PL8                                                              
TOTMRK   DS    PL8                                                              
*&&UK                                                                           
TOTEXDF  DS    PL8                                                              
TOTDSC   DS    PL8                                                              
*&&                                                                             
TOTCHQ   DS    PL8                                                              
TOTBAL   DS    PL8                                                              
REPMRK   DS    PL8                                                              
*&&UK                                                                           
         ORG   CURTOTS                                                          
CTOCRS   DS    PL8                                                              
CTOMRK   DS    PL8                                                              
CTOEXDF  DS    PL8                                                              
CTODSC   DS    PL8                                                              
CTOCHQ   DS    PL8                                                              
CTOBAL   DS    PL8                                                              
CREMRK   DS    PL8                                                              
                                                                                
         ORG   SCUTOTS                                                          
STOCRS   DS    PL8                                                              
STOMRK   DS    PL8                                                              
STOEXDF  DS    PL8                                                              
STODSC   DS    PL8                                                              
STOCHQ   DS    PL8                                                              
STOBAL   DS    PL8                                                              
SREMRK   DS    PL8                                                              
*&&                                                                             
                                                                                
         ORG   SSAVOLY                                                          
SSIND1   DS    XL1                                                              
SSI1XSYS EQU   X'80'                                                            
SSI1LOCB EQU   X'40'                                                            
SSI1FORB EQU   X'20'                                                            
SSATLX   DS    XL(L'ATLX)                                                       
         DS    XL(L'SSAVOLY-(*-SSAVOLY))                                        
                                                                                
TWAD     DSECT                                                                  
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKEDD                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE3D                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKDAD                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKD9D                                                       
                                                                                
* ACADDTRND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACADDTRND                                                      
         PRINT ON                                                               
                                                                                
* GEGENCUR                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENCUR                                                       
         PRINT ON                                                               
                                                                                
* ACPAY2JD                                                                      
*        PRINT OFF                                                              
*      ++INCLUDE ACPAY2JD                                                       
*        PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114ACMRK0D   05/14/20'                                      
         END                                                                    
