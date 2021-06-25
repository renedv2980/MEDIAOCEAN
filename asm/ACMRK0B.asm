*          DATA SET ACMRK0B    AT LEVEL 010 AS OF 05/14/20                      
*PHASE T6160BB                                                                  
*INCLUDE PAY2JOB                                                                
ACMRK0B  TITLE 'BANK - VOID'                                                    
***********************************************************************         
*                                                                     *         
* ABID 010 16MAY20  EXPENSES EU MF A55/5P - SEED CLAIM ON  DSRD-25289 *         
*                   PAYMENTS - RELINK FOR ACPAY2JOB                   *         
* NMAL 009 09MAR20  FIX-CLEARING 31-BIT MEMORY              SPEC-43879*         
* RGUP 008 21JUN18  ADDITIONAL MEDIA FOR DIGIAL AUDIO       SPEC-20692*         
*                   RESOLVED ISSUE WITH * REF#              SPEC-27644          
* NMAL 007 24AUG18  RECOMPILE TO PICK LATEST PAY2JOB                  *         
* NMAL 006 12JUN18  REFRENCE NUMBER ISSUE                    SPEC-9193*         
* NMAL 005 17OCT17  PAY2JOB PAYMENT SEEDING TO SJ            SPEC-9021*         
***********************************************************************         
* JFOX 205 HANDLE BALANCE/DIFFERENCE TRPELS FROM CREDITOR/MANUAL                
*          FIX UNVOID BUG - DON'T DROP REVERSED EX DIFF CREDIT @ VEND30         
*          CHEQUE CURED MINUS=YES TO SHOW CREDITOR/MANUAL CREDIT NOTES          
* JFOX 206 INVTABN=500/CLEAR NON-MATCHING CREDIT INVTAB ENTRY IF VOID           
* JFOX 207 USE SORAWRK IF PRESENT, INSTEAD OF CPJWRK                            
*          USE SECOND TSAR BUFFER FOR INVRECD                                   
* JFOX 208 SET EARLIEST PAYMENT DATE IN INVOICE TSAR RECORD                     
* JNEW 209 FIX MISSING BANK CHARGE POSTING BUG                                  
* JFOX 210 SKIP NON-EXISTENT EXTRA ACCOUNTS/CONTRAS IN VENDOR ROUTINE           
* JFOX 211 MAKE DEFAULT TAX RATE 15%.  TEST DIFF/TAX ADJ AMT.  DSECT            
* ???? 212 DON'T MAKE LAST ADDTRN CALL OR DMUNLK IF DRAFT                       
* JNEW 213 NOP FFTKREF CODE                                                     
* JFOX 214 VENDOR/VENFILT ONLY TEST TRXSADVC/TRXSDIFF IF TPTAB IN USE           
* JFOX 215 TEST TRANSACTION POINTER OFFICE ONLY IF BANK OFFICE SET              
* JFOX 216 ENSURE ALL TRPEL ADVANCES FOUND IN READINV/VENDOR                    
* JFOX 217 YEAR 2000 FIX IN SETKEY                                              
* JFOX 218 CHANGES FOR U.S. COMPATIBILITY                                       
* JNEW 219-222 MOVE LOAD ADDRESS INSTRUCTION IN VENFILT + OTHER BUGS            
* JFOX 223 REINSTATE FFTKREF CODE                                               
* JFOX 224 BUT ONLY USE FFTKREF FOR DEBITS                                      
* JFOX 225 UNVOID - SET NEW DR TRNKSBR IN CR MPYSUB.  FIX BUGS                  
* JFOX 226 ALLOW MULTIPLE DRS (IE. 2+ DRS RELATED TO AN INVOICE)                
* JFOX 227 REMOVE REFERENCE STACKING CODE                                       
* JFOX 228 DON'T ADD TSAR RECORD FOR INCOMPLETE INVOICE RECORDS                 
* JFOX 229 CARRY INVOICE TRNKREF/TRNKSBR IN TSAR RECORD                         
* JFOX 230 EXCLUDE CHEQUES NOT FOR CONNECTED USER-ID                            
* JFOX 231 MOVE TSAR#INV INTO ACMRKWRK BANK/VOID TSARFREE DEFINITION            
* JFOX 232 TEST FOR USER-ID BEFORE EXCLUDING                                    
* JFOX 233 MAKE USER-ID FILTERING UK-ONLY                                       
* JFOX 234 VOID INCOME BY CLI/PRO (GERMANY)                                     
* JFOX 235 VOID COSTING POSTINGS (GERMANY)                                      
* JFOX 236 ENSURE BTYPYPRG IS NON-ZERO WHEN LIST EXISTS                         
* JFOX 237 HANDLE EXCHANGE DIFFERENCES POSTED TO SJ                             
* JFOX 238 DON'T TEST DISCOUNT/TAX/BANK CHARGE ZERO BALANCE IF DRAFT            
* JFOX 239 EXCLUDE ADVANCES WHICH HAVE BEEN CONTRA'D                            
* JFOX 240 KEEP INVOICE OFFICE/CONTRA/DATE FOR INVOICE SCREEN TSAR KEYS         
* JFOX 241 CHANGE DEFAULT TAX RATE FROM 15%->16% FOR DISCOUNT TAX ADJ.          
* JFOX 242 USE EUREKA INSTEAD OF EXCHP, FOR EURO COMPLIANCE                     
* JFOX 243 TEST REVERSED STATUS FOR PSTTAB POSTINGS IN VENDOR ROUTINE           
* JFOX 244 EXTRACT ONLY CLIENT/PRODUCT FROM PRODUCTION SOURCE ELEMENT           
* JFOX 245 GET MAXIMUM NUMBER OF BIG TEMPEST PAGES                              
* JFOX 246 UK - USE TOBACCO, NOT HELLO TO ADD/DEL CASH ELEMENTS EXCEPT          
*               DEL/RE-ADD OF TRNEL IF AMOUNT & POSITION ARE FIXED              
* JFOX 247 UK - USE TOBACCO TO REVERSE TRANSACTION VALUES                       
* JFOX 248 IF POTENTIAL COSTING, READ/TEST 1C/12 IN CHECKUP                     
* JFOS 249 INCREASE CLPTABN AND COSTABN FROM 16 TO 30                           
* JFOX 250 UK - CAN'T ADD NULL MPYEL VIA TOBACCO - SET VALUES FIRST             
* JFOX 251 UK - PASS TOBACCO VALUES TO ADDTRN                                   
* MWHE 252 INCREASE CLPTABN AND COSTABN FROM 30 TO 65                           
* JFOX 253 UK - PASS MORE TOBACCO VALUES TO ADDTRN                              
* JFOX 254 UK - ALWAYS DELETE/RE-ADD MPYEL (TO REFRESH OCAEL)                   
* JFOX 255 UK - PASS CURRENCY CODES TO TOBACCO ON ACTION TOBAAADD               
* JFOX 001 FIX BUG MISSING RE-READ AFTER EXDF                                   
* JFOX 002 US/UK COMPATIBILITY                                                  
**************************                                                      
**                                                                              
ACMRK0B  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL TABLESL,**MRKB**,RA,R9,R8,CLEAR=YES,RR=RE                        
         LR    RF,RC               SAVE A(ACQUIRED STORAGE)                     
         LR    RC,R1                                                            
         USING WORKD,RC            RC=A(GENERAL WORKING STORAGE)                
         USING SAVED,R7            R7=A(GENERAL SAVED STORAGE)                  
         USING TWAD,R6             R6=A(TERMINAL WORK AREA)                     
         USING OVRWRKD,R5          R5=A(OVERLAY WORK AREA)                      
*&&UK                                                                           
C        USING CURTABD,COMPCURT                                                 
S        USING CURTABD,SCNDCURT                                                 
*&&                                                                             
         LA    R5,L'OVERWORK                                                    
         SH    R5,=Y(OVRWRKDL)                                                  
         JM    *+2                 OVERLAY WORK AREA TOO BIG                    
         L     R5,AOVERWRK                                                      
         ST    RE,OVRELO                                                        
         TM    COMPSTA4,CPYSICPY   TEST MAKING INTERCOMPANY POSTINGS            
         JNO   *+8                                                              
         ST    RF,AICOTAB          SAVE A(ICOTAB)                               
         LA    RF,ICOLTAB(RF)                                                   
         ST    RF,AOFFTAB          SAVE A(OFFTAB)                               
         MVI   OFFLTAB-1(RF),FF    SET EOT FOR OFFTAB                           
         LA    RF,OFFLTAB(RF)                                                   
         ST    RF,APAYTAB          SAVE A(PAYTAB)                               
         MVI   0(RF),FF            SET NEXT SLOT FOR PAYTAB                     
         AH    RF,=Y(PAYLTAB)                                                   
         ST    RF,ATPTAB           SAVE A(TPTAB)                                
         LA    RF,TPLTAB(RF)                                                    
         XC    ACLPTAB,ACLPTAB                                                  
         XC    ACOSTAB,ACOSTAB                                                  
*&&UK                                                                           
         CLI   AGYCTRY,CTRYGER     TEST GERMANY                                 
         BNE   INIT02                                                           
         ST    RF,ACLPTAB          SAVE A(CLPTAB)                               
         LA    RF,CLPLTAB(RF)                                                   
         ST    RF,ACOSTAB          SAVE A(CLPTAB)                               
INIT02   TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    INIT04                                                           
         OI    CHQINDS,CHQISPLT    MAY BE SPLIT POSTINGS BY OFFICE              
*&&                                                                             
INIT04   DS    0H                                                               
                                                                                
         SAM31 ,                                                                
         L     R1,A31BWS           GET AJARAY FROM 31BIT W/S                    
         AHI   R1,JARAY-SVR31BD                                                 
         ST    R1,AJARAY                                                        
         XC    0(255,R1),0(R1)     CLEARING THE MEMORY                          
         SAM24 ,                                                                
                                                                                
         LHI   R1,INPHEDH-TWAD     SET INPUT SCREEN DISPS FOR ROOT              
         STH   R1,DISPHED          DISPLACEMENT OF INPUT HEADLINE               
         AR    R1,R6                                                            
         ST    R1,ADISHEAD         A(INPUT HEADLINE)                            
         LH    R1,=Y(INPHD2H-TWAD) SET INPUT SCREEN DISPS FOR ROOT              
         STH   R1,DISPHED2         DISPLACEMENT OF INPUT HEADLINE               
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
                                                                                
         LA    R1,OVROUTS          SET A(ROOT ROUTINES IN W/S)                  
         LA    R0,OVROUTSN                                                      
         XR    RE,RE                                                            
         L     RF,=A(OVROUT)                                                    
         A     RF,OVRELO           RF=A(GLOBAL ROUTINES)                        
INIT06   STCM  RE,1,0(R1)          SET ROUTINE NUMBER                           
         STCM  RF,7,1(R1)          SET ROUTINE ADDRESS                          
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         JCT   R0,INIT06                                                        
                                                                                
         L     RF,=A(BTYPTAB)                                                   
         A     RF,OVRELO                                                        
         ST    RF,ABTYPTAB                                                      
         L     RF,=A(PROGS)                                                     
         A     RF,OVRELO                                                        
         ST    RF,APROGS                                                        
                                                                                
         LA    R1,TSA2BLK                                                       
         USING TSARD,R1            R1=A(TSAR BLOCK)                             
         LA    R0,TSA2REC                                                       
         ST    R0,TSAREC           SET A(RECORD)                                
         MVC   TSACOM,ACOM         SET A(COMFACS)                               
         MVI   TSKEYL,TSA2KEYL     SET KEY LENGTH                               
         MVC   TSRECL,=Y(TSA2RECL) SET RECORD LENGTH                            
         XC    TSRECI,TSRECI       CLEAR INDICATOR                              
         MVI   TSINDS,TSIALLOC+TSIXTTWA  ALLOCATE BIG TEMPEST PAGES             
         MVI   TSIND2,TSI2BUF2     SET SECOND TSAR BUFFER                       
         MVI   TSNBUF,2            SET NUMBER OF CORE BUFFERS                   
         MVI   TSPAGN,TSNMAX       SET MAX NUMBER OF 14K PAGES                  
         TM    TSA2MODE,TSA2MRSV   TEST TSAR 2 TEMPEST RESERVED                 
         BZ    INIT08                                                           
         MVC   TSINDS,TSA2TSIN     SET INDICATORS                               
         OI    TSINDS,TSIREUSE     SET TO RE-USE PREVIOUS ALLOCATION            
         MVC   TSPAGL,TSA2PAGL     SET LOW PAGE NUMBER                          
         MVC   TSPAGN,TSA2PAGN     SET NUMBER OF PAGES ALLOCATED                
INIT08   MVI   TSACTN,TSAINI       SET INITIALISE                               
         TM    TSA2MODE,TSA2MINI   TEST TSAR 2 INITIALISED                      
         BZ    *+8                                                              
         MVI   TSACTN,TSARES       SET RESTORE                                  
         GOTO1 VTSAR               CALL TO INITIALISE/RESTORE                   
         JNZ   *+2                 KILL IF INITIALISE/RESTORE FAILS             
         MVC   TSA2TSIN,TSINDS                                                  
         NI    TSA2TSIN,TSIALLOC+TSIXTTWA  SAVE TEMPEST INDICATOR               
         MVC   TSA2PAGL,TSPAGL     SAVE LOW PAGE NUMBER                         
         MVC   TSA2PAGN,TSPAGN     SAVE NUMBER OF PAGES ALLOCATED               
         OI    TSA2MODE,TSA2MINI+TSA2MRSV                                       
         DROP  R1                                                               
                                                                                
         CLI   BYTE,ACTIPRVL                                                    
         BE    PREVAL              ROUTINE TO PRE-VALIDATE HEADER               
         TM    TWAMODE2,TWAM2NXA                                                
         BO    NXTACC              ROUTINE TO GET NEXT BANK ACCOUNT             
         CLI   XACTION,ACTUPDT     TEST LIVE UPDATE                             
         BE    UPDATE                                                           
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATE              DRAFT (UPDATE WITHOUT UPDATE)                
         CLI   XACTION,ACTQUIT                                                  
         BE    QUIT                QUIT                                         
         CLI   XACTION,ACTVOID                                                  
         JNE   *+2                 VOID                                         
         CLI   TWASCROV,BVSCR1                                                  
         BE    VALHED              VOID - VALIDATE HEADER                       
         CLI   TWASCROV,BVSCR2                                                  
         BE    VALSEL              VOID - VALIDATE SELECTIONS                   
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* PRE-VALIDATE HEADER                                                 *         
***********************************************************************         
                                                                                
PREVAL   TM    COMPSTA6,CPYSFOCR+CPYSFMCR FOREIGN CURRENCY?                     
         BNZ   PREV02                                                           
*&&UK                                                                           
         XC    VOICURC,VOICURC                  CLEAR BATCH TOTAL TEXT          
         OI    VOICURCH+(FVATRB-FVIHDR),FVAPROT PROTECT BATCH TOTAL             
         OI    VOICURCH+(FVOIND-FVIHDR),FVOXMT                                  
*&&                                                                             
         XC    VOIBCH,VOIBCH                    CLEAR BANK CHARGES              
         OI    VOIBCHH+(FVATRB-FVIHDR),FVAPROT  PROTECT BANK CHARGES            
         OI    VOIBCHH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    VOIBCHT,VOIBCHT                  CLEAR BANK CHARGES              
         OI    VOIBCHTH+(FVATRB-FVIHDR),FVAPROT PROTECT BANK CHARGES            
         OI    VOIBCHTH+(FVOIND-FVIHDR),FVOXMT                                  
PREV02   CLI   PROFBTIC,C'C'       TEST BATCH TOTAL/ITEM COUNT REQUIRED         
         BE    PREV04                                                           
         CLI   PROFBTIC,C'N'       TEST DO NOT SHOW AT ALL                      
         BNE   PREV06                                                           
         OI    VOIITEH+(FVATRB-FVIHDR),FVAPROT  PROTECT ITEM COUNT              
         OI    VOITOTH+(FVATRB-FVIHDR),FVAPROT  PROTECT BATCH TOTAL             
         OI    VOIITEH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    VOITOTH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    VOIITET,VOIITET                  CLEAR ITEM COUNT TEXT           
         XC    VOITOTT,VOITOTT                  CLEAR BATCH TOTAL TEXT          
         OI    VOIITETH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    VOITOTTH+(FVOIND-FVIHDR),FVOXMT                                  
         B     PREV06                                                           
                                                                                
PREV04   OI    VOIITETH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT ITEM COUNT           
         OI    VOITOTTH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT BATCH TOTAL          
         OI    VOIBMOTH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT BATCH MONTH          
         OI    VOIBRFTH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT BATCH REF#           
         OI    VOIBNATH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT BATCH NAME           
         OI    VOIITETH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    VOITOTTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    VOIBMOTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    VOIBRFTH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    VOIBNATH+(FVOIND-FVIHDR),FVOXMT                                  
                                                                                
PREV06   OC    VOIBNA,VOIBNA       TEST BATCH NAME PRESENT                      
         BNZ   PREV08              LEAVE AS FOUND                               
         L     RE,AUTL             SET LUID AS DEFAULT NAME                     
         MVC   VOIBNA(L'TSYM),TSYM-UTLD(RE)                                     
         OI    VOIBNAH+(FVOIND-FVIHDR),FVOXMT                                   
                                                                                
PREV08   DS    0H                  DEFAULT MOA RANGE SUSPENDED                  
                                                                                
PREV10   CLI   PROFVPAY,C'Y'                                                    
         BE    PREV12                                                           
         OI    VOIANPH+(FVATRB-FVIHDR),FVAPROT  PROTECT PAYMENT FIELDS          
         OI    VOIFXRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    VOIANPH+(FVOIND-FVIHDR),FVOXMT   TRANSMIT PAYMENT FIELDS         
         OI    VOIFXRH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    VOIANPT,VOIANPT                  CLEAR FIELD TEXT                
         XC    VOIFXRT,VOIFXRT                                                  
                                                                                
PREV12   LA    R1,LACCOUNT                                                      
         USING ACTRECD,R1                                                       
         CLC   ACTKCULA,SPACES     TEST LAST TYPE/ACTION ACCOUNT                
         BNH   PREV16                                                           
         CLI   LTYPE,TYPBNK        TEST BANK ACTION LAST                        
         BNE   PREV14                                                           
         XC    VOIBNK,VOIBNK                                                    
         OI    VOIBNKH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   VOIBNK(L'ACTKACT),ACTKACT                                        
         CLC   ACTKUNT(L'BANKUL),BANKUL                                         
         BE    PREV16                                                           
         MVI   VOIBNK,C'*'         OVERRIDE BANK UNIT/LEDGER                    
         MVC   VOIBNK+1(L'ACTKULA),ACTKULA                                      
         B     PREV16                                                           
                                                                                
PREV14   DS    0H                                                               
*&&UK                                                                           
         CLI   LTYPE,TYPCRD        TEST CREDITOR ACTION LAST                    
         BNE   PREV16                                                           
         XC    VOICON,VOICON                                                    
         OI    VOICONH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   VOICON(L'ACTKULA),ACTKULA                                        
         B     PREV16                                                           
*&&                                                                             
PREV16   XC    LACCOUNT,LACCOUNT   CLEAR LAST TYPE/ACTION ACCOUNT               
                                                                                
PREVALX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* SET NEXT ACCOUNT IN RELEVANT SCREEN FIELD                           *         
***********************************************************************         
                                                                                
         USING ACTRECD,R2                                                       
NXTACC   CLI   VOIBNKH+(FVILEN-FVIHDR),0  TEST INPUT TO BANK ACCOUNT            
         BNE   NXTACC00                                                         
         XC    TWASKEY,TWASKEY     YES - CLEAR KEY SAVED IN TWA                 
         XC    ACCOUNT,ACCOUNT     AND MUST CLEAR BANK ACCOUNT                  
NXTACC00 OC    ACCOUNT,ACCOUNT     TEST FIRST TIME FOR TWAM2NXA                 
         BNZ   NXTACC02                                                         
         LA    R2,ACCOUNT          YES - REBUILD ACCOUNT                        
         MVC   ACCOUNT,SPACES                                                   
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'BANKUL),BANKUL                                         
         OC    TWASKEY,TWASKEY     TEST ACCOUNT SAVED IN TWA                    
         BZ    NXTACC04                                                         
         MVC   ACTKUNT(L'ACTKCULA-1),TWASKEY  RESTORE ACCOUNT                   
NXTACC02 LA    R2,KEY                                                           
         MVC   ACTKEY,SPACES       BUILD KEY FOR IO ROUTINE                     
         MVC   ACTKCULA,ACCOUNT                                                 
         B     NXTACC08                                                         
                                                                                
NXTACC04 LA    R2,KEY                                                           
         MVC   ACTKCULA,ACCOUNT                                                 
NXTACC06 XR    RF,RF               BUMP KEY FOR NEXT ACCOUNT                    
         IC    RF,ACTKACT+L'ACTKACT-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,ACTKACT+L'ACTKACT-1                                           
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC12            END OF LEDGER                                
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
         JNE   *+2                                                              
         GOTO1 AACCELS                                                          
         OC    RECABLEL,RECABLEL   TEST LOW LEVEL ACCOUNT                       
         BZ    NXTACC06            NO - TRY AGAIN                               
         MVC   ACCOUNT,ACTKCULA                                                 
         B     NXTACC10                                                         
                                                                                
NXTACC08 XR    RF,RF               BUMP KEY FOR NEXT BANK ACCOUNT               
         IC    RF,ACTKCULA+L'ACTKCULA-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,ACTKCULA+L'ACTKCULA-1                                         
         MVI   GETIND,GETIABLQ+GETINLOK                                         
         GOTO1 AGETACC,0                                                        
         BNE   NXTACC10            NOT FOUND/NOT VALID                          
         CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BE    NXTACC14            UNIT/LEDGER IS STILL OK                      
         B     NXTACC12            UNIT/LEDGER HAS CHANGED - FINISH             
                                                                                
NXTACC10 CLC   ACTKCULA(ACTKACT-ACTKEY),ACCOUNT                                 
         BNE   NXTACC12            PAST BANK LEDGER                             
         MVI   GETIND,GETIABLQ+GETINLOK  (RE-)READ ACCOUNT                      
         GOTO1 AGETACC,0                                                        
         BE    NXTACC14            VALID THIS TIME                              
         B     NXTACC08            STILL NO GOOD - TRY NEXT                     
                                                                                
NXTACC12 MVC   FVMSGNO,=AL2(EANOACCS)                                           
         MVC   FVXTRA,SPACES                                                    
         LA    R1,VOIBNKH                                                       
         ST    R1,FVADDR                                                        
         B     NXTACCX                                                          
                                                                                
NXTACC14 MVC   ACCOUNT,ACTKCULA                                                 
         GOTO1 VACSRCHC,DMCB,VOIBNKH,TWAD,BANKUL,                      X        
               (X'C0',BNKNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
*&&UK                                                                           
         GOTO1 AVALBCUR            VALIDATE BANK CURRENCY & SECURITY            
         BH    EXIT                                                             
*&&                                                                             
NXTACC16 OI    VOIBNKH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    VOIBNKH+(FVIIND-FVIHDR),FF-FVIVAL                                
                                                                                
         MVC   FVMSGNO,=AL2(IAEPAP1N)      PROCESS A/C OR PF1 NEXT              
         MVI   FVOMTYP,GTMINF                                                   
         LA    R1,VOIBNKH                                                       
         ST    R1,FVADDR                                                        
                                                                                
NXTACCX  NI    TWAMODE2,FF-TWAM2NXA                                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADER SCREEN FIELDS                                       *         
***********************************************************************         
                                                                                
VALHED   XC    FACTOR,FACTOR       NO SUPPLIER TO VALIDATE                      
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
                                                                                
VALBNK   TM    VOIBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNKX                                                          
*&&UK                                                                           
         XC    PRSTCURT,PRSTCURT                                                
         XC    BANKCUR,BANKCUR                                                  
         NI    SAFCIND1,FF-SAFCI1BC                                             
*&&                                                                             
         CLI   VOIBNK,C'*'         TEST NON-STANDARD BANK U/L                   
         BNE   VALBNK06                                                         
         LA    R1,LDGLIST                                                       
VALBNK02 CLI   0(R1),EOT                                                        
         BNE   VALBNK04                                                         
         MVC   FVMSGNO,=AL2(EALDGINV)                                           
         LA    R1,VOIBNKH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                EXIT WITH ERROR SET                          
VALBNK04 CLC   0(L'LDGLIST,R1),VOIBNK+1                                         
         BE    VALBNK06                                                         
         LA    R1,L'LDGLIST(R1)                                                 
         B     VALBNK02                                                         
                                                                                
VALBNK06 MVI   FVMINL,1                                                         
         GOTO1 AVALBNK,VOIBNKH                                                  
         BH    EXIT                                                             
         GOTO1 VACSRCHC,DMCB,VOIBNKH,TWAD,BANKUL,                      X        
               (X'C0',BNKNDSP),AIOBUFF,(L'RECNAME,RECNAME)                      
         MVC   ACCNAME,RECNAME     SAVE NAME FOR LATER                          
         ZAP   ACCBAL,RECBAL       SAVE BALANCE FOR LATER                       
         MVC   TWASKEY,ACCOUNT+(ACTKUNT-ACTKEY)                                 
*&&US*&& MVC   SACCIND1,ACCIND1    SAVE BANK A/C INDICATOR BYTE 1               
*&&UK                                                                           
         TM    VOIBNKH+(FVIIND-FVIHDR),FVIVAL                                   
         BO    VALBNKX                                                          
         GOTO1 AVALBCUR            VALIDATE BANK CURRENCY & SECURITY            
         BH    EXIT                                                             
*&&                                                                             
VALBNKX  OI    VOIBNKH+(FVIIND-FVIHDR),FVIVAL                                   
                                                                                
***********************************************************************         
* VALIDATE BATCH ITEM COUNT                                           *         
***********************************************************************         
                                                                                
VALITE   TM    VOIITEH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALITEX                                                          
         CLI   PROFBTIC,C'C'       TEST BATCH TOTAL/ITEM COUNT REQ'D            
         BNE   *+8                                                              
         MVI   FVMINL,1            SET REQUIRED                                 
         GOTO1 AVALITE,VOIITEH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALITEX  DS    0H                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE BATCH TOTAL CURRENCY                                       *         
***********************************************************************         
                                                                                
VALCUR   TM    VOICURCH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALCURX                                                          
         PUSH  USING               TEST UNACCEPTABLE CURRENCY FILTER            
PRESET   USING CURTABD,PRSTCURT                                                 
         MVC   PRESET.CURTCUR,VOICURC                                           
         GOTO1 AVALCUR,VOICURCH    PUT VALID CURRENCIES INTO TABLE              
         BH    EXIT                                                             
         TM    VOICURCH+(FVIIND-FVIHDR),FVIVAL                                  
         BO    VALCURX                                                          
         OI    VOICURCH+(FVATRB-FVIHDR),FVAHIGH                                 
         CLC   PRESET.CURTCUR,VOICURC                                           
         BE    VALCURX                                                          
         MVC   VOICURC,SPACES       UNACCEPTABLE CURRENCY FILTER                
         OI    VOICURCH+(FVOIND-FVIHDR),FVOXMT                                  
         MVI   VOICURC,C'*'                                                     
         MVI   VOICURCH+(FVILEN-FVIHDR),1                                       
         CLI   PRESET.CURTCUR,ASTCANY                                           
         BE    *+14                                                             
         MVC   VOICURC(L'CURTCUR),PRESET.CURTCUR                                
         MVI   VOICURCH+(FVILEN-FVIHDR),L'CURTCUR                               
         MVC   FVMSGNO,=AL2(AI$CURFC)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
VALCURX  OI    VOICURCH+(FVIIND-FVIHDR),FVIVAL                                  
         CLC   PRESET.CURTCUR,C.CURTCUR                                         
         BE    *+14                                                             
         CLC   PRESET.CURTCUR,SPACES                                            
         BH    *+10                                                             
         MVC   PRSTCURT,COMPCURT                                                
         POP   USING                                                            
*&&                                                                             
***********************************************************************         
* VALIDATE BATCH TOTAL                                                *         
***********************************************************************         
                                                                                
VALTOT   XC    BATCTOT,BATCTOT                                                  
*&&UK*&& XC    BATCTOTC,BATCTOTC                                                
         TM    VOITOTH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALTOTX                                                          
         OI    VOITOTH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 AFLDVAL,VOITOTH                                                  
         BE    VALTOT02                                                         
         BH    EXIT                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         CLI   PROFBTIC,C'C'       TEST BATCH TOTAL/ITEM COUNT REQ'D            
         BE    EXIT                                                             
         B     VALTOTX                                                          
                                                                                
VALTOT02 XR    R0,R0               TEST VALID AMOUNT                            
         IC    R0,FVILEN                                                        
*&&UK                                                                           
         XR    RF,RF                                                            
         IC    RF,PRSTCURT+(CURTDECP-CURTABD)                                   
         LA    RF,X'80'(RF)                                                     
*&&                                                                             
*&&US*&& LA    RF,X'82'                                                         
         GOTO1 VCASHVAL,DMCB,((RF),VOITOT),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BE    *+14                INVALID AMOUNT                               
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     EXIT                                                             
*&&UK                                                                           
         CLC   VOICURC,C.CURTCUR                                                
         BE    *+14                                                             
         CLC   VOICURC,SPACES      TEST CURRENCY CODE GIVEN                     
         BH    VALTOT04                                                         
*&&                                                                             
         ZAP   BATCTOT,4(8,R1)                                                  
         BP    VALTOTX                                                          
*&&UK                                                                           
VALTOT04 ZAP   BATCTOTC,4(8,R1)                                                 
         BP    *+14                                                             
*&&                                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         B     EXIT                                                             
*&&UK*&& CURED BATCTOTC,(L'VOITOT,VOITOT),PRSTCURT,ALIGN=LEFT,FLOAT=-           
*&&US*&& CURED BATCTOTC,(L'VOITOT,VOITOT),2,ALIGN=LEFT,FLOAT=-                  
VALTOTX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
                                                                                
VALBRF   TM    VOIBRFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBRFX                                                          
         CLI   PROFBTIC,C'C'       TEST BATCH TOTAL/ITEM COUNT REQ'D            
         BNE   *+8                                                              
         MVI   FVMINL,1            SET REQUIRED                                 
         GOTO1 AVALBRF,VOIBRFH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALBRFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
                                                                                
VALBMO   TM    VOIBMOH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBMOX                                                          
         CLI   PROFBTIC,C'C'       TEST BATCH TOTAL/ITEM COUNT REQ'D            
         BNE   *+8                                                              
         MVI   FVMINL,1            SET REQUIRED                                 
         GOTO1 AVALBMO,VOIBMOH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALBMOX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE BATCH NAME                                                 *         
***********************************************************************         
                                                                                
VALBNA   TM    VOIBNAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNAX                                                          
         CLI   PROFBTIC,C'C'       TEST BATCH TOTAL/ITEM COUNT REQ'D            
         BNE   *+8                                                              
         MVI   FVMINL,1            SET REQUIRED                                 
         GOTO1 AVALBNA,VOIBNAH                                                  
         BH    EXIT                INPUT NOT VALID                              
VALBNAX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFICE                                                     *         
***********************************************************************         
                                                                                
VALOFF   TM    VOIOFFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALOFFX                                                          
         XC    VOIOFFN,VOIOFFN                                                  
         OI    VOIOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALOFF,VOIOFFH                                                  
         BL    VALOFFX             NOT REQUIRED, NOT INPUT                      
         BH    EXIT                                                             
         MVC   VOIOFFN,RECNAME                                                  
         OI    VOIOFFNH+(FVOIND-FVIHDR),FVOXMT                                  
VALOFFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CONTRA-ACCOUNT                                             *         
***********************************************************************         
                                                                                
VALCON   TM    VOICONH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALCONX                                                          
         XC    VOICONN,VOICONN                                                  
         OI    VOICONNH+(FVOIND-FVIHDR),FVOXMT                                  
         GOTO1 AVALCON,VOICONH                                                  
         BH    EXIT                                                             
         BL    VALCONX                                                          
         MVC   VOICONN,RECNAME                                                  
         OI    VOICONNH+(FVOIND-FVIHDR),FVOXMT                                  
VALCONX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CHEQUE NUMBER RANGE (REFERENCE NUMBER)                     *         
***********************************************************************         
                                                                                
VALREF   TM    VOIREFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALREFX                                                          
         GOTO1 AVALREF,VOIREFH                                                  
         BH    EXIT                                                             
VALREFX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CHEQUE DATE RANGE (PERIOD)                                 *         
***********************************************************************         
                                                                                
VALPER   TM    VOIPERH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALPERX                                                          
         GOTO1 AVALPER,VOIPERH                                                  
         BH    EXIT                                                             
VALPERX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTIVITY DATE RANGE                                        *         
***********************************************************************         
                                                                                
VALADA   TM    VOIADAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALADAX                                                          
         GOTO1 AVALADA,VOIADAH                                                  
         BH    EXIT                                                             
VALADAX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE CHEQUE MOS RANGE                                           *         
***********************************************************************         
                                                                                
VALMOS   TM    VOIMOAH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,VOIMOAH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE VOID/UNVOID                                                *         
***********************************************************************         
                                                                                
VALVOI   TM    VOIVOIH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALVOIX                                                          
         NI    VOIDIND1,FF-(VOIDVOID+VOIDUVOI)                                  
         GOTO1 AVALICL,VOIVOIH                                                  
         BH    EXIT                                                             
         CLI   ICLMARK,ICLMNO                                                   
         BNE   *+12                                                             
         OI    VOIDIND1,VOIDVOID   SET TO VOID                                  
         B     VALVOIX                                                          
         CLI   ICLMARK,ICLMYES     TEST UNVOIDING                               
         BNE   *+12                                                             
         OI    VOIDIND1,VOIDUVOI   SET TO UNVOID                                
         B     VALVOIX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
                                                                                
VALVOIX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE ANALYSIS BANK POSTINGS                                     *         
***********************************************************************         
                                                                                
VALANP   TM    VOIANPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALANPX                                                          
         NI    VOIDIND1,FF-VOIDANBP                                             
         GOTO1 AVALICL,VOIANPH                                                  
         BH    EXIT                                                             
         CLI   ICLMARK,ICLMNO                                                   
         BE    VALANPX                                                          
         CLI   ICLMARK,ICLMYES                                                  
         BNE   *+12                                                             
         OI    VOIDIND1,VOIDANBP   SET ANALYSED BANK POSTINGS                   
         B     VALANPX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
                                                                                
VALANPX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE ASSOCIATED BANK CHARGES                                    *         
***********************************************************************         
                                                                                
VALBCH   TM    VOIBCHH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBCHX                                                          
         OI    VOIDIND1,VOIDBCHA   DEFAULT TO INCLUDE BANK CHARGES              
         TM    VOIDIND1,VOIDUVOI   IF UNVOIDING ALWAYS DO BCHA                  
         BO    VALBCHX                                                          
         GOTO1 AFLDVAL,VOIBCHH                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
         BL    VALBCHX                                                          
         LA    RE,L'AC@NO-1                                                     
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BNE   *+18                                                             
         CLC   FVIFLD(0),AC@NO                                                  
         NI    VOIDIND1,FF-VOIDBCHA                                             
         B     VALBCHX                                                          
         LA    RE,L'AC@YES-1                                                    
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    VALBCHX                                                          
         CLC   FVIFLD(0),AC@YES                                                 
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
                                                                                
VALBCHX  DS    0H                                                               
                                                                                
                                                                                
***********************************************************************         
* VALIDATE FIXED REFERENCE RANGE                                      *         
***********************************************************************         
                                                                                
VALFXR   TM    VOIFXRH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALFXRX                                                          
         XC    REFVALS(REFVALSL),FXRVALS  SAVE REFVALS IN FXRVALS               
         XC    FXRVALS(REFVALSL),REFVALS                                        
         XC    REFVALS(REFVALSL),FXRVALS                                        
         GOTO1 AVALREF,VOIFXRH            VALIDATE AS REFERENCE                 
         BH    EXIT                       REFVALS WILL BE REVALIDATED           
         XC    REFVALS(REFVALSL),FXRVALS  SWAP REFVALS WITH FXRVALS             
         XC    FXRVALS(REFVALSL),REFVALS                                        
         XC    REFVALS(REFVALSL),FXRVALS                                        
         OC    FXRVALS(REFVALSL),FXRVALS  TEST ANY FIXED REFERENCE              
         BZ    VALFXRX                                                          
         TM    VOIDIND1,VOIDANBP          TEST ANALYSED BANK POSTINGS           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     EXIT                                                             
VALFXRX  DS    0H                                                               
                                                                                
         B     READTRN             READ AND FILTER TRANSACTIONS                 
         EJECT                                                                  
***********************************************************************         
* READ AND FILTER TRANSACTIONS.  PUT QUALIFYING TRANSACTIONS TO TSAR  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
READTRN  OI    DISIND,DISIOFLO     PRESET OVERFLOW                              
         XC    TOT#INV,TOT#INV                                                  
         ZAP   CHQTOT,PZERO                                                     
*&&UK*&& ZAP   CHQTOTC,PZERO                                                    
         LA    R1,TOTALS           CLEAR TOTALS ACCUMULATORS                    
         LA    R0,TOTALSN                                                       
         ZAP   0(L'TOTALS,R1),PZERO                                             
         LA    R1,L'TOTALS(R1)                                                  
         BCT   R0,*-10                                                          
         LA    R2,KEY              BUILD START KEY                              
         GOTO1 SETKEY,SETALL                                                    
*&&US*&& GOTO1 AVALPUB,TRNRECD     VALIDATE SPECIAL PUBLICATION NUMBER          
         MVI   TSARLEN+1,TSARBVL   SET RECORD LENGTH                            
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   READ48                                                           
*&&UK                                                                           
         NI    SAFCIND1,FF-SAFCIGBP ASSUME NO AGENCY CURRENCY                   
         XC    FORECURT,FORECURT   SET FIRST CURRENCY FOUND                     
*&&                                                                             
         B     READ04                                                           
                                                                                
READ02   LA    R2,KEY                                                           
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    READ04                                                           
         XC    TRNKEY,TRNKEY                                                    
                                                                                
READ04   CLC   TRNKCULA,ACCOUNT                                                 
         BE    *+10                                                             
         XC    TRNKEY,TRNKEY                                                    
         CLC   TRNKEY(TRNKSBR-TRNKEY),KEYSAVE                                   
         BE    READ08                                                           
         CP    CHQTOT,PZERO        TEST CHEQUE TOTAL ZERO                       
         BNE   *+12                                                             
         TM    CHQINDS,CHQIFILT    AND NO PART FILTERED                         
         BZ    READ06                                                           
         OC    CHQRNUM,CHQRNUM     TEST FIRST PART OF CHEQUE IN BUFFER          
         BZ    READ06                                                           
         GOTO1 ATSARGET,CHQRNUM    SET FIRST RECORD TO DISPLAY ONLY             
         TM    TSARINDS,TSARDISQ   TEST RECORD IS DISPLAY ONLY                  
         BNZ   READ06                                                           
         OI    TSARINDS,TSARDISQ   NO - SET RECORD IS DISPLAY ONLY              
         MVI   TSARCHA,C'*'                                                     
         L     R1,ATSARBLK                                                      
         MVI   TSACTN-TSARD(R1),TSAPUT                                          
         GOTO1 VTSAR                                                            
         JNE   *+2                                                              
READ06   NI    CHQINDS,FF-CHQIFILT                                              
         XC    CHQRNUM,CHQRNUM                                                  
         ZAP   CHQTOT,PZERO                                                     
*&&UK*&& ZAP   CHQTOTC,PZERO                                                    
         OC    TRNKEY,TRNKEY       TEST END OF ACCOUNT                          
         BZ    READ48                                                           
READ08   TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   READ02                                                           
         LA    RF,BOQ              EXCLUDE REVERSALS IF VOIDING                 
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+8                                                              
         LA    RF,BZQ              REVERSALS ONLY IF UNVOIDING                  
         TM    TRNKSTAT,TRNSREVS                                                
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   READ02                                                           
         TM    TRNKSTA2,TRNSPEEL                                                
         BNZ   READ02                                                           
                                                                                
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READ10              NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    READ10              YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    READ10                                                           
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         BNE   READ48                                                           
         CLC   TRNKOFF(0),OFFICE                                                
READ10   OC    CONTRA,CONTRA                                                    
         BZ    READ12                                                           
         IC    RF,CONTRAXL                                                      
         EX    RF,*+8                                                           
         BE    READ12                                                           
         CLC   TRNKCULC(0),CONTRA                                               
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READ48              NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    READ48              YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READ48              YES - GONE TOO FAR                           
         GOTO1 SETKEY,SETCON+NXTOFF                                             
         B     READ02                                                           
READ12   CLC   TRNKDATE,PERSTA                                                  
         BNL   READ14                                                           
         GOTO1 SETKEY,SETSDT                                                    
         B     READ02                                                           
READ14   CLC   TRNKDATE,PEREND                                                  
         BNH   READ18                                                           
         TM    CONTIND,CONTILOQ    TEST LOW-LEVEL CONTRA FILTER                 
         BNZ   READ16              YES - DON'T SET NEXT CONTRA                  
         GOTO1 SETKEY,SETSDT+NXTCON                                             
         B     READ02                                                           
READ16   TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    READ48              NO - GONE TOO FAR                            
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    READ48              YES - GONE TOO FAR                           
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BNZ   READ48              YES - GONE TOO FAR                           
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
         JNE   *+2                                                              
         DROP  R2                                                               
         GOTO1 ASETELAD,AIOBUFF    SET ELEMENT A-TYPES                          
         GOTO1 AGENFILT,AIOBUFF                                                 
         BNE   READ02                                                           
         BAS   RE,RFILTER          OVERLAY SPECIFIC FILTERING                   
         BNE   READ02                                                           
         XC    TSARREC+L'TSARLEN(TSARRECL-L'TSARLEN),TSARREC+L'TSARLEN          
                                                                                
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
         DROP  R1                                                               
                                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL        EXTRACT TRNEL VALUES                         
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    *+10                NO - DON'T SET TSAROFF                       
         MVC   TSAROFF,TRNOFFC                                                  
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         MVI   TSARCHA,C' '                                                     
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         MVI   TSARINDS,TSARMCHQ                                                
         ZAP   TSARAMNT,TRNAMNT                                                 
         ZAP   TSARFDIS,PZERO                                                   
         TM    VOIDIND1,VOIDVOID   TEST VOIDING                                 
         BZ    READ26                                                           
         TM    TRNSTAT,TRNSBREC    TEST BANK RECONCILED                         
         BNO   READ26                                                           
         OI    TSARINDS,TSARDISQ   DISPLAY-ONLY TRANSACTION                     
         MVI   TSARCHA,C'*'                                                     
         B     READ32                                                           
         DROP  R2                                                               
                                                                                
READ26   DS    0H                                                               
*&&UK                                                                           
         USING AFCELD,R2           EXTRACT AFC AMOUNT                           
         ZAP   TSARAFCA,PZERO                                                   
         ICM   R2,15,AAFCEL                                                     
         BZ    *+10                                                             
         ZAP   TSARAFCA,AFCAMNT                                                 
         DROP  R2                                                               
*&&                                                                             
         USING SCIELD,R2           DEAL WITH CROSS-OFFICE CHEQUE                
         ICM   R2,15,ASCICHQT                                                   
         BZ    READ32                                                           
         TM    CHQINDS,CHQIFILT    TEST OFFICE TRANSACTION FILTERED             
         BNZ   READ28                                                           
         OC    CHQRNUM,CHQRNUM     TEST WE HAVE ALREADY STARTED                 
         BNZ   READ28                                                           
         CP    TSARAMNT,SCIAMNT    FIRST CHEQUE - TEST TOTAL DIFFERS            
         BE    *+10                                                             
         ZAP   TSARCHQT,SCIAMNT    YES - SAVE TOTAL OF CHEQUE                   
         ZAP   CHQTOT,SCIAMNT                                                   
*&&UK                                                                           
         CLI   SCILN,SCILN2Q       CALCULATE AMOUNT IN CURRENCY                 
         BL    *+20                                                             
         ZAP   TSARCHQC,SCIADMN                                                 
         ZAP   CHQTOTC,SCIADMN                                                  
         B     READ30                                                           
         ZAP   CHQTOTC,TSARAMNT                                                 
         BZ    READ30                                                           
         ZAP   PKWK16A,SCIAMNT                                                  
         MP    PKWK16A,TSARAFCA                                                 
         SRP   PKWK16A,RNDING,0                                                 
         DP    PKWK16A,TSARAMNT                                                 
         ZAP   PKWK16B,PKWK16A(L'PKWK16A-L'TSARAMNT)                            
         SRP   PKWK16B,64-RNDING,5                                              
         ZAP   CHQTOTC,PKWK16B                                                  
         ZAP   TSARCHQC,PKWK16B                                                 
         B     READ30                                                           
*&&                                                                             
READ28   OI    TSARINDS,TSARDISQ   STARTED - SET DISPLAY-ONLY CHEQUE            
         MVI   TSARCHA,C'*'                                                     
         ZAP   TSARCHQT,SCIAMNT    DISPLAY-ONLY - SAVE TOTAL OF CHEQUE          
*&&UK                                                                           
         CLI   SCILN,SCILN2Q       CALCULATE AMOUNT IN CURRENCY                 
         BL    *+14                                                             
         ZAP   TSARCHQC,SCIADMN                                                 
         B     READ30                                                           
         ZAP   TSARCHQC,TSARAMNT                                                
         BZ    READ30                                                           
         ZAP   PKWK16A,SCIAMNT                                                  
         MP    PKWK16A,TSARAFCA                                                 
         SRP   PKWK16A,RNDING,0                                                 
         DP    PKWK16A,TSARAMNT                                                 
         ZAP   PKWK16B,PKWK16A(L'PKWK16A-L'TSARAMNT)                            
         SRP   PKWK16B,64-RNDING,5                                              
         ZAP   TSARCHQC,PKWK16B                                                 
*&&                                                                             
READ30   SP    CHQTOT,TSARAMNT     TAKE THIS CHEQUE FROM TOTAL                  
*&&UK*&& SP    CHQTOTC,TSARAFCA                                                 
         DROP  R2                                                               
                                                                                
         USING TRSELD,R2           EXTRACT TRSEL VALUES                         
READ32   ICM   R2,15,ATRSEL                                                     
         JZ    *+2                                                              
         TM    TRSSTAT,TRSSVOID    TEST VOID                                    
         BNO   *+14                                                             
         OI    TSARINDS,TSARMKQ+TSARINMQ                                        
         AP    TOTVOID,TSARAMNT    ADD TO VOID TOTAL                            
         MVC   TSARADAT,TRSDATE                                                 
         MVC   TSARSSTA,TRSSTAT                                                 
         MVC   TSARSST2,TRSSTAT2                                                
         MVC   TSARSST3,TRSSTAT3                                                
*&&UK                                                                           
         TM    TRSSTAT,TRSSVOID    TEST VOID                                    
         BNO   READ34                                                           
         DROP  R2                                                               
                                                                                
         USING AFCELD,R2                                                        
         ICM   R2,15,AAFCEL                                                     
         BZ    READ34                                                           
         AP    CURVOID,AFCAMNT                                                  
         DROP  R2                                                               
*&&                                                                             
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
READ34   ICM   R2,15,AOTHEL                                                     
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
         DROP  R2                                                               
                                                                                
         USING CEDELD,R2           EXTRACT CHEQUE EXTRA DETAILS                 
         MVC   TSAR#INV,=H'1'      ASSUME JUST ONE INVOICE                      
         ICM   R2,15,ACEDEL                                                     
         BZ    *+10                                                             
         MVC   TSAR#INV,CED#INV    NUMBER OF INVOICES PAID BY CHEQUE            
         DROP  R2                                                               
                                                                                
*&&UK*&& GOTO1 AVAL1FC             VALIDATE ONE FOREIGN CURRENCY                
                                                                                
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    READ36                                                           
         USING MPYELD,R2           EXTRACT MPYEL VALUES                         
         ICM   R2,15,AMPYEL                                                     
         BZ    READ36                                                           
         MVC   TSARFDIS,MPYNO      USE DISCOUNT FIELD FOR PAYMENT REF           
         MVC   TSARFSAC,MPYBNK     USE SOURCE A/C FOR PAYMENT BANK A/C          
         DROP  R2                                                               
                                                                                
READ36   ZAP   TSARCADV,PZERO      CLEAR ADVANCE PART OF CHEQUE                 
         ZAP   TSARCDIF,PZERO      CLEAR DIFFERENCE PART OF CHEQUE              
         ICM   R2,15,ATRPEL1                                                    
         BZ    READ42                                                           
         USING TRPELD,R2           EXTRACT TRPEL VALUES                         
READ38   CLI   TRPEL,0             TEST EOR                                     
         BE    READ42                                                           
         CLI   TRPEL,TRPELQ        TEST TRPEL                                   
         BNE   READ40                                                           
         CLI   TRPTYPE,TRPTADV     TEST ADVANCE                                 
         BNE   *+14                                                             
         AP    TSARCADV,TRPAMNT    SET ADVANCE PART OF CHEQUE                   
         B     READ40                                                           
         CLI   TRPTYPE,TRPTDIF     TEST MAIN DIFFERENCE POSTING                 
         BNE   READ40                                                           
         AP    TSARCDIF,TRPAMNT    SET ADVANCE PART OF CHEQUE                   
READ40   SR    R1,R1               NEXT TRPEL                                   
         IC    R1,TRPLN                                                         
         AR    R2,R1                                                            
         B     READ38                                                           
                                                                                
READ42   DS    0H                  NEXT ELEMENT FOR DATA EXTRACTION             
                                                                                
READ44   GOTO1 ATSARADD            PUT RECORD TO TSAR                           
         BNE   READ46                                                           
         OC    CHQRNUM,CHQRNUM     TEST FIRST RECORD FOR NEW CHEQUE             
         BNZ   *+14                                                             
         L     R1,ATSARBLK                                                      
         MVC   CHQRNUM,TSRNUM-TSARD(R1)                                         
         ICM   R1,3,CHQCNT         NUMBER OF CHEQUES ADDED                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,CHQCNT         SAVE COUNT                                   
*&&UK                                                                           
         USING AFCELD,R2                                                        
         ICM   R2,15,AAFCEL                                                     
         BZ    *+10                                                             
         AP    CURCHQS,AFCAMNT                                                  
         DROP  R2                                                               
*&&                                                                             
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         AP    TOTCHQS,TRNAMNT                                                  
         MVI   ANYADD,1            SET TRANSACTION(S) ADDED                     
         B     READ02              READ SEQUENTIAL                              
         DROP  R2                                                               
                                                                                
READ46   TM    DISIND,DISIOFLO     TEST ERROR WAS OVERFLOW                      
         BNO   EXIT                NO - EXIT WITH ROOT ERROR SET                
         LA    R1,VOIBNKH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EATRNMAX)                                           
         NI    TWAMODE,TWAMRSRV                                                 
         B     EXIT                                                             
                                                                                
READ48   NI    DISIND,FF-DISIOFLO  CLEAR OVERFLOW (DID NOT OCCUR)               
         CLI   ANYADD,1            TEST ANYTHING ADDED                          
         BE    READ50                                                           
         MVC   FVMSGNO,=AL2(IANOTRAN)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         LA    R1,VOIBNKH          CURSOR TO SUPPLIER FIELD                     
         ST    R1,FVADDR                                                        
         NI    TWAMODE,TWAMRSRV                                                 
         B     EXIT                                                             
                                                                                
                                                                                
READ50   GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(1,0),MRKOLAYH               
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*&&UK                                                                           
         GOTO1 ABLDCURR            BUILD CURRENCY ENTRIES                       
         JH    *+2                                                              
         GOTO1 ASETFORE            SET UP SINGLE FOREIGN CURRENCY               
*&&                                                                             
         OI    TWAMODE,TWAMHDRS    INDICATE WE HAVE A HEADER SAVED              
         GOTO1 AOVRSCR,CCSCR2      OVERLAY ACTION INPUT SCREEN                  
         OI    MRKTYPH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    MRKACTH+(FVATRB-FVIHDR),FVAPROT                                  
         LH    R1,=Y((6133/TSARBVL)*TSPEXPN)                                    
         SH    R1,DISMAX           FIND OUT HOW MANY RECORDS LEFT               
         STH   R1,INVMAX                                                        
         CLC   CHQCNT,=H'1'        TEST SINGLE CHEQUE                           
         BNE   READ52                                                           
         MVC   TWAHALF,CHQCNT                                                   
         GOTO1 ATSARGET,TWAHALF    GET IT BACK AGAIN                            
         TM    TSARINDS,TSARDISQ   TEST DISPLAY-ONLY CHEQUE                     
         BO    READ52              DON'T SHOW INVOICE(S)                        
         MVC   SDISLCNT,TWAHALF    SET 'SAVED' CHEQUE DISPLAY VALUES            
         XC    SDISLIST,SDISLIST                                                
         MVC   SDISLIST(L'TWAHALF),TWAHALF                                      
         B     PROSEL                                                           
                                                                                
READ52   XC    TWAHALF,TWAHALF     CLEAR TWA TSAR RECORD NUMBER                 
         MVI   FILTCHQ,INCLUDE     DISPLAY CHEQUES                              
         B     DISTRN                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SELECTION                                                  *         
***********************************************************************         
         USING DISLINED,R2                                                      
VALSEL   CLI   FILTCHQ,INCLUDE     TEST CHEQUE MODE                             
         BNE   VALINP              NO - INVOICE MODE                            
         XC    TWAHALF,TWAHALF     CLEAR TWA TSAR RECORD NUMBER                 
         MVI   ANYMARK,0                                                        
         LA    R3,DISLIST          R3=A(LIST OF TSAR RECORDS DISPLAYED)         
         L     R2,ADISDET1         R2=A(1ST DETAIL LINE)                        
         XC    FVADDR,FVADDR       CLEAR FIELD ADDRESS                          
         XR    R0,R0                                                            
         ICM   R0,3,DISLCNT        NUMBER OF DISPLAY LINES                      
         BZ    VALS22              NO RECORDS ON DISPLAY                        
         TM    TWAMODE2,TWAM2SKP   TEST SKIP VALIDATION                         
         BZ    VALS02                                                           
         NI    TWAMODE2,FF-TWAM2SKP  RESET SKIP VALIDATION                      
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BNE   VALS22              YES - CALL DISPLAY                           
                                                                                
VALS02   MVC   HALF,0(R3)                                                       
         GOTO1 ATSARGET,HALF                                                    
         JNE   *+2                                                              
         TM    TSARINDS,TSARDISQ   TEST DISPLAY-ONLY RECORD                     
         BO    VALS18                                                           
         CLI   DISLMARK,C'*'       TEST USER IS PROTECTING                      
         BNE   *+12                                                             
         MVI   DISLMARK,C' '       CLEAR PROTECT CHARACTER                      
         B     VALS16              AND RE-DISPLAY                               
         CLI   OPTPAG,0            TEST PAGE OPTION                             
         BE    *+10                                                             
         MVC   DISLMARK,OPTPAG     SET IN MARK FIELD                            
         CLC   DISLMARK,AC3SELC    TEST CHEQUE SELECTED                         
         BE    VALS04                                                           
         CLI   DISLMARK,C' '       TEST SPACE - MIGHT BE DE-SELECTING           
         BH    VALS08                                                           
         MVI   DISLMARK,C' '       SET ANYTHING < C' ' TO C' '                  
VALS04   CLC   TSARCHA,DISLMARK    TEST CHANGE - SELECT/DE-SELECT               
         BE    VALS18              NO CHANGE                                    
         CLI   DISLMARK,C' '       TEST DESELECTING                             
         BE    VALS06                                                           
         LA    R1,DISLHDR2         SAVE A(MARK FIELD)                           
         ST    R1,FVADDR                                                        
         GOTO1 CHQMAN,CHQSETQ      SET CHEQUE DATA FROM TSAR RECORD             
         MVI   OVFLAG,OVFRCK       READ AND CHECK INVOICES WILL ALL FIT         
         BAS   RE,READINV                                                       
         BE    VALS06                                                           
         MVC   FVMSGNO,=AL2(EATRNMAX)                                           
         B     VALSELX                                                          
                                                                                
VALS06   MVC   TSARCHA,DISLMARK    SET NEW SELECT STATUS                        
         MVI   ANYMARK,1           SET CHANGE                                   
         L     RF,ATSARBLK                                                      
         USING TSARD,RF                                                         
         MVI   TSACTN,TSAPUT       WRITE BACK THE CHEQUE                        
         GOTO1 VTSAR,TSARD                                                      
         BE    VALS16                                                           
         DC    H'0'                                                             
         DROP  RF                                                               
                                                                                
VALS08   LA    RF,DISLHDR2                                                      
         ST    RF,FVADDR                                                        
         LA    RF,BOQ              BO                                           
         CLC   DISLMARK,AC@YES     TEST VOIDING                                 
         BE    VALS10                                                           
         LA    RF,BZQ              BZ                                           
         CLC   DISLMARK,AC@NO      TEST UNVOIDING                               
         BE    VALS10                                                           
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         B     VALSELX             ERROR - INVALID INPUT                        
                                                                                
VALS10   TM    TSARINDS,TSARMKQ    TEST MARKED                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   VALS16              NO ACTION - RE-DISPLAY                       
         XC    WORK(L'TRSPMOS),WORK                                             
         OC    WORK(L'TRSPMOS),OBATMONP                                         
         BNZ   *+14                                                             
         OC    WORK(L'TRSPMOS),BATMONP                                          
         BZ    VALS12              DON'T KNOW BATCH MONTH YET                   
         CLC   TSARMOS,WORK        TEST TRANSACTION MONTH > BATCH MONTH         
         BNH   VALS12                                                           
         MVC   FVMSGNO,=AL2(AE$BMECM)                                           
         B     VALSELX                                                          
VALS12   OC    TSARCHQT,TSARCHQT   TEST WE HAVE TOTAL CHEQUE AMOUNT             
         BZ    VALS14                                                           
         CP    TSARCHQT,TSARAMNT   TEST FULL AMOUNT                             
         BE    VALS14                                                           
*??????? OC    TWAACCS,TWAACCS                                                  
*??????? BZ    *+14                                                             
*??????? MVC   FVMSGNO,=AL2(EALIMACC)  CAN'T MARK DUE TO LIMIT ACCESS           
*??????? B     VALSELX                                                          
         OC    OFFICE,OFFICE                                                    
         BZ    VALS14                                                           
         MVC   FVMSGNO,=AL2(EAOFFILT)  CAN'T MARK DUE TO OFFICE FILTER          
         B     VALSELX                                                          
VALS14   MVC   STSARNUM,HALF       SAVE TSAR RECORD NUMBER                      
         GOTO1 CHQMAN,CHQSETQ      SET CHEQUE DATA FROM TSAR RECORD             
         MVI   OVFLAG,OVFCHK       SET TO CHECK INVOICES                        
         BAS   RE,READINV          READ INVOICES FROM FILE, CHECK ZERO          
         BNE   VALSELX                                                          
                                                                                
         XR    RF,RF                                                            
         ICM   RF,3,TSAR#INV       NUMBER OF INVOICES TO VOID                   
         CLC   DISLMARK,AC@YES     TEST VOIDING                                 
         BE    *+6                                                              
         LCR   RF,RF               UNVOIDING -VE                                
         AH    RF,TOT#INV                                                       
         LPR   RE,RF                                                            
         CH    RE,=H'1500'                                                      
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(EATRNMX2)  TOO MANY INVOICES TO REVERSE             
         B     VALSELX                                                          
         STH   RF,TOT#INV                                                       
                                                                                
         GOTO1 CHQMAN,CHQMRKQ      (UN)MARK CHEQUE(S), UPDATE TOTALS            
         B     VALS18                                                           
                                                                                
VALS16   GOTO1 ABLDLIN,DISLHDR1    REBUILD DISPLAY LINE                         
                                                                                
VALS18   LA    R2,DISLINEL(R2)     R2=A(NEXT INPUT LINE)                        
         LA    R3,L'DISLIST(R3)    R3=A(NEXT TSAR RECORD NUMBER)                
         BCT   R0,VALS02                                                        
         DROP  R2                                                               
                                                                                
*&&UK*&& CLC   INPSUB,AC@END       TEST USER WANTS TO INSPECT CHEQUE(S)         
*&&US*&& CLC   INPSUB,AC3SELC                                                   
         MVI   INPSUB,C' '         CLEAR ANYTHING ELSE                          
         BE    *+8                                                              
         CLI   TWAPFK,PFK05        MAY USE PF KEY                               
         MVI   TWAPFK,0            CLEAR ANYTHING ELSE                          
         BNE   VALS20              NO - CARRY ON IN CHEQUE MODE                 
         CLC   CHQCNT,=H'1'        TEST SINGLE CHEQUE                           
         BNE   *+12                                                             
         TM    TSARINDS,TSARDISQ   TEST DISPLAY-ONLY                            
         BNZ   VALS20              CARRY ON IN CHEQUE MODE                      
         MVC   SMRKOPT,MRKOPT      SAVE OPTIONS FIELD                           
         MVC   STOPTDIS,OPTDIS     SAVE DISPLAY OPTION FROM STORAGE             
         MVC   SDISLCNT,DISLCNT    SAVE COUNT OF RECORDS                        
         MVC   SDISLIST,DISLIST    SAVE RECORD LIST                             
         B     PROSEL              PROCESS SELECTED CHEQUE(S)                   
                                                                                
VALS20   CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   *+16                                                             
         CLI   OPTPAG,0            TEST PAGE OPTION IN USE                      
         BE    *+8                                                              
         OI    TWAMODE2,TWAM2SKP   SET SKIP VALIDATION                          
         TM    DISIND,DISINCOL     TEST NEW COLUMN DISPLAY                      
         BNZ   VALS22              DISPLAY NEW COLUMNS (NO SCROLLING)           
         CLI   ANYMARK,1           TEST ANY CHANGES                             
         BNE   VALS22                                                           
         OI    DISIND,DISIFFLT     FORCE FILTERING OF DISLIST NEXT TIME         
         B     VALS24              AND RE-DISPLAY THIS SCREEN                   
                                                                                
VALS22   GOTO1 ADISPLAY                                                         
                                                                                
VALS24   MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         MVC   FVMSGNO,=AL2(IAMKTNOM)                                           
         TM    DISIND,DISIBOF+DISIEOF                                           
         BNZ   *+10                                                             
         MVC   FVMSGNO,=AL2(IAMKTEPA)                                           
         ICM   R1,15,FVADDR        TEST/SET CURSOR ADDRESS                      
         BNZ   VALS28                                                           
         TM    DISIND,DISIBOF+DISIEOF                                           
         BNZ   VALS26                                                           
         XR    R0,R0                                                            
         ICM   R0,3,DISLCNT                                                     
         BZ    VALS26                                                           
         LA    R1,INPMRKH          SET CURSOR TO 1ST UNPROT MARK FIELD          
         TM    FVATRB-FVIHDR(R1),FVAPROT                                        
         BZ    VALS28                                                           
         LA    R1,DISLINEL(R1)                                                  
         BCT   R0,*-12             TRY ALL DISPLAY FIELDS                       
                                                                                
VALS26   LA    R1,INPSUBH          ELSE - CURSOR TO SUB-ACTION FIELD            
                                                                                
VALS28   ST    R1,FVADDR                                                        
         GOTO1 ABLDTOT,INPTOTH                                                  
                                                                                
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PROCESS SELECTION                                                   *         
***********************************************************************         
PROSEL   NI    TWAMODE2,FF-TWAM2DIS  UNPROTECT DISPLAY                          
         CLC   CHQCNT,=H'1'        TEST SINGLE CHEQUE FROM READ                 
         BNE   PROS02                                                           
         XC    CHQCNT,CHQCNT       CLEAR COUNT TO FORCE LOOP NEXT TIME          
         B     PROS06              PROCESS THE SINGLE CHEQUE                    
                                                                                
PROS02   XC    MRKOPT,MRKOPT                                                    
         XC    OPTIONS(KEYOPTSL),OPTIONS                                        
         XC    OPTALPG,OPTALPG       CLEAR ALL/PAGE OPTION                      
         NI    TWAMODE2,FF-TWAM2SKP  CLEAR SKIP MODE                            
         OC    OPTDIS,OPTDIS                                                    
         BZ    PROS04                                                           
         MVC   MRKOPT(L'OP3DSP),OP3DSP                                          
         LA    R1,MRKOPT+L'OP3DSP                                               
         CLI   0(R1),C' '                                                       
         BH    *+8                 FIRST NON-SPACE                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'='          RESTORE CHARACTER DISPLAY OPTION             
         MVC   2(L'SOPTDIS,R1),SOPTDIS                                          
                                                                                
PROS04   LH    R1,TWAHALF                                                       
         LA    R1,1(R1)                                                         
         STH   R1,TWAHALF                                                       
         GOTO1 ATSARGET,TWAHALF                                                 
         BNE   PROSELX             NO MORE CHEQUES                              
         CLC   TSARCHA,AC3SELC                                                  
         BNE   PROS04                                                           
                                                                                
PROS06   MVC   STSARNUM,TWAHALF    SAVE TSAR NUMBER OF SELECTED CHEQUE          
         MVC   INPCHQ,TSARREF                                                   
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(17,INPCDT)                             
         ZAP   DUB,TSARAMNT                                                     
         OC    TSARCHQT,TSARCHQT                                                
         BZ    *+10                                                             
         ZAP   DUB,TSARCHQT                                                     
         CURED DUB,(13,INPCAM),2,ALIGN=LEFT,MINUS=YES                           
*&&UK                                                                           
         OC    TSARAFCC,TSARAFCC                                                
         BZ    PROS08                                                           
         L     RF,ACURRTAB                                                      
         LA    R0,CURRTABN                                                      
         USING CURTABD,RF                                                       
         CLC   CURTCUR,TSARAFCC                                                 
         BE    *+16                                                             
         LA    RF,CURTABL(RF)                                                   
         BCT   R0,*-14                                                          
         B     PROS08                                                           
         ZAP   DUB,TSARAFCA                                                     
         OC    TSARCHQC,TSARCHQC                                                
         BZ    *+10                                                             
         ZAP   DUB,TSARCHQC                                                     
         CURED DUB,(13,INPCAM),CURTABD,CURSYMB=YES,ALIGN=LEFT,MINUS=YES         
         DROP  RF                                                               
*&&                                                                             
PROS08   LA    R1,L'INPCHQT-1                                                   
         LA    R0,L'LC@CHK-1                                                    
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INPCHQT(0),LC@CHK                                                
         LA    R1,L'INPCDTT-1                                                   
         LA    R0,L'LC@DATED-1                                                  
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INPCDTT(0),LC@DATED                                              
         LA    R1,L'INPCAMT-1                                                   
         LA    R0,L'LC@AMT-1                                                    
         CR    R1,R0                                                            
         BNH   *+6                                                              
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INPCAMT(0),LC@AMT                                                
         MVI   FILTCHQ,EXCLUDE     SHOW INVOICES NOW                            
         GOTO1 CHQMAN,CHQSETQ      SET CHEQUE DATA FROM TSAR RECORD             
         XC    INPSUB,INPSUB                                                    
*&&US                                                                           
         LA    R0,VENDLDGN                                                      
         LA    R1,VENDLDGS                                                      
         CLC   0(L'ACTKLDG,R1),TSARCON+(ACTKLDG-ACTRECD)                        
         BE    *+16                                                             
         LA    R1,L'VENDLDGS(R1)                                                
         BCT   R0,*-14                                                          
         B     *+10                                                             
         MVC   ACCIND1,1(R1)       SET VENDOR ACCOUNT INDICATOR                 
*&&                                                                             
         MVI   OVFLAG,OVFRDA       SET TO READ AND ADD INVOICES                 
         BAS   RE,READINV          READ INVOICES FROM FILE, ADD TO TSAR         
                                                                                
         OI    INPCHQH+(FVATRB-FVIHDR),FVAHIGH  HIGHLIGHT CHQ DETAILS           
         OI    INPCDTH+(FVATRB-FVIHDR),FVAHIGH                                  
         OI    INPCAMH+(FVATRB-FVIHDR),FVAHIGH                                  
         OI    TWAMODE2,TWAM2DIS   SET PROTECTED DISPLAY                        
         NI    INPMRKH+(FVOIND-FVIHDR),FF-FVOCUR  AND MOVE CURSOR TO            
         OI    INPSUBH+(FVOIND-FVIHDR),FVOCUR+FVAHIGH  SUB-ACTION FIELD         
         OI    INPSUBTH+(FVATRB-FVIHDR),FVAHIGH                                 
         B     DISTRN                                                           
                                                                                
PROSELX  XC    INPCHQT,INPCHQT     END OF ALL SELECTIONS                        
         XC    INPCDTT,INPCDTT                                                  
         XC    INPCAMT,INPCAMT                                                  
         XC    INPCHQ,INPCHQ                                                    
         XC    INPCDT,INPCDT                                                    
         XC    INPCAM,INPCAM                                                    
         XC    INPSUB,INPSUB                                                    
         NI    INPSUBH+(FVATRB-FVIHDR),FF-FVAHIGH                               
         NI    INPSUBTH+(FVATRB-FVIHDR),FF-FVAHIGH                              
         NI    TWAMODE2,FF-TWAM2SKP  CLEAR SKIP MODE                            
         XC    OPTIONS(KEYOPTSL),OPTIONS                                        
         XC    OPTALPG,OPTALPG     CLEAR ALL/PAGE OPTION                        
*&&US*&& MVC   ACCIND1,SACCIND1    RESTORE BANK A/C INDICATOR BYTE 1            
         MVC   MRKOPT,SMRKOPT      RESTORE OPTIONS                              
         MVC   OPTDIS,STOPTDIS     RESTORE DISPLAY OPTION IN STORAGE            
         MVC   DISLCNT,SDISLCNT    RESTORE COUNT OF RECORDS                     
         MVC   DISLIST(DISLISTL),SDISLIST  RESTORE RECORD LIST                  
         MVI   FILTCHQ,INCLUDE     SHOW CHEQUES NOW                             
         OI    DISIND,DISINCOL     DON'T SCROLL - JUST RESHOW                   
         GOTO1 ADISPLAY            DISPLAY THEM                                 
         B     DISTRN04            BUILD TOTALS AND EXIT                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY TRANSACTIONS                                                *         
***********************************************************************         
DISTRN   OI    DISIND,DISIRST          SET START FROM BEGINNING                 
         GOTO1 ADISPLAY                                                         
         CLI   FILTCHQ,EXCLUDE         TEST INVOICE MODE                        
         BNE   DISTRN04                                                         
         CP    TOTDIFF,PZERO           TEST INVOICES BALANCE CHEQUE             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(IAMINECH)  WARN USER                                
         B     DISTRN04                                                         
         CP    OCHQADV,PZERO           TEST ADVANCES BALANCE TO ZERO            
         BE    DISTRN02                                                         
         MVC   FVMSGNO,=AL2(AI$ADMIS)  WARN USER                                
         B     DISTRN04                                                         
                                                                                
DISTRN02 MVC   FVMSGNO,=AL2(IAINVDIS)  INVOICES DISPLAYED - PAGE                
         TM    DISIND,DISIBOF+DISIEOF  TEST TOP/BOTTOM                          
         BZ    DISTRN04                                                         
         MVC   FVMSGNO,=AL2(IAINVNOM)  INVOICES DISPLAYED - NO MORE             
         OC    DISLCNT,DISLCNT         TEST ANYTHING DISPLAYED                  
         BNZ   DISTRN04                                                         
         MVC   FVMSGNO,=AL2(IANOINVS)  NO INVOICES SELECTED                     
                                                                                
DISTRN04 GOTO1 ABLDTOT,INPTOTH                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY INVOICES PAID BY A CHEQUE OR END INVOICE DISPLAY            *         
***********************************************************************         
                                                                                
VALINP   CLC   INPSUB,AC@NXT       TEST LEAVING INVOICE MODE                    
         BNE   *+12                                                             
         MVI   TWAPFK,0            CLEAR TWA PF KEY                             
         B     VINP06                                                           
*&&UK*&& CLC   INPSUB,AC@END       TEST END AND VOID/UNVOID CHEQUE              
*&&US*&& CLC   INPSUB,AC@MARK                                                   
         BNE   *+12                                                             
         MVI   TWAPFK,0                                                         
         B     VINP02                                                           
         CLI   TWAPFK,PFK02        TEST LEAVING VIA PF KEY                      
         BE    VINP06                                                           
         CLI   TWAPFK,PFK05        TEST ENDING VIA PF KEY                       
         BE    VINP02                                                           
         MVI   INPSUB,C' '                                                      
         MVI   TWAPFK,0                                                         
         GOTO1 ADISPLAY                                                         
         B     DISTRN02            SET MESSAGE AND EXIT                         
                                                                                
VINP02   CP    TOTDIFF,PZERO       TEST INVOICES BALANCE CHEQUE                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EAMINECH)  NO - CAN'T END                           
         B     VINP04                                                           
         CP    OCHQADV,PZERO       TEST ADVANCE BALANCES TO ZERO                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ADMIS)  NO - CAN'T END                           
         B     VINP04                                                           
         XC    WORK(L'TRSPMOS),WORK                                             
         OC    WORK(L'TRSPMOS),OBATMONP                                         
         BNZ   *+14                                                             
         OC    WORK(L'TRSPMOS),BATMONP                                          
         BZ    VINP06              DON'T KNOW BATCH MONTH YET                   
         CLC   STSARMOS,WORK       TEST TRANSACTION MONTH > BATCH MONTH         
         BNH   VINP06                                                           
         MVC   FVMSGNO,=AL2(AE$BMECM)                                           
VINP04   MVI   TWAPFK,0            CAN'T MARK/UNMARK CLEAR PFK IN TWA           
         LA    R1,INPSUBH                                                       
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
                                                                                
                                                                                
         USING TSARD,R1                                                         
VINP06   L     R1,ATSARBLK         R1=A(TSAR CONTROL BLOCK)                     
         XC    TSARKEY,TSARKEY                                                  
         USING TSIKEYD,TSARKEY                                                  
         MVI   TSIKEY1,FF                                                       
         MVI   TSIKEY2,FF                                                       
         MVI   TSACTN,TSARDH       DELETE ALL INVOICE RECORDS WE ADDED          
         L     RF,VTSAR                                                         
         BASR  RE,RF                                                            
         CLI   TSIKEY1,FF                                                       
         BNE   VINP10                                                           
         CLI   TSIKEY2,FF                                                       
         BNE   VINP10                                                           
VINP08   MVI   TSACTN,TSADEL                                                    
         L     RF,VTSAR                                                         
         BASR  RE,RF                                                            
         BE    VINP08                                                           
                                                                                
VINP10   L     R1,ATSARBLK         R1=A(TSAR CONTROL BLOCK)                     
         MVC   DISMAX,TSPRECN      SET HIGHEST RECORD NUMBER                    
         DROP  R1                                                               
                                                                                
*&&UK*&& CLC   INPSUB,AC@END       TEST END AND VOID/UNVOID CHEQUE              
*&&US*&& CLC   INPSUB,AC@MARK                                                   
         BE    VINP12                                                           
         CLI   TWAPFK,PFK05        MAY USE PF KEY                               
         BE    VINP12                                                           
         GOTO1 ATSARGET,STSARNUM   NO - RE-READ THE CHEQUE                      
         JNE   *+2                                                              
         MVI   TSARCHA,C' '        CLEAR SELECT MARKER                          
         USING TSARD,R1                                                         
         L     R1,ATSARBLK                                                      
         MVI   TSACTN,TSAPUT       PUT BACK CHEQUE                              
         DROP  R1                                                               
         GOTO1 VTSAR                                                            
         JE    VINP14                                                           
         DC    H'0'                                                             
                                                                                
VINP12   GOTO1 CHQMAN,CHQMRKQ        (UN)MARK CHEQUE(S), UPDATE TOTALS          
                                                                                
VINP14   MVI   INPSUB,C' '                                                      
         MVI   TWAPFK,0                                                         
         ZAP   TOTINVS,PZERO                                                    
         ZAP   TOTBCHA,PZERO                                                    
         ZAP   TOTDTAX,PZERO                                                    
         ZAP   TOTDIFF,PZERO                                                    
*&&UK                                                                           
         ZAP   TOTEXDF,PZERO                                                    
         ZAP   TOTDISC,PZERO                                                    
         ZAP   CURINVS,PZERO                                                    
         ZAP   CUREXDF,PZERO                                                    
         ZAP   CURDISC,PZERO                                                    
         ZAP   CURDIFF,PZERO                                                    
*&&                                                                             
         B     PROSEL              PROCESS NEXT SELECTED CHEQUE                 
         EJECT                                                                  
***********************************************************************         
* CHECK UPDATE, ADD BATCH RECORD, POST REVERSALS TO CHEQUES/INVOICES  *         
*                                                                     *         
* LIVE CHEQUE MODE   - PASS TWICE THROUGH TSAR BUFFER, MAKE LIVE      *         
*                      POSTINGS, EXTRA PASS FOR OPTIONAL REPORT       *         
* LIVE INVOICE MODE  - N/A                                            *         
*                                                                     *         
* DRAFT CHEQUE MODE  - PASS THREE TIMES THROUGH TSAR BUFFER, PRODUCE  *         
*                      REPORT WITH POSTING INFORMATION                *         
* DRAFT INVOICE MODE - PASS ONCE THROUGH TSAR BUFFER, PRODUCE REPORT  *         
*                      BUT POSTING INFORMATION IS N/A                 *         
***********************************************************************         
                                                                                
UPDATE   GOTO1 VDICTATE,DMCB,C'LU  ',DCDICTU,DSDICTU                            
         GOTO1 (RF),(R1),C'LL  ',DCDICTM,DSDICTM                                
         GOTO1 AGOTTXT                                                          
         TM    TWAMODE2,TWAM2CHG   TEST ANY CHANGES                             
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(EANOTHIN)                                           
         B     UPDATEX2                                                         
                                                                                
         LA    R1,CTRYTAB          SET UPDATE INDICATOR FROM                    
         USING CTRYTABD,R1         COUNTRY TABLE OR DEFAULT                     
UPD002   CLI   CTRYTABD,EOT                                                     
         BNE   *+14                                                             
         MVC   UPDIND1,UIXPDFLT                                                 
         B     UPD004                                                           
         CLC   CTRYCTRY,AGYCTRY                                                 
         BE    *+12                                                             
         LA    R1,CTRYTABL(R1)                                                  
         B     UPD002                                                           
         MVC   UPDIND1,CTRYUIXP                                                 
         DROP  R1                                                               
UPD004   TM    VOIDIND1,VOIDBCHA   TEST VOIDING BANK CHARGES                    
         JNZ   *+8                                                              
         NI    UPDIND1,FF-UPDIBCHA                                              
                                                                                
         CLI   BYTE,ACTICHUP       TEST READY FOR POSTINGS                      
         BE    CHECKUP                                                          
         CLI   FILTCHQ,INCLUDE     CAN'T DO POSTINGS IF INVOICE MODE            
         BNE   UPD008                                                           
         GOTO1 BATMAN,BATADDQ      CHECK/ADD BATCH RECORD                       
         BNE   EXIT                                                             
         ZAP   TOTCASH,PZERO                                                    
         ZAP   TOTITEM,PZERO                                                    
                                                                                
         LA    R0,IOAMAX           SAVE N# OF A(N IOSAVE),A(N IOBUFF)           
         L     R1,AIOAS                                                         
         LA    RF,AIOSAVE1                                                      
UPD006   ST    R1,0(RF)                                                         
         LA    RE,IOBUFF-IOAS(R1)                                               
         ST    RE,4(RF)                                                         
         LA    RF,8(RF)                                                         
         LA    R1,IOALQ(R1)                                                     
         BCT   R0,UPD006                                                        
                                                                                
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
         MVC   TRNACC,AIOBUFF3     A(FOR ACCOUNT I/O)                           
         MVC   TRNBMOS,SBATMONP    PWOS BATCH MONTH                             
         MVC   TRNPUSER,TWAUSRID   SET USER-ID FROM TWA                         
         MVI   TRNINDS1,TRNIVDAT   DON'T CHECK TRNKDATE/TRNDATE                 
*&&UK                                                                           
         MVC   TRNTOBA,VTOBACCO    PASS A(TOBACCO)                              
         MVC   TRNCCCUR,C.CURTCUR                                               
         MVC   TRNCCURS,S.CURTCUR                                               
*&&                                                                             
         DROP  RF                                                               
                                                                                
UPD008   ICM   R2,15,AICOTAB       TEST/SET A(INTERCOMPANY TABLE)               
         BZ    *+8                                                              
         BAS   RE,BLDICO           BUILD INTERCOMPANY TABLE                     
                                                                                
         OC    PRTSUB,PRTSUB       INITIALISE AND PRINT FIRST PAGE              
         BZ    UPD016                                                           
         L     R3,AREPWRK          R3=A(REPORT W/S)                             
         USING REPD,R3                                                          
         GOTO1 APRTINI                                                          
         MVC   REPH5+L'DISLLINE+1(L'LC@VOID),LC@VOID                            
         LA    R1,REPH5+L'DISLLINE+1+L'LC@VOID-1                                
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'?'                                                       
*&&US                                                                           
         MVC   REPH8(L'REPH4-L'BVM@BNKA),REPH4+L'BVM@BNKA                       
         MVC   REPH4,SPACES                                                     
         MVC   REPH4(L'BVM@BNKA),BVM@BNKA                                       
         MVC   REPH4+L'BVM@NRTV(L'REPH4-L'BVM@NRTV),REPH8                       
         MVC   REPH8,REPH5         SHUFFLE DOWN HEADLINE 5                      
         MVC   REPH5,SPACES                                                     
                                                                                
UPD010   ICM   R1,3,OVRNUM         ESTABLISH NUMBER OF ITEMS                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         GOTO1 ATSARGET,OVRNUM                                                  
         BNE   UPD012                                                           
         TM    TSARINDS,TSARINMQ+TSARMKQ                                        
         BNM   UPD010                                                           
         ICM   R1,3,ITEMS                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,ITEMS                                                       
         B     UPD010                                                           
                                                                                
UPD012   CLI   XACTION,ACTDRFT                                                  
         BNE   UPD014                                                           
         GOTO1 BATMAN,BATCHKQ      SETS BATCH DETAILS IF DRAFT                  
UPD014   LA    R1,REPH5                                                         
         ICM   R1,8,=AL1(100)                                                   
         BAS   RE,BLDDESC          BUILD BATCH DETAILS LINE                     
         XC    REPH4,REPH5         SWAP HEADLINES 4 & 5                         
         XC    REPH5,REPH4                                                      
         XC    REPH4,REPH5                                                      
         MVC   REPH6,SPACES                                                     
         MVI   TSARBTY,BT3         FIX BATCH TYPE FOR DOCUMENT NAME             
         BAS   RE,BLDNARR          BUILD SOME NARRATIVE                         
         MVC   REPH6,BVM@NRTV                                                   
         LA    R1,REPH6+L'BVM@NRTV+1                                            
         IC    RF,VOIDNARL                                                      
         CHI   RF,L'REPH6-L'BVM@NRTV-1                                          
         BNH   *+8                                                              
         LA    RF,L'REPH6-L'BVM@NRTV-1                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),VOIDNARR    DISPLAY GENERAL NARRATIVE                    
*&&                                                                             
                                                                                
         MVI   OVFLAG,OVFDSP       DISPLAY CHEQUES/INVOICES                     
         B     UPD018                                                           
                                                                                
UPD016   MVI   OVFLAG,OVFPST                                                    
         GOTO1 AREPINI                                                          
UPD018   LA    R1,INPMRKH                                                       
         ST    R1,FVADDR                                                        
         LA    R1,1                                                             
         STCM  R1,3,OVRNUM                                                      
                                                                                
         USING TSARD,RF                                                         
UPD020   GOTO1 ATSARGET,OVRNUM                                                  
         BE    UPD024                                                           
                                                                                
         L     RF,ATSARBLK         ALL TSAR RECORDS READ                        
         TM    TSERRS,TSEEOF                                                    
         BO    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         CLI   OVFLAG,OVFDSP       TEST DISPLAY PASS                            
         BNE   UPD022                                                           
         MVI   OVFLAG,OVFPST       START POSTING PASS                           
         GOTO1 AREPINI                                                          
         B     UPD018                                                           
                                                                                
UPD022   CLI   FILTCHQ,INCLUDE     TEST NOT CHEQUE MODE                         
         BNE   UPD090                                                           
         CLI   OVFLAG,OVF3RD       TEST NOT DONE 3RD PASS                       
         BE    UPD090                                                           
         MVI   OVFLAG,OVF3RD       START THIRD PASS                             
         B     UPD018                                                           
                                                                                
UPD024   CLI   FILTCHQ,INCLUDE     TEST INVOICE MODE                            
         BE    UPD026                                                           
         TM    TSARINDS,TSARMCHQ   TEST PRINTABLE ITEM                          
         BNO   UPD028                                                           
         CLC   OVRNUM,STSARNUM                                                  
         BE    UPD028                                                           
         GOTO1 CHQMAN,CHQTSTQ                                                   
         BE    UPD028                                                           
         B     UPD064                                                           
                                                                                
UPD026   TM    TSARINDS,TSARMKQ+TSARINMQ  TEST MARK STATUS CHANGED              
         BNM   UPD064                                                           
         LA    RF,BOQ                                                           
         TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BZ    *+8                                                              
         LA    RF,BZQ                                                           
         TM    TSARINDS,TSARMKQ    TEST IF USER HAS (UN)MARKED                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   UPD064                                                           
         BAS   RE,BLDNARR          BUILD NARRATIVE                              
                                                                                
UPD028   CLI   OVFLAG,OVFDSP       TEST DISPLAY PASS                            
         BNE   UPD030                                                           
         LA    R1,REPP1-1                                                       
         ICM   R1,8,=C'R'                                                       
         GOTO1 ABLDLIN             BUILD PRINT LINE USING REPDISP               
         MVC   REPP1+L'DISLLINE+1(L'LC4YES),LC4YES                              
         TM    TSARINDS,TSARMKQ                                                 
         BO    *+10                                                             
         MVC   REPP1+L'DISLLINE+1(L'LC4NO),LC4NO                                
         GOTO1 VREPORT,REPD        PRINT IT                                     
         B     UPD064                                                           
                                                                                
UPD030   CLI   FILTCHQ,INCLUDE     TEST CHEQUE MODE                             
         BNE   UPD064                                                           
         CLI   OVFLAG,OVF3RD       TEST POSTING PASS                            
         BE    UPD032                                                           
         TM    TSARINDS,TSARDISQ   TEST DISPLAY-ONLY CHEQUE                     
         BO    UPD064              DON'T MARK VENDOR POSTINGS                   
         GOTO1 CHQMAN,CHQSETQ      SET CHEQUE DATA FROM TSAR RECORD             
         ZAP   TOTDISC,PZERO       CLEAR DISCOUNT/DTAX/BCHA/EXDF ETC            
         ZAP   TOTDTAX,PZERO                                                    
         ZAP   TOTBCHA,PZERO                                                    
*&&UK                                                                           
         ZAP   TOTEXDF,PZERO                                                    
         ZAP   CURDISC,PZERO                                                    
         ZAP   CUREXDF,PZERO                                                    
*&&                                                                             
         GOTO1 AVENDOR             READ/MARK/POST ON VENDOR                     
         B     UPD064              GET NEXT CHEQUE                              
                                                                                
         USING CHDRECD,R1                                                       
UPD032   LA    R1,KEY              CLEAR KEY                                    
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCULA,ACCOUNT                                                 
         MVC   CHDKCULC,TSARCON                                                 
         CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE PAIR             
         BNE   UPD034                                                           
         XC    CHDKNULL,CHDKNULL                                                
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    UPD034                                                           
         MVC   CHDKOFF,TSAROFF     SET OFFICE CODE                              
         DROP  R1                                                               
                                                                                
UPD034   LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         GOTO1 AGETCON,AIOBUFF                                                  
                                                                                
         MVC   IODA,TSARDADR       SET DISK ADDRESS                             
         LA    R1,IOGET+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE            
         CLI   XACTION,ACTDRFT                                                  
         BE    *+8                                                              
         LA    R1,IOGETRUP+IOACCMST+IO1Q  GETRUP BUILDS KEY ON OLD FILE         
         TM    TSARRSTA,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BNO   *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         L     R0,AIOSAVE2                                                      
         LA    R1,IOALQ                                                         
         L     RE,AIOSAVE1                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE EMULATED RECORD IN IOAREA2              
         GOTO1 ASETELAD,AIOBUFF    SET ELEMENT ADDRESSES                        
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         JZ    *+2                                                              
*        CLI   PROFNREC,C'Y'       TEST NOT AUTO-RECONCILING                    
*        BE    *+16                                                             
*        TM    VOIDIND1,VOIDVOID   TEST VOIDING                                 
*        BZ    *+8                                                              
*        OI    TRNSTAT,TRNSBREC    SET BANK RECONCILED                          
         L     R0,ATPTAB           CLEAR TRANSACTION POINTER TABLE              
         LH    R1,=Y(TPLTAB)                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ICM   RF,15,ATRPEL1                                                    
         BZ    UPD040                                                           
         USING TPTABD,R4                                                        
         L     R4,ATPTAB           R4=A(TRANSACTION POINTER TABLE)              
         LA    R0,TPTABN                                                        
         USING TRPELD,RF                                                        
UPD036   CLI   TRPEL,0             TEST EOR                                     
         BE    UPD040                                                           
         CLI   TRPEL,TRPELQ        TEST TRPEL                                   
         BNE   UPD038                                                           
         MVC   TPTYPE,TRPTYPE                                                   
         MVC   TPSTAT,TRPSTAT                                                   
         ZAP   TPAMNT,TRPAMNT                                                   
         MVC   TPUL,TRPUL          UNIT/LEDGER                                  
         MVC   TPACT,SPACES                                                     
         IC    RE,TRPLN                                                         
         SH    RE,=Y(TRPLN1Q+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TPACT(0),TRPACT     ACCOUNT                                      
         MVC   TPOFFC,TRNOFFC      OFFICE FROM BANK TRANSACTION                 
         ICM   RE,15,ATRSEL                                                     
         MVC   TPPMOS,TRSPMOS-TRSELD(RE)                                        
         LA    R4,TPTABL(R4)       R4=A(NEXT TPTAB ENTRY)                       
         BCT   R0,UPD038                                                        
         DC    H'0'                INCREASE TPTABN                              
UPD038   SR    R1,R1                                                            
         IC    R1,TRPLN                                                         
         AR    RF,R1                                                            
         B     UPD036                                                           
         DROP  R4,RF                                                            
*                                                                               
UPD040   TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    UPD046                                                           
         USING MPYELD,RF                                                        
         ICM   RF,15,AMPYEL                                                     
         JZ    *+2                                                              
         USING PAYTABD,R1                                                       
         L     R1,APAYTAB          R1=A(PAYMENTS TABLE)                         
         B     *+8                                                              
UPD042   LA    R1,PAYTABL(R1)                                                   
         CLI   PAYTABD,EOT         TEST EOT                                     
         JE    *+2                                                              
         CLI   PAYTABD,FF          TEST EMPTY SLOT                              
         BE    UPD044                                                           
*                                  MATCH BANK ACC,NUMBER,DATE,OFFICE            
         CLC   MPYBNK,PAYBNK       BANK ACCOUNT                                 
         BNE   UPD042                                                           
         CLC   MPYNO,PAYNO         NUMBER                                       
         BNE   UPD042                                                           
         CLC   MPYDTE,PAYDTE       DATE                                         
         BNE   UPD042                                                           
         CLC   TRNOFFC,PAYOFF      OFFICE                                       
         BNE   UPD042                                                           
         AP    PAYPAMT,TRNAMNT     ADD TO AMOUNT VOIDED/UNVOIDED                
         B     UPD046                                                           
                                                                                
UPD044   MVC   PAYBNK,MPYBNK                                                    
         MVC   PAYNO,MPYNO                                                      
         MVC   PAYDTE,MPYDTE                                                    
         DROP  RF                                                               
         MVC   PAYOFF,TRNOFFC                                                   
         ZAP   PAYPAMT,TRNAMNT                                                  
         DROP  R2                                                               
                                                                                
         ICM   RF,15,ATRSEL                                                     
         MVC   PAYMOS,TRSPMOS-TRSELD(RF)                                        
         MVI   PAYTABL(R1),FF      SET NEXT AVAILABLE SLOT                      
         DROP  R1                                                               
                                                                                
         USING TRSELD,R2                                                        
UPD046   ICM   R2,15,ATRSEL                                                     
         JZ    *+2                                                              
         CLI   TRSLN,TRSLNQ        TEST SHORT ELEMENT                           
         BNL   UPD048                                                           
         GOTO1 AEXTRSL             YES - EXTEND IT                              
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         ICM   R2,15,ATRSEL                                                     
UPD048   MVI   TRSMARK,TRSMBVQ     SET TYPE/ACTION IS BANK/VOID                 
         OI    TRSSTAT,TRSSVOID    SET VOID                                     
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+12                                                             
         OI    TRSMARK,TRSMUMQ          SET ACTION IS NEGATIVE                  
         NI    TRSSTAT,FF-TRSSVOID      RESET VOID                              
         DROP  R2                                                               
         TM    VOIDIND1,VOIDBCHA                                                
         BO    UPD054                                                           
         GOTO1 VHELLO,PARM,(C'D',=C'ACCMST '),('SPAELQ',AIOBUFF),      X        
               (1,=AL1(SPATBCHA))                                               
*&&US*&& GOTO1 (RF),(R1),,('SCIELQ',AIOBUFF),(1,=AL1(SCITBCHA))                 
*&&UK                                                                           
         L     R2,AIOBUFF          DELETE ANY BANK CHARGE SCIELS                
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING SCIELD,R2                                                        
UPD050   CLI   SCIEL,0                                                          
         BE    UPD054                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITBCHA                                                 
         BE    UPD052                                                           
         SR    R1,R1                                                            
         IC    R1,SCILN                                                         
         AR    R2,R1                                                            
         B     UPD050                                                           
UPD052   GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),AIOBUFF,ACOM,0,SCIELD,0             
         B     UPD050                                                           
*&&                                                                             
UPD054   L     R2,AIOBUFF               R2=A(DATA RECORD)                       
         USING TRNRECD,R2                                                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPD056                                                           
         LA    R1,IOPUT+IOACCMST+IO1Q   PUT BACK TO ACCMST                      
         TM    TRNRSTAT,TRNSARCH        TEST TRANSACTION ON ACCARC              
         BNO   *+12                                                             
         NI    TRNRSTAT,FF-TRNSARCH     CLEAR ACCARC INDICATOR                  
         LA    R1,IOADFR+IOACCMST+IO1Q  RE-ADD ACCARC RECORD TO ACCMST          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
                                                                                
         MVC   KEY(L'TRNKEY),TRNKEY     EXTRACT TRANSACTION KEY                 
         CLI   IOCTCOMM,IOADFR          TEST RECORD RE-ADDED TO ACCMST          
         BNE   UPD056                                                           
         LA    R1,IORDUP+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         MVC   KEY+(TRNKSTA-TRNRECD)(L'TRNRSTA),TRNRSTA                         
         DROP  R2                                                               
         L     R2,AIOSAVE               R2=A(SAVED DATA RECORD VALUES)          
         MVC   KEY+(TRNKDA-TRNRECD)(L'TRNKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO1Q                                         
         GOTO1 AIOEXEC                                                          
         BE    UPD056                                                           
         DC    H'0'                                                             
                                                                                
UPD056   MVC   AIOSAVE,AIOSAVE2    SET IOSAVE IS IOSAVE2                        
         MVC   AIOBUFF,AIOBUFF2    SET IOBUFF IS IOBUFF2                        
         TM    VOIDIND1,VOIDBCHA                                                
         BO    UPD062                                                           
         GOTO1 VHELLO,PARM,(C'D',=C'ACCMST '),('SPAELQ',AIOBUFF),      X        
               (1,=AL1(SPATBCHA))                                               
*&&US*&& GOTO1 (RF),(R1),,('SCIELQ',AIOBUFF),(1,=AL1(SCITBCHA))                 
*&&UK                                                                           
         L     R2,AIOBUFF          DELETE ANY BANK CHARGE SCIELS                
         LA    R2,TRNRFST-TRNRECD(R2)                                           
         USING SCIELD,R2                                                        
UPD058   CLI   SCIEL,0                                                          
         BE    UPD062                                                           
         CLI   SCIEL,SCIELQ                                                     
         BNE   *+12                                                             
         CLI   SCITYPE,SCITBCHA                                                 
         BE    UPD060                                                           
         SR    R1,R1                                                            
         IC    R1,SCILN                                                         
         AR    R2,R1                                                            
         B     UPD058                                                           
UPD060   GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),AIOBUFF,ACOM,0,SCIELD,0             
         B     UPD058                                                           
*&&                                                                             
UPD062   GOTO1 APOST,POSTISTD      POST BANK -CR/CR REVERSE TRANSACTION         
         GOTO1 APOSTPT             POST POINTER REVERSE TRANSACTIONS            
                                                                                
UPD064   ICM   R1,3,OVRNUM                                                      
         BZ    UPD090              PRINTING INVOICES, BUT NONE FOUND            
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         B     UPD020                                                           
         EJECT                                                                  
***********************************************************************         
* INTERCOMPANY POSTINGS                                               *         
***********************************************************************         
                                                                                
UPD090   ICM   R4,15,AICOTAB       TEST/SET A(INTERCOMPANY TABLE)               
         USING ICOTABD,R4                                                       
         BZ    UPD100              NO INTERCOMPANY POSTINGS                     
         L     R2,AIOBUFF          BUILD BASIC KEY                              
         USING TRNRECD,R2                                                       
         B     *+8                                                              
UPD092   LA    R4,ICOTABL(R4)      NEXT ENTRY IN ICOTAB                         
         CLI   ICOTABD,EOT         TEST EOT                                     
         BE    UPD100                                                           
         CP    ICOAMNT,PZERO       TEST ANYTHING FOR THIS OFFICE                
         BE    UPD092                                                           
         MVC   POSTCPY,COMPANY     FROM INTERCOMPANY ACCOUNT                    
         MVC   POSTULA,ICOFACT                                                  
         MVC   POSTCCPY,COMPANY    TO INTERCOMPANY ACCOUNT                      
         MVC   POSTULC,ICOTACT                                                  
         MVI   POSTSTAT,TRNSDR     ** DEBIT FIRST INTERCOMPANY **               
         MVC   POSTOFFC(1),POSTACT OFFICE FROM ACCOUNT BYTE ONE                 
         MVI   POSTOFFC+1,C' '                                                  
         MVC   POSTCACN,ICOTNAM    TO ACCOUNT NAME IS CONTRA NAME               
                                                                                
UPD094   MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,POSTCULA                                                
         MVC   TRNKCULC,POSTCULC                                                
         MVC   TRNKDATE,TODAYP                                                  
         MVC   TRNKREF,BVU@VOID                                                 
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         LA    R1,TRNRFST                                                       
         USING TRNELD,R1                                                        
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q       MINIMUM LENGTH (POST ADDS NARRATIVE)         
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVI   TRNSUB,0                                                         
         MVC   TRNTYPE,STSARBTY    BANK TRNTYPE (POST MAY SET VOID)             
         MVC   TRNSTAT,POSTSTAT                                                 
         ZAP   TRNAMNT,ICOAMNT     FOR TABLE AMOUNT                             
         MVC   TRNOFFC,POSTOFFC                                                 
         MVI   TRNELD+TRNLN1Q,0    SET EOR                                      
         DROP  R1                                                               
                                                                                
         MVC   CONTRAN,POSTCACN                                                 
         XC    DATPMOS,DATPMOS     REVERSING MOS UNKNOWN                        
         GOTO1 APOST,POSTINEW                                                   
                                                                                
         XC    POSTCULA,POSTCULC   SWAP ACCOUNT WITH CONTRA                     
         XC    POSTCULC,POSTCULA                                                
         XC    POSTCULA,POSTCULC                                                
         CLC   ICOFACT,POSTULA     TEST FIRST INTERCOMPANY ACCOUNT              
         BE    UPD092                                                           
         MVI   POSTSTAT,0          ** CREDIT SECOND INTERCOMPANY **             
         MVC   POSTOFFC,ICOOFFC    OFFICE FROM INTERCOMPANY TABLE               
         MVC   POSTCACN,ICOFNAM    FROM ACCOUNT NAME IS CONTRA NAME             
         B     UPD094              SECOND INTERCOMPANY POSTING                  
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ANALYSIS BANK/BANK POSTINGS                                         *         
***********************************************************************         
                                                                                
UPD100   TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    UPD120                                                           
         L     R4,APAYTAB          SORT PAYTAB INTO ACCOUNT SEQUENCE            
         USING PAYTABD,R4                                                       
UPD102   XR    R0,R0               SET NO SWAP THIS PASS                        
UPD104   CLI   PAYTABD,FF          TEST EOT                                     
         BE    UPD108                                                           
         CLC   PAYTABD(PAYKEY2L),PAYTABD+PAYTABL                                
         BNH   UPD106                                                           
         XC    PAYTABD(PAYTABL),PAYTABD+PAYTABL                                 
         XC    PAYTABD+PAYTABL(PAYTABL),PAYTABD                                 
         XC    PAYTABD(PAYTABL),PAYTABD+PAYTABL                                 
         L     R0,APAYTAB          SET A SWAP THIS PASS                         
UPD106   LA    R4,PAYTABL(R4)      NEXT PAYMENT TABLE ENTRY                     
         B     UPD104                                                           
                                                                                
UPD108   LTR   R4,R0               TEST/SET A(PAYTAB)                           
         BNZ   UPD102              THERE WAS A SORT THIS PASS                   
                                                                                
         LA    R2,KEY              BUILD KEY FOR ANALYSIS BANK ACCOUNT          
         USING CHDRECD,R2                                                       
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCULA,ACCOUNT                                                 
         MVC   POSTCULA,CHDKCULA                                                
         L     R4,APAYTAB          R4=A(PAYMENT TABLE)                          
UPD110   CLI   PAYTABD,FF          TEST EOT                                     
         BE    UPD120                                                           
         LA    R2,KEY              R2=A(KEY)                                    
         CLC   PAYBNK,CHDKCUNT     TEST CHANGE OF ANALYSIS BANK                 
         BE    UPD112                                                           
         MVC   CHDKCCPY,COMPANY    READ NEW CONTRA HEADER                       
         MVC   CHDKCUNT(L'PAYBNK),PAYBNK                                        
         MVC   POSTCULC,CHDKCULC                                                
         MVC   CHDKSPCS(CHDKEND-(CHDKSPCS-CHDKEY)),SPACES                       
         CLI   FILEFORM,ISDAQ      TEST NEW FILE                                
         BNE   *+10                                                             
         XC    CHDKNULL,CHDKNULL                                                
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOBUFF                                                       
         LA    R1,CHDRFST                                                       
         DROP  R2                                                               
                                                                                
         USING CACELD,R1                                                        
         CLI   CACEL,CACELQ        TEST HEADER ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MAINNAME,SPACES     CLEAR MAIN BANK ACCOUNT NAME                 
         CLI   CACLN,CACLN1Q       TEST NAME PRESENT                            
         BNH   UPD112                                                           
         XR    RF,RF                                                            
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     UPD112                                                           
         MVC   MAINNAME(0),CACNAME CONTRA A/C NAME (MAIN BANK ACCOUNT)          
         DROP  R1                                                               
                                                                                
UPD112   L     R2,AIOBUFF          ANALYSIS BANK/BANK POSTINGS                  
         LR    R0,R4               SAVE CURRENT ADDRESS IN PAYTAB               
         MVI   POSTSTAT,TRNSDR     ** DEBIT ANALYSIS BANK ACCOUNT **            
         MVC   POSTCACN,MAINNAME   WITH MAIN BANK AS CONTRA A/C                 
                                                                                
         USING TRNRECD,R2                                                       
UPD114   MVC   TRNKEY,SPACES       BUILD TRANSACTION KEY                        
         MVC   TRNKCULA,POSTCULA                                                
         MVC   TRNKCULC,POSTCULC                                                
         GOTO1 VDATCON,DMCB,(2,PAYDTE),(1,TRNKDATE)                             
         MVC   TRNKREF,PAYNO                                                    
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
                                                                                
         LA    R1,TRNRFST                                                       
         USING TRNELD,R1                                                        
         MVI   TRNEL,TRNELQ        BUILD SKELETAL TRANSACTION ELEMENT           
         MVI   TRNLN,TRNLN1Q                                                    
         MVC   TRNDATE,TRNKDATE                                                 
         MVC   TRNREF,TRNKREF                                                   
         MVI   TRNSUB,0                                                         
         MVC   TRNTYPE,STSARBTY    BANK TRNTYPE (POST MAY SET VOID)             
         MVC   TRNSTAT,POSTSTAT                                                 
         ZAP   TRNAMNT,PAYPAMT     FOR TABLE AMOUNT                             
         MVC   TRNOFFC,PAYOFF      TO TABLE OFFICE                              
         MVI   TRNELD+TRNLN1Q,0    SET EOR                                      
         DROP  R1,R2                                                            
                                                                                
         MVC   CONTRAN,POSTCACN                                                 
         MVC   DATPMOS,PAYMOS      SET POSSIBLE REVERSING MOS                   
         GOTO1 APOST,POSTINEW                                                   
                                                                                
         CLC   PAYBNK,PAYBNK+PAYTABL                                            
         LA    R4,PAYTABL(R4)      NEXT PAYTAB ENTRY                            
         BE    UPD114              SAME ANALYSIS BANK ACCOUNT                   
         XC    POSTCULA,POSTCULC   NEW ACCOUNT - SWAP WITH CONTRA               
         XC    POSTCULC,POSTCULA                                                
         XC    POSTCULA,POSTCULC                                                
         CLC   POSTCULA,ACCOUNT    TEST POSTING TO ANALYSIS BANK                
         BE    UPD110              POST TO ANALYSIS BANK                        
         LR    R4,R0               R4=SAVED A(PAYTAB)                           
         MVI   POSTSTAT,0          ** CREDIT MAIN BANK ACCOUNT **               
         MVC   POSTCACN,ACCNAME    WITH ANALYSIS BANK AS CONTRA A/C             
         B     UPD114              POST TO MAIN BANK ACCOUNT                    
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CLOSE REPORT, MAKE FINAL ADDTRN CALL, UPDATE BATCH RECORD           *         
***********************************************************************         
                                                                                
UPD120   OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPD126              NO - MUST BE LIVE UPDATE                     
         CLI   FILTCHQ,EXCLUDE     TEST INVOICE MODE                            
         BNE   UPD122                                                           
         LA    R2,REPVOID          SUBSTITUTE CHEQUE AMOUNT FOR TOTAL           
         TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BZ    *+8                                                              
         LA    R2,REPUVOI                                                       
         ZAP   CHQTOT,0(L'REPVOID,R2)     SAVE VOID/UNVOID TOTAL                
         ZAP   0(L'REPVOID,R2),FILTCHA    REPLACE WITH CHEQUE AMOUNT            
*&&UK                                                                           
         ZAP   CHQTOTC,CURTOTS-TOTALS(L'REPVOID,R2)                             
         ZAP   CURTOTS-TOTALS(L'REPVOID,R2),FILTCHAF                            
*&&                                                                             
         TM    STSARIND,TSARINMQ+TSARMKQ  TEST CHANGED                          
         BM    UPD122                                                           
         ZAP   0(L'REPVOID,R2),PZERO  CLEAR TOTAL - NOT (UN)MARKED YET          
*&&UK*&& ZAP   CURTOTS-TOTALS(L'REPVOID,R2),PZERO  CLEAR TOTAL                  
UPD122   GOTO1 APRTCLO             CLOSE REPORT, BUILD SPOOL-ID MESSAGE         
         CLI   FILTCHQ,EXCLUDE     TEST INVOICE MODE                            
         BNE   UPD124                                                           
         ZAP   0(L'REPVOID,R2),CHQTOT  RESTORE VOID/UNVOID TOTAL                
*&&UK*&& ZAP   CURTOTS-TOTALS(L'REPVOID,R2),CHQTOTC                             
UPD124   CLI   FILTCHQ,INCLUDE     TEST INVOICE MODE - NOT FULL DRAFT           
         BNE   UPDATEX             PRTCLO HAS SET MESSAGE                       
         CLI   XACTION,ACTDRFT                                                  
         BE    UPDATEX                                                          
                                                                                
UPD126   TM    UPDIND2,UPDIADDQ    TEST ANYTHING ADDED                          
         BO    UPD128                                                           
         GOTO1 BATMAN,BATUPDQ      DELETE BATCH RECORD                          
         MVC   FVMSGNO,=AL2(EANOTUPD)  NOTHING TO UPDATE                        
         MVI   FVOMTYP,GTMERR      ERROR MESSAGE                                
         OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    UPDATEX                                                          
         MVC   FVMSGNO,=AL2(EANOTUPR)                                           
         B     UPDATEX                                                          
                                                                                
         USING TRNBLK,RF                                                        
UPD128   LA    RF,TRNBLOCK                                                      
         MVC   TRNREC,AIOBUFF      A(TRANSACTION RECORD)                        
         OI    TRNINDS,TRNILAST                                                 
         OI    TRNINDS2,TRNIUPDG   UPDATE GL POSTINGS TOO                       
         CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    UPD130                                                           
         GOTO1 VADDTRN,TRNBLK      GIVE ADDTRN LAST TIME CALL                   
         DROP  RF                                                               
         BE    *+6                                                              
         DC    H'0'                                                             
UPD130   GOTO1 BATMAN,BATUPDQ      UPDATE BATCH RECORD                          
                                                                                
         GOTO1 ADOP2J              PROCESS PAY TO JOBS/INVOICES                 
         JE    UPD132                                                           
         MVC   FVMSGNO,=AL2(AE$PNFUS)                                           
         J     UPDABEND                                                         
                                                                                
UPD132   NI    TWAMODE2,FF-(TWAM2CHG+TWAM2DIS)                                  
         MVC   FVMSGNO,=AL2(IATRNUPS)                                           
         MVI   FVOMTYP,GTMINF      INFORMATION MESSAGE                          
         OC    PRTSUB,PRTSUB       TEST REPORT GENERATED                        
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(IATRNUPR)                                           
         LA    R1,MRKACTH                                                       
         XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         B     *+14                                                             
                                                                                
UPDATEX  XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
UPDATEX2 LA    R1,INPSUBH          SET CURSOR TO SUB-ACTION                     
         ST    R1,FVADDR                                                        
         B     EXIT                                                             
                                                                                
UPDABEND OI    TWAMODE3,TWAM3UWD                                                
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK UPDATE IS OK                                                  *         
***********************************************************************         
                                                                                
CHECKUP  LA    R1,INPSUBH          CHECK LIVE UPDATE IS OK                      
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EALUPINV)                                           
         CLI   FILTCHQ,INCLUDE     CAN'T UPDATE IF INVOICE MODE                 
         BNE   EXIT                                                             
         ZAP   CHQTOT,TOTVOID      ASSUME VOID TOTAL                            
*&&UK*&& ZAP   CHQTOTC,CURVOID     ASSUME VOID TOTAL                            
         TM    VOIDIND1,VOIDVOID                                                
         BO    CHKUP02                                                          
         ZAP   CHQTOT,TOTCHQS      TAKE TOTAL CHEQUES                           
         SP    CHQTOT,TOTVOID      SUBTRACT VOID TO GIVE UNVOIDED               
*&&UK                                                                           
         ZAP   CHQTOTC,CURCHQS     TAKE TOTAL CHEQUES                           
         SP    CHQTOTC,CURVOID     SUBTRACT VOID TO GIVE UNVOIDED               
*&&                                                                             
CHKUP02  DS    0H                                                               
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1BC                                                
         BZ    CHKUP04                                                          
         OC    BATCTOTC,BATCTOTC   TEST BATCH CONTROL TOTAL PRESENT             
         BZ    CHKUP04                                                          
         MVC   FVMSGNO,=AL2(EABATTOT)                                           
         CP    BATCTOTC,CHQTOTC    TEST VOID TOTAL MATCHES BATCH TOTAL          
         BNE   EXIT                                                             
         B     CHKUP06                                                          
*&&                                                                             
CHKUP04  OC    BATCTOT,BATCTOT     TEST BATCH CONTROL TOTAL PRESENT             
         BZ    CHKUP06                                                          
         MVC   FVMSGNO,=AL2(EABATTOT)                                           
         CP    BATCTOT,CHQTOT      TEST VOID TOTAL MATCHES BATCH TOTAL          
         BNE   EXIT                                                             
                                                                                
CHKUP06  ICM   R1,3,OVRNUM         ESTABLISH NUMBER OF ITEMS                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         GOTO1 ATSARGET,OVRNUM                                                  
         BNE   CHKUP08                                                          
         TM    TSARINDS,TSARINMQ+TSARMKQ                                        
         BNM   CHKUP06                                                          
         SP    CHQTOT,TSARAMNT                                                  
*&&UK*&& SP    CHQTOTC,TSARAFCA                                                 
         ICM   R1,3,ITEMS                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,ITEMS                                                       
         B     CHKUP06                                                          
                                                                                
CHKUP08  L     R1,ATSARBLK                                                      
         TM    TSERRS-TSARD(R1),TSEEOF  TEST EOF                                
         BO    *+6                                                              
         DC    H'0'                                                             
         CP    CHQTOT,PZERO        ENSURE ALL IS WELL                           
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    BATITEM,BATITEM     TEST BATCH ITEM COUNT PRESENT                
         BZ    CHKUP10                                                          
         MVC   FVMSGNO,=AL2(EABATITE)                                           
         CLC   ITEMS,BATITEM       TEST ITEM COUNTS MATCH                       
         BNE   EXIT                                                             
                                                                                
CHKUP10  OC    ACOSTAB,ACOSTAB     TEST COSTING                                 
         BZ    CHKUP12                                                          
K        USING LDGRECD,KEY                                                      
         MVC   K.LDGKEY,SPACES     CHECK COSTING LEDGERS                        
         MVC   K.LDGKCPY,COMPANY                                                
         MVI   K.LDGKUNT,C'1'                                                   
         MVI   K.LDGKLDG,C'C'      CHECK 1C LEDGER                              
         GOTO1 AGETLDG,0           READS & TESTS SECURITY, ETC.                 
         BNE   EXIT                ERROR - EXIT WITH MESSAGE SET                
         MVI   K.LDGKLDG,C'2'      CHECK 12 LEDGER                              
         GOTO1 AGETLDG,0           READS & TESTS SECURITY, ETC.                 
         BNE   EXIT                ERROR - EXIT WITH MESSAGE SET                
         DROP  K                                                                
                                                                                
CHKUP12  GOTO1 BATMAN,BATCHKQ      CHECK BATCH DETAILS ARE OK                   
         B     EXIT                CURSOR SET IF ERROR FOUND                    
         EJECT                                                                  
***********************************************************************         
* QUIT                                                                *         
***********************************************************************         
                                                                                
QUIT     XC    MRKSCR,MRKSCR       CLEAR SCROLL FIELD                           
         LA    R1,MRKACTH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(IAHDRECH)                                           
         MVI   FVOMTYP,GTMINF                                                   
         NI    TWAMODE2,FF-(TWAM2CHG+TWAM2DIS)                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
                                                                                
***********************************************************************         
* BATCH MANAGEMENT ROUTINES                                           *         
***********************************************************************         
                                                                                
BATMAN   NTR1  ,                                                                
         SLL   R1,2                ENTRY, R1=ACTION NUMBER                      
         B     *+0(R1)                                                          
         B     BATCHK              1 - CHECK BATCH UPDATE IS VALID              
         B     BATADD              2 - CHECK AND (RE)ADD BATCH RECORD           
         B     BATUPD              3 - UPDATE BATCH RECORD                      
                                                                                
BATMNEQ  LTR   RB,RB               ERROR EXIT                                   
         B     *+6                                                              
BATMEQU  CR    RB,RB               GOOD EXIT                                    
         B     XIT                                                              
                                                                                
BATCHK   LA    R1,MRKOPTH          TEST USER HAS GIVEN BATCH DATA               
         ST    R1,FVADDR                                                        
         OC    OBATMON,OBATMON     TEST OPTIONS BATCH MONTH                     
         BNZ   BATCHK02                                                         
         MVC   OBATMONP,BATMONP    (SET PWOS MONTH)                             
         OC    OBATMON,BATMON      TEST/SET HEADER MONTH IN OPTION              
         BNZ   BATCHK02                                                         
         MVC   FVMSGNO,=AL2(EAMISBMO)                                           
         B     BATMNEQ             BATCH MONTH MISSING - USE OPTION             
                                                                                
BATCHK02 OC    OBATREF,OBATREF     TEST OPTIONS BATCH REFERENCE                 
         BNZ   BATCHK04                                                         
         OC    OBATREF,BATREF      TEST/SET HEADER REFERENCE IN OPTION          
         BNZ   BATCHK04                                                         
         MVC   FVMSGNO,=AL2(EAMISBRF)                                           
         B     BATMNEQ             BATCH REF MISSING - USE OPTION               
                                                                                
BATCHK04 OC    OBATNAM,OBATNAM     TEST OPTIONS BATCH NAME                      
         BNZ   BATCHK06                                                         
         OC    OBATNAM,BATNAM      TEST/SET HEADER NAME IN OPTION               
         BNZ   BATCHK06                                                         
         MVC   FVMSGNO,=AL2(EAMISBNA)                                           
         B     BATMNEQ             BATCH NAME MISSING - USE OPTION              
                                                                                
BATCHK06 MVC   SBATMON,OBATMON     SAVE BATCH VALUES (OPTIONS GET LOST)         
         MVC   SBATREF,OBATREF                                                  
         MVC   SBATNAM,OBATNAM                                                  
         MVC   SBATMONP,OBATMONP                                                
         B     BATMEQU             CC EQU - ROOT LOADS CONFIRM SCREEN           
         EJECT                                                                  
BATADD   LA    R2,KEY              CHECK BATCH RECORD DOESN'T EXIST             
         USING BATRECD,R2                                                       
         MVC   BATKEY,SPACES                                                    
         XC    BATKEY(BATKEND),BATKEY                                           
         MVI   BATKTYP,BATKTYPQ                                                 
         MVC   BATKCPY,COMPANY                                                  
         MVC   BATKOFF,TWAUSRID                                                 
         MVI   BATKGRUP,TBAGGENQ   GENERAL ACCOUNTING                           
         MVI   BATKTYPE,BT37       BT37                                         
         MVC   BATKDATE,TODAYP     DATE                                         
         MVC   BATKREF,SBATMON     YMRRRR (SBATMON+SBATREF)                     
         MVC   SBATKEY,BATKEY      SAVE IT FOR LATER                            
         LA    R1,IOREAD+IOACCDIR+IO3Q                                          
         CLI   XACTION,ACTDRFT     TEST DRAFT (NO UPDATE)                       
         BE    *+8                                                              
         LA    R1,IORDUPD+IOACCDIR+IO3Q                                         
         GOTO1 AIOEXEC                                                          
         MVC   BAERROR,IOERROR                                                  
         BNE   BATADD02                                                         
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
         B     BATMNEQ                   FOUND AND NOT DELETED - ERROR          
                                                                                
BATADD02 TM    BAERROR,IOEALL-IOERNF-IOEDEL  ALLOW DELETED/NOT FOUND            
         JNZ   *+2                 DIE ON ANY OTHER ERROR                       
         TM    BAERROR,IOEDEL      TEST RECORD FOUND BUT DELETED                
         BZ    BATADD04                                                         
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    BATADD04            SKIP GETREC (TRANSLATES TO DMREAD)           
         LA    R1,IOGET+IORDEL+IOACCMST+IO3Q                                    
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    *+8                                                              
         LA    R1,IOGETRUP+IORDEL+IOACCMST+IO3Q                                 
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
                                                                                
BATADD04 L     R2,AIOBUFF          ADD/RE-ADD BATCH RECORD                      
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
         CLI   XACTION,ACTDRFT                                                  
         BE    BATMEQU                                                          
         LA    R1,IOADDREC+IOACCMST+IO3Q  ADD RECORD                            
         TM    BAERROR,IOEDEL      TEST RECORD FOUND BUT DELETED                
         BZ    *+8                                                              
         LA    R1,IOPUT+IOACCMST+IO3Q  PUT RECORD BACK                          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                 DIE ON ERROR                                 
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    BATMEQU                                                          
         MVC   KEY,BATRECD         EXTRACT KEY FROM DATA RECORD KEY             
         MVC   KEY+(BATKSTA-BATRECD)(L'BATKSTA),BATRSTA                         
         DROP  R2                                                               
         TM    BAERROR,IOEDEL      TEST REVIVING DELETED RECORD                 
         BZ    BATMEQU                                                          
         L     R2,AIOSAVE          R2=A(SAVED DATA RECORD VALUES)               
         MVC   KEY+(BATKDA-BATRECD)(L'BATKDA),0(R2)                             
         GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO3Q                                    
         BE    BATMEQU                                                          
         DC    H'0'                                                             
         EJECT                                                                  
         USING BATRECD,R2                                                       
BATUPD   LA    R2,KEY              RE-READ BATCH RECORD                         
         MVC   BATKEY,SBATKEY                                                   
         LA    R1,IOREAD+IOACCDIR+IO3Q                                          
         CLI   XACTION,ACTDRFT                                                  
         BE    BATMEQU                                                          
         LA    R1,IORDUPD+IOACCDIR+IO3Q                                         
         GOTO1 AIOEXEC                                                          
         JNE   *+2                 DIE ON OTHER ERROR                           
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    BATUPD02            SKIP GETREC (TRANSLATES TO DMREAD)           
         LA    R1,IOGETRUP+IOACCMST+IO3Q                                        
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
         JNE   *+2                                                              
         ZAP   BTHCASH,TOTCASH                                                  
         ZAP   BTHITEM,TOTITEM                                                  
         DROP  RF                                                               
BATUPD04 LA    R1,IOPUT+IOACCMST+IO3Q  PUT RECORD BACK                          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                 DIE ON ERROR                                 
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    BATMEQU                                                          
         MVC   KEY+(BATKSTA-BATRECD)(L'BATKSTA),BATRSTA                         
         DROP  R2                                                               
         L     R2,AIOSAVE          R2=A(SAVED DATA RECORD VALUES)               
         MVC   KEY+(BATKDA-BATRECD)(L'BATKDA),0(R2)                             
         LA    R1,IOWRITE+IOACCDIR+IO3Q  WRITE RECORD                           
         GOTO1 AIOEXEC                                                          
         BE    BATMEQU                                                          
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD BATCH DETAILS LINE                                            *         
* NTRY - R1=A(OUTPUT)                                                 *         
***********************************************************************         
                                                                                
BLDDESC  STM   RE,R1,OVSVRER1      BUILD SUBSTITUTION BLOCK                     
         XC    WORK,WORK                                                        
                                                                                
         MVI   WORK,L'SBATREF+1    &1 - BATCH REFERENCE                         
         OC    WORK+1(L'SBATREF),SBATREF                                        
         LA    R2,WORK+L'SBATREF+1                                              
         BNZ   BLDDES02                                                         
         MVI   WORK,2                                                           
         MVI   WORK+1,C'?'         BATCH REFERENCE UNKNOWN                      
         LA    R2,WORK+2                                                        
                                                                                
BLDDES02 XC    TEMP,TEMP           &2 - BATCH MONTH                             
         OC    TEMP(L'SBATMONP),SBATMONP                                        
         BNZ   BLDDES04                                                         
         MVI   0(R2),2                                                          
         MVI   1(R2),C'?'          BATCH MONTH UNKNOWN                          
         LA    R2,2(R2)                                                         
         B     BLDDES06                                                         
BLDDES04 MVI   TEMP+L'SBATMONP,01                                               
         MVI   0(R2),9                                                          
         MVC   1(8,R2),SPACES                                                   
         GOTO1 VDATCON,DMCB,(1,TEMP),(9,1(R2))                                  
         LA    R2,9(R2)                                                         
*                                  &3 - ITEMS ADDED                             
BLDDES06 CURED ITEMS,(3,1(R2)),0,ALIGN=LEFT                                     
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                  &4 - ITEM COUNT CONTROL                      
         CURED BATITEM,(3,1(R2)),0,ALIGN=LEFT                                   
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         ZAP   DUB,TOTVOID         &5 - CASH TOTAL SO FAR                       
         TM    VOIDIND1,VOIDVOID   TEST UNVOID                                  
         BO    *+16                                                             
         ZAP   DUB,TOTCHQS         TAKE TOTAL CHEQUES                           
         SP    DUB,TOTVOID         SUBTRACT VOID TO GIVE UNVOIDED               
         CURED DUB,(13,1(R2)),2,ALIGN=LEFT,MINUS=YES                            
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         ZAP   DUB,PZERO           &6 - CASH CONTROL TOTAL                      
         OC    BATCTOT,BATCTOT                                                  
         BZ    *+10                                                             
         ZAP   DUB,BATCTOT                                                      
         CURED DUB,(13,1(R2)),2,ALIGN=LEFT,MINUS=YES                            
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*&&UK                                                                           
         TM    SAFCIND1,SAFCI1BC   &6 - CASH CONTROL CURRENCY TOTAL             
         BZ    BLDDES08                                                         
         ZAP   DUB,PZERO                                                        
         OC    BATCTOTC,BATCTOTC                                                
         BZ    *+10                                                             
         ZAP   DUB,BATCTOTC                                                     
         CURED DUB,(13,1(R2)),FORECURT,ALIGN=LEFT,MINUS=YES                     
         AH    R0,=H'1'                                                         
         STC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*&&                                                                             
BLDDES08 MVI   0(R2),0             SET END OF SUBSTITUTION BLOCK                
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         L     R0,OVSVR1                                                        
         STCM  R0,8,GTMAXL         MAXIMUM LENGTH                               
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,WORK                                                          
         STCM  R0,7,GTASUBST                                                    
         MVC   GTMSGNO,=AL2(AS$BATRM)                                           
         GOTO1 VGETTXT,(R1)                                                     
BLDDESCX LM    RE,R1,OVSVRER1                                                   
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* READ BANK ACCOUNT AND BUILD INTERCOMPANY TABLE IF NECESSARY         *         
* NTRY - R2=A(INTERCOMPANY TABLE                                      *         
***********************************************************************         
                                                                                
         USING ICOTABD,R2                                                       
BLDICO   NTR1  ,                                                                
         LA    R1,KEY              CLEAR KEY                                    
         USING ACTRECD,R1                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,ACCOUNT                                                 
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
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
         BZ    BLDICO08            EOR                                          
         ZAP   ICOAMNT,PZERO       PRESET AMOUNT TO ZERO                        
         MVC   ICOOFFC,ICPSOFF     SET OFFICE CODE                              
         MVC   ICOFACT,SPACES                                                   
         SH    RF,=Y((ICPSACC-ICPSUBEL)+1)                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ICOFACT(0),ICPSACC  SET FROM ACCOUNT                             
         IC    RF,ICPSLN           RESET SUB-ELEMENT LENGTH                     
         AR    R1,RF                                                            
         LA    R2,ICOTABL(R2)                                                   
         BCT   R0,BLDICO06                                                      
         DC    H'0'                INTERCOMPANY TABLE FULL                      
         DROP  R1                                                               
                                                                                
BLDICO08 L     R2,AICOTAB          NOW READ FROM INTERCOMPANY ACCOUNTS          
         LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         MVC   ACTKEY,SPACES       SET COMPANY IN KEY                           
         MVC   ACTKCPY,COMPANY                                                  
                                                                                
BLDICO10 CLI   ICOTABD,EOT         TEST EOT                                     
         BE    BLDICO22                                                         
         MVC   ICOFNAM,SPACES      CLEAR FROM ACCOUNT NAME                      
         MVC   ICOTNAM,SPACES      CLEAR TO ACCOUNT NAME                        
         MVC   ACTKUNT(L'ICOTACT),ICOFACT                                       
         XR    R4,R4               SET FIRST TIME FOR MINI-LOOP                 
                                                                                
BLDICO12 LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
         L     R1,AIOBUFF                                                       
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    R0,R0                                                            
         B     *+10                                                             
BLDICO14 IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         CLI   NAMEL,0             TEST EOR                                     
         BE    BLDICO18                                                         
         CLI   NAMEL,NAMELQ        TEST NAME ELEMENT                            
         BNE   BLDICO16                                                         
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         BNM   *+6                                                              
         DC    H'0'                                                             
         LA    RF,ICOFNAM          FROM ACCOUNT NAME                            
         LTR   R4,R4               TEST SECOND TIME                             
         BZ    *+8                                                              
         LA    RF,ICOTNAM          TO ACCOUNT NAME                              
         EX    RE,*+8                                                           
         B     BLDICO14                                                         
         MVC   0(0,RF),NAMEREC                                                  
         DROP  R1                                                               
                                                                                
BLDICO16 LTR   R4,R4               TEST FIRST TIME                              
         BNZ   BLDICO14                                                         
         USING APTELD,R1                                                        
         CLI   APTEL,APTELQ        TEST ACCOUNT POINTER ELEMENT                 
         BNE   BLDICO14                                                         
         TM    APTSTAT,APTSINTL    TEST INTERCOMPANY POINTER ACCOUNT            
         BZ    BLDICO14                                                         
         MVC   ICOTACT,APTACCU     EXTRACT U/L/ACCOUNT                          
         B     BLDICO14                                                         
         DROP  R1                                                               
                                                                                
BLDICO18 LTR   R4,R4               TEST FIRST TIME                              
         BNZ   BLDICO20                                                         
         MVC   ACTKUNT(L'ICOTACT),ICOTACT                                       
         LA    R4,1                                                             
         B     BLDICO12            RETURN TO SET TO ACCOUNT VALUES              
                                                                                
BLDICO20 CLC   ICOFNAM,SPACES      TEST FROM ACCOUNT NAME                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   ICOTNAM,SPACES      TEST TO ACCOUNT NAME                         
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ICOTABL(R2)                                                   
         B     BLDICO10            RETURN TO READ/SET NEXT TABLE ENTRY          
                                                                                
BLDICO22 DS    0H                                                               
                                                                                
BLDICOX  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD NARRATIVE EXPRESSION FOR TRANSACTIONS BY TYPE                 *         
***********************************************************************         
                                                                                
BLDNARR  STM   RE,R1,OVSVRER1                                                   
         MVC   VOIDNARR,SPACES                                                  
         MVC   WORK(L'DATETEMP+1),SPACES                                        
         GOTO1 VDATCON,DMCB,(5,0),(17,WORK)                                     
                                                                                
         L     RF,ABTYPTAB                                                      
         USING BTYPTABD,RF                                                      
                                                                                
BLDNAR02 CLI   BTYPTABD,EOT        TEST EOT                                     
         JE    *+2                                                              
         CLC   BTYPBANK,TSARBTY    TEST CORRECT BANK LEDGER BATCH TYPE          
         BNE   BLDNAR04                                                         
         CLI   BTYPCTRY,0          TEST COUNTRY RESTRICTION                     
         BE    BLDNAR06                                                         
         CLC   BTYPCTRY,AGYCTRY    TEST COUNTRY MATCHES                         
         BE    BLDNAR06                                                         
BLDNAR04 LA    RF,BTYPTABL(RF)                                                  
         B     BLDNAR02                                                         
                                                                                
BLDNAR06 MVC   OVHALF,BTYPNARV     VOID NARRATIVE MESSAGE                       
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+10                                                             
         MVC   OVHALF,BTYPNARU     UNVOID NARRATIVE MESSAGE                     
         DROP  RF                                                               
         LA    RE,OV$MSGS                                                       
         AH    RE,OVHALF                                                        
         MVC   VOIDNARR,0(RE)                                                   
         XR    RF,RF                                                            
         IC    RF,L'VOIDNARR(RE)                                                
         BCTR  RF,0                                                             
         STC   RF,VOIDNARL         SET EXECUTE L'NARRATIVE                      
                                                                                
         LA    RE,VOIDNARR         REPLACE ******** WITH DATE                   
         CLC   DATETEMP,0(RE)                                                   
         BE    *+14                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
                                                                                
         LA    RF,L'DATETEMP       LEFT JUSTIFY DATE                            
         CLI   WORK,C' '                                                        
         BH    *+16                                                             
         MVC   WORK(L'DATETEMP),WORK+1                                          
         BCT   RF,*-14                                                          
         DC    H'0'                NO DATE FOUND                                
         MVC   0(L'DATETEMP,RE),WORK                                            
                                                                                
         IC    RF,VOIDNARL         CALCULATE LENGTH REMAINING                   
         LA    R1,VOIDNARR(RF)                                                  
         LA    RE,L'DATETEMP-1(RE)                                              
         SR    R1,RE                                                            
                                                                                
         CLI   0(RE),C' '          REMOVE TRAILING SPACES                       
         BH    *+22                                                             
         EX    R1,*+12                                                          
         BCTR  RE,0                                                             
         BCT   RF,*-14                                                          
         DC    H'0'                                                             
         MVC   0(0,RE),1(RE)                                                    
                                                                                
         STC   RF,VOIDNARL                                                      
                                                                                
BLDNARRX LM    RE,R1,OVSVRER1                                                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CHEQUE MANAGEMENT ROUTINES                                          *         
***********************************************************************         
                                                                                
CHQMAN   NTR1  ,                                                                
         SLL   R1,2                                                             
         B     *+0(R1)                                                          
         B     CHQSET              1 - SET CHEQUE DATA FROM TSAR RECORD         
         B     CHQTST              2 - TEST CHEQUE .V. SAVED CHEQUE             
         B     CHQMRK              3 - (UN)MARK CHEQUES, UPDATE TOTALS          
                                                                                
CHQMNEQ  LTR   RB,RB                                                            
         B     XIT                                                              
CHQMEQU  CR    RB,RB                                                            
CHQMANX  B     XIT                                                              
                                                                                
CHQSET   MVC   FILTCHN,TSARREF                       NUMBER                     
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(2,FILTCHD)  DATE                       
         MVC   OCHQDATP,TSARDAT                      PWOS DATE                  
         MVC   FILTCHB,ACCOUNT+(ACTKUNT-ACTRECD)     BANK A/C                   
         ZAP   FILTCHA,TSARAMNT                      AMOUNT                     
         OC    TSARCHQT,TSARCHQT                     TEST CROSS OFFICE          
         BZ    *+10                                                             
         ZAP   FILTCHA,TSARCHQT                      CHEQUE TOTAL               
*&&UK                                                                           
         ZAP   FILTCHAF,PZERO                                                   
         OC    TSARAFCA,TSARAFCA                                                
         BZ    *+10                                                             
         ZAP   FILTCHAF,TSARAFCA                     AFC AMOUNT                 
         OC    TSARCHQC,TSARCHQC                     TEST CROSS OFFICE          
         BZ    *+10                                                             
         ZAP   FILTCHAF,TSARCHQC                     CHEQUE TOTAL               
*&&                                                                             
         XC    OCHQIND,OCHQIND     CLEAR CHEQUE INDICATOR                       
         ZAP   OCHQADV,TSARCADV    TAKE ADVANCE AMOUNT                          
         BZ    *+8                                                              
         OI    OCHQIND,OCHQIADV    NON-ZERO - SET ADVANCE CARRIED               
         ZAP   OCHQDIF,TSARCDIF    TAKE DIFFERENCE AMOUNT                       
         BZ    *+8                                                              
         OI    OCHQIND,OCHQIADV    NON-ZERO - SET DIFFERENCE CARRIED            
                                                                                
         MVC   FILTSBR,TSARSBR                       SUB-REFERENCE              
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    *+10                                                             
         MVC   STSAROFF,TSAROFF    SAVE OFFICE FOR INVOICE MATCHING             
         MVC   STSARCON,TSARCON    SAVE CHEQUE CONTRA (SUPPLIER)                
         MVC   SUPP,TSARCON        SAVE SUPPLIER                                
         MVI   STSARCXL,L'STSARCON-1                                            
         NI    CHQINDS,FF-CHQIARTQ  CLEAR ARTIST CHEQUE                         
         CLC   ARTFUL,TSARCON+(ACTKUNT-ACTRECD)  TEST ARTIST CHEQUE             
         BNE   CHQSET02                                                         
         LA    R1,TSARCON+L'TSARCON-1                                           
         LA    RE,TSARCON                                                       
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,RE                                                            
         STC   R1,STSARCXL                                                      
         OI    CHQINDS,CHQIARTQ    SET ARTIST CHEQUE                            
                                                                                
CHQSET02 MVC   STSARIND,TSARINDS                                                
         NI    STSARIND,TSARMKQ+TSARINMQ  SAVE MARKED BITS                      
         MVC   STSARBTY,TSARBTY    SAVE BATCH TYPE                              
         MVC   STSARMOS,TSARMOS    SAVE BATCH MOA                               
         ZAP   TOTDIFF,FILTCHA     SAVE CHEQUE VALUE                            
*&&UK*&& ZAP   CURDIFF,FILTCHAF    SAVE CHEQUE VALUE                            
                                                                                
         OC    TSARCHQT,TSARCHQT   TEST CROSS OFFICE CHEQUE                     
         BZ    CHQSET08                                                         
         ZAP   DUB,TSARCHQT        TAKE TOTAL OF CHEQUE                         
         SP    DUB,TSARAMNT        MINUS THIS PORTION                           
         MVC   TEMP(L'TSARDAT),TSARDAT                                          
         L     R1,ATSARBLK                                                      
         MVC   HALF,TSRNUM-TSARD(R1)                                            
         MVC   OVHALF,HALF         SAVE FIRST CHEQUE                            
CHQSET04 LH    R1,HALF                                                          
         LA    R1,1(R1)                                                         
         STH   R1,HALF                                                          
         GOTO1 ATSARGET,HALF       GET FURTHER CHEQUE(S)                        
         BNE   CHQSET06                                                         
         TM    TSARINDS,TSARMCHQ+TSARDISQ  TEST DISPLAY-ONLY CHEQUE             
         BNO   CHQSET04                                                         
         CLC   TSARREF,FILTCHN     TEST REF                                     
         BNE   CHQSET04                                                         
         CLC   TSARDAT,TEMP        TEST DATE                                    
         BNE   CHQSET04                                                         
         AP    OCHQADV,TSARCADV    ADD ANY ADVANCE                              
         BZ    *+8                                                              
         OI    OCHQIND,OCHQIADV    NON-ZERO - SET ADVANCE CARRIED               
         AP    OCHQDIF,TSARCDIF    ADD ANY DIFFERENCE                           
         BZ    *+8                                                              
         OI    OCHQIND,OCHQIDIF    NON-ZERO - SET DIFFERENCE CARRIED            
         SP    DUB,TSARAMNT        ADJUST CHEQUE TOTAL                          
         B     CHQSET04                                                         
CHQSET06 CP    DUB,PZERO           TEST TOTAL CHEQUE ACCOMMODATED               
         JNE   *+2                                                              
         GOTO1 ATSARGET,OVHALF     RESTORE FIRST CHEQUE TSAR RECORD             
                                                                                
CHQSET08 AP    TOTDIFF,OCHQADV     ADD ADVANCE                                  
         AP    TOTDIFF,OCHQDIF     ADD DIFFERENCE                               
*&&UK                                                                           
         XC    STSARAFX,STSARAFX                                                
         CP    TSARAFCA,PZERO      TEST CURRENCY VALUE HELD                     
         BE    CHQSET10                                                         
*        EXCHP OCHQADV,TSARAFCX    USE TRANSACTION EXCHANGE RATE                
         ZAP   DUB,OCHQADV                                                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,C.CURTCUR  FROM CURRENCY                                
         MVC   EURKCUTO,TSARAFCC   TO CURRENCY                                  
         MVC   EURKRULE,TSARAFCX   EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         JNE   *+2                                                              
         AP    CURDIFF,DUB         ADD ADVANCE CURRENCY                         
*        EXCHP OCHQDIF,TSARAFCX                                                 
         ZAP   DUB,OCHQDIF                                                      
         LA    RF,WORK             BUILD EUREKA CONTROL BLOCK                   
         USING EURKBLKD,RF                                                      
         XC    EURKBLKD(EURKBLKL),EURKBLKD                                      
         MVC   EURKCUFR,C.CURTCUR  FROM CURRENCY                                
         MVC   EURKCUTO,TSARAFCC   TO CURRENCY                                  
         MVC   EURKRULE,TSARAFCX   EXCHANGE RATE RULE                           
         GOTO1 VEUREKA,DMCB,('APPLYQ',EURKBLKD),DUB,DUB,0,0                     
         DROP  RF                                                               
         CLI   0(R1),0                                                          
         JNE   *+2                                                              
         AP    CURDIFF,DUB         ADD DIFFERENCE CURRENCY                      
         MVC   STSARAFX,TSARAFCX   SAVE EXCHANGE RULE                           
*&&                                                                             
CHQSET10 B     CHQMANX                                                          
                                                                                
CHQTST   TM    TSARINDS,TSARMCHQ+TSARDISQ  TEST DISPLAY-ONLY CHEQUE             
         BNO   CHQMNEQ                                                          
         OC    TSARCHQT,TSARCHQT   TEST CROSS-OFFICE CHEQUE                     
         BZ    CHQMNEQ                                                          
*        CLI   PROFNREC,C'Y'       TEST NOT AUTO-RECONCILING                    
*        BE    CHQTST02                                                         
*        TM    VOIDIND1,VOIDVOID   TEST VOIDING                                 
*        BZ    CHQTST02                                                         
*        TM    TSARSTA,TRNSBREC    TEST RECONCILED                              
*        BO    CHQMNEQ                                                          
CHQTST02 CLC   FILTCHN,TSARREF                                                  
         BNE   CHQMANX                                                          
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(2,DATWRK)                              
         CLC   FILTCHD,DATWRK                                                   
         BNE   CHQMANX                                                          
         CP    FILTCHA,TSARCHQT                                                 
         B     CHQMANX                                                          
         EJECT                                                                  
CHQMRK   OI    TWAMODE2,TWAM2CHG   SET CHANGES MADE                             
         MVI   ANYMARK,1           SET CHANGES THIS TIME                        
         ZAP   CHQTOT,FILTCHA      SET CHEQUE TOTAL                             
*&&UK*&& ZAP   CHQTOTC,FILTCHAF    SET CHEQUE TOTAL                             
         XC    OVRNUM,OVRNUM                                                    
                                                                                
CHQMRK02 ICM   R1,3,OVRNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,3,OVRNUM                                                      
         GOTO1 ATSARGET,OVRNUM                                                  
         BE    CHQMRK04                                                         
         USING TSARD,RF                                                         
         L     RF,ATSARBLK                                                      
         TM    TSERRS,TSEEOF                                                    
         BO    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
                                                                                
         CP    CHQTOT,PZERO        TEST CHEQUE TOTAL ACCOUNTED FOR              
         BE    CHQMRK22                                                         
         DC    H'0'                                                             
CHQMRK04 CLC   OVRNUM,STSARNUM     TEST THIS IS THE MAIN CHEQUE                 
         BNE   CHQMRK12                                                         
         TM    TSARINDS,TSARMKQ    TEST MARKING                                 
         BNO   CHQMRK08                                                         
         SP    TOTVOID,FILTCHA     UNMARKING - REDUCE VOID TOTAL                
*&&UK*&& SP    CURVOID,FILTCHAF                                                 
         TM    TSARINDS,TSARINMQ   TEST CAME IN MARKED                          
         BO    CHQMRK06                                                         
         SP    REPVOID,FILTCHA     NO - SUBTRACT FROM REPORT VOIDED             
*&&UK*&& SP    RCPVOID,FILTCHAF                                                 
         B     CHQMRK14                                                         
CHQMRK06 AP    REPUVOI,FILTCHA     YES - ADD TO REPORT UNVOIDED                 
*&&UK*&& AP    RCPUVOI,FILTCHAF                                                 
         B     CHQMRK14                                                         
                                                                                
CHQMRK08 AP    TOTVOID,FILTCHA     MARKING - INCREASE VOID TOTAL                
*&&UK*&& AP    CURVOID,FILTCHAF                                                 
         TM    TSARINDS,TSARINMQ   TEST CAME IN MARKED                          
         BO    CHQMRK10                                                         
         AP    REPVOID,FILTCHA     NO - ADD TO REPORT VOIDED                    
*&&UK*&& AP    RCPVOID,FILTCHAF                                                 
         B     CHQMRK14                                                         
CHQMRK10 SP    REPUVOI,FILTCHA     YES - SUBTRACT FROM REPORT UNVOIDED          
*&&UK*&& SP    RCPUVOI,FILTCHAF                                                 
         B     CHQMRK14                                                         
                                                                                
CHQMRK12 GOTO1 CHQMAN,CHQTSTQ      TEST DISPLAY-ONLY CHEQUE WHICH FITS          
         BNE   CHQMRK02                                                         
                                                                                
CHQMRK14 SP    CHQTOT,TSARAMNT     REDUCE TOTAL FOR CHEQUE                      
*&&UK*&& SP    CHQTOTC,TSARAFCA                                                 
         XI    TSARINDS,TSARMKQ    SWITCH MARKED STATUS                         
         CLI   FILTCHQ,INCLUDE     TEST CAME FROM CHEQUE DISPLAY                
         BE    *+18                                                             
         CLC   TSARCHA,AC3SELC     NO - TEST SELECTED CHEQUE                    
         BNE   *+8                                                              
         MVI   TSARCHA,C' '        CLEAR SELECT MARKER                          
         TM    TSARINDS,TSARINMQ+TSARMKQ                                        
         BNM   CHQMRK16            ORIGINAL STATUS UNCHANGED                    
         CLC   TSARMOS,HICMONP     TEST NEW HIGH MOS                            
         BNH   CHQMRK16                                                         
         MVC   HICMONP,TSARMOS     SET HIGH MOS                                 
         USING TSARD,RF                                                         
CHQMRK16 L     RF,ATSARBLK                                                      
         MVI   TSACTN,TSAPUT       WRITE BACK THE CHEQUE                        
         GOTO1 VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         CLI   FILTCHQ,INCLUDE     TEST CAME FROM CHEQUE DISPLAY                
         BNE   CHQMRK02                                                         
         L     R1,ADISDET1                                                      
         XR    RE,RE                                                            
         ICM   RE,3,DISLCNT                                                     
         LA    RF,DISLIST                                                       
CHQMRK18 CLC   OVRNUM,0(RF)        TEST THIS RECORD IS ON DISPLAY               
         BNE   CHQMRK20                                                         
         GOTO1 ABLDLIN,DISLHDR1-DISLINED(R1)                                    
         B     CHQMRK02            RE-DISPLAYED, NOW GET NEXT RECORD            
CHQMRK20 LA    R1,DISLINEL(R1)     R1=A(NEXT INPUT LINE)                        
         LA    RF,L'DISLIST(RF)    RF=A(NEXT TSAR RECORD NUMBER)                
         BCT   RE,CHQMRK18                                                      
         B     CHQMRK02            NOT DISPLAYED, GET NEXT RECORD               
                                                                                
CHQMRK22 B     CHQMEQU                                                          
         EJECT                                                                  
***********************************************************************         
* READ INVOICES PAID BY A CHEQUE AND ADD TO TSAR WITH A HIGH KEY      *         
***********************************************************************         
                                                                                
READINV  NTR1  ,                                                                
         MVC   KEY,SPACES          SET UP LEDGTAB ENTRY IF NECESSARY            
         MVC   KEY(LDGKEND),STSARCON                                            
         LA    R0,LEDGMAXN         R0=MAXIMIUM ENTRIES                          
         L     R1,ALEDGTAB                                                      
         USING LEDGTABD,R1         R1=A(LEDGER TABLE)                           
         CLC   LEDGTUL,KEY+(LDGKUNT-LDGRECD)  TEST LEDGER ALREADY THERE         
         BE    RINV02              DON'T NEED TO READ IT                        
         LA    R1,LEDGTABL(R1)     NEXT ENTRY                                   
         BCT   R0,*-14                                                          
         DROP  R1                                                               
                                                                                
         GOTO1 AGETLDG,0           NOT FOUND - GETLDG WILL READ FILE            
         BE    *+6                                                              
         DC    H'0'                CAN'T ALLOW ANY ERROR                        
                                                                                
         USING TRNRECD,R2                                                       
RINV02   LA    R2,KEY                                                           
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,STSARCON                                                
         MVC   TRNKOFF,SPACES                                                   
         MVI   TRNKCACT+L'TRNKCACT-1,X'41'                                      
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ATS2CLR             CLEAR INVOICE BUFFER                         
                                                                                
*&&US*&& GOTO1 AVALPUB,TRNRECD     VALIDATE SPECIAL PUBLICATION NUMBER          
         CLI   OVFLAG,OVFCHK       TEST CHECK ONLY                              
         BE    RINV04                                                           
         XC    INVRNUM,INVRNUM     KEY SEQUENCE NUMBER                          
         CLI   OVFLAG,OVFRCK       TEST READ AND CHECK INVOICES ALL FIT         
         BE    RINV04                                                           
         XC    TSARREC+L'TSARLEN(TSARRECL-L'TSARLEN),TSARREC+L'TSARLEN          
                                                                                
RINV04   LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         LA    R2,KEY                                                           
         GOTO1 AIOEXEC                                                          
         BNE   RINV30                                                           
         IC    R1,STSARCXL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TRNKCULA(0),STSARCON                                             
         BNE   RINV30                                                           
                                                                                
RINV06   OC    TRNKDATE,TRNKDATE   TEST TRANSACTION                             
         BZ    RINV04                                                           
         CLC   TRNKDATE,SPACES                                                  
         BE    RINV04                                                           
         TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   RINV04                                                           
         TM    TRNKSTA2,TRNSPEEL                                                
         BNZ   RINV04                                                           
                                                                                
RINV08   LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         DROP  R2                                                               
                                                                                
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF    SET ELEMENT ADDRESSES                        
                                                                                
         USING TRNELD,R2                                                        
RINV16   ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    RINV18                                                           
         TM    TRNSTAT,TRNSREV     DROP REVERSED CREDIT                         
         BO    RINV04              (NOT INTERESTED IN EX DIFFS YET)             
         B     RINV20                                                           
RINV18   TM    VOIDIND1,VOIDUVOI   TEST VOID/UNVOID                             
         BZ    *+16                                                             
         TM    TRNSTAT,TRNSREV     UNVOID - WANT REVERSED DEBITS ONLY           
         BZ    RINV04                                                           
         B     RINV20                                                           
         TM    TRNSTAT,TRNSREV     VOID - DROP REVERSED DEBITS                  
         BNZ   RINV04                                                           
RINV20   GOTO1 AVENFILT,IGNEXQ     FILTER VENDOR TRANS. R3 @ INVREC             
         LTR   R2,R2               R2=A(TRANSACTION ELEMENT) OR 0               
         BZ    RINV04              TRANSACTION FAILS                            
         USING INVRECD,R3                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   RINV22                                                           
         GOTO1 ACRBUILD,PARM,INVRECD,TRNELD                                     
         B     RINV24                                                           
RINV22   GOTO1 ADRBUILD,PARM,INVRECD,TRNELD                                     
RINV24   GOTO1 ATS2APW,TSAPUT      PUT BACK UPDATED RECORD                      
         GOTO1 ADCMATCH,PARM,INVRECD,TRNELD                                     
         BE    RINV04                                                           
                                                                                
         DROP  R2,R3                                                            
                                                                                
RINV30   CLI   OVFLAG,OVFRCK       TEST READ AND CHECK INVOICES FIT             
         BE    READINVX            YES - ALL IS WELL                            
         CLI   OVFLAG,OVFCHK       TEST ONLY CHECK ZERO BALANCE                 
         BNE   RINV34                                                           
         CP    TOTDIFF,PZERO       TEST ZERO BALANCE                            
         BE    RINV32                                                           
         ZAP   TOTDIFF,PZERO       CLEAR DIFFERENCE                             
         ZAP   OCHQADV,PZERO       CLEAR ADVANCE BALANCE                        
         MVC   FVMSGNO,=AL2(EAMINECH)                                           
         LTR   RB,RB               SET CC NEQ                                   
         B     READINVX                                                         
RINV32   CP    OCHQADV,PZERO       TEST ZERO BALANCE (ADVANCES)                 
         BE    READINVX                                                         
         ZAP   OCHQADV,PZERO       CLEAR ADVANCE BALANCE                        
         MVC   FVMSGNO,=AL2(AE$ADMIS)                                           
         LTR   RB,RB               SET CC NEQ                                   
         B     READINVX                                                         
                                                                                
         USING TSARD,R1                                                         
RINV34   L     R1,ATSARBLK         R1=A(TSAR CONTROL BLOCK)                     
         MVC   DISMAX,TSPRECN      SET HIGH RECORD NUMBER                       
         DROP  R1                                                               
         CR    RB,RB               SET CC EQU                                   
                                                                                
READINVX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OVERLAY SPECIFIC FILTERING                                          *         
***********************************************************************         
                                                                                
RFILTER  NTR1  ,                                                                
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BZ    RFILTNEQ                                                         
         TM    TRNSTAT,TRNSDR      TEST CREDIT                                  
         BO    RFILTNEQ                                                         
         USING BTYPTABD,RF                                                      
         L     RF,ABTYPTAB         TEST VALID TRNTYPE                           
         B     *+8                                                              
RFILT02  LA    RF,BTYPTABL(RF)                                                  
         CLI   BTYPTABD,EOT        TEST EOT                                     
         BE    RFILTNEQ            NO MATCH FOUND                               
         CLC   BTYPBANK,TRNTYPE    TEST BANK TRNTYPE                            
         BNE   RFILT02                                                          
         CLI   BTYPCTRY,0          TEST COUNTRY RESTRICTION                     
         BE    *+14                                                             
         CLC   BTYPCTRY,AGYCTRY    TEST CORRECT COUNTRY                         
         BNE   RFILT02                                                          
                                                                                
         XR    RE,RE                                                            
         ICM   RE,3,BTYPYPRG       TEST PROGRAM RESTRICTION                     
         BZ    RFILT06                                                          
         A     RE,APROGS                                                        
         ICM   R1,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         MVC   OVBYTE,TRSSTAT3-TRSELD(R1)                                       
         NI    OVBYTE,TRSSPROG     LEAVE ONLY PROGRAM NUMBER INTACT             
RFILT04  CLI   0(RE),EOT           TEST EOT                                     
         BE    RFILT02                                                          
         CLC   OVBYTE,0(RE)        TEST PROGRAM IN VALID LIST                   
         BE    RFILT06                                                          
         LA    RE,1(RE)                                                         
         B     RFILT04                                                          
         DROP  RF                                                               
RFILT06  L     RF,AIOBUFF                                                       
         CLC   PRODUL,TRNKCUNT-TRNRECD(RF) TEST PRODUCTION CONTRA U/L           
         BE    RFILTNEQ            YES - BILLABLE CHEQUE (ALSO BT3)             
         CLC   BANKUL,TRNKCUNT-TRNRECD(RF) TEST BANK CONTRA U/L                 
         BE    RFILTNEQ            YES - MAIN PAYMENT (ALSO BT60/BT57)          
                                                                                
RFILT08  DS    0H                                                               
*FILT08  CLI   PROFNREC,C'Y'       TEST NOT AUTO-RECONCILING                    
*        BE    RFILT10                                                          
*        TM    VOIDIND1,VOIDUVOI   TEST VOID/UNVOID                             
*        BZ    RFILT10                                                          
*        TM    TRNSTAT,TRNSBREC    UNVOID - MUST BE RECONCILED                  
*        BZ    RFILTNEQ                                                         
*        B     RFILT12                                                          
                                                                                
RFILT10  CLI   PROFXREC,C'Y'       VOID - TEST EXCLUDING RECONCILED             
         BNE   RFILT12                                                          
         TM    TRNSTAT,TRNSBREC    TEST RECONCILED                              
         BO    RFILTNEQ                                                         
         B     RFILT12                                                          
                                                                                
         USING TRSELD,R2                                                        
RFILT12  ICM   R2,15,ATRSEL                                                     
         LA    RF,BOQ              ASSUME DROP IF VOID                          
         TM    VOIDIND1,VOIDUVOI   IF UNVOIDING                                 
         BZ    *+8                                                              
         LA    RF,BZQ              DROP IF NOT VOID                             
         TM    TRSSTAT,TRSSVOID    TEST VOID                                    
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   RFILTNEQ                                                         
                                                                                
         LA    RF,BOQ              ASSUME DROP IF ANALYSED                      
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    *+8                                                              
         LA    RF,BZQ              DROP IF NOT ANALYSED                         
         TM    TRSSTAT2,TRSSANBP   TEST RUN WITH ANALYSIS BANK POSTINGS         
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   RFILTNEQ                                                         
                                                                                
         CLC   TRSDATE,ADASTA      TEST ACTIVITY DATE                           
         BL    RFILTNEQ                                                         
         CLC   TRSDATE,ADAEND                                                   
         BH    RFILTNEQ                                                         
                                                                                
         CLI   TRSMARK,TRSMCMQ     TEST CRED/MAN OR BANK/VOID                   
         BE    *+12                                                             
         CLI   TRSMARK,TRSMBVQ                                                  
         BNE   *+28                                                             
         L     RF,AIOBUFF                                                       
         CLC   EXPENSE,TRNKULC-TRNRECD(RF)   TEST SE OR SI CONTRA               
         BE    RFILTNEQ                                                         
         CLC   INCOME,TRNKULC-TRNRECD(RF)                                       
         BE    RFILTNEQ                                                         
*&&UK                                                                           
         OC    TRSUSER,TRSUSER     TEST USER ID KNOWN                           
         BZ    *+14                                                             
         CLC   TRSUSER,TWAUSRID    TEST USER-ID MATCHES                         
         BNE   RFILTNEQ                                                         
*&&                                                                             
         DROP  R2                                                               
                                                                                
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    RFILT18                                                          
         USING MPYELD,R2                                                        
         ICM   R2,15,AMPYEL                                                     
         BZ    RFILTNEQ            NO MPYEL, SO DROP                            
         OC    FXRSTA,FXRSTA                                                    
         BZ    RFILT14                                                          
         IC    RF,FXRSTAXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   MPYNO(0),FXRSTA     TEST AGAINST START FIXED REFERENCE           
         BL    RFILTNEQ            LOWER THAN START, SO DROP                    
         BE    RFILT14                                                          
         OC    FXREND,FXREND       HIGHER - TEST END FIXED REFERENCE            
         BNZ   RFILT16                                                          
         B     RFILTNEQ            NO - START IS ABSOLUTE, SO DROP              
                                                                                
RFILT14  OC    FXREND,FXREND                                                    
         BZ    RFILT18                                                          
RFILT16  IC    RF,FXRENDXL         TEST AGAINST END FIXED REFERENCE             
         EX    RF,*+8                                                           
         BH    RFILTNEQ            HIGHER THAN END REFERENCE, SO DROP           
         CLC   MPYNO(0),FXREND                                                  
         DROP  R2                                                               
                                                                                
RFILT18  GOTO1 ATCLOSE             TEST TRANSACTION IS CLOSED OR OFFICE         
         BNE   RFILT20             ACCESS IS RESTRICTED                         
         OC    OFFICE,OFFICE       TEST FIXED OFFICE                            
         BZ    RFILT22                                                          
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         IC    R1,OFFICEXL                                                      
         EX    R1,*+8                                                           
         BE    RFILT22                                                          
         CLC   TRNOFFC(0),OFFICE   TEST OFFICE MATCH                            
         DROP  R2                                                               
RFILT20  OI    CHQINDS,CHQIFILT    SET OFFICE CHEQUE FILTERED                   
         B     RFILTNEQ            DON'T INCLUDE THIS TRANSACTION               
                                                                                
RFILT22  DS    0H                                                               
                                                                                
RFILTEQU CR    RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTNEQ LTR   RB,RB                                                            
         B     RFILTERX                                                         
                                                                                
RFILTERX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A KEY FOR TRANSACTION READING                      *         
* NTRY - R1=KEY BUILD MASK (SEE SETXXX EQUATES)                       *         
*        R2=A(TRANSACTION KEY)                                        *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
SETKEY   NTR1  ,                                                                
         STC   R1,WORK                                                          
                                                                                
         TM    WORK,SETACC         SET ACCOUNT                                  
         BZ    SETKEY00                                                         
         MVC   TRNKEY,SPACES                                                    
         OC    ACCOUNT,ACCOUNT                                                  
         BNZ   *+6                                                              
         DC    H'0'                BANK ACCOUNT MISSING                         
         MVC   TRNKCULA,ACCOUNT                                                 
                                                                                
SETKEY00 TM    WORK,SETOFF         SET OFFICE                                   
         BZ    SETKEY02                                                         
         MVC   TRNKOFF,SPACES                                                   
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    SETKEY02            NO - OFFICE NOT IN KEY                       
         CLI   FILEFORM,VLISQ      TEST V/L INDEX SEQUENTIAL FILE               
         BE    SETKEY02            YES - OFFICE NOT IN KEY                      
         OC    OFFICE,OFFICE                                                    
         BZ    SETKEY02                                                         
         IC    RF,OFFICEXL                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TRNKOFF(0),OFFICE                                                
                                                                                
SETKEY02 TM    WORK,SETCON         SET CONTRA                                   
         BZ    SETKEY04                                                         
         MVC   TRNKCULC,SPACES                                                  
         LA    RF,TRNKCULC         POINT TO COMPANY IN CONTRA A/C               
         CLC   BANKUL,TRNKUNT      TEST ACCOUNT IS IN BANK U/L                  
         BE    *+8                 YES - SET TO SKIP '   ***VOID***'            
         LA    RF,L'TRNKCULC-1(RF) ELSE SET FOR 1ST CONTRA A/C > SPACES         
         MVI   0(RF),X'41'         SET FIRST/LAST BYTE OF TRNKCULC              
         OC    CONTRA,CONTRA                                                    
         BZ    SETKEY04                                                         
         MVC   TRNKCULC,CONTRA                                                  
                                                                                
SETKEY04 TM    WORK,SETSDT         SET MIN TRANSACTION DATE                     
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
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL EXIT                                                        *         
***********************************************************************         
                                                                                
EXIT     MVI   DUB,0               SET FOR CC LOW                               
         BL    EXIT02                                                           
         MVI   DUB,1               SET FOR CC EQUAL                             
         BE    EXIT02                                                           
         MVI   DUB,2               SET FOR CC HIGH                              
EXIT02   LA    R1,TSA2BLK                                                       
         USING TSARD,R1            R1=A(TSAR 2 BLOCK)                           
         MVI   TSACTN,TSASAV       SET SAVE                                     
         GOTO1 VTSAR               CALL TO SAVE                                 
         BE    *+6                                                              
         DC    H'0'                KILL IF SAVE FAILS                           
         CLI   DUB,1               SET CC LOW/EQUAL/HIGH                        
XIT      XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
UIXPDFLT DC    AL1(UPDICDSC+UPDIBCHA+UPDIEXDF)                                  
                                                                                
DCDICTU  DS    0X                  BANK/VOID UPPER CASE DICTIONARY              
         DCDDL AC#VOID,L'BVU@VOID  VOID FOR TRANSACTION REFERENCE               
DCDICTUX DC    AL1(EOT)                                                         
                                                                                
DCDICTM  DS    0X                  BANK/VOID MIXED CASE DICTIONARY              
         DCDDL AC#BNKA,L'BVM@BNKA  BANK A/C                                     
         DCDDL AC#NRTV,L'BVM@NRTV  NARRATIVE                                    
DCDICTMX DC    AL1(EOT)                                                         
                                                                                
EXPENSE  DC    C'SE'                                                            
INCOME   DC    C'SI'                                                            
                                                                                
LDGLIST  DS    0CL(L'LEDGER)       VALID LEDGERS FOR 'BANK A/C'                 
         DC    C'SB'               IN CASE THEY PUT *ULACCOUNT                  
         DC    C'SC'               ALLOW THIS, THOUGH IT'S THE DEFAULT          
                                                                                
VENDLDGS DS    0XL2                                                             
         DC    CL1'S',AL1(ACCISPOT)                                             
         DC    CL1'T',AL1(ACCISPOT)                                             
         DC    CL1'U',AL1(ACCISPOT+ACCINETW)                                    
         DC    CL1'P',AL1(ACCIPRNT)                                             
         DC    CL1'Q',AL1(ACCIPRNT)                                             
         DC    CL1'V',AL1(ACCIPROD)                                             
         DC    CL1'Y',AL1(ACCIPROD)                                             
VENDLDGN EQU   (*-VENDLDGS)/L'VENDLDGS                                          
                                                                                
CTRYTAB  DS    0XL4                                                             
         DC    AL1(CTRYUSA)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(CTRYCAN)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0)                                                         
                                                                                
         DC    AL1(CTRYGER)                                                     
         DC    AL1(UPDICDSC+UPDIDTAX+UPDIBCHA+UPDIEXDF)                         
         DC    AL1(0,0)                                                         
                                                                                
CTRYTABX DC    AL1(EOT)                                                         
                                                                                
PROGS    DS    0X                  VALID PROGRAM LISTS                          
         DC    AL1(0)              ENSURE NON-ZERO DISPLACEMENT                 
PROGCM   DC    AL1(TRSSMRK1)       ** CREDITOR/MANUAL VALID PROGRAMS **         
         DC    AL1(TRSSMRK2)                                                    
         DC    AL1(0)                                                           
                                                                                
BTYPTAB  DS    0X                  BANK CR & VENDOR DR TRNTYPE TABLE            
         DC    AL1(BT3,BT129)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(OV$CHQVD-OV$MSGS)                                            
         DC    AL2(OV$CHQUV-OV$MSGS)                                            
                                                                                
         DC    AL1(BT57,BT57)                                                   
         DC    AL1(CTRYGER)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(OV$XFRVD-OV$MSGS)                                            
         DC    AL2(OV$XFRUV-OV$MSGS)                                            
                                                                                
         DC    AL1(BT60,BT60)                                                   
         DC    AL1(CTRYGBR)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(OV$PAYVD-OV$MSGS)                                            
         DC    AL2(OV$PAYUV-OV$MSGS)                                            
                                                                                
         DC    AL1(BT60,BT60)                                                   
         DC    AL1(CTRYHOL)                                                     
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(OV$PAYVD-OV$MSGS)                                            
         DC    AL2(OV$PAYUV-OV$MSGS)                                            
                                                                                
         DC    AL1(BT36,BT36)                                                   
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL2(PROGCM-PROGS)                                                
         DC    AL2(OV$CHQVD-OV$MSGS)                                            
         DC    AL2(OV$CHQUV-OV$MSGS)                                            
                                                                                
BTYPTABX DC    AL1(EOT)                                                         
         DROP  R9,RA,RB                                                         
         EJECT                                                                  
***********************************************************************         
* OVERLAY ROUTINES                                                              
***********************************************************************         
                                                                                
         DS    0F                                                               
OVROUT   NMOD1 0,**OVRO**,RA,R9                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     ADDOFF                                                           
         B     CRBUILD                                                          
         B     DRBUILD                                                          
         B     DCMATCH                                                          
         B     DETACC                                                           
         B     EXTACC                                                           
         B     EXTLDG                                                           
         B     GETCON                                                           
         B     GOTTXT                                                           
         B     POST                                                             
         B     POSTPT                                                           
         B     REVCR                                                            
         B     REVVST                                                           
         B     SRTOFF                                                           
         B     TAXADJ                                                           
         B     TS2APW                                                           
         B     TS2CLR                                                           
         B     TS2DEL                                                           
         B     TS2GET                                                           
         B     VALCQD                                                           
*&&UK                                                                           
         B     VALBCUR                                                          
         B     VCREXCH                                                          
*&&                                                                             
         B     VENDOR                                                           
         B     VENCR                                                            
         B     VENDR                                                            
         B     VENDC                                                            
         B     VENFILT                                                          
         B     DOP2J                                                            
                                                                                
OVROUTL  MVI   DUB,0                                                            
         B     *+8                                                              
OVROUTE  MVI   DUB,1                                                            
         B     *+8                                                              
OVROUTH  MVI   DUB,2                                                            
         CLI   DUB,1                                                            
OVROUTX  XIT1  ,                                                                
                                                                                
OVROUTXS XIT1  REGS=(R2,R3)        LEAVE R2,R3 INTACT                           
         EJECT                                                                  
***********************************************************************         
* ADD/AMEND OFFICE TABLE ENTRIES TO ALLOW EACH POSTING ACCOUNT        *         
***********************************************************************         
                                                                                
ADDOFF   LA    R3,PSTTAB                                                        
         USING PSTTABD,R3                                                       
ADDOFF02 OC    PSTACC,PSTACC       TEST POSTING ACCOUNT IN A TABLE              
         BZ    ADDOFF08            YES - IGNORE IT                              
         MVC   LARFADDR,PSTACC     FOR EACH POSTING ACCOUNT                     
         USING ACCXD,RF                                                         
         EX    0,LARF                                                           
         OC    ACCX,ACCX           TEST ACCOUNT ENTRY USED                      
         BZ    ADDOFF08            NO - IGNORE IT                               
         XR    R1,R1                                                            
         ICM   R1,1,ACCXOPIK                                                    
         BZ    ADDOFF08                                                         
         TM    PSTIND2,PST2OPIK                                                 
         BNZ   ADDOFF08                                                         
                                                                                
         USING OFFTABD,R4                                                       
         L     R4,AOFFTAB                                                       
         LA    R1,ACCX+(ACTKLDG-ACTKCPY)(R1)                                    
         DROP  RF                                                               
ADDOFF04 CLI   OFFTABD,FF          FOR EACH OFFICE                              
         BNE   *+6                                                              
         DC    H'0'                NO MORE ROOM IN OFFTAB                       
         CLI   OFFTABD,0           SCAN FOR APROPRIATE OR LAST OFFICE           
         BNE   ADDOFF06                                                         
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         MVC   OFFOFFC(1),0(R1)                                                 
         MVI   OFFOFFC+1,C' '                                                   
ADDOFF06 CLC   OFFOFFC(1),0(R1)                                                 
         BE    *+12                                                             
         LA    R4,OFFTABL(R4)      NEXT OFFICE TABLE ENTRY                      
         B     ADDOFF04                                                         
                                                                                
         OC    OFFIND1,PSTIND1     THIS OFFICE HAS THIS POSTING                 
         OI    OFFIND1,OFFIBCHA                                                 
         TM    VOIDIND1,VOIDBCHA   TEST VOIDING BANK CHARGES                    
         BNZ   *+8                                                              
         NI    OFFIND1,FF-OFFIBCHA                                              
                                                                                
ADDOFF08 LA    R3,PSTTABL(R3)      NEXT POSTING ACCOUNT                         
         CLI   PSTIND1,EOT                                                      
         BNE   ADDOFF02                                                         
                                                                                
ADDOFFX  B     OVROUTX                                                          
         DROP  R4,R3                                                            
         EJECT                                                                  
***********************************************************************         
* CRBUILD - BUILD AN INVOICE TSAR 2 RECORD FROM INVOICE (CREDIT)      *         
* NTRY - P1 A(INVOICE TSAR 2 RECORD), P2 A(TRNEL)                     *         
***********************************************************************         
                                                                                
CRBUILD  L     R3,0(R1)                                                         
         USING INVRECD,R3          R3=A(NEW/RELEVANT INVREC)                    
         L     R2,4(R1)                                                         
         USING TRNELD,R2                                                        
*&&UK*&& SP    INVEXCH,TRNAMNT                                                  
         ZAP   INVDSC,PZERO                                                     
         USING SCIELD,R1           EXTRACT SCIEL VALUES                         
         ICM   R1,15,ASCICDSC                                                   
         BZ    CRBLD02                                                          
         ZAP   INVDSC,SCIAMNT      SET AMOUNT OF DISCOUNT                       
*&&UK                                                                           
         CLI   SCILN,SCILN2Q                                                    
         BL    *+14                                                             
         ZAP   INVDSCC,SCIADMN                                                  
         B     CRBLD02                                                          
         ZAP   INVDSCC,TRNAMNT                                                  
         BZ    CRBLD02                                                          
         ZAP   PKWK16A,SCIAMNT                                                  
         DROP  R1                                                               
         ICM   R1,15,AAFCEL                                                     
         BZ    CRBLD02                                                          
         USING AFCELD,R1                                                        
         MP    PKWK16A,AFCAMNT                                                  
         DROP  R1                                                               
         SRP   PKWK16A,RNDING,0                                                 
         DP    PKWK16A,TRNAMNT                                                  
         ZAP   PKWK16B,PKWK16A(L'PKWK16A-L'TRNAMNT)                             
         SRP   PKWK16B,64-RNDING,5                                              
         ZAP   INVDSCC,PKWK16B                                                  
*&&                                                                             
CRBLD02  DS    0H                                                               
*&&US                                                                           
         USING XPYELD,R1           EXTRACT XPYEL VALUES                         
         ICM   R1,15,AXPYEL                                                     
         BZ    *+16                                                             
         MVC   INVMIV,XPYINV       SET MEDIA INVOICE#                           
         ZAP   INVDSC,XPYCD        SET DISCOUNT                                 
         DROP  R1                                                               
                                                                                
         USING FFTELD,R1           EXTRACT LONG INVOICE NUMBER                  
         ICM   R1,15,AFFTLEL                                                    
         BZ    CRBLD03                                                          
         XR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         C     RE,=F'20'                                                        
         BNH   *+8                                                              
         LA    RE,20                                                            
         BCTR  RE,0                TAKE MAX OF 20 CHAR                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   INVMIV(0),FFTDATA                                                
         DROP  R1                                                               
*&&                                                                             
CRBLD03  GOTO1 ABLDSRC             BUILD SOURCE ACCOUNT IN SRCWORK              
         MVC   INVSRC,SRCWORK      TAKE SOURCE A/C                              
*&&US*&& MVC   INVSIND,TSARIND2    INDICATOR BYTE 2 SET BY BLDSRC               
*&&UK                                                                           
         USING SORELD,R1                                                        
         ICM   R1,15,ASOREL        USE SOREL WORKCODE, IF PRESENT               
         BZ    CRBLD04                                                          
         CLI   SORSYS,SORSACC      TEST ACCOUNTING SOURCE A/C                   
         BNE   CRBLD04                                                          
         CLI   SORLN,SORAL2Q       TEST LONG ACCOUNTING ELEMENT                 
         BL    CRBLD04                                                          
         MVC   INVSWRK,SORAWRK     YES TAKE WORKCODE                            
         B     CRBLD06                                                          
         USING CPJELD,R1                                                        
CRBLD04  ICM   R1,15,ACPJEL                                                     
         BZ    CRBLD06                                                          
         CLI   CPJTYPE,CPJTJOB     TEST PRODUCTION SOURCE A/C                   
         BNE   *+10                                                             
         MVC   INVSWRK,CPJWRK      YES TAKE WORKCODE                            
         DROP  R1                                                               
*&&                                                                             
CRBLD06  L     R1,AIOBUFF                                                       
         USING TRNRECD,R1                                                       
         MVC   INVKREF,TRNKREF     SET TRANSACTION KEY REFERENCE                
         MVC   INVKOCD,TRNKOFF     SAVE OFFICE/CONTRA/DATE                      
         DROP  R1                                                               
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BZ    CRBLD08                                                          
         USING MPYELD,R1                                                        
         ICM   R1,15,AMPYEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   MPYLN,MPYLN2Q       TEST LONG ELEMENT                            
         BNL   *+6                                                              
         DC    H'0'                MUST CARRY DR SUB REFERENCE ON CR            
         MVC   INVSUBDR,MPYSUB     SAVE IN INVOICE TABLE                        
CRBLD08  OI    INVIND2,INVCR                                                    
         XC    INVERPD,INVERPD                                                  
         USING GDAELD,R1           EXTRACT GDAEL VALUES                         
         ICM   R1,15,AGDAERPD      R1=A(EARLIEST PAYMENT DATE ELEMENT)          
         BZ    *+10                                                             
         MVC   INVERPD,GDADATE                                                  
CRBUILDX B     OVROUTE                                                          
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
***********************************************************************         
* DRBUILD - CREATE AN INVOICE TSAR 2 RECORD FROM PAYMENT (DEBIT)      *         
* NTRY - P1 A(INVOICE TSAR 2 RECORD), P2 A(TRNEL)                     *         
***********************************************************************         
                                                                                
DRBUILD  L     R3,0(R1)                                                         
         USING INVRECD,R3                                                       
         L     R2,4(R1)                                                         
         USING TRNELD,R2                                                        
*&&UK                                                                           
         TM    INVIND2,INVDR       TEST PREVIOUS DR                             
         BZ    *+10                                                             
         SP    INVEXCH,INVAMNT     REMOVE FROM THE EXCHANGE BALANCE             
         AP    INVEXCH,TRNAMNT                                                  
         ZAP   INVAMNTC,PZERO                                                   
         ICM   R1,15,AAFCEL                                                     
         BZ    *+10                                                             
         ZAP   INVAMNTC,AFCAMNT-AFCELD(L'AFCAMNT,R1)                            
*&&                                                                             
         ZAP   INVAMNT,TRNAMNT                                                  
         OI    INVIND2,INVDR                                                    
DRBUILDX B     OVROUTE                                                          
         EJECT                                                                  
***********************************************************************         
* DCMATCH - WHEN BOTH CR & DR (INVOICE AND PAYMENT) ARE FOUND         *         
* NTRY - P1 A(INVOICE TSAR 2 RECORD), P2 A(TRNEL)                     *         
***********************************************************************         
                                                                                
DCMATCH  L     R3,0(R1)                                                         
         USING INVRECD,R3                                                       
         L     R2,4(R1)                                                         
         USING TRNELD,R2                                                        
         TM    INVIND2,INVCR+INVDR TEST CR & DR FOUND                           
         BNO   DCMATCHX                                                         
         CLI   OVFLAG,OVFRCK       TEST JUST READ FOR FIT                       
         BE    DCMAT06                                                          
         SP    TOTDIFF,INVAMNT                                                  
*&&UK                                                                           
         OC    INVDSC,INVDSC                                                    
         BZ    *+10                                                             
         AP    TOTDIFF,INVDSC      YES - ADJUST DIFFERENCE NOW                  
                                                                                
         SP    CURDIFF,INVAMNTC                                                 
         OC    INVDSCC,INVDSCC     TEST ANY LIVE DISCOUNT                       
         BZ    *+10                                                             
         AP    CURDIFF,INVDSCC     YES - ADJUST DIFFERENCE NOW                  
*&&                                                                             
         CLI   OVFLAG,OVFRDA       TEST READ AND ADD                            
         BNE   DCMAT08                                                          
                                                                                
         XC    TSARKEY,TSARKEY     CR & DR MATCHED - ADD TSAR RECORD            
         MVI   TSIKEY1,FF          ENSURE HIGH KEY                              
         MVI   TSIKEY2,FF                                                       
         MVC   TSIKOCD,INVKOCD                                                  
         MVC   TSIKREF,INVKREF                                                  
         MVC   TSIKSBRF,INVSUB                                                  
         MVC   WORK(L'TSARKEY),TSARKEY                                          
         USING TSARD,R1                                                         
         L     R1,ATSARBLK         R1=A(TSAR CONTROL BLOCK)                     
         MVI   TSACTN,TSARDH                                                    
         MVI   TSERRS,0            CLEAR ERROR RETURN BYTE                      
         L     RF,VTSAR                                                         
         BASR  RE,RF               READ FOR RECORD (MULTIPLE VOID DRS)          
         LA    RE,TSAPUT           SET TO OVERWRITE RECORD                      
         CLI   TSERRS,0            TEST RECORD EXISTS                           
         BE    DCMAT02                                                          
         ICM   RE,3,INVRNUM        NO -                                         
         LA    RE,1(RE)                                                         
         STCM  RE,3,INVRNUM        BUMP SEQUENTIAL RECORD NUMBER                
         MVC   TSARKEY,WORK        RESTORE SAVED KEY                            
         LA    RE,TSAADD           AND SET TO ADD                               
DCMAT02  ST    RE,OVFULL                                                        
         USING TRNRECD,R1                                                       
         L     R1,AIOBUFF          R1=A(TRANSACTION RECORD KEY)                 
         TM    COMPSTAT,CPYSOROE   TEST OFFICES IN USE                          
         BZ    *+10                                                             
         MVC   TSAROFF,TRNOFFC     SET OFFICE IN RECORD BODY                    
         MVC   TSARCON,TRNKCULC                                                 
         MVC   TSARDAT,TRNKDATE                                                 
         MVC   TSARREF,TRNKREF                                                  
         MVC   TSARSBR,TRNKSBR                                                  
         MVC   TSARMOS,TRNRSMOS                                                 
         MVI   TSARRSTA,0                                                       
         TM    TRNRSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         OI    TSARRSTA,TRNSARCH   SET RECORD IS ON ARCHIVE                     
         MVC   TSARBAT,TRNBTCH                                                  
         MVC   TSARBTY,TRNTYPE                                                  
         MVC   TSARSTA,TRNSTAT                                                  
         MVC   TSARINDS,STSARIND   TAKE SAVED MARKED BIT(S)                     
         OI    TSARINDS,TSARDRQ+TSARDISQ  SET DISPLAY ONLY DEBIT                
         MVI   TSARCHA,C'*'        SET DISPLAY ONLY CHARACTER                   
         XC    TSARVAR,TSARVAR     CLEAR VARIABLE KEY BYTE                      
         ZAP   TSARAMNT,TRNAMNT                                                 
         ZAP   TSARFDIS,PZERO      CLEAR LIVE DISCOUNT                          
         MVC   TSARFSAC,SPACES     CLEAR SOURCE A/C                             
         MVC   TSARFWRK,SPACES     CLEAR SOURCE WORKCODE                        
         MVC   TSARFSAC,TRNKUNT    SET ARTIST U/L/ACC                           
         DROP  R1,R2                                                            
                                                                                
         OC    INVDSC,INVDSC       TEST ANY LIVE DISCOUNT                       
         BZ    *+10                                                             
         ZAP   TSARFDIS,INVDSC     SET AMOUNT OF DISCOUNT                       
         CLC   INVSRC,SPACES                                                    
         BNH   *+10                                                             
         MVC   TSARFSAC,INVSRC     SET SOURCE A/C                               
*&&UK                                                                           
         OC    INVDSCC,INVDSCC     TEST ANY LIVE DISCOUNT                       
         BZ    *+10                                                             
         ZAP   TSARFDIC,INVDSCC    SET AMOUNT OF DISCOUNT                       
         OC    INVSWRK,INVSWRK                                                  
         BZ    *+10                                                             
         MVC   TSARFWRK,INVSWRK    SET SOURCE WORKCODE                          
*&&                                                                             
*&&US                                                                           
         OC    INVSIND,INVSIND                                                  
         BZ    *+10                                                             
         MVC   TSARIND2,INVSIND    SET INDICATOR BYTE 2                         
         MVC   TSARFINV,SPACES                                                  
         OC    INVMIV,INVMIV       TEST MEDIA INVOICE# PRESENT                  
         BZ    *+10                                                             
         MVC   TSARFINV,INVMIV     EXTRACT MEDIA/LONG INVOICE NUMBER            
*&&                                                                             
*&&UK*&& MVC   TSARERPD,INVERPD    EXTRACT EARLIEST PAYMENT DATE (OR 0)         
                                                                                
         OC    ANOTELS,ANOTELS     TEST MEMO ITEMS ATTACHED                     
         BZ    *+8                                                              
         OI    TSARIND2,TSARMEMO                                                
                                                                                
         USING TRSELD,R2           EXTRACT TRSEL VALUES                         
         ICM   R2,15,ATRSEL        R2=A(TRANSACTION STATUS ELEMENT)             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   TSARADAT,TRSDATE                                                 
         MVC   TSARSSTA,TRSSTAT                                                 
         MVC   TSARSST2,TRSSTAT2                                                
         MVC   TSARSST3,TRSSTAT3                                                
         CLI   TRSLN,TRSLNQ        TEST ELEMENT CARRIES USED DATE               
         BL    *+10                                                             
         MVC   TSARUSDT,TRSUDAT    EXTRACT USED DATE (OR 0)                     
         DROP  R2                                                               
                                                                                
*&&UK*&& GOTO1 AVAL1FC                                                          
                                                                                
         USING OTHELD,R2           EXTRACT OTHEL VALUES                         
         ICM   R2,15,AOTHEL        R2=A(OTHER NUMBER ELEMENT)                   
         BZ    *+10                                                             
         MVC   TSARFOTH,OTHNUM                                                  
         DROP  R2                                                               
                                                                                
         L     R1,AIOSAVE          R1=A(SAVED DIRECTORY VALUES)                 
         MVC   TSARDADR,0(R1)      EXTRACT DATA RECORD DISK ADDRESS             
         USING TSARD,R1                                                         
         L     R1,ATSARBLK         R1=A(TSAR CONTROL BLOCK)                     
         L     RE,OVFULL           RE=TSAR ACTION                               
         STC   RE,TSACTN           SET PUT/ADD                                  
         MVI   TSERRS,0            CLEAR ERROR RETURN BYTE                      
         L     RF,VTSAR                                                         
         BASR  RE,RF               PUT/ADD RECORD TO TSAR                       
         BE    DCMAT04                                                          
         TM    TSERRS,TSEDUP       TSAR ERROR - TEST DUPLICATE RECORD           
         BZ    *+6                                                              
         DC    H'0'                CANNOT BE - I BUILT THE KEY                  
         TM    TSERRS,TSEEOF       TEST BUFFER FULL                             
         BO    *+6                 LET USER SEE                                 
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R1                                                               
                                                                                
DCMAT04  XC    TSARREC+L'TSARLEN(TSARRECL-L'TSARLEN),TSARREC+L'TSARLEN          
                                                                                
         AP    TOTINVS,INVAMNT     TOTAL DEBITS                                 
         OC    INVDSC,INVDSC       TEST ANY LIVE DISCOUNT                       
         BZ    *+10                                                             
         AP    TOTDISC,INVDSC      DISCOUNT                                     
*&&UK                                                                           
         SP    TOTINVS,INVEXCH                                                  
         AP    TOTEXDF,INVEXCH                                                  
         ZAP   CUREXDF,TOTEXDF                                                  
         AP    CURINVS,INVAMNTC    TOTAL DEBITS                                 
         OC    INVDSCC,INVDSCC     TEST ANY LIVE DISCOUNT                       
         BZ    *+10                                                             
         AP    CURDISC,INVDSCC     DISCOUNT                                     
*&&                                                                             
         B     DCMAT08             CLEAR INVREC AND RETURN                      
                                                                                
DCMAT06  ICM   R1,3,INVRNUM        TAKE INVOICE COUNT SO FAR                    
         LA    R1,1(R1)                                                         
         STCM  R1,3,INVRNUM        SAVE NEW COUNT                               
         CLC   INVRNUM,INVMAX      TEST WE HAVE ROOM                            
         BH    DCMATCHH            LEAVE CC NEQ AND EXIT                        
                                                                                
DCMAT08  GOTO1 ATS2DEL             DELETE CURRENT TSAR 2 RECORD                 
         CLI   OVFLAG,OVFRDA       TEST READ AND ADD                            
         BNE   DCMATCHX                                                         
         USING TSARD,R1                                                         
         L     R1,ATSARBLK         R1=A(TSAR CONTROL BLOCK)                     
         TM    TSERRS,TSEEOF       TEST BUFFER FULL WAS ENCOUNTERED             
         BO    DCMATCHH                                                         
                                                                                
DCMATCHX B     OVROUTE                                                          
DCMATCHH B     OVROUTH                                                          
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO INSERT SPECIAL ACCOUNT DETAILS IF A/C CODE PRESENT       *         
* NTRY - R1= A(FIRST TRANSACTION ELEMENT)                             *         
***********************************************************************         
                                                                                
                                                                                
DETACC   LA    R3,PSTTAB           FOR EACH POSTING TABLE ENTRY                 
         USING PSTTABD,R3                                                       
DETACC02 OC    PSTACC,PSTACC       TEST POSTING ACCOUNT IN A TABLE              
         BZ    DETACC04            YES - IGNORE IT                              
         MVC   LARFADDR,PSTACC     EXTRACT ACCOUNT NAME                         
         EX    0,LARF                                                           
         USING ACCXD,RF                                                         
         XC    KEY(LDGKEND),KEY                                                 
         OC    KEY(LDGKEND),ACCX                                                
         BZ    DETACC04                                                         
         DROP  RF                                                               
                                                                                
         GOTO1 AGETLDG,GLNOSECY    GET ACCOUNT LEDGER                           
         BE    *+6                                                              
         DC    H'0'                DON'T ALLOW ANY ERROR                        
         USING LEDGTABD,R1                                                      
         ICM   R1,15,RECALDGT      GET LEDGER ENTRY                             
         BNZ   *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING ACCXD,RF                                                         
         MVC   LARFADDR,PSTACC                                                  
         EX    0,LARF              (LARFADDR DESTROYED BY AGETLDG CALL)         
         CLI   LEDGTOFF,LDGOKEY    TEST OFFICE WITHIN KEY                       
         BH    DETACC04                                                         
         MVC   ACCXOPIK,LEDGTOFF   SAVE OFFICE POSITION IN KEY                  
         SR    RE,RE                                                            
         ICM   RE,1,ACCXOPIK                                                    
         BZ    DETACC04                                                         
         LA    RE,ACCX+(ACTKACT-ACTRECD-1)(RE)                                  
         MVI   0(RE),C' '                                                       
         DROP  RF,R1                                                            
                                                                                
DETACC04 LA    R3,PSTTABL(R3)      NEXT POSTING TABLE ENTRY                     
         CLI   PSTIND1,EOT                                                      
         BNE   DETACC02                                                         
                                                                                
DETACCX  B     OVROUTX                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT SPECIAL ACCOUNT NAMES                            *         
* NTRY - R1= A(FIRST TRANSACTION ELEMENT)                             *         
***********************************************************************         
                                                                                
EXTACC   XR    R0,R0                                                            
         L     R1,0(R1)                                                         
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING SPAELD,R1                                                        
EXTACC02 CLI   SPAEL,SPAELQ        FOR EACH SPAEL                               
         BNE   EXTACC20                                                         
                                                                                
         LA    R3,PSTTAB           FIND POSTING TABLE SPA TYPE                  
         USING PSTTABD,R3                                                       
EXTACC04 CLI   PSTIND1,EOT                                                      
         BE    EXTACC20                                                         
         OC    PSTACC,PSTACC       TEST POSTING ACCOUNT IN A TABLE              
         BZ    *+14                YES - IGNORE IT                              
         CLC   PSTSPAT,SPATYPE                                                  
         BE    EXTACC06                                                         
         LA    R3,PSTTABL(R3)                                                   
         B     EXTACC04                                                         
                                                                                
EXTACC06 MVC   LARFADDR,PSTACC     EXTRACT SPA DETAILS                          
         EX    0,LARF                                                           
         MVC   ACTKCPY-ACTKEY(L'COMPANY,RF),COMPANY                             
         MVC   (ACTKCPY-ACTKEY)+L'COMPANY(L'SPAAULA,RF),SPAAULA                 
         DROP  R3                                                               
                                                                                
EXTACC20 IC    R0,SPALN                                                         
         AR    R1,R0                                                            
         CLI   SPAEL,0             TEST EOR                                     
         BNE   EXTACC02                                                         
         DROP  R1                                                               
         MVC   BANK,ACCOUNT                                                     
EXTACCX  B     OVROUTX                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT POSTING ACCOUNTS FROM LEDGER RECORD              *         
* NTRY - R1 A(LEDGER RECORD)                                          *         
***********************************************************************         
                                                                                
         USING LDGRECD,R1                                                       
EXTLDG   L     R1,0(R1)            LEDGER RECORD                                
         USING PSTTABD,R3                                                       
         LA    R3,PSTTAB                                                        
         XR    R0,R0                                                            
                                                                                
EXTLDG02 OC    PSTACC,PSTACC       TEST POSTING ACCOUNT IN A TABLE              
         BZ    EXTLDG20            YES - IGNORE IT                              
         MVC   LARFADDR,PSTACC     TEST POSTING TO THIS ACCOUNT                 
         EX    0,LARF                                                           
         USING ACCXD,RF                                                         
         XC    ACCX,ACCX                                                        
         IC    R4,PSTIND1                                                       
         EX    R4,*+8                                                           
         BZ    EXTLDG20                                                         
         TM    UPDIND1,0                                                        
                                                                                
EXTLDG04 LA    R2,LDGRFST-LDGRECD(R1)                                           
                                                                                
         USING LDGELD,R2                                                        
EXTLDG06 CLI   LDGEL,LDGELQ        TEST POSTING ACCOUNT AVAILABLE               
         BNE   EXTLDG08            FROM LEDGER ELEMENT                          
         TM    PSTLE,PSTLELDG                                                   
         BZ    EXTLDG18                                                         
         CLI   LDGLN,LDGLNQ                                                     
         BL    EXTLDG18                                                         
         CLC   LDGCDSC,SPACES                                                   
         BNH   EXTLDG18                                                         
         MVC   ACCX,LDGCDSC                                                     
         B     EXTLDG18                                                         
                                                                                
         USING OCNELD,R2                                                        
EXTLDG08 CLI   OCNEL,OCNELQ        TEST POSTING ACCOUNT AVAILABLE               
         BNE   EXTLDG10            FROM OFFICE CHEQUE # ELEMENT                 
         TM    PSTLE,PSTLEOCN                                                   
         BZ    EXTLDG18                                                         
         CLI   OCNLN,OCNLN2Q                                                    
         BL    EXTLDG18                                                         
         CLC   OCNOFFID,TWAUSRID                                                
         BNE   EXTLDG18                                                         
         MVC   ACCX(L'COMPANY),COMPANY                                          
         MVC   ACCX+L'COMPANY(L'OCNDISC),OCNDISC                                
         B     EXTLDG18                                                         
                                                                                
         USING APTELD,R2                                                        
EXTLDG10 CLI   APTEL,APTELQ        TEST POSTING ACCOUNT AVAILABLE               
         BNE   EXTLDG12            FROM ACCOUNT POINTER ELEMENT                 
         TM    PSTLE,PSTLEAPT                                                   
         BZ    EXTLDG18                                                         
         TM    APTSTAT,APTSTAXL                                                 
         BZ    EXTLDG18                                                         
         MVC   ACCX,APTACC                                                      
         B     EXTLDG18                                                         
                                                                                
EXTLDG12 DS    0H                                                               
                                                                                
         USING LDGEL,R2                                                         
EXTLDG18 IC    R0,LDGLN            NEXT ELEMENT                                 
         AR    R2,R0                                                            
         CLI   LDGEL,0                                                          
         BNE   EXTLDG06                                                         
         DROP  R2                                                               
                                                                                
EXTLDG20 LA    R3,PSTTABL(R3)      REPEAT FOR EACH POSTING ACCOUNT              
         CLI   PSTIND1,EOT                                                      
         BNE   EXTLDG02                                                         
         DROP  R1,R3                                                            
EXTLDGX  B     OVROUTX                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT CONTRA ACCOUNT NAME                              *         
***********************************************************************         
                                                                                
GETCON   MVC   CONTRAN,SPACES      CLEAR CONTRA NAME                            
         MVI   CONTRAN,C'?'                                                     
         USING CHDRECD,R1                                                       
         L     R1,0(R1)                                                         
         XR    R0,R0                                                            
         XR    RF,RF                                                            
         LA    R1,CHDRFST          POINT TO FIRST ELEMENT                       
                                                                                
         USING CACELD,R1                                                        
         CLI   CACEL,CACELQ        FIRST TEST NAME AVAILABLE FROM               
         BNE   GETCON02            CONTRA ACCOUNT NAME ELEMENT                  
         CLI   CACLN,CACLN1Q                                                    
         BNH   GETCONX                                                          
         IC    RF,CACLN                                                         
         SH    RF,=Y(CACLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRAN(0),CACNAME                                               
         B     GETCONX                                                          
                                                                                
         USING NAMELD,R1                                                        
GETCON02 CLI   NAMEL,0             TEST NAME AVAILABLE FROM                     
         BE    GETCONX             NAME ELEMENT                                 
         CLI   NAMEL,NAMELQ                                                     
         BE    GETCON04                                                         
         IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         B     GETCON02                                                         
                                                                                
GETCON04 IC    RF,NAMLN                                                         
         SH    RF,=Y(NAMEREC+1-NAMELD)                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CONTRAN(0),NAMEREC                                               
         DROP  R1                                                               
                                                                                
GETCONX  B     OVROUTX                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD VOID AND UNVOID TEMPLATE MESSAGES                  *         
***********************************************************************         
                                                                                
GOTTXT   CLC   DATETEMP,ASTERIS    TEST MESSAGES ALREADY LOADED                 
         BE    GOTTXTX                                                          
         MVC   DATETEMP,ASTERIS                                                 
         MVI   WORK,9                                                           
         MVC   WORK+1(8),DATETEMP                                               
         MVI   WORK+9,0                                                         
                                                                                
         LA    R2,MSGTAB                                                        
         USING MSGTABD,R2                                                       
GOTTXT02 CLC   MSGTNUM,=AL2(EOT)   FOR EACH MESSAGE TABLE ENTRY                 
         BE    GOTTXTX                                                          
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK     GET THE TEXT                                 
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GTMAXL,L'VOIDNARR   SET MAXIMUM LENGTH                           
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         LA    R0,WORK                                                          
         STCM  R0,7,GTASUBST                                                    
         LA    R3,OV$MSGS          PLACE TEXT IN OV$MSGS TABLE                  
         AH    R3,MSGTOFST                                                      
         STCM  R3,7,GTAOUT                                                      
         MVC   GTMSGNO,MSGTNUM                                                  
         GOTO1 VGETTXT,(R1)                                                     
         MVC   L'VOIDNARR(1,R3),GTMAXL                                          
         LA    R2,MSGTABL(R2)      NEXT TABLE ENTRY                             
         B     GOTTXT02                                                         
                                                                                
GOTTXTX  B     OVROUTE                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* POST A -DR/DR TO VOID/UNVOID A VENDOR PAYMENT, OR                   *         
* POST A -CR/CR TO VOID/UNVOID A CHEQUE (OR DISCOUNT)                 *         
* POST A CR/-CR/-DR/DR TO VOID/UNVOID AN ADVANCE OR DIFFERENCE        *         
* ENTRY - AIOBUFF=A(ORIGINAL DEBIT OR ORIGINAL CREDIT)                *         
***********************************************************************         
                                                                                
POST     LR    R4,R1               SAVE CALLING PARAMETER                       
         CLM   R4,1,=AL1(POSTIBLT) TEST POSTING BUILT BY CALLER                 
         BE    POST42                                                           
         XC    TEMP,TEMP                                                        
         GOTO1 ASETELAD,AIOBUFF    SET ELEMENT ADDRESSES                        
         USING TRNELD,R2                                                        
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         NI    TRNSTAT,FF-TRNSREV  CLEAR REVERSAL BIT IF ON                     
         MVC   TRNMOS,SBATMON      SET BATCH YEAR DIGIT/MONTH                   
         MVC   TRNBREF,SBATREF     SET BATCH REFERENCE                          
*        L     R1,AIOBUFF                                                       
*        CLC   BANKUL,TRNKUNT-TRNRECD(R1)  TEST BANK ACCOUNT POSTING            
*        BNE   POST02                                                           
*        CLI   PROFNREC,C'Y'       TEST NOT AUTO-RECONCILING                    
*        BE    POST02                                                           
*        OI    TRNSTAT,TRNSBREC    SET BANK RECONCILED                          
*        TM    VOIDIND1,VOIDVOID   TEST VOID/UNVOID                             
*        BO    POST04                                                           
*        NI    TRNSTAT,FF-TRNSBREC CLEAR BANK RECONCILED                        
*        B     POST10                                                           
POST02   TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BZ    POST10                                                           
POST04   MVI   TRNTYPE,BT37        SET VOID CHEQUE                              
*&&UK                                                                           
         XC    OVELEM,OVELEM       CLEAR SAVED MPYEL                            
         XC    TEMP,TEMP           CLEAR SAVED OCA VALUES                       
         ICM   RF,15,AMPYEL        TEST PAYMENT ELEMENT                         
         BZ    POST06                                                           
         USING MPYELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,MPYLN                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OVELEM(0),MPYELD    SAVE MPYEL IN OVELEM                         
         GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),AIOBUFF,ACOM,0,MPYELD,0             
         DROP  RF                                                               
         ICM   RF,15,DMCB+(TOBALIST-TOBAACTN)                                   
         BZ    POST06                                                           
         MVC   TEMP,0(RF)          SAVE DELETED OCA VALUES                      
POST06   GOTO1 VTOBACCO,DMCB,('TOBAAREV',0),AIOBUFF,ACOM,0,0,0                  
         OC    OVELEM,OVELEM       TEST SAVED MPYEL TO RESTORE                  
         BZ    POST08                                                           
         MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAAADD',WORK),AIOBUFF,ACOM,TEMP,      X        
               OVELEM,0                                                         
POST08   DS    0H                                                               
*&&                                                                             
*&&US                                                                           
         ZAP   DUB,TRNAMNT                                                      
         MP    DUB,PONENEG         AND REVERSE SIGN                             
         ZAP   TRNAMNT,DUB                                                      
         ICM   R1,15,AAFCEL                                                     
         BZ    POST10                                                           
         USING AFCELD,R1                                                        
         ZAP   DUB,AFCAMNT                                                      
         MP    DUB,PONENEG                                                      
         ZAP   AFCAMNT,DUB                                                      
         DROP  R1                                                               
*&&                                                                             
POST10   TM    TRNSTAT,TRNSDR      TEST A DEBIT                                 
         BZ    *+10                                                             
         AP    TOTCASH,TRNAMNT     ACCUMULATE BATCH CASH                        
         AP    TOTITEM,PONE        ALWAYS ACCUMULATE ITEM COUNT                 
         TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BZ    POST24                                                           
*&&US                                                                           
         USING SCIELD,R1                                                        
         LA    RF,ASCIELS          A(SCI ELEMENTS)                              
         LA    R0,ASCIELN                                                       
                                                                                
POST12   ICM   R1,15,0(RF)         TEST/SET A(SUBSIDIARY CASH ELEMENT)          
         BZ    POST14                                                           
         ZAP   DUB,SCIAMNT         REVERSE SUBSIDIARY CASH AMOUNT               
         MP    DUB,PONENEG                                                      
         ZAP   SCIAMNT,DUB                                                      
                                                                                
         CLI   SCITYPE,SCITBENE    TEST SECONDARY AMOUNT                        
         BE    *+12                                                             
         CLI   SCITYPE,SCITGRNT                                                 
         BNE   *+22                                                             
         ZAP   DUB,SCIADMN         REVERSE SECONDARY AMOUNT                     
         MP    DUB,PONENEG                                                      
         ZAP   SCIADMN,DUB                                                      
                                                                                
         LA    RF,L'ASCIELS(RF)                                                 
         BCT   R0,POST12                                                        
                                                                                
         USING TRPELD,R1                                                        
POST14   ICM   R1,15,ATRPEL1       R1=A(FIRST TRPEL)                            
         BZ    POST20                                                           
POST16   CLI   TRPEL,0             TEST EOR                                     
         BE    POST20                                                           
         CLI   TRPEL,TRPELQ        TEST TRPEL                                   
         BNE   POST18                                                           
         ZAP   DUB,TRPAMNT         REVERSE TRANSACTION POINTER AMOUNT           
         MP    DUB,PONENEG                                                      
         ZAP   TRPAMNT,DUB                                                      
POST18   SR    R0,R0               TRY NEXT ELEMENT                             
         IC    R0,TRPLN                                                         
         AR    R1,R0                                                            
         B     POST16                                                           
         DROP  R1                                                               
                                                                                
POST20   DS    0H                  VOID - NEXT ELEMENT TO REVERSE               
         B     POST24                                                           
*&&                                                                             
                                                                                
POST24   CLM   R4,1,=AL1(POSTINEW) TEST NEW POSTING                             
         BNE   *+12                                                             
         LA    RF,TRNNARR          ADD NARRATIVE DIRECTLY TO TRNEL              
         B     POST28                                                           
         IC    R1,TRNLN            CURRENT L'TRANSACTION                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP(0),TRNEL       SAVE EXISTING TRANSACTION                    
         GOTO1 VHELLO,DMCB,(C'D',OVACCMST),('TRNELQ',AIOBUFF),0                 
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,TEMP             BUILD A NEW ELEMENT                          
         MVI   TRNEL,X'01'         ENSURE IT IS THE FIRST ELEMENT               
         XR    R1,R1                                                            
         IC    R1,TRNLN                                                         
         XR    R0,R0                                                            
         IC    R0,VOIDNARL         EXECUTE L'NARRATIVE                          
         AR    R0,R1                                                            
         CH    R0,=Y(TRNLN1Q+L'TRNNARR-2)                                       
         BH    POST26              ADD-ON NARRATIVE WON'T FIT                   
         LA    RF,TRNEL(R1)        RF=A(NEXT BYTE OF NARRATIVE)                 
         SH    R1,=Y(TRNLN1Q)      R1=L'NARRATIVE OR ZERO                       
         BZ    POST28              NO NARRATIVE                                 
         BP    *+6                                                              
         DC    H'0'                BAD TRNEL                                    
         BCTR  RF,0                FIND LAST SIGNIFICANT CHARACTER              
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         BCT   R1,*-10                                                          
         B     POST28                                                           
         MVI   1(RF),C' '          ADD A SPACE                                  
         LA    RF,2(RF)                                                         
         B     POST28              ADD NEW NARRATIVE                            
                                                                                
POST26   SH    R1,=Y(TRNLN1Q+2)                                                 
         MVI   TRNNARR,C' '                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR+1(0),TRNNARR  CLEAR EXISITING NARRATIVE                  
         LA    RF,TRNNARR          AND ADDRESS FIRST BYTE                       
                                                                                
POST28   XR    R1,R1               BUILDING IN TEMP OR IOBUFF                   
         IC    R1,VOIDNARL                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),VOIDNARR                                                 
         LA    RF,1(RF,R1)                                                      
         SR    RF,R2                                                            
         STC   RF,TRNLN                                                         
         CLM   R4,1,=AL1(POSTINEW) TEST NEW TRANSACTION                         
         BNE   POST34                                                           
         AR    RF,R2               BUILD A TRSEL IN IOBUFF                      
         USING TRSELD,RF                                                        
         XC    TRSELD(TRSLNQ),TRSELD                                            
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,TODAYC                                                   
         MVI   TRSMARK,TRSMSBVQ    SET SUBSIDIARY TYPE/ACTION                   
         MVI   TRSSTAT3,TRSSMRK2   SET CREATED BY MARKER BANK/VOID              
         CLI   TRNTYPE,BT57        TEST TRANSFER                                
         BE    *+12                                                             
         CLI   TRNTYPE,BT60        TEST PAYMENT                                 
         BNE   POST30                                                           
         OI    TRSSTAT2,TRSSPMNT   SET PAYMENT                                  
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    POST30                                                           
         OI    TRSSTAT2,TRSSANBP   SET ANALYSED BANK POSTINGS                   
POST30   TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BO    *+12                                                             
         OI    TRSMARK,TRSMUMQ     UNVOID - SET TYPE/ACTION IS NEGATIVE         
         B     POST32                                                           
         OI    TRSSTAT,TRSSVOID    SET VOID                                     
         OC    DATPMOS,DATPMOS                                                  
         BZ    POST32                                                           
         MVC   TRSRMOS,DATPMOS     SET REVERSING TRANSACTION MOS                
         OI    TRSSTAT,TRSSREVS    SET 'REVERSE ME'                             
                                                                                
POST32   MVI   TRSEL+TRSLNQ,0      SET EOR                                      
         DROP  RF                                                               
         LA    RF,TRSLNQ+1(RF)     ESTABLISH RECORD LENGTH                      
         L     R1,AIOBUFF                                                       
         SR    RF,R1                                                            
         STCM  RF,3,TRNRLEN-TRNRECD(R1)                                         
         B     POST42              GO STRAIGHT TO EMU                           
                                                                                
POST34   GOTO1 VHELLO,DMCB,(C'P',OVACCMST),AIOBUFF,(R2)                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,16(R1)           R2=A(TRNEL AFTER ADD)                        
         MVI   TRNEL,TRNELQ        NOW SET TRUE ELEMENT CODE                    
         LR    R1,R2               SAVE A(TRNEL)                                
         XR    R0,R0                                                            
POST36   IC    R0,TRNLN                                                         
         AR    R2,R0                                                            
         CLI   TRNEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   TRNEL,TRSELQ        TEST TRANSACTION STATUS ELEMENT              
         BNE   POST36                                                           
         USING TRSELD,R2                                                        
         CLI   TRSLN,TRSLNQ        TEST LONG ELEMENT                            
         BNL   POST38                                                           
         LR    R0,R1                                                            
         GOTO1 AEXTRSL             NO - EXTEND IT                               
         GOTO1 ASETELAD,AIOBUFF    RESET ELEMENT ADDRESSES                      
         LR    R1,R0                                                            
         ICM   R2,15,ATRSEL                                                     
POST38   MVC   DATPMOS,TRSPMOS     SAVE TRANSACTION MOS                         
         MVC   DATUDAT,TRSUDAT     AND USED DATE                                
         XC    TRSDATE(TRSLNQ-2),TRSDATE  CLEAR THE ELEMENT                     
         MVI   TRSMARK,TRSMSBVQ    SET SUBSIDIARY TYPE/ACTION                   
         MVI   TRSSTAT3,TRSSMRK2   SET CREATED BY MARKER BANK/VOID              
         CLI   TRNTYPE-TRNEL(R1),BT57  TEST TRANSFER                            
         BE    *+12                                                             
         CLI   TRNTYPE-TRNEL(R1),BT60  TEST PAYMENT                             
         BNE   POST40                                                           
         OI    TRSSTAT2,TRSSPMNT   SET PAYMENT                                  
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    POST40                                                           
         OI    TRSSTAT2,TRSSANBP   SET ANALYSED BANK POSTINGS                   
POST40   MVC   TRSUDAT,DATUDAT     PUT BACK USED DATE                           
         TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BO    *+12                                                             
         OI    TRSMARK,TRSMUMQ     UNVOID - SET TYPE/ACTION IS NEGATIVE         
         B     *+14                                                             
         OI    TRSSTAT,TRSSVOID+TRSSREVS  SET VOID & 'REVERSE ME'               
         MVC   TRSRMOS,DATPMOS     SET REVERSING TRANSACTION MOS                
         B     POST42                                                           
                                                                                
POST42   CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    POST44                                                           
         USING TRNBLK,R3                                                        
         LA    R3,TRNBLOCK                                                      
         MVC   TRNREC,AIOBUFF      A(TRANSACTION RECORD)                        
         ICM   RE,3,TRNBSEQN                                                    
         LA    RE,1(RE)                                                         
         STCM  RE,3,TRNBSEQN       UPDATE NUMBER OF RECORDS ADDED               
         MVC   TRNCACNM,CONTRAN    SET CONTRA A/C NAME                          
         OI    TRNINDS,TRNICONV    SET CONVERTED RECORD                         
         OI    TRNINDS2,TRNIADDG   ADD GL POSTING                               
         GOTO1 VADDTRN,TRNBLK      ADD TRANSACTION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    TRNINDS,FF-TRNICONV RESET CONVERTED RECORD FLAG                  
         OI    UPDIND2,UPDIADDQ    SET RECORD(S) ADDED VIA ADDTRN               
                                                                                
POST44   GOTO1 AREPTRN,AIOBUFF     REPORT TRANSACTION POSTING                   
         GOTO1 ASETELAD,AIOBUFF    REFRESH ELEMENT ADDRESSES                    
         XR    RE,RE                                                            
         ICM   RE,3,NUMTRNS                                                     
         LA    RE,1(RE)                                                         
         STCM  RE,3,NUMTRNS                                                     
         CHI   RE,128              TEST MAXIMUM CHUNK SIZE                      
         BL    POST48                                                           
         XC    NUMTRNS,NUMTRNS     CALL ADDTRN WITH LAST TIME HOOK              
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    POST48                                                           
         MVI   TRNINDS,TRNILAST                                                 
         OI    TRNINDS2,TRNIUPDG                                                
         GOTO1 VADDTRN,TRNBLK                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TRNINDS,0                                                        
         CLI   FILEFORM,ISDAQ      CLEAR LOCKTAB                                
         BE    POST46                                                           
         GOTO1 VDATAMGR,DMCB,DMUNLK,OVACCFIL                                    
         B     POST48                                                           
                                                                                
POST46   GOTO1 VDATAMGR,DMCB,DMUNLK,OVACCDIR                                    
         GOTO1 VDATAMGR,DMCB,DMUNLK,OVACCMST                                    
                                                                                
POST48   DS    0H                                                               
                                                                                
POSTX    B     OVROUTE                                                          
                                                                                
POSTE    OI    TWAMODE3,TWAM3UWD                                                
         XC    FVMSGNO,FVMSGNO                                                  
         L     RF,AIOBUFF                                                       
         MVC   FVXTRA,SPACES                                                    
         MVC   FVXTRA(L'TRNKCULA),TRNKCULA-TRNRECD(RF)                          
         LA    RF,TRNBLOCK                                                      
         CLI   TRNERRS,TRNEACCI    TEST INVALID ACCOUNT                         
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         CLI   TRNERRS,TRNEACOL    TEST ACCOUNT CLOSED OR LOCKED                
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$LOCKD)                                           
         OC    FVMSGNO,FVMSGNO     TEST ACCEPTABLE ERROR                        
         BNZ   *+6                                                              
         DC    H'0'                ADDTRN ERROR                                 
         L     RD,SAVERD                                                        
         LM    RE,RC,12(RD)                                                     
         BR    RE                  RETURN TO THE ROOT                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* POST TO ADVANCE/MINOR DIFFERENCE TRANSACTIONS FROM TPTAB            *         
***********************************************************************         
                                                                                
         PUSH  USING                                                            
POSTPT   L     R4,ATPTAB           SET A(TRANSACTION POINTER TABLE)             
         USING TPTABD,R4                                                        
         MVC   POSTCACN,CONTRAN    SAVE CONTRA A/C NAME AS FOUND                
         B     *+8                                                              
POSTPT02 LA    R4,TPTABL(R4)       NEXT ENTRY IN TPTAB                          
         CLI   TPTABD,EOT          TEST EOT                                     
         BE    POSTPT60            NO FURTHER POINTER ENTRIES                   
         CP    TPAMNT,PZERO        TEST ANYTHING FOR THIS OFFICE                
         BE    POSTPT02                                                         
                                                                                
         ZAP   TPBAL,TPAMNT        SET ADVANCE/DIFFERENCE AMOUNT                
         USING TRNRECD,KEY                                                      
         MVC   TRNKEY,SPACES       BUILD TRANSACTION KEY                        
         MVC   TRNKCPY,COMPANY                                                  
         MVC   TRNKULA,TPULA       ACCOUNT FROM POINTER TABLE                   
         MVC   TRNKCULC,TSARCON    CONTRA A/C IS SUPPLIER                       
         MVC   CONTRAN,POSTCACN                                                 
         CLI   TPTYPE,TRPTADV      UNLESS ADVANCE                               
         BE    *+12                                                             
         CLI   TPTYPE,TRPTAXV      OR ADVANCE EXCHANGE DIFF TO VENDOR           
         BNE   POSTPT04                                                         
         MVC   TRNKCULC,ACCOUNT    IN WHICH CASE, CONTRA A/C IS BANK            
         MVC   CONTRAN,ACCNAME                                                  
POSTPT04 MVC   TRNKDATE,TSARDAT    ORIGINAL CHEQUE DATE                         
         MVC   TRNKREF,TSARREF     ORIGINAL CHEQUE REFERENCE                    
         XC    TRNKSBR(TRNRFST-TRNKSBR),TRNKSBR                                 
         LA    R1,IOHI+IOACCDIR+IO1Q                                            
         B     *+8                                                              
POSTPT06 LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         CLC   TRNKEY(TRNKSBR-TRNRECD),KEYSAVE                                  
         BE    POSTPT08                                                         
         CP    TPBAL,PZERO         TEST POINTER POSTINGS MATCHED                
         BE    POSTPT02                                                         
         DC    H'0'                DIDN'T FIND ADVANCE/DIFFERENCE               
POSTPT08 LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF                                                 
                                                                                
         USING TRXELD,R3                                                        
         ICM   R3,15,ATRXEL        NEED TRANSACTION EXTRA STATUS                
         BZ    POSTPT06                                                         
         CLI   TPTYPE,TRPTADV      ADVANCE POSTING                              
         BNE   POSTPT10                                                         
         TM    TRXSTA3,TRXSADVC    TEST ADVANCE                                 
         BZ    POSTPT06                                                         
         TM    TRXSTA1,TRXSXDIF    BUT NOT EXCHANGE DIFFERENCE                  
         BO    POSTPT06                                                         
         B     POSTPT22                                                         
POSTPT10 CLI   TPTYPE,TRPTAXV      ADVANCE EXCHANGE DIFF - VENDOR               
         BE    *+12                                                             
         CLI   TPTYPE,TRPTAXX      ADVANCE EXCHANGE DIFF - EX DIFF A/C          
         BNE   POSTPT12                                                         
         TM    TRXSTA3,TRXSADVC    TEST ADVANCE                                 
         BZ    POSTPT06                                                         
         TM    TRXSTA1,TRXSXDIF    TEST EXCHANGE DIFFERENCE POSTING             
         BO    POSTPT22                                                         
         B     POSTPT06                                                         
                                                                                
POSTPT12 CLI   TPTYPE,TRPTDIF      DIFFERENCE POSTING                           
         BE    POSTPT14                                                         
         CLI   TPTYPE,TRPTDTD      DIFFERENCE TAX ADJ - DIFFERENCE              
         BE    POSTPT14                                                         
         CLI   TPTYPE,TRPTDTT      DIFFERENCE TAX ADJ - TAX A/C                 
         BE    POSTPT14                                                         
         DC    H'0'                UNKNOWN TRPTYPE                              
POSTPT14 TM    TRXSTA3,TRXSDIFF    TEST DIFFERENCE                              
         BO    POSTPT22                                                         
         B     POSTPT06                                                         
         USING TRNELD,R2                                                        
POSTPT22 L     R2,ATRNEL                                                        
         CLI   TRNTYPE,BT36        TEST BT36                                    
         BNE   POSTPT06                                                         
         CLC   TPOFFC,SPACES       TEST BANK OFFICE KNOWN                       
         BNH   *+14                                                             
         CLC   TRNOFFC,TPOFFC      TEST OFFICE                                  
         BNE   POSTPT06                                                         
         USING TRSELD,R1                                                        
         L     R1,ATRSEL                                                        
         TM    VOIDIND1,VOIDVOID   TEST VOID/UNVOID                             
         BZ    POSTPT24                                                         
         TM    TRSSTAT,TRSSVOID    VOID - TEST NOT VOID                         
         BO    POSTPT06                                                         
         TM    TRNSTAT,TRNSREVS           TEST NOT REVERSED                     
         BO    POSTPT06                                                         
         B     POSTPT26                                                         
POSTPT24 TM    TRSSTAT,TRSSVOID    UNVOID - TEST VOID                           
         BZ    POSTPT06                                                         
         TM    TRNSTAT,TRNSREVS             TEST REVERSED                       
         BZ    POSTPT06                                                         
                                                                                
POSTPT26 TM    TRXSTA3,TRXSDIFF    TEST DIFFERENCE                              
         BZ    POSTPT28                                                         
         CP    TPBAL,TRNAMNT       TEST CORRECT POSTING                         
         BNE   POSTPT06                                                         
                                                                                
POSTPT28 SP    TPBAL,TRNAMNT       ADJUST POINTER BALANCE                       
         GOTO1 AREVVST             READ INTO IO2 & REVERSE VOID STATUS          
         L     R1,AIOBUFF                                                       
         MVC   STRNKEY,0(R1)       SAVE THIS KEY                                
         GOTO1 APOST,POSTISTD      POST NEW CR/REVERSAL -CR                     
         MVC   KEY,STRNKEY         RE-ESTABLISH SEQUENCE IN IOAREA 1            
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    POSTPT06            CONTINUE THROUGH ALL KEY SUBREFS             
         DC    H'0'                                                             
                                                                                
POSTPT60 MVC   CONTRAN,POSTCACN    RESTORE CONTRA A/C NAME                      
         B     OVROUTE                                                          
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* REVERSE CREDIT: RETURN WITH OVAMNT SET TRANSACTION AMOUNT                     
***********************************************************************         
                                                                                
REVCR    MVC   OVDRSBR,0(R1)                                                    
         LA    R1,IOREAD+IOACCDIR+IO2Q   READ CREDIT INTO IO2 AND LOCK          
         CLI   XACTION,ACTDRFT                                                  
         BE    *+8                                                              
         LA    R1,IORDUP+IOACCDIR+IO2Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,KEY                                                           
         MVC   SVDA,TRNKDA-TRNRECD(R1)                                          
         LA    R1,IOGET+IOACCMST+IO2Q  READ DATA RECORD                         
         CLI   XACTION,ACTDRFT                                                  
         BE    *+8                                                              
         LA    R1,IOGETRUP+IOACCMST+IO2Q  READ DATA RECORD                      
         TM    KEY+(TRNKSTAT-TRNRECD),TRNSARCH   TEST RECORD ON ARCHIVE         
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO2Q  READ DATA RECORD                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 ASETELAD,AIOBUFF2   DEBIT IN IO1, CREDIT IN IO2                  
                                                                                
         ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                TRANSACTION ELEMENT MISSING                  
         USING TRNELD,R2                                                        
         ZAP   OVAMNT,TRNAMNT                                                   
                                                                                
         TM    VOIDIND1,VOIDVOID   TEST VOIDING                                 
         BZ    *+8                                                              
         NI    TRNSTAT,FF-(TRNSAPPR+TRNSHOLD)                                   
         DROP  R2                                                               
         USING TRSELD,R2                                                        
         ICM   R2,15,ATRSEL        R2=A(TRSEL)                                  
         BNZ   *+6                                                              
         DC    H'0'                STATUS ELEMENT MISSING                       
         CLI   TRSLN,TRSLNQ                                                     
         BNL   REVCR10                                                          
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+6                                                              
         DC    H'0'                MUST BE LONG IF WE ARE UNVOIDING             
         GOTO1 AEXTRSL                                                          
         GOTO1 ASETELAD,AIOBUFF2                                                
         ICM   R2,15,ATRSEL                                                     
*                                  R2=A(TRSEL)                                  
REVCR10  MVI   TRSMARK,TRSMSBVQ    SET MARKER SUBSIDIARY TYPE/ACTION            
         NI    TRSSTAT,FF-TRSSACHQ   CLEAR SYSTEM CHEQUE                        
         NI    TRSSTAT2,FF-TRSSMCHQ  CLEAR MANUAL CHEQUE                        
         OI    TRSSTAT,TRSSVOID    SET VOID                                     
         XC    TRSUMOS,TRSUMOS     CLEAR USED MONTH OF SERVICE                  
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BNZ   REVCR20                                                          
         OC    TRSUDAT,TRSUDAT     VOID MUST CARRY USED DATE                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    TRSVOID,TRSVOID     VOID MUSTN'T HAVE SAVED USED DATE            
         BZ    REVCR30                                                          
         DC    H'0'                                                             
                                                                                
REVCR20  OC    TRSUDAT,TRSUDAT     UNVOID MUSTN'T CARRY USED DATE               
         BZ    *+6                                                              
         DC    H'0'                                                             
         OC    TRSVOID,TRSVOID     UNVOID MUST CARRY SAVED USED DATE            
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    TRSMARK,TRSMUMQ     SET TYPE/ACTION IS NEGATIVE                  
         OI    TRSSTAT,TRSSACHQ    SET SYSTEM CHEQUE                            
         CLI   STSARBTY,BT36       TEST UNVOIDING MANUAL CHEQUE                 
         BNE   *+12                                                             
         NI    TRSSTAT,FF-TRSSACHQ CLEAR SYSTEM CHEQUE                          
         OI    TRSSTAT2,TRSSMCHQ   SET MANUAL CHEQUE                            
                                                                                
         CLI   STSARBTY,BT57       TEST UNVOIDING TRANSFERS                     
         BE    *+12                                                             
         CLI   STSARBTY,BT60       TEST UNVOIDING PAYMENTS                      
         BNE   *+8                                                              
         OI    TRSSTAT2,TRSSPMNT                                                
                                                                                
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    *+8                                                              
         OI    TRSSTAT2,TRSSANBP                                                
                                                                                
         NI    TRSSTAT,FF-TRSSVOID CLEAR VOID                                   
         GOTO1 VDATCON,DMCB,(2,TRSVOID),(1,DATWRK)                              
         MVC   TRSUMOS,DATWRK      RESET USED MONTH OF SERVICE                  
                                                                                
REVCR30  XC    TRSVOID,TRSUDAT     SWAP USED DATE AND SAVED USED DATE           
         XC    TRSUDAT,TRSVOID     ONE IS BINARY ZEROES, THE OTHER IS           
         XC    TRSVOID,TRSUDAT     THE (SAVED) USED DATE                        
         DROP  R2                                                               
                                                                                
         USING MPYELD,R2                                                        
         ICM   R2,15,AMPYEL        R2=A(MPYEL)                                  
         BZ    REVCR40             ADD ONE IF NOT FOUND                         
*&&US                                                                           
         CLI   MPYLN,MPYLN2Q       TEST ELEMENT HAS SUB REF                     
         BE    REVCR60             (NOT IN UK)                                  
         GOTO1 VHELLO,DMCB,(C'D',OVACCMST),('MPYELQ',AIOBUFF2),0                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
*&&UK*&& GOTO1 VTOBACCO,DMCB,('TOBAADEL',0),AIOBUFF2,ACOM,0,MPYELD,0            
                                                                                
REVCR40  LA    R2,ELEMT            BUILD NULL ELEMENT                           
         XC    MPYEL(MPYLN2Q),MPYEL                                             
         MVI   MPYEL,MPYELQ                                                     
         MVI   MPYLN,MPYLN2Q       MAKE IT A LONG ONE                           
*&&UK                                                                           
         MVC   MPYNO,FILTCHN       SET MYPEL DETAILS PRIOR TO ADD               
         MVC   MPYDTE,FILTCHD                                                   
         ZAP   MPYAMNT,FILTCHA                                                  
         MVC   MPYBNK,FILTCHB                                                   
         XC    MPYSUB,MPYSUB       CANNOT BE SET YET                            
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BO    REVCR50                                                          
         MVC   MPYNO,SPACES        RESET CHEQUE DETAILS                         
         XC    MPYDTE,MPYDTE                                                    
         ZAP   MPYAMNT,PZERO                                                    
         MVC   MPYBNK,SPACES                                                    
         MVC   MPYSUB,OVDRSBR      ADD THE NEW MPYEL                            
REVCR50  MVC   WORK(L'CURTCUR),C.CURTCUR                                        
         MVC   WORK+L'CURTCUR(L'CURTCUR),S.CURTCUR                              
         GOTO1 VTOBACCO,DMCB,('TOBAAADD',WORK),AIOBUFF2,ACOM,0,MPYELD,0         
         GOTO1 ASETELAD,AIOBUFF2                                                
         L     R2,AMPYEL           R2=A(MPYEL AFTER ADD)                        
         B     REVCR70             SKIP UPDATE OF MPYEL VALUES                  
*&&                                                                             
*&&US                                                                           
         GOTO1 VHELLO,DMCB,(C'P',OVACCMST),AIOBUFF2,(R2)                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF2                                                
         L     R2,AMPYEL           R2=A(MPYEL AFTER ADD)                        
                                                                                
REVCR60  MVC   MPYNO,FILTCHN       SET CHEQUE DETAILS                           
         MVC   MPYDTE,FILTCHD                                                   
         ZAP   MPYAMNT,FILTCHA                                                  
         MVC   MPYBNK,FILTCHB                                                   
         XC    MPYSUB,MPYSUB       CANNOT BE SET YET                            
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BO    REVCR70                                                          
         MVC   MPYNO,SPACES        RESET CHEQUE DETAILS                         
         XC    MPYDTE,MPYDTE                                                    
         ZAP   MPYAMNT,PZERO                                                    
         MVC   MPYBNK,SPACES                                                    
         MVC   MPYSUB,OVDRSBR                                                   
*&&                                                                             
         DROP  R2                                                               
                                                                                
         USING TRNRECD,R2                                                       
REVCR70  L     R2,AIOBUFF2         SET RECORD KEY VALUES                        
                                                                                
         XC    TRNRSUSE,TRNRSUSE   CLEAR USED MOS                               
         ICM   R1,15,ADUEEL                                                     
         BZ    *+10                                                             
         MVC   TRNRSUSE,DUEDATE-DUEELD(R1)                                      
                                                                                
         NI    TRNRSTA2,FF-TRNSUSED                                             
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BZ    *+14                                                             
         OI    TRNRSTA2,TRNSUSED   SET USED STATUS AND MOS                      
         MVC   TRNRSUSE,DATWRK                                                  
                                                                                
         CLI   XACTION,ACTDRFT     PUT BACT TO ACCMST IF NOT DRAFT              
         BE    REVCRX                                                           
         MVC   BYTE,TRNRSTAT       SAVE RECORD STATUS                           
                                                                                
         LA    R1,IOPUT+IOACCMST+IO2Q                                           
         TM    TRNRSTAT,TRNSARCH   TEST TRANSACTION ON ACCARC                   
         BNO   *+12                                                             
         LA    R1,IOADFR+IOACCARC+IO2Q  PROMOTE ACCARC RECORD TO ACCMST         
         NI    TRNRSTAT,FF-TRNSARCH  CLEAR ACCARC INDICATOR                     
                                                                                
         GOTO1 AIOEXEC                                                          
         JNE   *+2                                                              
                                                                                
         L     R1,AIOBUFF2         UPDATE DIRECTORY RECORD                      
         LA    R2,KEY                                                           
         MVC   TRNKEY,TRNKEY-TRNRECD(R1)                                        
         MVC   TRNKSTA,TRNRSTA-TRNRECD(R1)                                      
         TM    BYTE,TRNSARCH       TEST RECORD PROMOTED FROM ACCARC             
         BNO   *+14                                                             
         L     R1,AIOSAVE          EXTRACT NEW DISK ADDRESS                     
         MVC   TRNKDA,0(R1)                                                     
                                                                                
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BZ    *+10                                                             
         MVC   OVCRDA,TRNKDA       SAVE CR DISK ADDRESS                         
                                                                                
         LA    R1,IOWRITE+IOACCDIR+IO2Q                                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
** IN CASE OF UNVOIDING, WE WILL BE BUILDING THE PAY2JOB CODE NMAL              
         TM    VOIDIND1,VOIDUVOI   TEST UNVOIDING             NMAL              
         JO    REVCR195                                       NMAL              
*                                                                               
* MARK AUTO APPROVAL POINTERS AS VOIDED                                         
*                                                                               
         TM    VOIDIND1,VOIDVOID   TEST VOIDING                                 
         BNO   REVCRX                                                           
         MVC   SVKEY,KEY           SAVE OFF KEY FOR LATER RESTORE               
         USING TRNRECD,R2                                                       
         L     R2,AIOBUFF2                                                      
*                                                                               
         LA    R4,TRNRFST          FIND TRSEL                                   
         LA    RF,0                                                             
         XC    SVMOS,SVMOS                                                      
         MVC   SVEST,SPACES                                                     
         MVC   SVOFF,SPACES                                                     
         MVI   SVSYS,0                                                          
REVCR80  CLI   0(R4),0             END OF RECORD                                
         BE    REVCR180                                                         
         CLI   0(R4),X'1A'         MEDIA TRANSFER ELEMENT                       
         BE    REVCR100                                                         
         CLI   0(R4),X'23'         OTHERS ELEMENT                               
         BE    REVCR110                                                         
         CLI   0(R4),X'44'         TRANSACTION ELEMENT                          
         BE    REVCR120                                                         
         CLI   0(R4),X'46'         EXTRA PAYMENT ELEMENT                        
         BE    REVCR130                                                         
         CLI   0(R4),X'6A'         MEDIA TRANSFER (PACKED DATA)                 
         BE    REVCR100                                                         
         CLI   0(R4),X'E5'         GENERAL DATE ELEMENT                         
         BE    REVCR160                                                         
         CLI   0(R4),X'F3'         NEW BILLING XFER(ONLY) ELEMENT               
         BE    REVCR170                                                         
REVCR90  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     REVCR80                                                          
*                                                                               
         USING MDTELD,R4                                                        
REVCR100 CLI   MDTSYS,C'J'         IS SYSTEM PRODUCTION?                        
         BE    REVCRX              YES NO POINTER NEEDED                        
*                                                                               
         MVC   SVSYS,MDTSYS        SYSTEM                                       
*                                                                               
         LA    R1,PSYSTAB          TABLE OF EQUIVALENT SYSTEMS-PRINT            
REVCR105 CLI   0(R1),X'FF'                                                      
         BE    REVCR108                                                         
         CLC   MDTSYS,0(R1)        IF MATCH ON SYSTEM THAN THE ACTUAL           
         BE    REVCR107            SYSTEM MUST BE PRINT                         
         LA    R1,1(R1)                                                         
         B     REVCR105                                                         
*                                                                               
REVCR107 MVI   SVSYS,C'P'          YES SO SAVE AS PRINT IN SVSYS                
REVCR108 MVC   SVMOS,MDTMOS        MOVE IN MONTH OF SERVICE                     
         MVC   SVEST,MDTEST        SAVE CHARACTER ESTIMATE                      
         OC    SVEST,SPACES                                                     
         B     REVCR90                                                          
         DROP  R4                                                               
*                                                                               
         USING OTHELD,R4                                                        
REVCR110 CHI   RF,23                                                            
         BH    REVCR90                                                          
         CLI   OTHPROF-3,C' '      IF SOMETHING IN THIS FIELD THAN NO           
         BH    REVCR90             MOS IN THIS ELEMENT                          
         LA    RF,23                                                            
         MVC   SVMOS,OTHDATE                                                    
         B     REVCR90                                                          
         DROP  R4                                                               
*                                                                               
         USING TRNELD,R4                                                        
REVCR120 MVC   SVOFF,TRNOFFC                                                    
         B     REVCR90                                                          
         DROP  R4                                                               
*                                                                               
         USING XPYELD,R4                                                        
REVCR130 OC    SVMOS,SVMOS                                                      
         BNZ   REVCR150                                                         
         CLC   XPYPER,SPACES       CHECK FOR ANY PERIOD DATE(S)                 
         BNH   REVCR150            NO - SKIP                                    
         CLI   TRNKCULA+3,C'N'       IF NETWORK                                 
         BE    *+8                                                              
         OI    FLAG,FLGBRD           GET DATE FROM BROADCAST CAL                
         MVC   WORK(L'XPYPER),XPYPER                                            
         LA    R3,WORK                                                          
         CLI   WORK+6,C' '                                                      
         BNH   *+8                                                              
         LA    R3,WORK+6                                                        
         CLI   WORK+6,C'-'                                                      
         BNE   *+8                                                              
         LA    R3,WORK+7                                                        
         TM    FLAG,FLGBRD                                                      
         BNO   REVCR140                                                         
         GOTO1 VGETBRD,DMCB,(1,(R3)),WORK+20,VGETDAY,VADDAY                     
         LA    R3,WORK+20                                                       
REVCR140 GOTO1 VDATCON,DMCB,(0,(R3)),(1,SVMOS)                                  
*                                                                               
REVCR150 CLC   XPYEST,SPACES       IS THERE AN ESTIMATE?                        
         BE    REVCR90                                                          
         OC    XPYEST,XPYEST                                                    
         BZ    REVCR90                                                          
         LH    RE,XPYEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVEST(3),DUB           UNPACK THE ESTIMATE                       
         B     REVCR90                                                          
         DROP  R4                                                               
*                                                                               
         USING GDAELD,R4                                                        
REVCR160 CLI   GDATYPE,GDAMMOS     IS THIS AN MOS DATE?                         
         BNE   REVCR90                                                          
         LA    RF,X'E5'                                                         
         MVC   SVMOS,GDAYYMM                                                    
         B     REVCR90                                                          
         DROP  R4                                                               
*                                                                               
         USING MBIELD,R4                                                        
REVCR170 CHI   RF,X'E5'                                                         
         BE    *+10                                                             
         MVC   SVMOS,MBIMOS                                                     
         MVC   SVEST,MBIEST                                                     
         B     REVCR90                                                          
         DROP  R4                                                               
*                                                                               
         USING AAVPASD,R4                                                       
REVCR180 LA    R4,KEY                                                           
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,TRNKCPY                                                  
         MVC   AAVPCLT,TRNKCACT+9  LAST 3 CHAR OF CONTRA IS CLIENT              
         MVC   AAVPPRD,TRNKREF     1ST 3 CHAR OF REFERENCE IS PRODUCT           
         MVC   AAVPEST,SVEST       ESTIMATE                                     
         MVC   AAVPMOS,SVMOS       MOS                                          
         LA    RF,LDSYTAB          TABLE OF LEDGER/SYSTEM                       
REVC182  CLI   0(RF),X'FF'         IF NOT IN TABLE THAN NO PASSIVE FOR          
         BE    REVCR190            THIS LEDGER.                                 
         CLC   TRNKLDG,0(RF)                                                    
         BE    REVC184                                                          
         LA    RF,2(RF)                                                         
         B     REVC182                                                          
*                                                                               
REVC184  MVC   AAVPSYS,1(RF)       SYSTEM                                       
         CLI   AAVPSYS,C'S'        IF IT'S SPOT CHECK TO SEE IF IT'S            
         BNE   *+16                REALLY NET BY CHECKING THE 1ST CHAR          
         CLI   TRNKACT,C'N'        OF THE ACCOUNT FOR 'N' (SSN)                 
         BNE   *+8                                                              
         MVI   AAVPSYS,C'N'                                                     
         MVC   AAVPOFF,SVOFF       OFFICE                                       
         MVC   AAVPACCT,TRNKLDG    STATION (SS ACCOUNT)                         
         MVC   AAVPKDA,SVDA        DISK ADDRESS                                 
         MVI   AAVPFLG1,0          SET FOR GOOD ESTIMATE AS DEFAULT             
         GOTO1 AIOEXEC,IOHI+IOACCDIR+IO1Q                                       
         CLC   AAVPKEY(AAVPFLG1-AAVPKEY),KEYSAVE                                
         BNE   REVCR190            END OF LEDGER                                
*                                                                               
         NI    AAVPSTAT,X'FF'-AAVPAPP   TURN OFF APPROVED BIT IF ON             
         XC    AAVPUSED,AAVPUSED        TURN OFF USED DATE                      
*                                                                               
         GOTO1 AIOEXEC,IOWRITE+IOACCDIR+IO1Q                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REVCR190 MVC   KEY,SVKEY                                                        
         GOTO1 AIOEXEC,IORD+IOACCDIR+IO1Q                                       
         JNE   *+2                 END OF LEDGER                                
*                                                                               
REVCR195 DS    0H                             NMAL                              
                                                                                
CRED     USING TRNRECD,R2                                                       
         L     R2,AIOBUFF2         APPLICABLE SCENARIO?                         
         CLI   CRED.TRNRSTYP,TRNTINV        TYPE-01 FOR US AND UK               
         JE    REVCR200                                                         
*&&US                                                                           
         CLI   CRED.TRNRSTYP,TRNTMEPY       TYPE-10 FOR US                      
         JE    REVCR200                                                         
         CLI   CRED.TRNRSTYP,TRNTMUBL       TYPE-46 FOR US                      
         JE    REVCR200                                                         
         CLI   CRED.TRNRSTYP,61             TYPE-61 FOR US                      
         JNE   REVCRX                                                           
REVCR200 CLI   CRED.TRNKLDG,C'V'                                                
         JE    REVCR210                                                         
         CLI   CRED.TRNKLDG,C'W'                                                
         JE    REVCR210                                                         
         CLI   CRED.TRNKLDG,C'X'                                                
         JE    REVCR210                                                         
         CLI   CRED.TRNKLDG,C'Y'                                                
         JNE   REVCRX                                                           
*&&                                                                             
**                                                                              
*&&UK                                                                           
         CLI   CRED.TRNRSTYP,TRNTNBIN       TYPE-21 FOR UK                      
         JE    REVCR200                                                         
         CLI   CRED.TRNRSTYP,72             TYPE-72 FOR UK                      
         JNE   REVCRX                                                           
REVCR200 CLI   CRED.TRNKLDG,C'V'                                                
         JE    REVCR210                                                         
         CLI   CRED.TRNKLDG,C'X'                                                
         JNE   REVCRX                                                           
*&&                                                                             
         DROP  CRED                                                             
                                                                                
                                                                                
REVCR210 TM    COMPSTAD,CPYSAP2J   PAY2JOB CHECKS (AFTER POTENTIOAL             
         JZ    REVCRX              PROMOTE)                                     
         USING JARAYD,R1                                                        
         SAM31 ,                                                                
         L     R1,AJARAY           PUT TO TABLE                                 
         LHI   R0,JARAY#                                                        
REVCR220 CLI   JARDFIL,JARDEOT                                                  
         JE    REVCR230                                                         
         AHI   R1,JARAYLQ                                                       
         JCT   R0,REVCR220                                                      
         SAM24 ,                                                                
         DC    H'0'                                                             
***      DOP2J CALL DISABLED HERE (JUST TO BE IN SYNC WITH DOP2M CODE)          
***      L     R1,AJARAY                                                        
                                                                                
REVCR230 MVI   JARDFIL,JARDFMQ     ADD TO TABLE (MUST BE ACCMST)                
         MVC   JARDPDA,IODA                                                     
         XC    JARDERR,JARDERR                                                  
         XC    JARDATE,CHQDATB                                                  
         MVC   JARPAYR,SPACES                                                   
         USING MPYELD,RE                                                        
         ICM   RE,B'1111',DUB2+4                                                
         JZ    REVCR240                                                         
         MVC   JARPAYR,MPYNO                                                    
         MVC   JARDATE,MPYDTE                                                   
                                                                                
REVCR240 MVI   JARPREV,C'Y'        REVERSALS? (REQUIRED AS PASSING              
         TM    VOIDIND1,VOIDUVOI   CREDITS)                                     
         JZ    REVCR250                                                         
         MVI   JARPREV,C'N'                                                     
         DROP  RE                                                               
                                                                                
REVCR250 AHI   R1,JARAYLQ                                                       
         MVI   JARDFIL,JARDEOT                                                  
         SAM24 ,                                                                
         DROP  R1                                                               
                                                                                
*                                                                               
REVCRX   B     OVROUTE                                                          
         SPACE 2                                                                
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
* REVERSE TRANSACTION VOID STATUS                                     *         
***********************************************************************         
                                                                                
REVVST   CLI   XACTION,ACTDRFT     TEST REPORT WAS DRAFT                        
         BE    REVVSTX                                                          
         LA    R1,IORDUP+IOACCDIR+IO2Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGETRUP+IOACCMST+IO2Q  READ DATA RECORD                      
         TM    KEY+(TRNKSTAT-TRNRECD),TRNSARCH   TEST RECORD ON ARCHIVE         
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO2Q  READ DATA RECORD                         
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIOBUFF2                                                      
         USING TRNRECD,R2                                                       
         LA    R1,TRNRFST          FIND TRSEL                                   
         USING TRSELD,R1                                                        
         XR    R0,R0                                                            
         CLI   TRSEL,TRSELQ                                                     
         BE    *+20                                                             
         IC    R0,TRSLN                                                         
         AR    R1,R0                                                            
         CLI   TRSEL,0                                                          
         BNE   *-18                                                             
         DC    H'0'                                                             
         XI    TRSSTAT,TRSSVOID     REVERSE VOID STATUS                         
         DROP  R1                                                               
                                                                                
         LA    R1,IOPUT+IOACCMST+IO2Q                                           
         TM    TRNRSTAT,TRNSARCH   TEST TRANSACTION ON ACCARC                   
         BNO   *+12                                                             
         LA    R1,IOADFR+IOACCARC+IO2Q  PROMOTE ACCARC RECORD TO ACCMST         
         NI    TRNRSTAT,FF-TRNSARCH  CLEAR ACCARC INDICATOR                     
                                                                                
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIOBUFF2         UPDATE DIRECTORY RECORD                      
         LA    R2,KEY                                                           
         MVC   TRNKEY,TRNKEY-TRNRECD(R1)                                        
         MVC   TRNKSTA,TRNRSTA-TRNRECD(R1)                                      
         TM    BYTE,TRNSARCH       TEST RECORD PROMOTED FROM ACCARC             
         BNO   *+14                                                             
         L     R1,AIOSAVE          EXTRACT NEW DISK ADDRESS                     
         MVC   TRNKDA,0(R1)                                                     
         DROP  R2                                                               
                                                                                
         LA    R1,IOWRITE+IOACCDIR+IO2Q                                         
         GOTO1 AIOEXEC                                                          
         BE    REVVSTX                                                          
         DC    H'0'                                                             
*                                                                               
REVVSTX  B     OVROUTE                                                          
         EJECT                                                                  
***********************************************************************         
* SORT OFFICE TABLE                                                             
***********************************************************************         
                                                                                
         USING OFFTABD,R4                                                       
SRTOFF   L     R4,AOFFTAB          SORT OFFTAB INTO ASCENDING SEQUENCE          
         CLI   OFFTABD,0                                                        
         BE    SRTOFFX                                                          
                                                                                
         XR    R0,R0               INITIALISE NO SWAP                           
SRTOFF02 TM    OFFTABD+OFFTABL,FF  TEST MORE ENTRIES TO COME                    
         BNM   SRTOFF06                                                         
                                                                                
         CLC   OFFOFFC,OFFOFFC+OFFTABL  TEST SWAP                               
         BNH   SRTOFF04                                                         
         XC    OFFTABD(OFFTABL),OFFTABD+OFFTABL                                 
         XC    OFFTABD+OFFTABL(OFFTABL),OFFTABD                                 
         XC    OFFTABD(OFFTABL),OFFTABD+OFFTABL                                 
         LTR   R0,R0                                                            
         BNZ   SRTOFF04                                                         
         L     R0,AOFFTAB          SET A SWAP THIS PASS                         
                                                                                
SRTOFF04 LA    R4,OFFTABL(R4)      NEXT OFFICE TABLE ENTRY                      
         B     SRTOFF02                                                         
                                                                                
SRTOFF06 LR    R4,R0               SET A(OFFTAB)                                
         XR    R0,R0               RESET SWAP INDICATOR                         
         LTR   R4,R4               TEST SWAP LAST PASS                          
         BNZ   SRTOFF02                                                         
                                                                                
SRTOFFX  B     OVROUTX                                                          
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TAX ADJUSTMENT -   EXITS WITH P1 CONTAINING PL6 ADJUSTMENT          *         
* NTRY - P1 A(TAX RATE), P2 A(TAXDSC)   MUST BE L'INVDSC              *         
***********************************************************************         
                                                                                
TAXADJ   L     R2,4(R1)            EXTRACT DISCOUNT                             
         ZAP   PL13,0(L'INVDSC,R2)                                              
         L     R2,0(R1)            EXTRACT TAX RATE                             
         XR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         CVD   RF,DUB                                                           
                                                                                
         MP    PL13,=P'10000'      CALCULATE ADJUSTMENT                         
         AP    DUB,=P'10000'                                                    
         DP    PL13,DUB                                                         
         ZAP   PL13,PL13(L'PL13-L'DUB)                                          
         SP    DUB,=P'10000'                                                    
         MP    PL13,DUB                                                         
         SRP   PL13,64-4,5         ROUND THE RESULT                             
                                                                                
         ZAP   0(6,R1),PL13                                                     
                                                                                
TAXADJX  B     OVROUTX                                                          
         EJECT                                                                  
***********************************************************************         
* TSAR 2 INTERFACE ROUTINES                                           *         
***********************************************************************         
                                                                                
         USING TSARD,R2                                                         
                                                                                
***********************************************************************         
* ADD/PUT/WRITE TSAR 2 RECORD                                         *         
***********************************************************************         
                                                                                
TS2APW   LA    R2,TSA2BLK                                                       
         STC   R1,TSACTN                                                        
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    OVROUTX                                                          
         DC    H'0'                COULDN'T ADD/PUT BACK RECORD                 
                                                                                
***********************************************************************         
* CLEAR TSAR 2 BUFFER (DELETE ALL RECORDS)                            *         
***********************************************************************         
                                                                                
TS2CLR   LA    R2,TSA2BLK                                                       
         MVC   TSRNUM,=H'1'                                                     
TS2CLR02 MVI   TSACTN,TSADEL                                                    
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0            TEST DELETE SUCCESSFUL                       
         BE    TS2CLR02            YES - DO ANOTHER                             
         TM    TSERRS,TSERNF       NO - ACCEPT NOT FOUND                        
         BO    *+6                                                              
         DC    H'0'                                                             
         XC    TSA2REC,TSA2REC     DELETE RECORD IN W/S                         
         B     OVROUTX                                                          
                                                                                
***********************************************************************         
* DELETE SPECIFIC TSAR 2 RECORD BY KEY                                *         
***********************************************************************         
                                                                                
TS2DEL   LA    R2,TSA2BLK                                                       
         MVI   TSACTN,TSADEL                                                    
         XC    TSRNUM,TSRNUM       DELETE SINGLE RECORD BY KEY                  
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                COULDN'T DELETE RECORD                       
         XC    TSA2REC,TSA2REC     DELETE RECORD IN W/S                         
         B     OVROUTX                                                          
                                                                                
***********************************************************************         
* GET FIRST/NEXT TSAR 2 RECORD BY NUMBER / READ TSAR 2 RECORD BY KEY  *         
***********************************************************************         
                                                                                
TS2GET   LA    R2,TSA2BLK                                                       
         STC   R1,TSACTN                                                        
         CLI   TSACTN,TSAGET       TEST GET                                     
         BNE   *+10                                                             
         MVC   TSRNUM,=H'1'        GET FIRST RECORD                             
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    OVROUTX                                                          
         TM    TSERRS,TSEEOF+TSERNF                                             
         BNZ   OVROUTL                                                          
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHEQUE DETAILS                                             *         
***********************************************************************         
                                                                                
         USING MPYELD,R1                                                        
VALCQD   ICM   R1,15,AMPYEL        R1=A(PAYMENT ELEMENT)                        
         BZ    OVROUTH                                                          
                                                                                
         CLC   FILTCHN,MPYNO       TEST NUMBER                                  
         BNE   OVROUTH                                                          
         CLC   FILTCHD,MPYDTE      TEST DATE                                    
         BNE   OVROUTH                                                          
         CLC   FILTCHB,MPYBNK      TEST BANK A/C                                
         BNE   OVROUTH                                                          
         TM    CHQINDS,CHQIARTQ    TEST AMOUNT (NOT ARTIST CHEQUES)             
         BO    *+14                                                             
         CP    FILTCHA,MPYAMNT                                                  
         BNE   OVROUTH                                                          
                                                                                
VALCQDX  B     OVROUTE                                                          
         DROP  R1                                                               
         EJECT                                                                  
*&&UK                                                                           
***********************************************************************         
* VALIDATE BANK CURRNCY CODE AND SECURITY                             *         
***********************************************************************         
                                                                                
VALBCUR  XC    PRSTCURT,PRSTCURT                                                
         MVC   BANKCUR,RECCURR     EXTRACT ACCOUNT CURRENCY                     
         OC    BANKCUR,BANKCUR     TEST BANK ACCOUNT CURRENCY KNOWN             
         BNZ   *+10                                                             
         MVC   BANKCUR,C.CURTCUR                                                
         CLC   BANKCUR,C.CURTCUR                                                
         BE    VALBC02                                                          
                                                                                
         CLI   BANKCUR,ASTCANY     TEST CURRENCY ACCEPTABLE                     
         BE    *+14                                                             
         CLC   BANKCUR,SPACES                                                   
         BNH   VALBC08                                                          
         TM    COMPSTA6,CPYSFMCR+CPYSFOCR                                       
         BZ    VALBC06                                                          
         OI    SAFCIND1,SAFCI1BC   SET BANK ACCOUNT IN CURRENCY                 
         MVI   FULL,ACTCURR                                                     
         GOTO1 VSECRET,DMCB,('SECPRACT',ASECBLK),(XTYPE,FULL)                   
         BNE   VALBC06                                                          
                                                                                
VALBC02  TM    COMPSTA6,CPYSFOCR+CPYSFMCR FOREIGN CURRENCY?                     
         BZ    VALBC04                                                          
         MVC   VOICURC,BANKCUR     GIVE CURRENCY SYMBOL                         
         MVI   VOICURCH+(FVILEN-FVIHDR),L'BANKCUR                               
         OI    VOICURCH+(FVIIND-FVIHDR),FVIVAL                                  
         OI    VOICURCH+(FVOIND-FVIHDR),FVOXMT                                  
         NI    VOICURCH+(FVATRB-FVIHDR),FF-FVAHIGH                              
         PUSH  USING                                                            
PRESET   USING CURTABD,PRSTCURT                                                 
VALBC04  MVC   PRESET.CURTCUR,BANKCUR                                           
         POP   USING                                                            
         B     VALBCURX                                                         
                                                                                
VALBC06  MVC   FVMSGNO,=AL2(AE$SECUB)                                           
         MVI   FVXTRA,C'*'                                                      
         CLI   BANKCUR,ASTCANY                                                  
         BE    VALBCURH                                                         
         MVC   FVXTRA(L'CURTCUR),BANKCUR                                        
         B     VALBCURH                                                         
                                                                                
VALBC08  TM    VOICURCH+(FVATRB-FVIHDR),FVAHIGH                                 
         BNZ   *+10                                                             
         XC    VOICURC,VOICURC                                                  
         OI    VOICURCH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    VOICURCH+(FVATRB-FVIHDR),FVAHIGH                                 
                                                                                
VALBCURX B     OVROUTE                                                          
VALBCURH B     OVROUTH                                                          
         EJECT                                                                  
***********************************************************************         
* PROCESS VENDOR CREDIT EXCHANGE DIFFERENCE                           *         
* NTRY - R1 - A(TRNELD)                                               *         
***********************************************************************         
                                                                                
         USING TRNELD,R2                                                        
VCREXCH  LR    R2,R1               A(TRNELD)                                    
         XC    OVCPJ,OVCPJ                                                      
         XC    OVCPJWK,OVCPJWK                                                  
         CLI   TRNTYPE,BT36                                                     
         BNE   VCREXCHX                                                         
         GOTO1 AVALCQD             CONFIRM MPY DETAILS                          
         BNE   VCREXCHX                                                         
                                                                                
         ICM   R1,15,ATRSEL        TEST EXCHANGE DIFFERENCE IS VOIDING          
         BNZ   *+6                 OR UNVOIDING AN EXCHANGE DIFFERENCE          
         DC    H'0'                                                             
         LA    RF,BOQ                                                           
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+8                                                              
         LA    RF,BZQ                                                           
         USING TRSELD,R1                                                        
         TM    TRSSTAT,TRSSVOID                                                 
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   VCREXCHX                                                         
         DROP  R1                                                               
         TM    TRNSTAT,TRNSREV                                                  
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   VCREXCHX                                                         
                                                                                
         USING TRXELD,R1                                                        
         ICM   R1,15,ATRXEL        EXTRA STATUS ELEMENT                         
         BZ    VCREX02                                                          
         TM    TRXSTA1,TRXSXDSJ    TEST EXCHANGE DIFFERENCE TO SJ               
         BZ    VCREX02                                                          
         USING SORELD,R1                                                        
         MVC   OVCPJCPY,COMPANY                                                 
         ICM   R1,15,ASORPROD      FIND SOURCE SJ ACCOUNT                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   SORLN,SORAL2Q       TEST WORKCODE KNOWN                          
         BNL   *+6                                                              
         DC    H'0'                                                             
         MVC   OVCPJULA,SORAULA                                                 
         MVC   OVCPJWK,SORAWRK                                                  
         B     VCREX04                                                          
                                                                                
VCREX02  OC    EXDF,EXDF                                                        
         BZ    VCREXCHX                                                         
                                                                                
VCREX04  MVC   OVKEY2,KEY          SAVE THE KEY WE HAVE REACHED                 
         ZAP   OVAMNT,TRNAMNT      AND AMOUNT OF SUPPLIER CR                    
         GOTO1 AREVVST             REVERSE EXCHANGE VOID STATUS                 
                                                                                
         GOTO1 APOST,POSTISTD      REVERSE SUPPLIER SIDE EXCHANGE               
                                                                                
         PUSH  USING               REVERSE INCOME/EXPENSE SIDE EXCHANGE         
         USING TRNRECD,KEY                                                      
         MVC   TRNKCULC,TRNKCULA                                                
                                                                                
         OC    OVCPJ,OVCPJ         TEST EXCHANGE DIFFERENCE TO JOB              
         BZ    VCREX06                                                          
         MVC   TRNKCULA,OVCPJ                                                   
         MVC   TRNKWORK,OVCPJWK                                                 
         B     VCREX08                                                          
                                                                                
VCREX06  MVC   TRNKCULA,EXDF                                                    
                                                                                
         XR    R1,R1                                                            
         ICM   R1,1,EXDFOPIK                                                    
         BZ    VCREX08                                                          
         LA    R1,TRNKACT-1(R1)                                                 
         CLI   TRNOFFC,C' '                                                     
         BNH   *+10                                                             
         MVC   0(1,R1),TRNOFFC                                                  
                                                                                
VCREX08  MVI   TRNKSBR,0                                                        
         MVC   OVKEY1,KEY                                                       
         LA    R1,IOREAD+IOACCDIR+IO2Q                                          
         B     VCREX12                                                          
                                                                                
VCREX10  CLC   OVKEY1,KEY          TRY NEXT TRANSACTION                         
         BNE   VCREX20             INCOME/EXPENSE EXDF NOT FOUND                
         LA    R1,IOSEQ+IOACCDIR+IO2Q                                           
VCREX12  GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                EXCHANGE DIFFERENCE MISSING                  
         TM    TRNKSTAT,TRNSDRFT+TRNSTIME+TRNSDELT                              
         BNZ   VCREX10                                                          
         CLI   TRNKSTYP,BT36                                                    
         BNE   VCREX10                                                          
                                                                                
         TM    VOIDIND1,VOIDVOID                                                
         BZ    VCREX14                                                          
         TM    TRNKSTAT,TRNSREVS                                                
         BNZ   VCREX10                                                          
         B     VCREX16                                                          
                                                                                
VCREX14  TM    TRNKSTAT,TRNSREVS                                                
         BNO   VCREX10                                                          
                                                                                
VCREX16  LA    R1,IOGET+IOACCMST+IO2Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST ON ARCHIVE                              
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO2Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                EXCHANGE DIFFERENCE MISSING                  
         POP   USING                                                            
         DROP  R2                                                               
                                                                                
         GOTO1 ASETELAD,AIOBUFF2                                                
         ICM   R1,15,ATRXEL        TEST EXCHANGE DIFFERENCE                     
         BZ    VCREX10                                                          
         TM    TRXSTA1-TRXELD(R1),TRXSXDIF                                      
         BZ    VCREX10                                                          
         GOTO1 AVALCQD             CONFIRM MPY DETAILS                          
         BNE   VCREX10                                                          
                                                                                
         ICM   R1,15,ATRNEL        TEST AMOUNT IS AS SUPPLIER SIDE              
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING TRNELD,R1                                                        
         ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+10                                                             
         MP    DUB,PONENEG                                                      
         CP    DUB,OVAMNT                                                       
         BNE   VCREX10                                                          
         DROP  R1                                                               
                                                                                
         SP    TOTEXDF,OVAMNT      FOUND CORRECT EXPENSE/INCOME TRANS           
         ZAP   CUREXDF,TOTEXDF                                                  
         GOTO1 APOST,POSTISTD      REVERSE EXPENSE/INCOME SIDE                  
                                                                                
VCREX20  MVC   KEY,OVKEY2          RE-READ INCOMING KEY                         
         LA    R1,IOREAD+IOACCDIR+IO1Q  READ TO RE-ESTABLISH SEQUENCE           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
VCREXCHX B     OVROUTX                                                          
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* READ VENDOR LEDGER                                                  *         
*                                                                     *         
* VOID   - INVOICE  - CLEAR USED DATE.  SAVE IN CASE OF LATER UNVOID  *         
*                     CLEAR MPYEL AND SET SEQUENCE NUMBER (TRNSUB) OF *         
*                     VOIDED PAYMENT IN MPYSUB                        *         
*          PAYMENT  - PRODUCE REVERSING POSTING (-DR)                 *         
*                                                                     *         
* UNVOID - INVOICE  - RESTORE SAVED USED DATE & MPYEL CHEQUE DETAILS  *         
*          PAYMENT  - PRODUCE FRESH PAYMENT (DR) FROM VOIDED PAYMENT  *         
***********************************************************************         
                                                                                
VENDOR   OC    ACOSTAB,ACOSTAB     TEST COSTING                                 
         BZ    VEND02                                                           
K        USING LDGRECD,KEY                                                      
         MVC   K.LDGKEY,SPACES     GET COSTING LEDGERS                          
         MVC   K.LDGKCPY,COMPANY                                                
         MVI   K.LDGKUNT,C'1'                                                   
         MVI   K.LDGKLDG,C'C'      GET 1C LEDGER                                
         GOTO1 AGETLDG,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   K.LDGKLDG,C'2'      GET 12 LEDGER                                
         GOTO1 AGETLDG,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  K                                                                
                                                                                
VEND02   MVC   KEY,SPACES          GET LEDGER RECORD                            
         MVC   KEY(LDGKEND),STSARCON                                            
         LA    R0,LEDGMAXN         TEST LEDGER DETAILS AVAILABLE                
         L     R1,ALEDGTAB                                                      
         USING LEDGTABD,R1                                                      
         CLC   LEDGTUL,KEY+(LDGKUNT-LDGRECD)                                    
         BE    VEND04                                                           
         LA    R1,LEDGTABL(R1)                                                  
         BCT   R0,*-14                                                          
         DROP  R1                                                               
                                                                                
         GOTO1 AGETLDG,0           NOT FOUND - GETLDG WILL READ FILE            
         BE    VEND06                                                           
         DC    H'0'                CAN'T ALLOW ANY ERROR                        
                                                                                
VEND04   LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VEND06   GOTO1 AEXTLDG,AIOBUFF     EXTRACT POSTING ACCOUNTS FROM LDG            
                                                                                
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BO    VEND08                                                           
         MVC   OVBYTE,TSARSST3                                                  
         NI    OVBYTE,TRSSPROG     LEAVE ONLY PROGRAM NUMBER INTACT             
         CLI   OVBYTE,TRSSMRK1     TEST MANUAL CHEQUE                           
         BE    VEND08                                                           
         CLI   STSARBTY,BT36       TEST MANUAL CHEQUE                           
         BNE   *+12                                                             
         CLI   OVBYTE,TRSSMRK2     TEST UNVOID                                  
         BE    VEND08                                                           
         CLI   STSARBTY,BT60       TEST NON ANALYSED PAYMENT                    
         BNE   VEND14                                                           
         EJECT                                                                  
VEND08   MVI   OVBYTE,0            CHEQUE MAY HAVE POSTING ACCOUNT              
         LA    R2,KEY              OVERRIDE SPAELS                              
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,COMPANY     PAYMENT BANK ACCOUNT                         
         MVC   TRNKUNT(L'TSARFSAC),TSARFSAC                                     
         MVC   TRNKCULC,ACCOUNT    ANALYSIS BANK CONTRA ACCOUNT                 
         MVC   TRNKDATE,TSARDAT    DATE                                         
         MVC   TRNKREF,TSARFDIS    FIXED REFERENCE                              
         MVI   TRNKSBR,0                                                        
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BO    VEND10                                                           
         MVC   TRNKCULA,ACCOUNT    NO - RESET BANK ACCOUNT                      
         MVC   TRNKCULC,TSARCON    VENDOR CONTRA ACCOUNT                        
         MVC   TRNKREF,TSARREF     CHEQUE NUMBER                                
         MVC   TRNKSBR,TSARSBR                                                  
VEND10   LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         B     *+8                                                              
VEND12   LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         CLC   TRNKEY(TRNKSBR-TRNRECD),KEYSAVE                                  
         BNE   VEND14                                                           
         GOTO1 AIOEXEC,IOGET+IOACCMST+IO1Q                                      
                                                                                
         L     R1,AIOBUFF                                                       
         LA    R1,TRNRFST-TRNRECD(R1)                                           
         USING TRNELD,R1                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION                             
         BNE   VEND12                                                           
         CLC   TRNTYPE,TSARBTY     TEST BATCH TYPE                              
         BNE   VEND12                                                           
         DROP  R1                                                               
         GOTO1 AEXTACC,AIOBUFF     EXTRACT SPECIAL POST A/C                     
         EJECT                                                                  
VEND14   GOTO1 ADETACC             DETAIL EXTRACTED ACCOUNTS                    
         USING CHDRECD,R2                                                       
         LA    R2,KEY              FIND CONTRA-ACCOUNT NAME                     
         MVC   CHDKEY,SPACES                                                    
         MVC   CHDKCULA,STSARCON                                                
         MVC   CHDKOFF,SPACES                                                   
         CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE PAIR             
         BNE   *+10                                                             
         XC    CHDKNULL,CHDKNULL                                                
         MVI   CHDKCACT+L'CHDKCACT-1,X'41'                                      
         LA    R1,IOHI+IOACCDIR+IO1Q                                            
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*&&US*&& GOTO1 AVALPUB,CHDRECD     VALIDATE SPECIAL PUBLICATION NUMBER          
         GOTO1 ATS2CLR             CLEAR INVOICE BUFFER                         
         B     VEND18                                                           
                                                                                
VEND16   LA    R2,KEY                                                           
         LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BNE   VEND50                                                           
VEND18   IC    R1,STSARCXL                                                      
         EX    R1,*+8                                                           
         BNE   VEND50                                                           
         CLC   CHDKCULA(0),STSARCON                                             
         CLC   CHDKCULC,SPACES     TEST CONTRA A/C OR LOWER                     
         BE    VEND16                                                           
         CLC   CHDKSPCS,SPACES     TEST CONTRA HEADER                           
         BNE   VEND22                                                           
                                                                                
VEND20   CLI   CHDKBTYP,C' '                                                    
         BH    VEND16              DON'T BOTHER WITH SPECIAL BUCKETS            
         CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE PAIR             
         BNE   *+14                                                             
         OC    CHDKNULL,CHDKNULL   TEST BUCKET RECORD                           
         BNZ   VEND16                                                           
                                                                                
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC             GET CONTRA HEADER RECORD                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETCON,AIOBUFF     EXTRACT CONTRA ACCOUNT NAME                  
         B     VEND16                                                           
         DROP  R2                                                               
         EJECT                                                                  
         USING TRNRECD,R2          TRANSACTION RECORD FOUND                     
VEND22   TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   VEND16                                                           
         TM    TRNKSTA2,TRNSPEEL                                                
         BNZ   VEND16                                                           
                                                                                
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
VEND24   GOTO1 ASETELAD,AIOBUFF    SET ELEMENT ADDRESSES                        
                                                                                
         USING TRNELD,R2                                                        
VEND30   ICM   R2,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         TM    TRNSTAT,TRNSDR                                                   
         BO    VEND32                                                           
         TM    TRNSTAT,TRNSREV     TEST REVERSED CREDIT                         
         BZ    VEND34                                                           
         ICM   RE,15,ATRXEL        IF EXCHANGE DIFFERENCE                       
         BZ    VEND16                                                           
         TM    TRXSTA1-TRXELD(RE),TRXSXDIF                                      
         BO    VEND34              MAY BE OK - LET VCREXCH DECIDE               
         B     VEND16                                                           
VEND32   TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+16                                                             
         TM    TRNSTAT,TRNSREV     TEST REVERSAL                                
         BZ    VEND16                                                           
         B     VEND34                                                           
         TM    TRNSTAT,TRNSREV     TEST REVERSAL                                
         BNZ   VEND16                                                           
VEND34   GOTO1 AVENFILT,REVEXQ     FILTER VENDOR TRANSACTION (R2,R3)            
         LTR   R2,R2               TEST TRANSACTION PASSES                      
         BZ    VEND16                                                           
                                                                                
         USING INVRECD,R3                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   VEND36                                                           
         GOTO1 AVENCR,PARM,INVRECD,TRNELD  CREDIT PROCESSING                    
         B     VEND38                                                           
VEND36   GOTO1 AVENDR,PARM,INVRECD,TRNELD  DEBIT PROCESSING                     
         DROP  R2                                                               
VEND38   GOTO1 ATS2APW,TSAPUT      PUT BACK UPDATED RECORD                      
         GOTO1 AVENDC,INVRECD                                                   
         DROP  R3                                                               
         B     VEND16                                                           
         EJECT                                                                  
***********************************************************************         
* INVOICES AND PAYMENTS INCLUDING EXCHANGE DIFFERENCES MUST BALANCE   *         
* FOR EACH ACCOUNT                                                    *         
***********************************************************************         
                                                                                
VEND50   CP    TOTDIFF,PZERO       TEST ZERO BALANCE                            
         BE    *+6                                                              
         DC    H'0'                THIS MUST NOT BE                             
*&&UK                                                                           
         CP    TOTEXDF,PZERO       TEST EXCHANGE DIFFERENCES REMOVED            
         BE    *+6                                                              
         DC    H'0'                THIS MUST NOT BE                             
*&&                                                                             
         CP    OCHQADV,PZERO       TEST ADVANCE ALL FOUND                       
         BE    *+6                                                              
         DC    H'0'                THIS MUST NOT BE                             
         GOTO1 AADDOFF             ENSURE ACCOUNTS HAVE OFFICE ENTRY            
         GOTO1 ASRTOFF                                                          
         EJECT                                                                  
         USING PSTTABD,R3                                                       
         LA    R3,PSTTAB           R3=A(POSTING TABLE)                          
         L     RF,ACLPTAB          A(CLIENT/PRODUCT TABLE) OR 0                 
         ST    RF,ACLPNOW          SAVE CURRENT A(CLPTAB) OR 0                  
         L     RF,ACOSTAB          A(COSTING TABLE) OR 0                        
         ST    RF,ACOSNOW          SAVE CURRENT A(COSTAB) OR 0                  
         USING OFFTABD,R4                                                       
VEND52   L     R4,AOFFTAB          R4=A(OFFICE TABLE)                           
                                                                                
         USING CHDRECD,R2                                                       
VEND54   LA    R2,KEY              R2=A(KEY)                                    
         MVC   CHDKEY,SPACES                                                    
         OC    PSTACC,PSTACC       TEST ACCOUNT FROM TABLE                      
         BNZ   VEND58                                                           
         ICM   RF,15,ACOSNOW       GET CURRENT COSTAB ENTRY                     
         BZ    VEND98                                                           
         USING COSTABD,RF                                                       
         MVC   CHDKCPY,COMPANY                                                  
         MVI   CHDKUNT,C'1'                                                     
         TM    PSTIND2,PST2COS2    TEST SECOND COSTING POSTING                  
         BO    VEND56                                                           
         MVI   CHDKLDG,C'C'        POST 1C/12                                   
         MVC   CHDKACT,COSACT                                                   
         B     VEND64              SKIP OFFPOS CODE FOR COSTING                 
VEND56   MVI   CHDKLDG,C'2'        POST 12/1C                                   
         MVC   CHDKACT(L'COSGRP),COSGRP                                         
         B     VEND64              SKIP OFFPOS CODE FOR COSTING                 
         DROP  RF                                                               
VEND58   MVC   LARFADDR,PSTACC                                                  
         USING ACCXD,RF                                                         
         EX    0,LARF                                                           
         OC    ACCX,ACCX           TEST ACCOUNT ENTRY USED                      
         BZ    VEND94              NO - IGNORE IT                               
         MVC   CHDKCULA,ACCX       SET ACCOUNT                                  
         IC    RE,PSTIND1                                                       
         LR    R1,R4               R1=A(OFFICE TABLE ENTRY)                     
VEND60   EX    RE,*+8              TEST DEALING WITH THIS OFFICE                
         BNZ   VEND62                                                           
         TM    OFFIND1-OFFTABD(R1),0                                            
         CLI   ACCXOPIK,0          IF NO OFFICE SUBSTITUTION                    
         BNE   VEND90                                                           
         TM    PSTIND2,PST2OPIK                                                 
         BNZ   VEND90                                                           
         LA    R1,OFFTABL(R1)      AND A MIXTURE OF OFFICES                     
         CLI   OFFTABD,FF          CHECK EVERY INDICATOR TO ENSURE              
         BE    VEND90              ALL POSTINGS ARE MADE.                       
         B     VEND60                                                           
VEND62   XR    R1,R1                                                            
         ICM   R1,1,ACCXOPIK       TEST/SET A/C KEY POSITION                    
         BZ    VEND64              READ THROUGH ACCOUNT                         
         DROP  RF                                                               
         LA    R1,CHDKACT-1(R1)                                                 
         CLI   OFFOFFC,C' '        TEST OFFICE IS NON-BLANK                     
         BNH   *+10                                                             
         MVC   0(1,R1),OFFOFFC     SET A/C OFFICE IN KEY                        
                                                                                
VEND64   CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE PAIR             
         BNE   VEND66                                                           
         TM    COMPSTA4,CPYSOFF2   TEST TWO CHARACTER OFFICES                   
         BZ    VEND66                                                           
         CLC   OFFOFFC,SPACES      TEST OFFICE IS NON-BLANK                     
         BNH   *+10                                                             
         MVC   CHDKOFF,OFFOFFC     SET A/C OFFICE IN KEY                        
                                                                                
VEND66   OC    PSTCACC,PSTCACC     TEST CONTRA IN TABLE                         
         BNZ   VEND72                                                           
         TM    PSTIND2,PST2COS1+PST2COS2 TEST COSTING                           
         BNZ   VEND68                                                           
         ICM   RF,15,ACLPNOW       GET CURRENT CLPTAB ENTRY                     
         BZ    VEND94                                                           
         USING CLPTABD,RF                                                       
         MVC   CHDKCCPY,CHDKCPY                                                 
         MVC   CHDKCUNT(L'PRODUL),PRODUL                                        
         MVC   CHDKCACT,CLPACT                                                  
         B     VEND74              SKIP OFFPOS CODE FOR PROD C/A                
VEND68   ICM   RF,15,ACOSNOW       GET CURRENT COSTAB ENTRY                     
         BZ    VEND94                                                           
         USING COSTABD,RF                                                       
         MVC   CHDKCCPY,CHDKCPY                                                 
         MVI   CHDKCUNT,C'1'                                                    
         TM    PSTIND2,PST2COS2    TEST SECOND COSTING POSTING                  
         BZ    VEND70                                                           
         MVI   CHDKCLDG,C'C'       POST 12/1C                                   
         MVC   CHDKCACT,COSACT                                                  
         B     VEND74              SKIP OFFPOS CODE FOR COSTING                 
VEND70   MVI   CHDKCLDG,C'2'       POST 1C/12                                   
         MVC   CHDKCACT(L'COSGRP),COSGRP                                        
         B     VEND74              SKIP OFFPOS CODE FOR COSTING                 
         DROP  RF                                                               
VEND72   MVC   LARFADDR,PSTCACC    SET CONTRA                                   
         USING ACCXD,RF                                                         
         EX    0,LARF                                                           
         OC    ACCX,ACCX           TEST ACCOUNT ENTRY USED                      
         BZ    VEND94              NO - IGNORE IT                               
         MVC   CHDKCULC,ACCX                                                    
         MVC   CHDKSPCS(CHDKEND-(CHDKSPCS-CHDKEY)),SPACES                       
         TM    PSTIND2,PST2OPIK                                                 
         BZ    VEND74                                                           
         XR    R1,R1                                                            
         ICM   R1,1,ACCXOPIK       TEST/SET A/C KEY POSITION                    
         BZ    VEND74                                                           
         DROP  RF                                                               
         LA    R1,CHDKCACT-1(R1)                                                
         CLI   OFFOFFC,C' '        TEST OFFICE IS NON-BLANK                     
         BNH   *+10                                                             
         MVC   0(1,R1),OFFOFFC     SET A/C OFFICE IN KEY                        
                                                                                
VEND74   CLI   FILEFORM,ISDAQ      TEST I/S DIRECT ACCESS FILE PAIR             
         BNE   *+10                                                             
         XC    CHDKNULL,CHDKNULL                                                
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BNE   VEND90              MAY NOT ACTUALLY FIND ACCOUNT                
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETCON,AIOBUFF     EXTRACT CONTRA ACCOUNT NAME                  
                                                                                
         USING TRNRECD,R2                                                       
         GOTO1 VDATCON,DMCB,(2,FILTCHD),(1,TRNKDATE)                            
         MVC   TRNKREF,FILTCHN                                                  
         MVI   TRNKSBR,0                                                        
         MVC   STRNKEY,TRNKEY      SAVE TRANSACTION KEY                         
         LA    R1,IOHIGH+IOACCDIR+IO1Q                                          
         B     *+8                                                              
VEND76   LA    R1,IOSEQ+IOACCDIR+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         LA    R2,KEY                                                           
         CLC   TRNKEY(TRNKSBR-TRNRECD),STRNKEY                                  
         BNE   VEND90                                                           
                                                                                
         TM    TRNKSTAT,TRNSDELT+TRNSDRFT                                       
         BNZ   VEND76                                                           
         TM    TRNKSTA2,TRNSPEEL                                                
         BNZ   VEND76                                                           
         LA    RF,BOQ              EXCLUDE REVERSALS IF VOIDING                 
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    *+8                                                              
         LA    RF,BZQ              REVERSALS ONLY IF UNVOIDING                  
         TM    TRNKSTAT,TRNSREVS                                                
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   VEND76                                                           
         LA    R1,IOGET+IOACCMST+IO1Q                                           
         TM    TRNKSTAT,TRNSARCH                                                
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q                                           
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF                                                 
*&&UK                                                                           
         USING AFCELD,R1                                                        
         ZAP   DUB2,PZERO                                                       
         ICM   R1,15,AAFCEL                                                     
         BZ    *+10                                                             
         ZAP   DUB2,AFCAMNT                                                     
*&&                                                                             
         USING TRNELD,R1                                                        
         ICM   R1,15,ATRNEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 ATCLOSE                                                          
         BNE   VEND76                                                           
         CLC   TRNTYPE,STSARBTY    TEST TRNTYPE MATCHES BANK TRNTYPE            
         BNE   VEND76                                                           
         ZAP   DUB,TRNAMNT                                                      
         TM    TRNSTAT,TRNSDR      DEBIT POSTING - REVERSE SIGN                 
         BZ    VEND78                                                           
         MP    DUB,PONENEG                                                      
*&&UK*&& MP    DUB2,PONENEG                                                     
                                                                                
         USING TRSELD,R1                                                        
VEND78   ICM   R1,15,ATRSEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   TRSLN,TRSLNQ                                                     
         BL    *+12                                                             
         CLI   TRSMARK,TRSMUMQ+TRSMSBVQ  TEST UNVOIDED DISCOUNT                 
         BE    VEND76              DON'T WANT THIS                              
                                                                                
         TM    OCHQIND,OCHQIADV+OCHQIDIF                                        
         BZ    VEND80              PAYMENT HAS NO ADVANCE/DIFFERENCE            
         USING TRXELD,R1                                                        
         ICM   R1,15,ATRXEL        TEST ADVANCE/MINOR DIFFERENCE                
         BZ    VEND80                                                           
         TM    TRXSTA3,TRXSDIFF    MINOR DIFFERENCE?                            
         BO    VEND76              YES - HANDLE ELSEWHERE                       
         TM    TRXSTA3,TRXSADVC    ADVANCE?                                     
         BZ    VEND80                                                           
         USING MPYELD,R1                                                        
         ICM   R1,15,AMPYEL        TEST STILL BELONGS                           
         BZ    VEND76                                                           
         CLC   MPYNO,SPACES                                                     
         BH    VEND80              NO - PAID BY ANOTHER CHEQUE                  
         B     VEND76              YES - HANDLE ELSEWHERE                       
                                                                                
         USING SPAELD,R1                                                        
VEND80   ICM   R1,15,ASPACOSG      SET COSTAB A/CS IF SPATCOSG SPAEL            
         BZ    VEND86                                                           
         USING COSTABD,RF                                                       
         L     RF,ACOSTAB                                                       
         LA    R0,COSTABN                                                       
VEND82   OC    COSACT,COSACT       TEST FREE SLOT                               
         BZ    VEND84                                                           
         CLC   COSACT,SPAAACT      TEST ALREADY HAVE THIS PAIR                  
         BNE   *+14                                                             
         CLC   COSGRP,SPACGRP                                                   
         BE    VEND86                                                           
         LA    RF,COSTABL(RF)                                                   
         BCT   R0,VEND82                                                        
         DC    H'0'                NEED TO EXTEND COSTAB                        
VEND84   MVC   COSGRP,SPACGRP      SET VALUES FOR THIS PAIR                     
         MVC   COSACT,SPAAACT                                                   
         DROP  R1,RF                                                            
VEND86   MVC   LAREADDR,PSTTOTS                                                 
         EX    0,LARE                                                           
         LTR   RE,RE                                                            
         BZ    VEND88                                                           
         SP    0(L'TOTDISC,RE),DUB                                              
*&&UK                                                                           
         LA    R0,TOTALS                                                        
         CR    RE,R0                                                            
         BL    VEND88                                                           
         LA    R0,CURTOTS                                                       
         CR    RE,R0                                                            
         BNL   VEND88                                                           
         SP    CURTOTS-TOTALS(L'CURDISC,RE),DUB2                                
*&&                                                                             
                                                                                
VEND88   MVC   STRNKEY,KEY         SAVE KEY WE HAVE REACHED                     
         GOTO1 APOST,POSTISTD                                                   
         MVC   KEY,STRNKEY         RE-READ FOR SEQUENCE                         
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    VEND76                                                           
         DC    H'0'                                                             
                                                                                
         USING ACCXD,RF                                                         
VEND90   OC    PSTACC,PSTACC       TEST ACCOUNT FROM TABLE                      
         BZ    VEND94              NO OFFPOS CODE                               
         MVC   LARFADDR,PSTACC                                                  
         EX    0,LARF                                                           
         CLI   ACCXOPIK,0          TEST A/C KEY POSITION                        
         BNE   VEND92                                                           
         OC    PSTCACC,PSTCACC     TEST CONTRA ACCOUNT FROM TABLE               
         BZ    VEND94              NO OFFPOS CODE                               
         TM    PSTIND2,PST2OPIK                                                 
         BZ    VEND94                                                           
         MVC   LARFADDR,PSTCACC                                                 
         EX    0,LARF                                                           
         CLI   ACCXOPIK,0                                                       
         BE    VEND94                                                           
         DROP  RF                                                               
VEND92   LA    R4,OFFTABL(R4)      NEXT ENTRY IN TABLE                          
         CLI   OFFTABD,EOT         TEST EOT                                     
         BE    VEND94                                                           
         CLI   OFFTABD,FF          TEST EOT                                     
         BNE   VEND54              DO FOR NEXT OFFICE                           
                                                                                
         USING CLPTABD,RF                                                       
VEND94   OC    PSTCACC,PSTCACC     TEST CONTRA IN TABLE                         
         BNZ   VEND98                                                           
         TM    PSTIND2,PST2COS1+PST2COS2 TEST COSTING                           
         BNZ   VEND96                                                           
         ICM   RF,15,ACLPNOW       TEST CLPTAB IN USE                           
         BZ    VEND98                                                           
         LA    RF,CLPTABL(RF)      UPDATE CURRENT CLPTAB POINTER                
         ST    RF,ACLPNOW                                                       
         OC    CLPACT,CLPACT       TEST NO MORE ENTRIES                         
         BNZ   VEND52                                                           
         MVC   ACLPNOW,ACLPTAB     RESET A(CURRENT CLPTAB ENTRY)                
         B     VEND98                                                           
         USING COSTABD,RF                                                       
VEND96   ICM   RF,15,ACOSNOW       TEST COSTAB IN USE                           
         BZ    VEND98                                                           
         LA    RF,COSTABL(RF)      UPDATE CURRENT COSTAB POINTER                
         ST    RF,ACOSNOW                                                       
         OC    COSACT,COSACT       TEST NO MORE ENTRIES                         
         BNZ   VEND52                                                           
         MVC   ACOSNOW,ACOSTAB     RESET A(CURRENT CLPTAB ENTRY)                
         B     VEND98                                                           
         DROP  RF                                                               
VEND98   LA    R3,PSTTABL(R3)      NEXT POSTING ENTRY                           
         CLI   PSTIND1,UPDIEXDF                                                 
         BE    VEND94                                                           
         CLI   PSTIND1,EOT         TEST EOT                                     
         BNE   VEND52                                                           
                                                                                
         CLI   XACTION,ACTDRFT     TEST DRAFT                                   
         BE    VENDORX             NO NEED TO TEST DISCOUNT ETC.                
                                                                                
         CP    TOTDISC,PZERO       TEST ALL DISCOUNT ADJUSTED                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    TOTDTAX,PZERO       TEST ALL TAX ADJUSTED                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CP    TOTBCHA,PZERO       TEST ALL BANK CHARGES ADJUSTED               
         BE    *+6                                                              
         DC    H'0'                SAME DATE/REF, DIFFERENT BANK A/C?           
         B     VENDORX                                                          
                                                                                
VENDORX  B     OVROUTX                                                          
                                                                                
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS VENDOR CREDIT                                               *         
* NTRY - P1 A(INVOICE TSAR 2 RECORD), P2 A(TRNEL)                     *         
***********************************************************************         
                                                                                
VENCR    L     R3,0(R1)            CREDIT PROCESSING                            
         USING INVRECD,R3                                                       
         L     R2,4(R1)                                                         
         USING TRNELD,R2                                                        
                                                                                
VENCR02  ZAP   INVDSC,PZERO        DETAIL INVOICE TABLE ENTRY                   
*&&UK*&& ZAP   INVDSCC,PZERO                                                    
         TM    UPDIND1,UPDICDSC    TEST POST DISCOUNT                           
         BZ    VENCR18                                                          
         ICM   R1,15,ASCICDSC                                                   
         BZ    VENCR18                                                          
         USING SCIELD,R1                                                        
         ZAP   INVDSC,SCIAMNT      AMOUNT OF DISCOUNT                           
*&&UK                                                                           
         CLI   SCILN,SCILN2Q                                                    
         BL    *+14                                                             
         ZAP   INVDSCC,SCIADMN                                                  
         B     VENCR04                                                          
         ZAP   INVDSCC,TRNAMNT                                                  
         BZ    VENCR04                                                          
         ZAP   PKWK16A,SCIAMNT                                                  
         DROP  R1                                                               
         ICM   R1,15,AAFCEL                                                     
         BZ    VENCR04                                                          
         USING AFCELD,R1                                                        
         MP    PKWK16A,AFCAMNT                                                  
         DROP  R1                                                               
         SRP   PKWK16A,RNDING,0                                                 
         DP    PKWK16A,TRNAMNT                                                  
         ZAP   PKWK16B,PKWK16A(L'PKWK16A-L'TRNAMNT)                             
         SRP   PKWK16B,64-RNDING,5                                              
         ZAP   INVDSCC,PKWK16B                                                  
                                                                                
         USING CLPTABD,RF                                                       
VENCR04  ICM   RF,15,ACLPTAB       TEST INCOME BY CLI/PRO (GERMANY)             
         BZ    VENCR10                                                          
         ICM   R1,15,ASORPROD      TEST/SET A(ACC PRODUCTION SOURCE)            
         BZ    VENCR10                                                          
         USING SORELD,R1                                                        
         LA    RE,CLPTABN          FIND/ADD THIS CLIENT/PRODUCT                 
         IC    R4,PRODBLEN                                                      
         BCTR  R4,0                R4=EXECUTE L'CLIENT/PRODUCT                  
VENCR06  OC    CLPACT,CLPACT       TEST EMPTY SLOT                              
         BNZ   VENCR08                                                          
         MVC   CLPACT,SPACES       SPACE FILL                                   
         EX    R4,*+8                                                           
         B     VENCR10                                                          
         MVC   CLPACT(0),SORAACT   SET CLIENT/PRODUCT                           
VENCR08  EX    R4,*+8                                                           
         BE    VENCR10                                                          
         CLC   CLPACT(0),SORAACT   TEST CLIENT/PRODUCT FOUND                    
         LA    RF,CLPTABL(RF)                                                   
         BCT   RE,VENCR06                                                       
         DC    H'0'                NEED TO EXTEND CLPTAB                        
         DROP  RF                                                               
                                                                                
VENCR10  TM    UPDIND1,UPDIDTAX    TEST POST DISCOUNT TAX ADJUSTMENT            
         BZ    VENCR18                                                          
         MVC   INVRAT,=H'1600'     DEFAULT OR EXTRACT RATE OF TAX               
         ICM   R1,15,ARAVEL                                                     
         BZ    *+10                                                             
         MVC   INVRAT,RATRATE-RATELD(R1)                                        
                                                                                
         ICM   R1,15,ASPAITAX                                                   
         BZ    VENCR18                                                          
         USING SPAELD,R1                                                        
         CLC   SPAAUNT(L'ACTKUNT+L'ACTKLDG),DTAX+(ACTKUNT-ACTRECD)              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMP(L'COMPANY),COMPANY                                          
         MVC   TEMP+L'COMPANY(L'SPAAULA),SPAAULA                                
         CLI   DTAXOPIK,0                                                       
         BE    VENCR12                                                          
         SR    RE,RE                                                            
         IC    RE,DTAXOPIK                                                      
         LA    RE,TEMP+(ACTKACT-ACTRECD-1)(RE)                                  
         MVI   0(RE),C' '                                                       
                                                                                
VENCR12  CLC   DTAX,TEMP                                                        
         BE    VENCR18                                                          
         LA    RF,TAX1                                                          
         USING ACCXD,RF                                                         
         LA    RE,TAXTABN                                                       
VENCR14  OC    ACCX,ACCX           TEST FREE ENTRY                              
         BZ    VENCR16                                                          
         CLC   ACCX,TEMP                                                        
         BE    VENCR18                                                          
         LA    RF,ACCXL(RF)                                                     
         BCT   RE,VENCR14                                                       
         DC    H'0'                                                             
                                                                                
VENCR16  MVC   ACCX,TEMP                                                        
         MVC   ACCXOPIK,DTAXOPIK                                                
*&&                                                                             
VENCR18  TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BZ    VENCR20                                                          
         ICM   R1,15,AMPYEL        MPYEL SBR IS DR SBR                          
         BNZ   *+6                                                              
         DC    H'0'                MUST HAVE ONE                                
         USING MPYELD,R1                                                        
         CLI   MPYLN,MPYLN2Q                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         MVC   INVSUBDR,MPYSUB     SAVE IN INVOICE TABLE                        
         DROP  R1                                                               
                                                                                
VENCR20  OI    INVIND2,INVCR                                                    
         L     R0,AIOSAVE2         R0=A(IOAREA2)                                
         LA    R1,IOALQ                                                         
         L     RE,AIOSAVE1                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY IO1 TO IO2                              
                                                                                
VENCRX   B     OVROUTE                                                          
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS VENDOR DEBIT                                                *         
***********************************************************************         
                                                                                
         PUSH  USING                                                            
VENDR    L     R3,0(R1)            DEBIT PROCESSING                             
         USING INVRECD,R3                                                       
         L     R2,4(R1)                                                         
         USING TRNELD,R2                                                        
*&&UK                                                                           
         ZAP   INVAMNTC,PZERO                                                   
         ICM   R1,15,AAFCEL                                                     
         BZ    *+10                                                             
         ZAP   INVAMNTC,AFCAMNT-AFCELD(L'AFCAMNT,R1)                            
*&&                                                                             
                                                                                
         OC    TRNOFFC,SPACES      ENSURE MINIMUM OFFICE IS SPACES              
         MVC   INVOFFC,TRNOFFC                                                  
         ZAP   INVAMNT,TRNAMNT                                                  
         OI    INVIND2,INVDR                                                    
         USING TRNRECD,KEY                                                      
         MVC   INVDREF,TRNKREF                                                  
         MVC   INVSUBDR,TRNKSBR                                                 
VENDRX   B     OVROUTE                                                          
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* MATCH, DR AND CR FOUND                                              *         
* NTRY  R1 - A(INVREC)                                                *         
***********************************************************************         
                                                                                
         USING INVRECD,R3                                                       
VENDC    LR    R3,R1                                                            
         TM    INVIND2,INVCR+INVDR TEST CR & DR FOUND                           
         BNO   VENDCX                                                           
         MVC   OVKEY2,KEY                                                       
         SP    TOTDIFF,INVAMNT     ADJUST DIFFERENCE                            
         AP    TOTDIFF,INVDSC                                                   
         AP    TOTDISC,INVDSC      ADJUST DISCOUNT                              
*&&UK                                                                           
         SP    CURDIFF,INVAMNTC                                                 
         AP    CURDIFF,INVDSCC                                                  
         AP    CURDISC,INVDSCC                                                  
*&&                                                                             
         USING OFFTABD,R1                                                       
         L     R1,AOFFTAB          LOCATE MATCHING OFFICE OR FREE SLOT          
VENDC02  CLI   OFFTABD,FF                                                       
         BNE   *+6                                                              
         DC    H'0'                NO ROOM FOR THIS OFFICE                      
         CLI   OFFTABD,0                                                        
         BE    VENDC04                                                          
         CLC   OFFOFFC,INVOFFC                                                  
         BE    VENDC04                                                          
         LA    R1,OFFTABL(R1)                                                   
         B     VENDC02                                                          
                                                                                
VENDC04  ST    R1,OVFULL           KEEP OFFICE TABLE ADDRESS                    
         MVC   OFFOFFC,INVOFFC     SET OFFICE CODE                              
                                                                                
         OI    OFFIND1,OFFIBCHA                                                 
         TM    VOIDIND1,VOIDBCHA   TEST VOIDING BANK CHARGES                    
         BNZ   *+8                                                              
         NI    OFFIND1,FF-OFFIBCHA                                              
                                                                                
         TM    UPDIND1,UPDICDSC    TEST POST DISCOUNT                           
         BZ    VENDC06                                                          
         ZAP   PL13,INVDSC                                                      
         BZ    VENDC06                                                          
         OI    OFFIND1,OFFIDISC                                                 
*&&UK                                                                           
         TM    UPDIND1,UPDIDTAX    TEST POST DISCOUNT TAX ADJUSTMENT            
         BZ    VENDC06                                                          
         OC    INVRAT,INVRAT                                                    
         BZ    VENDC06                                                          
         OI    OFFIND1,OFFIDTAX                                                 
         GOTO1 ATAXADJ,PARM,INVRAT,INVDSC                                       
         AP    TOTDTAX,0(6,R1)     ADD TO TAX ADJUSTMENT TOTAL                  
         SP    TOTDISC,0(6,R1)     ADJUST DISCOUNT                              
         GOTO1 ATAXADJ,PARM,INVRAT,INVDSCC                                      
         SP    CURDISC,0(6,R1)     ADJUST CURRENCY DISCOUNT                     
*&&                                                                             
VENDC06  ICM   R1,15,AICOTAB       POST INTO ICOTAB IF NECESSARY                
         BZ    VENDC10                                                          
         USING ICOTABD,R1                                                       
         B     *+8                                                              
VENDC08  LA    R1,ICOTABL(R1)      NO - NEXT ENTRY                              
         CLI   ICOTABD,EOT         TEST EOT                                     
         BE    VENDC10                                                          
         CLC   ICOOFFC,INVOFFC     TEST OFFICE MATCHES                          
         BNE   VENDC08                                                          
         AP    ICOAMNT,INVAMNT     ADD INVOICE AMOUNT                           
         SP    ICOAMNT,INVDSC      MINUS DISCOUNT                               
         DROP  R1                                                               
                                                                                
VENDC10  LA    R2,KEY              READ CREDIT INTO IO2 AND LOCK                
         USING TRNRECD,R2                                                       
         MVC   TRNKREF,INVKREF                                                  
         MVC   TRNKSBR,INVSUB                                                   
         DROP  R2                                                               
                                                                                
         XC    OVCRDA,OVCRDA       CLEAR SAVED CR DISK ADDRESS                  
                                                                                
         GOTO1 AREVCR,INVSUBDR                                                  
                                                                                
         CP    INVAMNT,OVAMNT       DIFFERENCE BETWEEN CR AND DR                
         BE    VENDC12                                                          
*&&UK                                                                           
         AP    TOTEXDF,INVAMNT                                                  
         SP    TOTEXDF,OVAMNT                                                   
         ZAP   CUREXDF,TOTEXDF                                                  
                                                                                
         ICM   RE,15,OVFULL                                                     
         OI    OFFIND1-OFFTABD(RE),OFFIEXDF                                     
         TM    UPDIND1,UPDIEXDF                                                 
         BNZ   VENDC12                                                          
*&&                                                                             
         DC    H'0'                EXCH DIFF. NOT ALLOWED                       
                                                                                
VENDC12  LA    R2,KEY              READ DEBIT INTO IO1                          
         USING TRNRECD,R2                                                       
         MVC   TRNKREF,INVDREF                                                  
         MVC   TRNKSBR,INVSUBDR                                                 
         GOTO1 ATS2DEL             DELETE CURRENT TSAR 2 RECORD                 
         LA    R1,IOREAD+IOACCDIR+IO1Q                                          
         GOTO1 AIOEXEC                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R1,IOGET+IOACCMST+IO1Q  READ DATA RECORD                         
         TM    TRNKSTAT,TRNSARCH   TEST RECORD ON ARCHIVE                       
         BZ    *+8                                                              
         LA    R1,IOGET+IOACCARC+IO1Q  READ DATA RECORD                         
         GOTO1 AIOEXEC                                                          
         BE    VENDC14                                                          
         DC    H'0'                                                             
         DROP  R2                                                               
                                                                                
VENDC14  MVC   AIOSAVE,AIOSAVE1    SET IOSAVE IS IOSAVE1                        
         MVC   AIOBUFF,AIOBUFF1    SET IOBUFF IS IOBUFF1                        
         GOTO1 APOST,POSTISTD      ADD A -DR (VOID) OR A DR (UNVOID)            
                                                                                
         CLI   XACTION,ACTDRFT     TEST LIVE UPDATE                             
         BE    VENDC16                                                          
         OC    OVCRDA,OVCRDA       TEST CREDIT NEEDS UPDATING                   
         BZ    VENDC16                                                          
         MVC   IODA,OVCRDA         GET SAVED CREDIT                             
         GOTO1 AIOEXEC,IOGETRUP+IOACCMST+IO2Q                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ASETELAD,AIOBUFF2                                                
         USING MPYELD,R2                                                        
         L     R2,AMPYEL           R2=A(CR PAYMENT ELEMENT - MPYLN2Q)           
         L     R1,AIOBUFF1         R1=A(NEW DR TRANSACTION)                     
         MVC   MPYSUB,TRNKSBR-TRNRECD(R1)                                       
         GOTO1 AIOEXEC,IOPUTREC+IOACCMST+IO2Q                                   
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
VENDC16  MVC   KEY,OVKEY2                                                       
         LA    R1,IOREAD+IOACCDIR+IO1Q  READ TO RE-ESTABLISH SEQUENCE           
         GOTO1 AIOEXEC                                                          
         BE    VENDCX              AND RETURN TO READ SEQUENTIAL                
         DC    H'0'                                                             
                                                                                
VENDCX   B     OVROUTE             RETURN                                       
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER VENDOR CREDITS/DEBITS                             *         
*                                                                     *         
* NTRY - R1 - IGNEXQ - RETURN TRANSACTION FAIL IF EXCHANGE DIFFERENCE *         
*             REVEXQ - REVERSE EXCHANGE DIFFERENCE                    *         
*                                                                     *         
* EXIT - R2=ZERO     - IF TRANSACTION FAILS OR                        *         
*           A(TRNEL) - IF TRANSACTION PASSES, IN WHICH CASE           *         
*        R3=A(RELEVANT MATCHING INVREC) OR                            *         
*           A(NEW INVREC)                                             *         
***********************************************************************         
                                                                                
         USING TRNELD,R2                                                        
         USING INVRECD,R3                                                       
         USING TRNRECD,R4                                                       
VENFILT  ICM   R2,15,ATRNEL        R2=A(TRANSACTION ELEMENT) OR 0               
         BZ    VENFILTX                                                         
         L     R4,AIOBUFF                                                       
*&&UK                                                                           
         TM    CHQINDS,CHQIARTQ    TEST ARTIST CHEQUE                           
         BZ    *+14                                                             
*&&                                                                             
*        CP    TRNAMNT,PZERO       TEST ZERO AMOUNT 'HISTORY'                   
*        BE    VENFILTN                                                         
                                                                                
         GOTO1 ATCLOSE             TEST TRANS IS CLOSED (USING OFFAL)           
         BNE   VENFILTN                                                         
                                                                                
         TM    OCHQIND,OCHQIADV+OCHQIDIF                                        
         BZ    VENF06              PAYMENT HAS NO ADVANCE/DIFFERENCE            
         USING TRXELD,RE                                                        
         ICM   RE,15,ATRXEL        TEST ADVANCE/MINOR DIFFERENCE                
         BZ    VENF06                                                           
         TM    TRXSTA3,TRXSDIFF    TEST MINOR DIFFERENCE                        
         BO    VENFILTN            YES - HANDLE ELSEWHERE                       
         TM    TRXSTA3,TRXSADVC    TEST ADVANCE                                 
         BZ    VENF06                                                           
         CLC   FILTCHN,TRNREF      TEST SAME CHEQUE NUMBER                      
         BNE   VENF06                                                           
         CLC   OCHQDATP,TRNDATE    TEST SAME CHEQUE DATE                        
         BNE   VENF06                                                           
         USING MPYELD,RE                                                        
         ICM   RE,15,AMPYEL        TEST STILL BELONGS                           
         BZ    *+14                                                             
         CLC   MPYNO,SPACES                                                     
         BH    VENF06              NO - PAID BY ANOTHER CHEQUE                  
         USING TRSELD,R1                                                        
         L     R1,ATRSEL                                                        
         TM    TRSSTAT,TRSSOFFS    EXCLUDE CONTRA'D ADVANCE                     
         BO    VENFILTN                                                         
         TM    VOIDIND1,VOIDVOID   TEST VOID/UNVOID                             
         BZ    VENF02                                                           
         TM    TRSSTAT,TRSSVOID    VOID - TEST NOT VOID                         
         BO    VENFILTN                                                         
         TM    TRNSTAT,TRNSREVS           TEST NOT REVERSED                     
         BO    VENFILTN                                                         
         B     VENF04                                                           
VENF02   TM    TRSSTAT,TRSSVOID    UNVOID - TEST VOID                           
         BZ    VENFILTN                                                         
         TM    TRNSTAT,TRNSREVS             TEST REVERSED                       
         BZ    VENFILTN                                                         
VENF04   SP    OCHQADV,TRNAMNT     ACCOUNT FOR ADVANCE WHICH BELONGS            
         B     VENFILTN            AND HANDLE ELSEWHERE                         
                                                                                
VENF06   TM    VOIDIND1,VOIDVOID   TEST VOID                                    
         BZ    *+12                                                             
         TM    TRNRSTA2,TRNSUSED   YES - USED ITEMS ONLY                        
         BZ    VENFILTN                                                         
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BO    VENF24                                                           
*&&UK                                                                           
         USING TRXELD,RE                                                        
         ICM   RE,15,ATRXEL        IF EXCHANGE DIFF CR                          
         BZ    VENF08                                                           
         TM    TRXSTA1,TRXSXDIF                                                 
         BZ    VENF08                                                           
         SH    R1,=AL2(IGNEXQ)     RETURN IF IGNORE EXCHANGE SET                
         BZ    VENFILTN                                                         
         DROP  RE                                                               
         GOTO1 AVCREXCH,TRNELD     REVERSE THIS EXCHANGE DIFFERENCE             
         B     VENFILTN                                                         
*&&                                                                             
         USING TRSELD,R1                                                        
VENF08   TM    TRNSTAT,TRNSREV     DROP REVERSED CREDIT                         
         BNZ   VENFILTN                                                         
         ICM   R1,15,ATRSEL        R1=A(TRSEL)                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,BZQ              VOID - SYSTEM CHEQUES ONLY                   
         TM    VOIDIND1,VOIDUVOI                                                
         BZ    VENF10                                                           
         OC    TRSUDAT,TRSUDAT     UNVOID - SKIP USED ITEMS &                   
         BNZ   VENFILTN                                SYSTEM CHEQUES           
         LA    RF,BOQ                                                           
                                                                                
VENF10   CLI   STSARBTY,BT36       TEST MANUAL CHEQUE                           
         BNE   *+12                                                             
         TM    TRSSTAT2,TRSSMCHQ   TEST PAID BY MANUAL CHEQUE                   
         B     *+8                                                              
         TM    TRSSTAT,TRSSACHQ    TEST PAID BY SYSTEM CHEQUE                   
         EX    RF,*+8                                                           
         B     *+8                                                              
         NOP   VENFILTN                                                         
         TM    VOIDIND1,VOIDUVOI   UNVOID - DON'T TEST PAYMENT BITS             
         BO    VENF12                                                           
         TM    VOIDIND1,VOIDANBP   TEST ANALYSED BANK POSTINGS                  
         BZ    VENF12                                                           
         TM    TRSSTAT2,TRSSANBP   TEST ANALYSED PAYMENT                        
         BZ    VENFILTN                                                         
VENF12   LA    R0,X'F0'                                                         
         XR    RF,R0               SWAP CC BZ->BO OR BO->BZ                     
         TM    TRSSTAT,TRSSVOID    TEST VOID ITEM                               
         EX    RF,*+8              IF ACTION IS VOID, SKIP VOID ITEMS           
         B     *+8                                                              
         NOP   VENFILTN            IF ACTION IS UNVOID, KEEP VOID ITEMS         
         DROP  R1                                                               
                                                                                
         LA    R3,TSA2REC                                                       
         LA    R1,TSAGET                                                        
         B     *+8                                                              
VENF14   LA    R1,TSANXT                                                        
         GOTO1 ATS2GET,(R1)        GET FIRST/NEXT TSAR 2 RECORD                 
         BNE   VENF18              NOT FOUND - ADD NEW                          
         TM    INVIND2,INVDR                                                    
         BZ    VENF14                                                           
         CLC   INVKREF,TRNKREF                                                  
         BNE   VENF14                                                           
         CLC   INVSUB,TRNSUB                                                    
         BNE   VENF14                                                           
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BZ    VENF16                                                           
         USING MPYELD,RE                                                        
         ICM   RE,15,AMPYEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO MPYEL                                     
         CLI   MPYLN,MPYLN2Q       MUST CARRY DR SUB REFERENCE ON CR            
         BNL   *+6                                                              
         DC    H'0'                SHORT MPYEL                                  
         CLC   INVSUBDR,MPYSUB                                                  
         BE    VENFILTX                                                         
         B     VENFILTN                                                         
         DROP  RE                                                               
                                                                                
VENF16   GOTO1 AVALCQD             VALIDATE CHEQUE DETAILS (MPYEL)              
         BNE   VENFILTN                                                         
         B     VENFILTX                                                         
                                                                                
VENF18   MVC   INVKREF,TRNKREF     READ FOR EXISTING REDUNDANT RECORD           
         MVC   INVSUB,TRNSUB                                                    
         XC    WORK(INVKEYL+L'INVDREF),WORK                                     
         MVC   WORK(INVKEYL),INVRECD                                            
         GOTO1 ATS2GET,TSARDH                                                   
         LA    R1,TSAPUT           SET TO REPLACE WITH NEW RECORD               
         BE    VENF19              IF READ FOUND ONE, OTHERWISE                 
         LA    R1,TSAADD           SET TO ADD NEW RECORD                        
         ST    R1,OVFULL           SAVE TSAR ACTION                             
         ICM   RE,15,AFFTCREF                                                   
         BZ    VENF19                                                           
         LA    RE,FFTDATA-FFTELD(RE)                                            
         MVC   WORK+INVKEYL(L'INVDREF),0(RE)       MOVE TRUE                    
VENF19   ST    R1,OVFULL                                                        
         XC    TSA2REC,TSA2REC     CLEAR AND (RE)BUILD RECORD                   
         MVC   INVRECD(INVKEYL+L'INVDREF),WORK                                  
*&&UK*&& ZAP   INVEXCH,PZERO                                                    
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID                                  
         BZ    VENF20                                                           
         USING MPYELD,RE                                                        
         ICM   RE,15,AMPYEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                NO MPYEL                                     
         CLI   MPYLN,MPYLN2Q       MUST CARRY DR SUB REFERENCE ON CR            
         BNL   *+6                                                              
         DC    H'0'                SHORT MPYEL                                  
         MVC   INVSUBDR,MPYSUB                                                  
         DROP  RE                                                               
         B     VENF22                                                           
VENF20   GOTO1 AVALCQD             VALIDATE CHEQUE DETAILS (MPYEL)              
         BE    VENF22                                                           
         XC    TSA2REC,TSA2REC     CLEAR CURRENT TSAR 2 RECORD                  
         B     VENFILTN            DROP CREDIT                                  
VENF22   L     R1,OVFULL           ADD/PUT NEW CREDIT                           
         GOTO1 ATS2APW,(R1)                                                     
         B     VENFILTX                                                         
                                                                                
         USING BTYPTABD,R1                                                      
VENF24   L     R1,ABTYPTAB         TEST VENDOR DEBIT HAS VALID TRNTYPE          
         B     *+8                                                              
VENF26   LA    R1,BTYPTABL(R1)                                                  
         CLI   BTYPTABD,EOT        TEST EOT                                     
         BE    VENFILTN            NO MATCH FOUND                               
         CLC   BTYPBANK,STSARBTY   FIND BANK TRNTYPE                            
         BNE   VENF26                                                           
         CLI   BTYPCTRY,0          TEST COUNTRY RESTRICTION                     
         BE    *+14                                                             
         CLC   BTYPCTRY,AGYCTRY    TEST CORRECT COUNTRY                         
         BNE   VENF26                                                           
         CLC   BTYPVEND,TRNTYPE    TEST VENDOR DEBIT TRNTYPE                    
         BNE   VENF26                                                           
         XR    RE,RE                                                            
         ICM   RE,3,BTYPYPRG                                                    
         BZ    VENF30                                                           
         A     RE,APROGS                                                        
         ICM   RF,15,ATRSEL        RF=A(TRSEL)                                  
         BNZ   *+6                                                              
         DC    H'0'                NO DEBIT STATUS ELEMENT                      
         MVC   OVBYTE,TRSSTAT3-TRSELD(RF)                                       
         NI    OVBYTE,TRSSPROG     LEAVE ONLY PROGRAM NUMBER INTACT             
VENF28   CLI   0(RE),EOT           TEST EOT                                     
         BE    VENF26                                                           
         CLC   OVBYTE,0(RE)        TEST CORRECT PROGRAM                         
         BE    VENF30                                                           
         LA    RE,1(RE)                                                         
         B     VENF28                                                           
         DROP  R1                                                               
                                                                                
VENF30   GOTO1 AVALCQD             VALIDATE CHEQUE DETAILS (MPYEL)              
         BNE   VENFILTN                                                         
                                                                                
                                                                                
         LA    R3,TSA2REC                                                       
         LA    R1,TSAGET                                                        
         B     *+8                                                              
VENF32   LA    R1,TSANXT                                                        
         GOTO1 ATS2GET,(R1)        GET FIRST/NEXT TSAR 2 RECORD                 
         BNE   VENF34              NOT FOUND - ADD NEW                          
         USING MPYELD,R1                                                        
         ICM   R1,15,AMPYEL        TEST ELEMENT CARRIES SUB REFERENCE           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   MPYLN,MPYLN2Q                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
* BELOW CODE IS COMMNETED, AS IT WAS NOT CONSIDERING ALL SCENARIO               
* WHICH CAN COMM FOR VOIDING OF CHECK, WHERE WE HAVE SPECIAL KEY                
* REFENCE NUMBER WITH '*'.                                                      
* INSTEAD WE HAVE INCLULDED NEW LOGIC                                           
*        ICM   RE,15,AFFTKREF                                                   
*        BZ    *+12                                                             
*        LA    RE,FFTDATA-FFTELD(RE)                                            
*        B     *+8                                                              
*                                                                               
*PUTTING A LOGIC TO VALIDATE 'CR' AND 'DR' SUB REFRENCE ARE CONSECUTIVE         
* IF NOT AND WE HAVE '*' IN REFERENCE NUMBER THEN USE NEW LOGIC                 
*                                                                               
         TM    VOIDIND1,VOIDUVOI                                                
         BO    VENF32A                                                          
         LLC   RE,TRNKSBR                                                       
         SHI   RE,1                                                             
         CLM   RE,1,MPYSUB                                                      
         BE    VENF32A                                                          
         CLI   TRNKREF+3,C'*'                                                   
         BNE   VENF32A                                                          
         CLI   TRNKREF+4,C'0'     | 4 AND 5 BYTE SHOULD BE                      
         BL    VENF32A            |     >= 0                                    
         CLI   TRNKREF+5,C'0'     |                                             
         BNL   VENF33                                                           
*                                                                               
VENF32A  LA    RE,TRNKREF                                                       
         CLC   INVKREF,0(RE)                                                    
         BNE   VENF32                                                           
         TM    INVIND2,INVCR                                                    
         BZ    VENF32                                                           
         CLC   INVSUB,MPYSUB                                                    
         BNE   VENF32                                                           
         TM    VOIDIND1,VOIDUVOI   TEST UNVOID HAS MATCHING MPY SUB REF         
         BZ    VENF36                                                           
         CLC   INVSUBDR,TRNSUB                                                  
         BE    VENF36                                                           
         B     VENFILTN                                                         
*                                                                               
VENF33   PACK  DUB,TRNKREF+4(2)                                                 
         CVB   RE,DUB                                                           
         LA    RE,1(RE)                    INCREMENT BY 1                       
         MVC   LTRNKREF,TRNKREF                                                 
*                                                                               
VENF33A  BCTR  RE,0                         DECREMENT BY 1                      
         CVD   RE,DUB                                                           
         LA    RE,1(RE)               INCREMENT BY 1, AS BELOW BCT              
         OI    DUB+7,X'0F'                                                      
         UNPK  LTRNKREF+4(2),DUB+6(2)                                           
         CLC   INVKREF,LTRNKREF                                                 
         BNE   VENF33B                                                          
         TM    INVIND2,INVCR                                                    
         BZ    VENF33B                                                          
         CLC   INVSUB,MPYSUB                                                    
         BE    VENF33C                                                          
VENF33B  BCT   RE,VENF33A                                                       
*                                                                               
         ICM   RE,15,AFFTCREF                                                   
         BZ    VENF32                                                           
         LA    RE,FFTDATA-FFTELD(RE)                                            
         CLC   INVKREF,0(RE)                                                    
         BNE   VENF32                                                           
         CLC   INVSUB,MPYSUB                                                    
         BNE   VENF32                                                           
         B     VENF36                                                           
*                                                                               
VENF33C  ICM   RE,15,AFFTCREF                                                   
         BZ    VENF32                                                           
         LA    RE,FFTDATA-FFTELD(RE)                                            
         CLC   INVDREF,0(RE)                                                    
         BNE   VENF32                                                           
         XC    INVDREF,INVDREF                                                  
         B     VENF36                                                           
*                                                                               
VENF34   ICM   RE,15,AFFTCREF                                                   
         BZ    *+12                                                             
         LA    RE,FFTDATA-FFTELD(RE)                                            
         B     *+8                                                              
         LA    RE,TRNKREF                                                       
         MVC   INVKREF,0(RE)                                                    
         ICM   R1,15,AMPYEL        TEST ELEMENT CARRIES SUB REFERENCE           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   MPYLN,MPYLN2Q                                                    
         BNL   *+6                                                              
         DC    H'0'                                                             
         MVC   INVSUB,MPYSUB       MOVE INVOICE SUB REFERENCE                   
         DROP  R1                                                               
         MVC   WORK(INVKEYL),INVRECD                                            
         GOTO1 ATS2GET,TSARDH      READ FOR EXISTING UNMATCHED DR               
         BE    VENF36              OVERWRITE WITH THESE DR DETAILS              
         XC    TSA2REC,TSA2REC     ADD A NEW RECORD FOR THIS DR                 
         MVC   INVRECD(INVKEYL),WORK                                            
*&&UK*&& ZAP   INVEXCH,PZERO                                                    
         LA    R1,TSAADD                                                        
         B     *+8                                                              
VENF36   LA    R1,TSAPUT                                                        
         MVC   INVDREF,TRNKREF                                                  
         MVC   INVSUBDR,TRNKSBR                                                 
         GOTO1 ATS2APW,(R1)                                                     
         B     VENFILTX                                                         
                                                                                
VENFILTN XR    R2,R2                                                            
                                                                                
VENFILTX B     OVROUTXS            LEAVE R2,R3 INTACT                           
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* DO PAY 2 JOBS/INVOICES REQUIRED                                     *         
***********************************************************************         
                                                                                
         USING JARAYD,R3                                                        
DOP2J    DS    0H                                                               
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
                                                                                
         L     RF,=V(PAY2JOBC)                                                  
         A     RF,OVRELO                                                        
         SAM24 ,                                                                
         GOTO1 (RF),JPYDETD                                                     
         JNE   *+2                                                              
***      JNE   DOP2JN              (IGNORE ANY ERRORS FOR NOW)                  
                                                                                
         SAM31 ,                                                                
         L     R3,AJARAY                                                        
         MVI   JARDFIL,JARDEOT                                                  
                                                                                
DOP2JY   DS    0H                                                               
         SAM24 ,                                                                
         J     OVROUTE                                                          
                                                                                
DOP2JN   DS    0H                                                               
         J     OVROUTL                                                          
         DROP  R2,R3                                                            
                                                                                
***********************************************************************         
                                                                                
         LTORG                                                                  
ASTERIS  DC    C'***************************************'                       
                                                                                
OVACCDIR DC    C'ACCDIR '                                                       
OVACCMST DC    C'ACCMST '                                                       
OVACCFIL DC    C'ACCOUNT'                                                       
DMUNLK   DC    C'DMUNLK '                                                       
                                                                                
         DS    0H                                                               
MSGTAB   DC    AL2(AS$CHQVD),AL2(OV$CHQVD-OV$MSGS)                              
         DC    AL2(AS$CHQUV),AL2(OV$CHQUV-OV$MSGS)                              
         DC    AL2(AS$PAYVD),AL2(OV$PAYVD-OV$MSGS)                              
         DC    AL2(AS$PAYUV),AL2(OV$PAYUV-OV$MSGS)                              
         DC    AL2(AS$XFRVD),AL2(OV$XFRVD-OV$MSGS)                              
         DC    AL2(AS$XFRUV),AL2(OV$XFRUV-OV$MSGS)                              
         DC    AL2(EOT)                                                         
                                                                                
PSTTAB   DS    0H                                                               
         DC    AL1(UPDICDSC,SPATCSHD,PSTLELDG+PSTLEOCN,0)                       
         DC    S(TOTDISC)                                                       
         DC    S(CDSC,SUPP)                                                     
*&&UK                                                                           
         DC    AL1(UPDICDSC,SPATCSHD,PSTLELDG+PSTLEOCN,0)                       
         DC    S(TOTDISC)                                                       
         DC    S(CDSC,0)           DISCOUNT BY CLIENT/PRODUCT                   
                                                                                
         DC    AL1(UPDICDSC,0,0,PST2COS1)                                       
         DC    S(0)                                                             
         DC    S(0,0)              COSTING TO 1C ACCOUNT, 12 CONTRA             
         DC    AL1(UPDICDSC,0,0,PST2COS2)                                       
         DC    S(0)                                                             
         DC    S(0,0)              COSTING TO 12 ACCOUNT, 1C CONTRA             
                                                                                
         DC    AL1(UPDIDTAX,0,PSTLEAPT,0)                                       
         DC    S(TOTDTAX)                                                       
         DC    S(DTAX,SUPP)                                                     
                                                                                
         DC    AL1(UPDIDTAX,0,0,PST2DTAX)                                       
         DC    S(TOTDTAX)                                                       
         DC    S(TAX1,SUPP)                                                     
                                                                                
         DC    AL1(UPDIDTAX,0,0,PST2DTAX)                                       
         DC    S(TOTDTAX)                                                       
         DC    S(TAX2,SUPP)                                                     
                                                                                
         DC    AL1(UPDIDTAX,0,0,PST2DTAX)                                       
         DC    S(TOTDTAX)                                                       
         DC    S(TAX3,SUPP)                                                     
                                                                                
         DC    AL1(UPDIDTAX,0,0,PST2DTAX)                                       
         DC    S(TOTDTAX)                                                       
         DC    S(TAX4,SUPP)                                                     
                                                                                
         DC    AL1(UPDIDTAX,0,0,PST2DTAX)                                       
         DC    S(TOTDTAX)                                                       
         DC    S(TAX5,SUPP)                                                     
                                                                                
         DC    AL1(UPDIEXDF,SPATEXDF,0,0)                                       
         DC    S(TOTEXDF)                                                       
         DC    S(EXDF,SUPP)                                                     
                                                                                
         DC    AL1(UPDIBCHA,SPATBCHA,0,0)                                       
         DC    S(TOTBCHA)                                                       
         DC    S(BCHA,SUPP)                                                     
                                                                                
         DC    AL1(UPDIBCHA,0,0,PST2OPIK)                                       
         DC    S(TOTBCHA)                                                       
         DC    S(BANK,BCHA)                                                     
*&&                                                                             
         DC    AL1(EOT)                                                         
                                                                                
PSTTABD  DSECT                                                                  
PSTIND1  DS    XL1                 UPDATE INDICATOR                             
PSTSPAT  DS    XL1                 SPATYPE                                      
PSTLE    DS    XL1                 LEDGER ELEMENT(S) HOLDING ACCOUNT            
PSTLELDG EQU   X'80'                                                            
PSTLEOCN EQU   X'40'                                                            
PSTLEAPT EQU   X'20'                                                            
PSTIND2  DS    XL1                 UPDATE INDICATOR 2                           
PST2OPIK EQU   X'80'               OFFICE POSITION ON CONTRA                    
PST2DTAX EQU   X'40'               THIS IS A TAX ACCOUNT                        
PST2COS1 EQU   X'20'               COSTING POSTING 1 - 1C/12                    
PST2COS2 EQU   X'10'               COSTING POSTING 2 - 12/1C                    
PSTTOTS  DS    S                   A(TOTALS)                                    
PSTACC   DS    S                   ACCOUNT DETAILS (SEE ACCXD DSECT)            
PSTCACC  DS    S                   CONTRA ACCOUNT - CULA                        
PSTTABL  EQU   *-PSTTABD                                                        
                                                                                
BATCHKQ  EQU   1                   CHECK BATCH OK TO UPDATE                     
BATADDQ  EQU   2                   CHECK FOR & (RE)ADD BATCH RECORD             
BATUPDQ  EQU   3                   UPDATE BATCH RECORD                          
                                                                                
CHQSETQ  EQU   1                   SET CHEQUE DATA FROM TSAR RECORD             
CHQTSTQ  EQU   2                   TEST CHEQUE IS CROSS-OFFICE MATCH            
CHQMRKQ  EQU   3                   TEST PAYMENT WAS FROM THIS CHEQUE            
                                                                                
POSTISTD EQU   0                   STANDARD POSTING (REVERSING COPY)            
POSTINEW EQU   1                   NEW TRANSACTION RECORD                       
POSTIBLT EQU   2                   TRANSACTION RECORD FULLY BUILT               
                                                                                
REVEXQ   EQU   1                   VENFILT - REVERSE EXCHANGE DIFFS             
IGNEXQ   EQU   2                           - IGNORE EXCHANGE DIFFS              
                                                                                
BNKNDSP  EQU   19                  DISPLACEMENT TO BANK NAME FROM CODE          
         EJECT                                                                  
OVRWRKD  DSECT                     ** OVERLAY W/S REDEFINED **                  
AICOTAB  DS    A                   A(INTERCOMPANY TABLE)                        
AOFFTAB  DS    A                   A(OFFICE TABLE FOR DISCOUNT/TAX ADJ)         
APAYTAB  DS    A                   A(PAYMENT TABLE)                             
ATPTAB   DS    A                   A(TRANSACTION POINTER TABLE)                 
ACLPTAB  DS    A                   A(CLIENT/PRODUCT TABLE)                      
ACOSTAB  DS    A                   A(COSTING TABLE)                             
                                                                                
AJARAY   DS    A                   A(JARAY)                                     
                                                                                
ACLPNOW  DS    A                   A(CURRENT CLPTAB ENTRY)                      
ACOSNOW  DS    A                   A(CURRENT COSTAB ENTRY)                      
                                                                                
OVRELO   DS    A                   RELOCATION FACTOR                            
ABTYPTAB DS    A                   A(BTYPTAB TABLE)                             
APROGS   DS    A                   A(PROGS TABLE)                               
                                                                                
OVROUTS  DS    0A                                                               
AADDOFF  DS    A                                                                
ACRBUILD DS    A                                                                
ADRBUILD DS    A                                                                
ADCMATCH DS    A                                                                
ADETACC  DS    A                                                                
AEXTACC  DS    A                                                                
AEXTLDG  DS    A                                                                
AGETCON  DS    A                                                                
AGOTTXT  DS    A                                                                
APOST    DS    A                                                                
APOSTPT  DS    A                                                                
AREVCR   DS    A                                                                
AREVVST  DS    A                                                                
ASRTOFF  DS    A                                                                
ATAXADJ  DS    A                                                                
ATS2APW  DS    A                                                                
ATS2CLR  DS    A                                                                
ATS2DEL  DS    A                                                                
ATS2GET  DS    A                                                                
AVALCQD  DS    A                                                                
*&&UK                                                                           
AVALBCUR DS    A                                                                
AVCREXCH DS    A                                                                
*&&                                                                             
AVENDOR  DS    A                                                                
AVENCR   DS    A                                                                
AVENDR   DS    A                                                                
AVENDC   DS    A                                                                
AVENFILT DS    A                                                                
ADOP2J   DS    A                                                                
OVROUTSN EQU   (*-OVROUTS)/L'OVROUTS                                            
                                                                                
OVCRDA   DS    A                   CREDIT TRANSACTION DISK ADDRESS              
OVSVRER1 DS    4F                  SAVED REGISTERS RE,RF,R0,R1                  
         ORG   OVSVRER1                                                         
OVSVRE   DS    F                                                                
OVSVRF   DS    F                                                                
OVSVR0   DS    F                                                                
OVSVR1   DS    F                                                                
                                                                                
OVFULL   DS    F                                                                
OVHALF   DS    H                                                                
                                                                                
OVKEY1   DS    XL(L'TRNKEY-L'TRNKSBR)                                           
OVKEY2   DS    XL(L'KEY)                                                        
*          DATA SET ACMRK0BA   AT LEVEL 235 AS OF 12/02/98                      
                                                                                
OVCPJ    DS    0CL(L'TRNKCULA)     SJ ACCOUNT FOR EXCHANGE DIFFERENCE           
OVCPJCPY DS    CL(L'TRNKCPY)                                                    
OVCPJULA DS    CL(L'TRNKULA)                                                    
                                                                                
OVCPJWK  DS    CL(L'TRNKWORK)      SJ W/C FOR EXCHANGE DIFFERENCE               
                                                                                
OVAMNT   DS    PL(L'TRNAMNT)                                                    
OVDRSBR  DS    XL(L'TRNKSBR)                                                    
                                                                                
DATWRK   DS    0XL16               WORK AREA FOR DATES                          
DATPMOS  DS    CL(L'TRSPMOS)                                                    
DATUDAT  DS    CL(L'TRSUDAT)                                                    
         ORG   DATWRK+L'DATWRK                                                  
OVRNUM   DS    XL2                                                              
OVMSGNO  DS    XL2                                                              
ITEMS    DS    XL2                                                              
OVFLAG   DS    XL1                 FLAG FOR INTERNAL USE                        
OVFDSP   EQU   0                   DISPLAY CHEQUES OR INVOICES - PASS 1         
OVFPST   EQU   1                   POST - 2ND PASS OF TSAR BUFFER               
OVF3RD   EQU   2                   UPDATE - 3RD PASS OF TSAR BUFFER             
OVFRDA   EQU   3                   READ AND ADD INVOICES                        
OVFCHK   EQU   4                   TEST CHEQUE/INVOICE BALANCE=ZERO             
OVFRCK   EQU   5                   READ AND CHECK INVOICES WILL ALL FIT         
OVBYTE   DS    XL1                 BYTE FOR INTERNAL USE                        
*UPDIND1  DS    XL1                 UPDATE INDICATOR                            
*UPDIDISC EQU   X'80'               MAY POST DISCOUNT                           
*UPDIDTAX EQU   X'40'               MAY POST DISCOUNT TAX ADJUSTMENT            
*UPDIEXDF EQU   X'20'               MAY POST EXCHANGE DIFFERENCES               
*UPDIBCHA EQU   X'10'               MAY POST BANK CHARGES                       
*UPDIND2  DS    XL1                 UPDATE INDICATOR 2                          
*UPDIADDQ EQU   X'80'               RECORD(S) ADDED VIA ADDTRN                  
STRNKEY  DS    CL(L'TRNKEY)        SAVED TRNKEY                                 
*                                                                               
SBATKEY  DS    CL(L'BATKEY)        SAVED BATCH RECORD KEY                       
INVRNUM  DS    XL2                 CURRENT INVOICE RECORD NUMBER                
CONTRAN  DS    CL(L'CACNAME)       CONTRA ACCOUNT NAME                          
CHQTOT   DS    PL6                 TOTAL OF CROSS-OFFICE CHEQUE                 
CHQTOTC  DS    PL6                                                              
CHQINDS  DS    XL1                 CHEQUE INDICATORS                            
CHQIARTQ EQU   X'80'               ARTIST CHEQUE                                
CHQIFILT EQU   X'40'               PART OF OFFICE CHEQUE WAS FILTERED           
CHQISPLT EQU   X'20'               BANK POSTINGS SPLIT BY OFFICE                
CHQRNUM  DS    XL2                 FIRST CROSS OFFICE CHEQUE RECORD#            
PL13     DS    PL13                                                             
NUMTRNS  DS    XL2                 NUMBER OF CALLS TO ADDTRN                    
BAERROR  DS    XL(L'IOERROR)       IOEXEC ERROR RETURN BYTE                     
TPBAL    DS    PL6                 POINTER TRANSACTION BALANCE                  
                                                                                
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
                                                                                
POSTCACN DS    CL(L'CONTRAN)       POSTING CONTRA ACCOUNT NAME                  
POSTSTAT DS    XL(L'TRNSTAT)       POSTING STATUS                               
POSTOFFC DS    XL(L'TRNOFFC)       POSTING OFFICE                               
                                                                                
TRNBLOCK DS    XL(TRNBLKL)         ADDTRN BLOCK                                 
                                                                                
TOTCASH  DS    PL6                 CASH TOTAL FOR BATCH RECORD                  
TOTITEM  DS    PL2                 ITEM TOTAL FOR BATCH RECORD                  
                                                                                
VOIDNARR DS    CL50                CHEQUE (UN)VOIDED ON DDMMMYY                 
VOIDNARL DS    XL1                 L'VOIDNARR                                   
                                                                                
DSDICTU  DS    0X                  BANK/VOID UPPER CASE DICTIONARY              
BVU@VOID DS    CL(L'TRNKREF)       VOID FOR TRANSACTION REFERENCE               
                                                                                
DSDICTM  DS    0X                  BANK/VOID MIXED CASE DICTIONARY              
BVM@BNKA DS    CL8                 BANK A/C                                     
BVM@NRTV DS    CL12                NARRATIVE                                    
                                                                                
DATETEMP DS    CL8                 CL8'********'                                
                                                                                
OV$MSGS  DS    0X                  PRE 'GOT'TXT MESSAGES                        
OV$CHQVD DS    CL(L'VOIDNARR),XL1  AS$CHQVD - CHEQUE VOIDED ON ******           
OV$CHQUV DS    CL(L'VOIDNARR),XL1  AS$CHQUV -   "   UNVOIDED "   "              
OV$PAYVD DS    CL(L'VOIDNARR),XL1  AS$PAYVD - PAYMENT VOIDED ON ******          
OV$PAYUV DS    CL(L'VOIDNARR),XL1  AS$PAYUV -    "   UNVOIDED "   "             
OV$XFRVD DS    CL(L'VOIDNARR),XL1  AS$XFRVD - TRANSFER VOIDED ON ******         
OV$XFRUV DS    CL(L'VOIDNARR),XL1  AS$XFRUV -     "   UNVOIDED "   "            
                                                                                
*&&UK                                                                           
TAX1     DS    XL(L'ACTKCULA)                                                   
TAXCOST1 DS    XL(L'RSTCOSTG)                                                   
TAXNAME1 DS    CL(L'NAMEREC)                                                    
TAXOPIK1 DS    XL1                                                              
                                                                                
TAX2     DS    XL(L'ACTKCULA)                                                   
TAXCOST2 DS    XL(L'RSTCOSTG)                                                   
TAXNAME2 DS    CL(L'NAMEREC)                                                    
TAXOPIK2 DS    XL1                                                              
                                                                                
TAX3     DS    XL(L'ACTKCULA)                                                   
TAXCOST3 DS    XL(L'RSTCOSTG)                                                   
TAXNAME3 DS    CL(L'NAMEREC)                                                    
TAXOPIK3 DS    XL1                                                              
                                                                                
TAX4     DS    XL(L'ACTKCULA)                                                   
TAXCOST4 DS    XL(L'RSTCOSTG)                                                   
TAXNAME4 DS    CL(L'NAMEREC)                                                    
TAXOPIK4 DS    XL1                                                              
                                                                                
TAX5     DS    XL(L'ACTKCULA)                                                   
TAXCOST5 DS    XL(L'RSTCOSTG)                                                   
TAXNAME5 DS    CL(L'NAMEREC)                                                    
TAXOPIK5 DS    XL1                                                              
                                                                                
TAXTABN  EQU   (*-TAX1)/ACCXL                                                   
*&&                                                                             
                                                                                
         DS    0A                  WORD ALIGN TSAR 2 BLOCK                      
TSA2BLK  DS    XL(TSARDL2)         TSAR 2 BLOCK                                 
TSA2REC  DS    XL(TSA2RECL)        TSAR 2 RECORD - SEE INVRECD                  
TSA2KEYL EQU   INVKEYL                                                          
TSA2RECL EQU   INVRECL                                                          
OVELEM   DS    XL256                                                            
OVRWRKDL EQU   *-OVRWRKD           MAX 2K                                       
                                                                                
INVRECD  DSECT                     ** INVOICE RECORD **                         
INVKREF  DS    XL(L'TRNKREF)       INVOICE KEY REFERENCE                        
INVSUB   DS    XL(L'TRNSUB)        INVOICE SUB REFERENCE                        
INVKEYL  EQU   *-INVRECD                                                        
INVDREF  DS    XL(L'TRNKREF)       PAYMENT KEY REFERENCE                        
INVSUBDR DS    XL(L'TRNSUB)        DR SUB REFERENCE IN INVOICE MPYEL            
INVSRC   DS    XL(L'TSARFSAC)      SOURCE ACCOUNT                               
INVSWRK  DS    XL(L'TSARFWRK)      SOURCE WORK CODE (UK)                        
         ORG   INVSWRK                                                          
INVSIND  DS    XL1                 SOURCE INDICATOR (US)                        
         DS    XL1                 N/D                                          
         ORG   ,                                                                
INVAMNT  DS    PL6                 AMOUNT OF PAYMENT (DEBIT)                    
INVDSC   DS    PL6                 AMOUNT OF LIVE DISCOUNT                      
*&&UK                                                                           
INVAMNTC DS    PL6                 AMOUNT IN CURRENCY                           
INVDSCC  DS    PL6                 AMOUNT OF DISCOUNT IN CURRENCY               
INVEXCH  DS    PL6                 AMOUNT OF EXCHANGE DIFFERENCE                
*&&                                                                             
INVRAT   DS    XL2                 INVOICE TAX RATE (14% BY DEFAULT)            
INVOFFC  DS    CL(L'TRNOFFC)       INVOICE OFFICE CODE                          
INVIND2  DS    XL1                 INDICATOR BYTE 2                             
INVCR    EQU   X'80'               CREDIT DETAILS FILLED IN                     
INVDR    EQU   X'40'               DEBIT DETAILS FILLED IN                      
*&&US                                                                           
INVMIV   DS    XL(L'TSARFINV)      MEDIA/LONG INVOICE NUMBER                    
*&&                                                                             
INVERPD  DS    XL(L'GDADATE)                                                    
INVKOCD  DS    XL(TRNKREF-TRNKOFF)                                              
INVRECL  EQU   *-INVRECD                                                        
*                                                                               
TSIKEYD  DSECT                                                                  
TSIKEY1  DS    XL1                                                              
TSIKEY2  DS    XL1                                                              
TSIKOCD  DS    XL(TRNKREF-TRNKOFF)                                              
TSIKREF  DS    XL(L'TRNKREF)                                                    
TSIKSBRF DS    XL(L'TRNKSBR)                                                    
*                                                                               
ICOTABD  DSECT                     ** INTERCOMPANY TABLE **                     
ICOOFFC  DS    CL(L'TRNOFFC)       OFFICE                                       
ICOAMNT  DS    PL8                 AMOUNT                                       
ICOFACT  DS    CL(L'ACTKCULA-1)    FROM U/L/ACCOUNT                             
ICOTACT  DS    CL(L'ACTKCULA-1)    TO U/L/ACCOUNT                               
ICOFNAM  DS    CL(L'NAMEREC)       FROM A/C NAME                                
ICOTNAM  DS    CL(L'NAMEREC)       TO A/C NAME                                  
ICOTABL  EQU   *-ICOTABD                                                        
ICOTABN  EQU   24                                                               
ICOLTAB  EQU   (ICOTABN*ICOTABL)+1                                              
                                                                                
OFFTABD  DSECT                     ** OFFICE TABLE **                           
OFFOFFC  DS    CL(L'TRNOFFC)       OFFICE                                       
OFFIND1  DS    XL1                 OFFICE INDICATOR                             
OFFIDISC EQU   X'80'               DISCOUNT POSTING(S)                          
OFFIDTAX EQU   X'40'               DISCOUNT TAX ADJUSTMENT POSTING(S)           
OFFIEXDF EQU   X'20'               EXCHANGE DIFFERENCE POSTING(S)               
OFFIBCHA EQU   X'10'               BANK CHARGE POSTING(S)                       
OFFTABL  EQU   *-OFFTABD                                                        
OFFTABN  EQU   48                                                               
OFFLTAB  EQU   (OFFTABN*OFFTABL)+1 NEED EOT MARKER                              
                                                                                
PAYTABD  DSECT                     ** PAYMENT TABLE **                          
PAYBNK   DS    CL(L'MPYBNK)        PAYMENT BANK ACCOUNT                         
PAYNO    DS    CL(L'MPYNO)         PAYMENT NUMBER                               
PAYDTE   DS    CL(L'MPYDTE)        PAYMENT DATE                                 
PAYKEY1L EQU   *-PAYTABD                                                        
PAYOFF   DS    CL2                 PAYMENT OFFICE                               
PAYKEY2L EQU   *-PAYTABD                                                        
PAYMOS   DS    PL2                 PWOS TRANSACTION MOS                         
PAYPAMT  DS    PL6                 PAYMENT PORTION VOIDED/UNVOIDED              
PAYTABL  EQU   *-PAYTABD                                                        
PAYTABN  EQU   128                                                              
PAYLTAB  EQU   (PAYTABN*PAYTABL)+1 NEED EOT MARKER                              
*                                                                               
TPTABD   DSECT                                                                  
TPTYPE   DS    XL(L'TRPTYPE)                                                    
TPSTAT   DS    XL(L'TRPSTAT)                                                    
TPAMNT   DS    PL6                                                              
TPOFFC   DS    CL2                                                              
TPPMOS   DS    PL2                                                              
TPULA    DS    0C                                                               
TPUL     DS    CL(L'TRPUL)                                                      
TPACT    DS    CL(L'ACTKACT)                                                    
TPTABL   EQU   *-TPTABD                                                         
TPTABN   EQU   6                                                                
TPLTAB   EQU   (TPTABN*TPTABL)+1                                                
                                                                                
CLPTABD  DSECT                                                                  
CLPACT   DS    XL(L'ACTKACT)                                                    
CLPTABL  EQU   *-CLPTABD                                                        
*LPTABN  EQU   16                                                               
*LPTABN  EQU   30                                                               
CLPTABN  EQU   65                                                               
CLPLTAB  EQU   (CLPTABL*CLPTABN)                                                
                                                                                
                                                                                
COSTABD  DSECT                                                                  
COSACT   DS    XL(L'ACTKACT)                                                    
COSGRP   DS    XL(L'RSTCOSTG)                                                   
COSTABL  EQU   *-COSTABD                                                        
*OSTABN  EQU   16                                                               
*OSTABN  EQU   30                                                               
COSTABN  EQU   65                                                               
COSLTAB  EQU   (COSTABL*COSTABN)                                                
                                                                                
TABLESL  EQU   ICOLTAB+OFFLTAB+PAYLTAB+TPLTAB+CLPLTAB+COSLTAB                   
                                                                                
BTYPTABD DSECT                     ** BATCH TYPE TABLE **                       
BTYPBANK DS    XL1                 BANK TRNTYPE                                 
BTYPVEND DS    XL1                 VENDOR TRNTYPE (DEBITS ONLY)                 
BTYPCTRY DS    XL1                 VALID COUNTRY (ZERO=ALL COUNTRIES)           
         DS    XL1                 N/D                                          
BTYPYPRG DS    AL2                 Y(VALID PROGRAM LIST)                        
BTYPNARV DS    AL2                 OFFSET TO VOID TEXT                          
BTYPNARU DS    AL2                 OFFSET TO UNVOID TEXT                        
BTYPTABL EQU   *-BTYPTABD                                                       
                                                                                
CTRYTABD DSECT                     ** COUNTRY VARIABLES TABLE **                
CTRYCTRY DS    XL1                 COUNTRY CODE                                 
CTRYUIXP DS    XL1                 UPDATE INDICATOR - EXTRA POSTINGS            
         DS    XL2                 N/D                                          
CTRYTABL EQU   *-CTRYTABD                                                       
                                                                                
MSGTABD  DSECT                     ** MESSAGE TABLE **                          
MSGTNUM  DS    AL2                                                              
MSGTOFST DS    AL2                                                              
MSGTABL  EQU   *-MSGTABD                                                        
                                                                                
* ACMRKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMRKWRK                                                       
         PRINT ON                                                               
SAVED    DSECT                     ** SAVED W/S REDEFINED **                    
         ORG   SOVRWRK             ** OVERLAY SAVED W/S REDEFINED **            
INVMAX   DS    H                   MAX NO. OF INVOICES AT ANY ONE TIME          
TOT#INV  DS    H                   TOTAL NO. OF INVOICES                        
STSARCON DS    CL(L'TSARCON)       SAVED CHEQUE CONTRA ACCOUNT                  
STSARCXL DS    XL1                 SAVED CHEQUE CONTRA A/C EX LENGTH            
STSARIND DS    XL(L'TSARINDS)      SAVED CHEQUE TSAR INDICATORS                 
STSARBTY DS    XL(L'TSARBTY)       SAVED CHEQUE/TRANSFER BATCH TYPE             
STSARMOS DS    XL(L'TSARMOS)       SAVED CHEQUE/TRANSFER BATCH MOA              
*&&UK                                                                           
STSARAFX DS    XL(L'TSARAFCX)      SAVED CHEQUE EXCHANGE RULE                   
*&&                                                                             
SBATMON  DS    XL(L'OBATMON)       SAVED BATCH MONTH                            
SBATREF  DS    XL(L'OBATREF)       SAVED BATCH REFERENCE                        
SBATNAM  DS    XL(L'OBATNAM)       SAVED BATCH NAME                             
SBATMONP DS    XL(L'OBATMONP)      SAVED BATCH MONTH (PWOS)                     
STOPTDIS DS    XL(L'OPTDIS)        SAVED DISPLAY OPTION IN STORAGE              
SDISLCNT DS    XL(L'DISLCNT)       SAVED COUNT OF RECORDS                       
SDISLIST DS    XL(DISLISTL)        SAVED RECORD LIST                            
TOTDTAX  DS    PL(L'TOTDISC)       TOTAL DISCOUNT TAX ADJUSTMENT                
TOTBCHA  DS    PL(L'TOTDISC)       TOTAL BANK CHARGES TO REVERSE                
FXRVALS  DS    XL(REFVALSL)        FIXED REFERENCE VALUES - SEE REFVALS         
         ORG   FXRVALS                                                          
FXRSTA   DS    CL6                 START FIXED REF (ABSOLUTE IF NO END)         
FXRSTAXL DS    XL1                 EXECUTE L'START FIXED REF                    
FXREND   DS    CL6                 END FIXED REF                                
FXRENDXL DS    XL1                 EXECUTE L'END FIXED REF                      
         ORG   FXRVALS+REFVALSL                                                 
SACCIND1 DS    XL1                 SAVED BANK A/C INDICATOR BYTE 1              
OCHQDATP DS    XL3                 PWOS CHEQUE DATE                             
OCHQIND  DS    XL1                 CHEQUE INDICATOR                             
OCHQIADV EQU   X'80'               CHEQUE CARRIES ADVANCE                       
OCHQIDIF EQU   X'40'               CHEQUE CARRIES DIFFERENCE                    
OCHQADV  DS    PL6                 CHEQUE ADVANCE AMOUNT                        
OCHQDIF  DS    PL6                 CHEQUE DIFFERENCE AMOUNT                     
MAINNAME DS    CL(L'NAMEREC)       MAIN BANK ACCOUNT NAME                       
SMRKOPT  DS    CL(L'MRKOPT)        SAVED OPTIONS                                
SVDA     DS    XL4                 SAVED AREA FOR DISK ADDRESS                  
SVEST    DS    CL6                 SAVED ESTIMATE                               
SVMOS    DS    XL2                 SAVED MOS                                    
         DS    CL1                 SPARE                                        
SVOFF    DS    CL2                 SAVED OFFICE                                 
SVSYS    DS    CL1                 SAVED SYSTEM                                 
SVKEY    DS    XL(L'KEY)                                                        
* FLAG BYTE                                                                     
FLGBRD   EQU   X'40'               GET DATE FROM BRAODCAST CALENDAR             
LTRNKREF DS    CL6                                                              
         ORG   SOVRWRK+L'SOVRWRK                                                
                                                                                
         ORG   TOTALS              ** TOTALS REDEFINED **                       
TOTCHQS  DS    PL8                                                              
TOTVOID  DS    PL8                                                              
TOTINVS  DS    PL8                                                              
*&&UK                                                                           
TOTEXDF  DS    PL8                                                              
*&&                                                                             
TOTDISC  DS    PL8                                                              
TOTDIFF  DS    PL8                                                              
REPVOID  DS    PL8                                                              
REPUVOI  DS    PL8                                                              
*&&UK                                                                           
         ORG   CURTOTS             ** CURRENCY TOTALS REDEFINED **              
CURCHQS  DS    PL8                                                              
CURVOID  DS    PL8                                                              
CURINVS  DS    PL8                                                              
CUREXDF  DS    PL8                                                              
CURDISC  DS    PL8                                                              
CURDIFF  DS    PL8                                                              
RCPVOID  DS    PL8                                                              
RCPUVOI  DS    PL8                                                              
*&&                                                                             
                                                                                
TWAD     DSECT                     ** TWA REDEFINED **                          
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKFCD                                                       
         ORG   MRKOLAYH                                                         
       ++INCLUDE ACMRKE7D                                                       
* ACADDTRND                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACADDTRND                                                      
         PRINT ON                                                               
* ACPAY2JD                                                                      
*        PRINT OFF                                                              
*      ++INCLUDE ACPAY2JD                                                       
*        PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACMRK0B   05/14/20'                                      
         END                                                                    
