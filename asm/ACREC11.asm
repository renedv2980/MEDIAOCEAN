*          DATA SET ACREC11    AT LEVEL 217 AS OF 05/01/02                      
*PHASE T60811A                                                                  
ACREC11  TITLE '- U.K. OVERLAY II - HEADER SCREEN AND POSTINGS'                 
ACREC11  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**RE11**,RA,R9,RR=RE                                           
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)                                                     
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
         SPACE 2                                                                
         L     RF,=A(UPDATE)                                                    
         AR    RF,RE                                                            
         ST    RF,AUPDATE                                                       
         MVC   OVLITS(OVLITSL),COVLITS                                          
         SR    RF,RF                                                            
         IC    RF,ACTION                                                        
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     SETLST              INITIALISE                                   
         B     VALHED              HEADER                                       
         B     EXIT                INPUT                                        
         B     EXIT                SPECIAL                                      
         B     RESHED              RESTORE (HEADER)                             
         B     EXIT                                                             
         B     GOUPDATE            UPDATE                                       
         B     GOUPDATE            DRAFT UPDATE                                 
         B     GOUPDATE            FILTERED DRAFT UPDATE                        
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     VALHED              CHANGE (HEADER)                              
         B     RELAST              RESTORE HEADER (AFTER UPDATE)                
*                                                                               
GOUPDATE GOTO1 AUPDATE                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALISE - SET Y-TYPES FOR VALID LEDGER LISTS                     *         
***********************************************************************         
         SPACE 1                                                                
SETLST   DS    0H                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DINLST,DOUTLST                             
         GOTO1 (RF),(R1),,TINLST,TOUTLST                                        
         MVI   TOTALSX,EOT                                                      
         MVC   BANKLIST,=Y(CBANKULS-ACREC11)                                    
         MVC   DISCLIST,=Y(CDISCULS-ACREC11)                                    
         MVC   RECVLIST,=Y(CRECVULS-ACREC11)                                    
         MVC   VATLIST,=Y(CRECVULS-ACREC11)                                     
         MVC   WOFFLIST,=Y(CWOFFULS-ACREC11)                                    
         MVC   SHEADTOT,=Y(RECHTOTH-RECOLAYH)                                   
         MVC   SHEADPFK,=Y(RECHPFKH-RECOLAYH)                                   
         MVC   SRECCUCT,=Y(RECCUCTH-RECOLAYH)                                   
         MVC   SRECCUC,=Y(RECCUCH-RECOLAYH)                                     
         MVC   SRECCUR,=Y(RECCURH-RECOLAYH)                                     
         MVC   SRECRATT,=Y(RECRATTH-RECOLAYH)                                   
         MVC   SRECRAT,=Y(RECRATH-RECOLAYH)                                     
         MVC   SRECBCCT,=Y(RECBCCTH-RECOLAYH)                                   
         MVC   SRECBCC,=Y(RECBCCH-RECOLAYH)                                     
         MVC   SRECBCH,=Y(RECBCHH-RECOLAYH)                                     
         MVC   SRECWACT,=Y(RECWACTH-RECOLAYH)                                   
         MVC   SRECWAC,=Y(RECWACH-RECOLAYH)                                     
         MVC   SRECVATT,=Y(RECVATTH-RECOLAYH)                                   
         MVC   SRECVAT,=Y(RECVATH-RECOLAYH)                                     
         MVC   SRECWRFT,=Y(RECWRFTH-RECOLAYH)                                   
         MVC   SRECWRF,=Y(RECWRFH-RECOLAYH)                                     
         MVC   SRECWDTT,=Y(RECWDTTH-RECOLAYH)                                   
         MVC   SRECWDT,=Y(RECWDTH-RECOLAYH)                                     
         MVC   SRECWNRT,=Y(RECWNRTH-RECOLAYH)                                   
         MVC   SRECWNR,=Y(RECWNRH-RECOLAYH)                                     
         MVC   SRECODTT,=Y(RECODTTH-RECOLAYH)                                   
         MVC   SRECODT,=Y(RECODTH-RECOLAYH)                                     
         MVC   SRECDSCT,=Y(RECDSCTH-RECOLAYH)                                   
         MVC   SRECDSC,=Y(RECDSCH-RECOLAYH)                                     
SETLSTX  B     EXIT                                                             
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* HEADER - VALIDATE ALL HEADER SCREEN FIELDS                          *         
***********************************************************************         
         SPACE 1                                                                
VALHED   DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH REFERENCE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALBAT   TM    RECBREFH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALBATX                                                          
         GOTO1 AVALBAT,RECBREFH                                                 
         BNE   EXIT                                                             
VALBATX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BATCH MONTH                                                *         
***********************************************************************         
         SPACE 1                                                                
VALMON   TM    RECBMONH+(FVATRB-FVIHDR),FVAPROT                                 
         BO    VALMONX                                                          
         GOTO1 AVALMON,RECBMONH                                                 
         BNE   EXIT                                                             
VALMONX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK ACCOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
VALBNK   NI    TWAMODE2,FF-(TWA2NALL)                                           
         NI    CURIND1,FF-(CUR1BNKC)                                            
         XC    BANKCUR,BANKCUR                                                  
         XC    RECCHC,RECCHC                                                    
         OI    RECCHCH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    BANKXTRA(BANKXTRL),BANKXTRA                                      
         GOTO1 AVALBNK,RECBNKH                                                  
         BH    EXIT                ERROR                                        
         BE    *+12                FIELD IS PRESENT                             
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALBNK02                                                         
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',BANKUL),                X        
               (X'C0',RECNDSP),AIO1,(L'BANKNAME,BANKNAME)                       
         MVC   EXDF,RECEXDF        EXTRACT EXCHANGE DIFFERENCE A/C              
         MVC   BCHA,RECBCHA        EXTRACT BANK CHARGES A/C                     
         MVC   BNKASTS1,RECASTS1   EXTRACT ASTEL STATUS BYTE - 1                
VALBNK02 MVC   DUB(L'CDEFCUR),CDEFCUR                                           
         OC    AGYCURR,AGYCURR                                                  
         BZ    *+10                                                             
         MVC   DUB(L'CDEFCUR),AGYCURR                                           
         OC    BANKCUR,BANKCUR     TEST BANK ACCOUNT CURRENCY KNOWN             
         BNZ   *+10                                                             
         MVC   BANKCUR,DUB                                                      
         CLC   BANKCUR,DUB         TEST BANK=AGENCY CURRENCY                    
         BE    VALBNK04                                                         
         TM    TWAMODE2,TWA2NCUR   TEST CURRENCY NOT VALID FOR USER             
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         B     VALHEDE                                                          
         OI    CURIND1,CUR1BNKC                                                 
         MVC   RECCHC,DUB          SET CHEQUE CURRENCY                          
VALBNK04 LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         OC    EXDF,EXDF                                                        
         BZ    VALBNK08                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,EXDF                                                     
         NI    OVRBYTE,FF-(ACCERR)                                              
         GOTO1 AGETACC,0                                                        
         BE    *+8                                                              
         OI    OVRBYTE,ACCERR      SET ACCOUNT IN ERROR                         
         MVI   EXDFOPOK,0          SET OFFICE POSITION IN KEY                   
         L     R1,RECLEDG                                                       
         CLI   LEDGTOFF-LEDGTABD(R1),01                                         
         BL    VALBNK06                                                         
         CLI   LEDGTOFF-LEDGTABD(R1),12                                         
         BH    VALBNK06                                                         
         MVC   EXDFOPOK,LEDGTOFF-LEDGTABD(R1)                                   
         B     VALBNK08            ALLOW ERROR & DON'T SET A/C VALUES           
*                                                                               
VALBNK06 TM    OVRBYTE,ACCERR      TEST ACCOUNT IN ERROR                        
         BO    VALHEDE             SET CC NEQ AND EXI                           
         MVC   EXDFNAME,RECNAME                                                 
         MVC   EXDFCOST,RECCSTG                                                 
*                                                                               
VALBNK08 OC    BCHA,BCHA                                                        
         BZ    VALBNK12                                                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,BCHA                                                     
         NI    OVRBYTE,FF-(ACCERR)                                              
         GOTO1 AGETACC,0                                                        
         BE    *+8                                                              
         OI    OVRBYTE,ACCERR      SET ACCOUNT IN ERROR                         
         MVI   BCHAOPOK,0          SET OFFICE POSITION IN KEY                   
         L     R1,RECLEDG                                                       
         CLI   LEDGTOFF-LEDGTABD(R1),01                                         
         BL    VALBNK10                                                         
         CLI   LEDGTOFF-LEDGTABD(R1),12                                         
         BH    VALBNK10                                                         
         MVC   BCHAOPOK,LEDGTOFF-LEDGTABD(R1)                                   
         B     VALBNK12            ALLOW ERROR & DON'T SET A/C VALUES           
*                                                                               
VALBNK10 TM    OVRBYTE,ACCERR      TEST ACCOUNT IN ERROR                        
         BO    VALHEDE             SET CC NEQ AND EXI                           
         MVC   BCHANAME,RECNAME                                                 
         MVC   BCHACOST,RECCSTG                                                 
*                                                                               
VALBNK12 MVC   RECBCC,RECCHC                                                    
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BZ    VALBNK14                                                         
         TM    CURIND1,CUR1BNKC                                                 
         BO    *+6                                                              
         DC    H'0'                LOCAL BANK ACCOUNT MUST BE CURRENCY          
         MVC   RECBCC,BANKCUR                                                   
VALBNK14 OI    RECBCCH+(FVOIND-FVIHDR),FVOXMT                                   
         TM    CURIND1,CUR1BNKC    TEST FOREIGN CURRENCY BANK ACCOUNT           
         BZ    *+14                                                             
         MVC   RECCUC,BANKCUR      GIVE CURRENCY SYMBOL                         
         OI    RECCUCH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RECCURH+(FVOIND-FVIHDR),FVOXMT                                   
         DS    0H                                                               
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK REFERENCE NUMBER (CHEQUE NUMBER)                      *         
***********************************************************************         
         SPACE 1                                                                
VALREF   XC    BANKREF,BANKREF                                                  
         OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - REQUIRED FIELD                         
         GOTO1 AFVAL,RECREFH                                                    
         BH    EXIT                ERROR                                        
         BE    *+12                FIELD IS PRESENT                             
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALREFX                                                          
         MVC   BANKREF,FVIFLD      SET FIELD                                    
VALREFX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATE CHEQUE WAS DEPOSITED                                  *         
***********************************************************************         
         SPACE 1                                                                
VALDEP   XC    BANKDATC,BANKDATC   CLEAR COMPRESSED DATE                        
         OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - REQUIRED FIELD                         
         GOTO1 AVALDEP,RECDEPH                                                  
         BH    EXIT                ERROR                                        
         BE    *+12                FIELD PRESENT                                
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALDEPX                                                          
         MVC   BANKDATC,WORK+(PVALCSTA-PERVALD)                                 
VALDEPX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CHEQUE (CONTROL) AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCHQ   TM    CURIND1,CUR1BNKC    TEST LOCAL CURRENCY BANK ACCOUNT             
         BO    VALCHQX                                                          
         ZAP   CHQAMT,PZERO        CLEAR CHEQUE AMOUNT                          
         OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - REQUIRED FIELD                         
         GOTO1 AFVAL,RECCHQH                                                    
         BH    EXIT                ERROR                                        
         BE    *+12                FIELD PRESENT                                
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALCHQ04                                                         
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),RECCHQ),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   EXIT                INVALID AMOUNT                               
         ZAP   CHQAMT,4(8,R1)                                                   
VALCHQ04 CURED CHQAMT,(L'RECCHQ,RECCHQ),AGYCURT,ALIGN=LEFT,FLOAT=-              
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
VALCHQX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CHEQUE DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALCDT   OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - REQUIRED FIELD                         
         GOTO1 AVALCDT,RECCDTH                                                  
         BH    EXIT                ERROR                                        
         BE    *+8                 FIELD IS PRESENT                             
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
VALCDTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE CURRENCY CODE AND AMOUNT.  IF BANK A/C IS FOREIGN CURRENCY *         
* READ EXCHANGE RATE AND CALCULATE AGENCY CURRENCY CHEQUE AMOUNT      *         
***********************************************************************         
         SPACE 1                                                                
VALCUC   ZAP   CHQAM2,PZERO        CLEAR CURRENCY CHEQUE AMOUNT                 
         NI    CURIND1,FF-(CUR1ALLC+CUR1SINC)                                   
         XC    ATLX,ATLX           CLEAR EXCHANGE RATE VALUES                   
         XC    FORCURT,FORCURT     FILTER VALUE                                 
         OI    RECCURH+(FVOIND-FVIHDR),FVOXMT                                   
         XC    RECRAT,RECRAT       EXCHANGE RATE FIELD                          
         OI    RECRATH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 AFVAL,RECCUCH                                                    
         BH    EXIT                ERROR                                        
         BL    VALCUCX             NO INPUT                                     
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CLC   RECCUC,AGYCURT+(CURTCUR-CURTABD)                                 
         BE    VALHEDE             SET CC NEQ AND EXIT                          
         CLI   FVILEN,1            TEST SINGLE CHARACTER INPUT                  
         BNE   VALCUC04                                                         
         CLI   RECCUC,C'*'         TEST 'ALL' INVOICE CURRENCIES                
         BNE   EXIT                ERROR                                        
         MVC   FVMSGNO,=AL2(AE$NVLCB)                                           
         TM    CURIND1,CUR1BNKC    TEST LOCAL CURRENCY BANK ACCOUNT             
         BO    VALHEDE             SET CC NEQ AND EXIT                          
         OI    CURIND1,CUR1ALLC                                                 
         B     VALCUCX                                                          
VALCUC04 GOTO1 VBLDCUR,DMCB,(0,RECCUC),(0,FORCURT),ACOM                         
         CLI   0(R1),0                                                          
         BNE   EXIT                                                             
         OI    CURIND1,CUR1SINC    SET SINGLE CURRENCY ALLOCATION               
VALCUCX  DS    0H                                                               
*                                                                               
VALCUR   NI    TWAMODE2,FF-(TWA2ALTO)                                           
         XC    RECRAC,RECRAC                                                    
         OI    RECRACH+(FVOIND-FVIHDR),FVOXMT                                   
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECCURH                                                    
         BH    EXIT                                                             
         BL    VALCURX                                                          
         MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    VALHEDE             THIS FIELD IS NOT PERMITTED                  
         OC    BANK,BANK           TEST BANK ACCOUNT MISSING                    
         BZ    VALHEDE             THIS FIELD IS NOT PERMITTED                  
         MVC   FVMSGNO,=AL2(AE$EXDAM)                                           
         OC    EXDF,EXDF           TEST DIFFERENCE ACCOUNT KNOWN                
         BZ    VALHEDE                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),RECCUR),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   EXIT                INVALID AMOUNT                               
         ZAP   CHQAM2,4(8,R1)                                                   
         TM    CURIND1,CUR1BNKC    TEST LOCAL CURRENCY BANK ACCOUNT             
         BZ    VALCUR16                                                         
         LA    R2,WORK                                                          
         USING GEXCD,R2            BUILD KEY OF EXCHANGE RECORD                 
         XC    GEKEY(GEKEYL),GEKEY                                              
         MVC   GEKAGY,COMPALFA                                                  
         MVC   GEKCURF,AGYCURT+(CURTCUR-CURTABD)                                
         MVC   GEKCURT,FORCURT+(CURTCUR-CURTABD)                                
         MVI   GEKACT,FF                                                        
         MVC   GEKACT+1(L'GEKACT-1),GEKACT                                      
         OC    EXACT,EXACT         TEST ACCOUNT KNOWN                           
         BZ    *+10                                                             
         MVC   GEKACT,EXACT        SET EXCHANGE RATE ACCOUNT, IF KNOWN          
         XC    TEMP(L'GEKACT),TEMP                                              
         CLI   RECCHQ,C'0'         TEST NUMERIC VALUE IN CHEQUE AMOUNT          
         BNL   VALCUR04                                                         
         SR    RF,RF                                                            
         ICM   RF,1,RECCHQH+(FVILEN-FVIHDR)                                     
         BZ    VALCUR04                                                         
         LA    R1,RECCHQ                                                        
         CLI   RECCHQ,C'*'         TEST PREFIX FOR NUMERIC CLIENT               
         BNE   VALCUR02                                                         
         LA    R1,RECCHQ+1                                                      
         BCT   RF,VALCUR02                                                      
         B     VALCUR04                                                         
VALCUR02 MVC   GEKACT,SPACES                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   GEKACT(0),0(R1)                                                  
         MVC   TEMP(L'GEKACT),GEKACT                                            
VALCUR04 MVC   GEKPEND,BANKDATC    SET COMPRESSED DEPOSIT DATE                  
         LA    RF,X'20'            SET TO GET ACCOUNTING RATE                   
         TM    COMPSTA6,CPYSFTXR   TEST FT RATES PERMITTED                      
         BZ    *+8                                                              
         LA    RF,X'0C'(RF)        FT RATES & SWAP CODES FOR FT LOOK-UP         
         GOTO1 VGETCUR,DMCB,((RF),GEXCD),ACOM                                   
         CLI   0(R1),0                                                          
         BNE   VALCUR08                                                         
         L     R2,0(R1)            R2=A(GENFIL RECORD)                          
         OC    TEMP(L'GEKACT),TEMP TEST SEEKING CLIENT RATE                     
         BZ    VALCUR10                                                         
         CLC   GEKACT,TEMP         TEST CLIENT RATE FOUND                       
         BE    VALCUR10                                                         
VALCUR08 LA    R1,RECCHQH          EXCHANGE RATE ERROR                          
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AE$NOEXR)                                           
         B     VALHEDE                                                          
VALCUR10 MVC   EXACT,GEKACT        SAVE EXCHANGE RATE ACCOUNT                   
         CLI   GEKSYS,GEKSFTQ      TEST FT RATE PASSED                          
         BNE   *+14                                                             
         MVC   RECRAC(L'OV@FTRAT),OV@FTRAT                                      
         B     VALCUR12                                                         
         CLI   GEKACT,FF           X'FF'S MEANS DEFAULT RATE                    
         BNE   *+14                                                             
         MVC   RECRAC(L'OV@DEF),OV@DEF                                          
         B     VALCUR12                                                         
         MVC   RECRAC(L'GEKACT),GEKACT                                          
VALCUR12 MVC   ATLXSTAT,GESTAT     EXTRACT EXCHANGE RATE VALUES                 
         NI    ATLXSTAT,FF-(GEFIXFT)                                            
         MVC   ATLXRATE,GEXRATE                                                 
         MVC   ATLXSHFT,GEXSHFT                                                 
         MVC   TEMP,ATLX           RECALCULATE CHEQUE AMOUNT                    
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP CHQAM2,TEMP                                                      
         ZAP   CHQAMT,DUB                                                       
VALCUR14 CURED CHQAMT,(L'RECCHQ,RECCHQ),AGYCURT,ALIGN=LEFT,FLOAT=-              
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VALCUR16 CP    CHQAMT,PZERO        TEST CHEQUE PRESENT                          
         BE    VALHEDE             SET CC NEQ AND EXIT                          
         CURED CHQAM2,(L'RECCUR,RECCUR),FORCURT,ALIGN=LEFT,FLOAT=-              
         CP    CHQAM2,PZERO        TEST ZERO AMOUNT                             
         BE    VALHEDE                                                          
         TM    CURIND1,CUR1BNKC    TEST LOCAL CURRENCY BANK ACCOUNT             
         BO    VALCUR18                                                         
         MVC   RECRAC(L'OV@CALC),OV@CALC                                        
         ZAP   PKWK16,CHQAM2       CALCULATE BANK-LOCAL EXCHANGE RATE           
         ZAP   DUB,CHQAMT                                                       
         LA    RE,11                                                            
         SR    RF,RF                                                            
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
         SR    RE,RF                                                            
         SRP   PKWK16,0(RE),0                                                   
         LA    RE,5                                                             
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,RF                                                            
         SRP   DUB,0(RE),0                                                      
         BO    VALHEDE             ERROR IF SHIFT OVERFLOW                      
         ZAP   DUB2,DUB            TAKE DIVISOR                                 
         LA    R1,1                ESTABLISH 2+ NO. OF LEADING ZEROES           
         SRP   DUB2,1,0            SHIFT DIVISOR UNTIL OVERFLOW                 
         BO    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-14                                                             
         SRA   R1,1                DIVIDE BY TWO                                
         EX    R1,*+8                                                           
         BNZ   VALHEDE                                                          
         OC    PKWK16(0),PKWK16                                                 
         DP    PKWK16,DUB                                                       
         SRP   PKWK16(8),64-1,5                                                 
         LM    R0,R1,PKWK16        FIRST 8 BYTES IS QUOTIENT                    
         SRDL  R0,4                                                             
         STM   R0,R1,PKWK16        LOSE PACKED SIGN                             
         OC    PKWK16(3),PKWK16    ENSURE RATE WILL FIT                         
         BNZ   VALHEDE                                                          
         MVC   ATLXRATE,PKWK16+3                                                
         OC    ATLXRATE,ATLXRATE   TEST RATE IS VALID                           
         BZ    VALHEDE                                                          
         SR    RE,RE                                                            
         IC    RE,FORCURT+(CURTDECP-CURTABD)                                    
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,RF                                                            
         STC   RE,ATLXSHFT         SET SHIFT VALUE                              
*                                                                               
VALCUR18 ZAP   DUB,PZERO           DISPLAY EXCHANGE RATE                        
         MVO   DUB,ATLXRATE                                                     
         CURED DUB,(L'RECRAT,RECRAT),5,ALIGN=LEFT                               
         LA    R1,RECRAT                                                        
         AR    R1,R0                                                            
         MVC   1(L'CURTCUR,R1),FORCURT+(CURTCUR-CURTABD)                        
         MVI   1+L'CURTCUR(R1),C'-'                                             
         MVC   1+L'CURTCUR+1(L'CURTCUR,R1),AGYCURT+(CURTCUR-CURTABD)            
*                                                                               
         OI    TWAMODE2,TWA2ALTO   DEFAULT TO ALTERNATE TOTALS                  
*                                                                               
VALCURX  TM    CURIND1,CUR1SINC    TEST DIFFERENCES/CHARGES PERMITTED           
         BO    *+10                                                             
         XC    BANKXTRA(BANKXTRL),BANKXTRA                                      
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK CHARGE AMOUNT                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBCH   ZAP   BCHAAMT,PZERO       CLEAR BANK CHARGE AMOUNT                     
         ZAP   BCHAAM2,PZERO       CLEAR BANK CHARGE AMOUNT (CURRENCY)          
         GOTO1 AFVAL,RECBCHH                                                    
         BH    EXIT                ERROR                                        
         BE    VALBCH02                                                         
         XC    BCHA,BCHA                                                        
         XC    BCHACOST,BCHACOST                                                
         XC    BCHANAME,BCHANAME                                                
         MVI   BCHAOPOK,0                                                       
         B     VALBCHX                                                          
*                                                                               
VALBCH02 MVC   FVMSGNO,=AL2(EGIFNOTV)                                           
         CP    CHQAMT,PZERO        TEST CHEQUE PRESENT                          
         BE    VALHEDE                                                          
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    VALHEDE                                                          
         MVC   FVMSGNO,=AL2(AE$BCHAM)                                           
         OC    BCHA,BCHA           TEST BANK CHARGE ACCOUNT KNOWN               
         BZ    VALHEDE                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BZ    *+8                                                              
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
         LA    RF,X'80'(RF)                                                     
         GOTO1 VCASHVAL,DMCB,((RF),RECBCH),(X'C0',(R0))                         
         CLI   0(R1),0                                                          
         BNE   EXIT                INVALID AMOUNT                               
         TM    BNKASTS1,ASTSLOCL   TEST BANK ACCOUNT HELD LOCALLY               
         BO    VALBCH04                                                         
         ZAP   BCHAAMT,4(8,R1)     BANK CHARGES IN AGENCY CURRENCY              
         EXCHP BCHAAMT,ATLX        CALCULATE LOCAL CURRENCY                     
         ZAP   BCHAAM2,DUB         SAVE BANK CHARGES IN LOCAL CURRENCY          
         CURED BCHAAMT,(L'RECBCH,RECBCH),AGYCURT,ALIGN=LEFT,FLOAT=-             
         CURED BCHAAM2,(L'RECBC2,RECBC2),FORCURT,ALIGN=LEFT,FLOAT=-,   *        
               CURSYMB=Y                                                        
         B     VALBCH06                                                         
*                                                                               
VALBCH04 ZAP   BCHAAM2,4(8,R1)     BANK CHARGES IN LOCAL CURRENCY               
         MVC   TEMP,ATLX                                                        
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP BCHAAM2,TEMP        CALCULATE AGENCY CURRENCY                    
         ZAP   BCHAAMT,DUB         SAVE BANK CHARGES IN AGENCY CURRENCY         
         CURED BCHAAM2,(L'RECBCH,RECBCH),FORCURT,ALIGN=LEFT,FLOAT=-             
         CURED BCHAAMT,(L'RECBC2,RECBC2),AGYCURT,ALIGN=LEFT,FLOAT=-,   *        
               CURSYMB=Y                                                        
*                                                                               
VALBCH06 CP    BCHAAMT,PZERO       TEST ZERO AMOUNT                             
         BE    VALHEDE                                                          
         CP    BCHAAM2,PZERO       TEST ZERO AMOUNT (CURRENCY)                  
         BE    VALHEDE                                                          
         OI    RECBCHH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RECBC2H+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VALBCHX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ANALYSIS BANK ACCOUNT                                      *         
***********************************************************************         
         SPACE 1                                                                
VALANL   TM    RECANLH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALANLX                                                          
         GOTO1 AVALANL,RECANLH                                                  
         BH    EXIT                                                             
         BL    VALANLX                                                          
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    VALANL02                                                         
         CLC   RECOFFC,SPACES                                                   
         BE    VALANL02                                                         
         CLC   BANKOFF,SPACES                                                   
         BE    VALANL02                                                         
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   RECOFFC,BANKOFF     MUST BE SAME OFFICE                          
         BNE   EXIT                EXIT WITH CC SET NEQ                         
VALANL02 TM    CURIND1,CUR1BNKC    TEST CURRENCY BANK ACCOUNT                   
         BZ    VALANL04                                                         
         MVC   FVMSGNO,=AL2(AE$CURMB)                                           
         CLC   RECCURR,BANKCUR     MUST BE SAME CURRENCY                        
         BNE   EXIT                EXIT WITH CC SET NEQ                         
VALANL04 GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',ANALUL),                X        
               (X'C0',RECNDSP),AIO1,(L'ANALNAME,ANALNAME)                       
VALANLX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECEIVING ACCOUNT (NEED NOT BE IN RECEIVABLE LEDGER)       *         
***********************************************************************         
         SPACE 1                                                                
VALRCV   TM    RECRCVH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALRCVX                                                          
         GOTO1 AVALRCV,RECRCVH                                                  
         BNE   EXIT                                                             
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    VALRCVX                                                          
         CLC   RECVOFF,SPACES                                                   
         BE    VALRCVX                                                          
         CLC   BANKOFF,SPACES      TEST SPACES OR ABSENT                        
         BNH   VALRCVX                                                          
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   RECVOFF,BANKOFF     MUST BE RECEIVABLE A/C OFFICE                
         BNE   EXIT                                                             
VALRCVX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF ACCOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALWAC   TM    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWACX                                                          
         GOTO1 AVALWAC,RECWACH                                                  
         BH    EXIT                                                             
         BL    VALWACX                                                          
         TM    TWAAUTH,ACTIAUT8    TEST AUTHORISED TO WRITE-OFF                 
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     VALHEDE                                                          
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,0,                            X        
               (X'C0',RECNDSP),AIO1,(L'WOFFNAME,WOFFNAME)                       
VALWACX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WRITE-OFF CLIENT/PRODUCT                                   *         
***********************************************************************         
         SPACE 1                                                                
VALWCP   TM    RECWCPH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWCPX                                                          
         CLI   EXDFOPOK,0          TEST EXCHANGE DIFF OFFPOS IN KEY             
         BNE   VALWCP10                                                         
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BH    VALWCP10                                                         
         CLC   DISCULSI,EXDF       TEST EXCHANGE DIFFERENCES TO SI              
         BE    VALWCP10                                                         
         CP    BCHAAMT,PZERO       TEST BANK CHARGES TO BE MADE                 
         BE    VALWCP02                                                         
         CLI   BCHAOPOK,0          TEST BANK CHARGE OFFPOS IN KEY               
         BNE   VALWCP10                                                         
         CLI   BCHACOST,C' '       TEST BANK CHARGE A/C ANALYSIS FLAG           
         BH    VALWCP10                                                         
         CLC   DISCULSI,BCHA       TEST BANK CHARGES TO SI                      
         BE    VALWCP10                                                         
VALWCP02 CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    VALWCP10                                                         
         CLC   DISCULSI,WOFF       TEST WRITE-OFFS TO SI                        
         BE    VALWCP10                                                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BH    VALWCP10                                                         
         CLC   DISCULSI,DISC       TEST DISCOUNTS TO SI                         
         BNE   *+8                                                              
VALWCP10 MVI   FVMINL,1            WRITE-OFF CLI/PRO REQUIRED                   
         GOTO1 AVALWCP,RECWCPH                                                  
         BH    EXIT                INVALID OR REQUIRED AND NOT INPUT            
VALWCPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE VAT ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
VALVAT   TM    RECVATH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALVATX                                                          
         GOTO1 AVALVAT,RECVATH                                                  
         BH    EXIT                                                             
         BL    VALVATX                                                          
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         CLI   PROFWVAT,C'Y'       TEST PROFILE FOR VAT WRITE-OFF ADJ.          
         BNE   EXIT                EXIT WITH CC SET NEQ                         
         TM    COMPSTAT,CPYSOROE   TEST COMPANY ON OFFICES                      
         BZ    VALVAT2                                                          
         CLC   RECVOFF,SPACES                                                   
         BE    VALVAT2                                                          
         CLC   RECOFFC,SPACES                                                   
         BE    VALVAT2                                                          
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   RECOFFC,RECVOFF     MUST BE SAME OFFICE                          
         BNE   EXIT                EXIT WITH CC SET NEQ                         
VALVAT2  OC    WOFF,WOFF                                                        
         BNZ   VALVAT4                                                          
         LA    R1,RECWACH          SET CURSOR TO WRITE-OFF ACCOUNT              
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     VALHEDE             ERROR EXIT SETS CC NEQ                       
VALVAT4  GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',VATUL)                  X        
               (X'C0',RECNDSP),AIO1,(L'VATNAME,VATNAME)                         
VALVATX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE WRITE-OFF DATE/REFERENCE/NARRATIVE                         *         
***********************************************************************         
         SPACE 1                                                                
VALWDR   TM    RECWRFH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALWDRX                                                          
         XC    WOFDATE,WOFDATE                                                  
         MVC   WOFREF,SPACES                                                    
         MVC   WOFNARR,SPACES                                                   
         OC    WOFF,WOFF           TEST WRITE-OFF ACCOUNT INPUT                 
         BNZ   VALWDR2                                                          
         LA    R1,RECWDTH          ENSURE NO WRITE-OFF DATE                     
         CLI   FVILEN-FVIHDR(R1),0                                              
         BNE   VALWDR1                                                          
         LA    R1,RECWRFH          ENSURE NO WRITE-OFF REFERENCE                
         CLI   FVILEN-FVIHDR(R1),0                                              
         BNE   VALWDR1                                                          
         LA    R1,RECWNRH          ENSURE NO WRITE-OFF NARRATIVE                
         CLI   FVILEN-FVIHDR(R1),0                                              
         BE    VALWDRX                                                          
VALWDR1  ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EANOWOFF)                                           
         TM    TWAAUTH,ACTIAUT8    TEST AUTHORISED TO WRITE-OFF                 
         BNZ   VALHEDE                                                          
         MVC   FVMSGNO,=AL2(EASECLOC)                                           
         B     VALHEDE             EXIT SETS CC NEQ                             
*                                                                               
VALWDR2  GOTO1 AFVAL,RECWRFH                                                    
         BH    EXIT                                                             
         BE    VALWDR3                                                          
         CLI   RECWDTH+(FVILEN-FVIHDR),0                                        
         BE    VALWDR5                                                          
         MVC   FVMSGNO,=AL2(EGIFMISS)                                           
         B     EXIT                                                             
*                                                                               
VALWDR3  MVC   WOFREF,FVIFLD                                                    
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECWDTH                                                    
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         BNE   EXIT                                                             
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   VALHEDE             EXIT SETS CC NEQ                             
         LA    RF,WORK             EDIT OUT DATE                                
         USING PERVALD,RF                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    *+12                                                             
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    VALHEDE             ERROR IF ONLY YEAR INPUT                     
         TM    COMPSTA4,CPYSOV12   TEST ALLOW OVER 12 MONTHS BACK DATE          
         BNZ   VALWDR4             YES - SKIP TEST FOR BACKDATE                 
         SR    RE,RE                                                            
         ICM   RE,1,TODAYB                                                      
         BNZ   *+8                                                              
         LA    RE,100              YEAR 2000 IS 100 RELATIVE                    
         MH    RE,=H'12'                                                        
         SR    R1,R1                                                            
         IC    R1,TODAYB+1                                                      
         AR    RE,R1               RE=(CURRENT YEAR*12)+MONTH                   
         SH    RE,=H'12'                                                        
         STH   RE,DUB                                                           
         SR    RE,RE                                                            
         ICM   RE,1,PVALBSTA                                                    
         BNZ   *+8                                                              
         LA    RE,100                                                           
         MH    RE,=H'12'                                                        
         IC    R1,PVALBSTA+1                                                    
         AR    RE,R1               RE=CURRENT RELATIVE MONTH                    
         CH    RE,DUB              TEST MORE THAN NN MONTHS AGO                 
         BL    VALHEDE                                                          
*                                                                               
VALWDR4  MVC   WOFDATE,PVALPSTA    PWOS YYMMDD                                  
         XC    RECWDT,RECWDT                                                    
         OI    RECWDTH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VDATCON,DMCB,(1,WOFDATE),(17,RECWDT)                             
         DROP  RF                                                               
*                                                                               
VALWDR5  GOTO1 AFVAL,RECWNRH                                                    
         BH    EXIT                                                             
         BL    VALWDRX                                                          
         MVC   WOFNARR,FVIFLD                                                   
VALWDRX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OFFSET DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALODT   MVC   OFSDATE,TODAYP                                                   
         GOTO1 AFVAL,RECODTH                                                    
         BL    VALODTX                                                          
         MVC   FVMSGNO,=AL2(EGDATINV)                                           
         BH    EXIT                                                             
         MVC   WORK(1),AGYLANG     SET LANGUAGE                                 
         OI    WORK,X'60'          SINGLE DATE ONLY, RETURN AS SINGLE           
         GOTO1 VPERVAL,DMCB,(FVILEN,FVIFLD),(WORK,WORK)                         
         TM    4(R1),X'03'         CHECK VALIDITY                               
         BNZ   VALHEDE             EXIT SETS CC NEQ                             
         LA    RF,WORK             EDIT OUT DATE                                
         USING PERVALD,RF                                                       
         TM    PVALASSM,PVALASD+PVALASM+PVALASY  TEST ALL ASSUMED               
         BO    *+12                                                             
         TM    PVALASSM,PVALASD+PVALASM          TEST DAY/MON ASSUMED           
         BO    VALHEDE             ERROR IF ONLY YEAR INPUT                     
         MVC   OFSDATE,PVALPSTA    PWOS YYMMDD                                  
         XC    RECODT,RECODT                                                    
         OI    RECODTH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 VDATCON,DMCB,(1,OFSDATE),(17,RECODT)                             
         DROP  RF                                                               
VALODTX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DISCOUNT/SURCHARGE/SERVICE FEE ACCOUNT                     *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   TM    RECDSCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDSCX                                                          
         MVI   DISCINDS,0          CLEAR DISCOUNT ACCOUNT INDICATOR             
         GOTO1 AVALDSC,RECDSCH                                                  
         BH    EXIT                ERROR                                        
         BL    VALDSCX             NOT INPUT - NOT REQUIRED                     
*                                                                               
         OI    DISCINDS,DISCIADD   SET DON'T ADD DISCOUNT TO BALANCE            
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,0,                            X        
               (X'C0',RECNDSP),AIO1,(L'DISCNAME,DISCNAME)                       
         CLC   DISCULSI,DISC       IF DISCOUNT A/C IS INCOME                    
         BE    *+12                                                             
         CLI   DISCCOST,C' '       OR DISCOUNT A/C FLAGGED FOR COSTING          
         BNH   VALDSCX                                                          
         CLI   WCPCNT,0            CHECK FOR A PRODUCTION CLI/PRO               
         BNE   VALDSCX                                                          
         LA    R1,RECWCPH          SET CURSOR TO WRITE-OFF CLI/PRO              
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(EAWCPREQ)                                           
         B     VALHEDE             ERROR EXIT SETS CC NEQ                       
*                                                                               
VALDSCX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATE RANGE FILTERS                                         *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   TM    RECDATH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDATX                                                          
         GOTO1 AVALDAT,RECDATH                                                  
         BH    EXIT                                                             
VALDATX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE MOS RANGE FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALMOS   TM    RECMOSH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALMOSX                                                          
         GOTO1 AVALMOS,RECMOSH                                                  
         BH    EXIT                                                             
VALMOSX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILL RANGE FILTERS                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBNR   TM    RECBNRH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNRX                                                          
         GOTO1 AVALBNR,RECBNRH                                                  
         BH    EXIT                                                             
VALBNRX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILLING SOURCE FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
VALBSO   TM    RECBSOH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBSOX                                                          
         GOTO1 AVALBSO,RECBSOH                                                  
         BH    EXIT                                                             
VALBSOX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALL COSTING ACCOUNTS WHICH MIGHT BE NEEDED                 *         
***********************************************************************         
         SPACE 1                                                                
VALCOST  CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BH    VALCOS02                                                         
         CLI   BCHACOST,C' '       TEST BANK CHARGE A/C ANALYSIS FLAG           
         BH    VALCOS02                                                         
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    VALCOS02                                                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   VALCOSTX            NO COSTING ANALYSIS POSTINGS                 
VALCOS02 LA    R3,RECVTAB          R3=A(RECEIVABLE A/C TABLE)                   
         USING RECVTABD,R3                                                      
         LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         LA    R1,RECRCVH          SET A(RECEIVABLE A/C(S) FIELD)               
         ST    R1,FVADDR                                                        
*                                                                               
VALCOS22 CLI   RECVNDX,FF          TEST E-O-T                                   
         BE    VALCOS24            FINISHED WITH RECEIVABLE A/C(S)              
         MVC   FVINDX,RECVNDX      SET MULTIPLE FIELD INDEX FOR ERROR           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVCOST),RECVCOST                                     
         GOTO1 AGETACC,0           GET 1C COSTING ACCOUNT                       
         BNE   EXIT                                                             
         LA    R3,RECVTABL(R3)     NEXT TABLE ENTRY                             
         B     VALCOS22            TEST NEXT ACCOUNT                            
*                                                                               
VALCOS24 MVI   FVINDX,0            CLEAR MULTIPLE FIELD INDEX FOR ERROR         
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   VALCOS28            NO - DEAL WITH DISCOUNT A/C FLAG             
         LA    R1,RECWACH          SET A(WRITE-OFF A/C FIELD)                   
         ST    R1,FVADDR                                                        
         CLC   DISCULSI,WOFF       TEST INCOME COSTING ANALYSIS                 
         BNE   VALCOS26                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS28                                                         
*                                                                               
VALCOS26 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'WOFFCOST),WOFFCOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS28                                                         
         EJECT                                                                  
VALCOS28 CLI   DISCCOST,C' '       NOW TEST DISCOUNT A/C ANALYSIS FLAG          
         BNH   VALCOS34                                                         
         LA    R1,RECDSCH                                                       
         ST    R1,FVADDR           SET A(DISCOUNT A/C FIELD)                    
         CLC   DISCULSI,DISC       TEST INCOME COSTING ANALYSIS                 
         BNE   VALCOS32                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS34                                                         
*                                                                               
VALCOS32 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'DISCCOST),DISCCOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   EXIT                                                             
         EJECT                                                                  
VALCOS34 CLI   EXDFOPOK,0          TEST EXCHANGE DIFF OFFPOS IN KEY             
         BNE   VALCOS38                                                         
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BNH   VALCOS38                                                         
         LA    R1,RECBNKH                                                       
         ST    R1,FVADDR           SET A(BANK A/C FIELD)                        
         CLC   DISCULSI,EXDF       TEST INCOME COSTING ANALYSIS                 
         BNE   VALCOS36                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'EXDFCOST),EXDFCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS38                                                         
*                                                                               
VALCOS36 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'EXDFCOST),EXDFCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'EXDFCOST),EXDFCOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   EXIT                                                             
*                                                                               
VALCOS38 CLI   BCHAOPOK,0          TEST BANK CHARGE OFFPOS IN KEY               
         BNE   VALCOS42                                                         
         CLI   BCHACOST,C' '       TEST BANK CHARGE A/C ANALYSIS FLAG           
         BNH   VALCOS42                                                         
         LA    R1,RECBCHH                                                       
         ST    R1,FVADDR           SET A(BANK CHARGES FIELD)                    
         CLC   DISCULSI,BCHA       TEST INCOME COSTING ANALYSIS                 
         BNE   VALCOS40                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'BCHACOST),BCHACOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS42                                                         
*                                                                               
VALCOS40 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'BCHACOST),BCHACOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'BCHACOST),BCHACOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   EXIT                                                             
*                                                                               
VALCOS42 DS    0H                                                               
*                                                                               
VALCOSTX DS    0H                                                               
         DROP  R2,R3                                                            
         SPACE 2                                                                
VALHEDX  CR    RB,RB               HEADER VALIDATION GOOD EXIT                  
         B     EXIT                                                             
*                                                                               
VALHEDE  LTR   RB,RB               HEADER VALIDATION ERROR EXIT                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* RESTORE - RESTORE HEADER SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
RESHED   DS    0H                                                               
         OI    RECBMONH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECBREFH+(FVATRB-FVIHDR),FVAPROT                                 
         OC    BANK,BANK           PROTECT BANK A/C IF PRESENT                  
         BZ    *+8                                                              
         OI    RECBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECRCVH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    RESHED02                                                         
         OI    RECCHQH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECCUCH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECDEPH+(FVATRB-FVIHDR),FVAPROT                                  
         TM    CURIND1,CUR1BNKC    TEST FOREIGN CURRENCY BANK ACCOUNT           
         BO    RESHED04                                                         
         OI    RECCURH+(FVATRB-FVIHDR),FVAPROT                                  
         B     RESHED04                                                         
*                                                                               
RESHED02 OI    RECCUCH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECCURH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
RESHED04 OC    WOFF,WOFF           PROT WRITE-OFF A/C IF PRESENT                
         BZ    *+8                                                              
         OI    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   WCPCNT,0            PROT W-O CLI/PRO IF PRESENT                  
         BE    *+8                                                              
         OI    RECWCPH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECVATH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECDSCH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECDATH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECMOSH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECBNRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECBSOH+(FVATRB-FVIHDR),FVAPROT                                  
RESHEDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LAST - RESTORE HEADER SCREEN AFTER UPDATE                           *         
***********************************************************************         
         SPACE 1                                                                
RELAST   DS    0H                                                               
         NI    RECBMONH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    RECBREFH+(FVATRB-FVIHDR),FF-(FVAPROT)                            
         NI    RECBNKH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECCHQH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         XC    RECCHQ,RECCHQ       CLEAR CHEQUE AMOUNT ALWAYS                   
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    RECDEPH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECCUCH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECCURH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECRCVH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECWACH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECWCPH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECVATH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECDSCH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECDATH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECMOSH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECBNRH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
         NI    RECBSOH+(FVATRB-FVIHDR),FF-(FVAPROT)                             
RELASTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT TO ROOT                                                        *         
***********************************************************************         
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
COVLITS  DS    0X                  ** CONSTANTS FOR OVERLAY LITERALS **         
*                                                                               
CBANKULS DC    C'GB'               VALID LEDGERS FOR BANK                       
         DC    C'GP'                                                            
         DC    C'SC'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
CANALULS DC    C'SC'               VALID LEDGERS FOR ANAL BANK                  
         DC    AL1(EOT)                                                         
*                                                                               
CDISCULS DC    C'SE'               VALID LEDGERS FOR DISCOUNT                   
         DC    C'SI'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
CRECVULS DC    C'SR'               VALID LEDGERS FOR RECEIVABLES                
         DC    AL1(EOT)                                                         
*                                                                               
CWOFFULS DC    C'SE'               VALID LEDGERS FOR WRITE-OFF                  
         DC    C'SI'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
         DC    C'12'               12 COSTING U/L                               
         DC    C'13'               13 COSTING U/L                               
         DC    C'1P99'             1P COSTING U/L/DEPT                          
*                                                                               
COVLITSL EQU   *-COVLITS                                                        
         EJECT                                                                  
DINLST   DS    0C                                                               
         DCDDL AC#CTRD,9                                                        
*                                                                               
         DCDDL AC#ACC,3                                                         
         DCDDL AC#DATE,4                                                        
         DCDDL AC#WRTFN,11                                                      
         DCDDL AC#WRTFD,14                                                      
         DCDDL AC#INVC2,9                                                       
*                                                                               
         DCDDL AC#XFRFR,16                                                      
         DCDDL AC#XFRTO,14                                                      
         DCDDL AC#ON,3                                                          
*                                                                               
         DCDDL AC#RCVA,14                                                       
         DCDDL AC#BNKA,12                                                       
         DCDDL AC#NRTV,9                                                        
         DCDDL AC#BATTS,12                                                      
         DCDDL AC#ALCTD,9                                                       
         DCDDL AC#SPCLS,8                                                       
         DCDDL AC#CHKP,14                                                       
         DCDDL AC#CHKAM,13                                                      
         DCDDL AC#WRTNF,11                                                      
         DCDDL AC#WRTFA,13                                                      
         DCDDL AC#WRTOV,14                                                      
         DCDDL AC#CTRD,9                                                        
         DCDDL AC#DSF,11                                                        
         DCDDL AC#DSFA,15                                                       
         DCDDL AC#XFRD,11                                                       
         DCDDL AC#TDC,11                                                        
         DCDDL AC#BAL,7                                                         
         DCDDL AC#ITEMS,5                                                       
         DCDDL AC#EXDIF,20                                                      
*                                                                               
         DCDDL AC#DEF,12                                                        
         DCDDL AC#FTRAT,12                                                      
         DCDDL AC#CALC,12                                                       
*                                                                               
DINLSTX  DC    AL1(EOT)            END OF DICTIONARY INPUT LIST                 
*                                                                               
TINLST   DS    0C                                                               
         DCDDL AC#BAL,12           TOTALS HEADINGS INPUT LIST                   
         DCDDL AC#CHK,12                                                        
         DCDDL AC#ALCTD,12                                                      
         DCDDL AC#SPCLS,12                                                      
         DCDDL AC#DSF,12                                                        
         DCDDL AC#WRTNF,12                                                      
         DCDDL AC#CTRD,12                                                       
         DCDDL AC#VTAD,12                                                       
*                                                                               
TINLSTX  DC    AL1(EOT)            END OF DICTIONARY INPUT LIST                 
         EJECT                                                                  
***********************************************************************         
* UPDATE - MAKE POSTINGS VIA ACUPDATE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RECVTABD,R4         R4=A(RECEIVABLES TABLE ENTRY)                
         USING REPD,R5             R5=A(REPORT WORKING STORAGE)                 
         DS    0F                                                               
UPDATE   NMOD1 0,**REUP**,RA,R9                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         SR    RF,RF                                                            
         ICM   RF,1,UPDMODE                                                     
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     UPDFRST                                                          
         B     UPDRCVF                                                          
         B     UPDPROC                                                          
         B     UPDRCVL                                                          
         B     UPDLAST                                                          
         EJECT                                                                  
UPDFRST  MVC   POSTNARR,SPACES                                                  
         TM    TWAMODE2,TWA2NALL    TEST NO ALLOCATION BATCH                    
         BO    UPDF02                                                           
         GOTO1 VDATCON,DMCB,(0,BANKDATE),(1,BANKDATP)                           
         GOTO1 ABLDNAR,POSTNARR                                                 
         LA    R1,REPH6                                                         
         USING JRNHEDD,R1                                                       
         MVC   JRNHTXT(L'JRNNARR),JRNNARR                                       
         MVC   JRNHACT(L'REPH6-(JRNHACT-JRNHEDD)),POSTNARR                      
UPDF02   TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+16                                                             
         MVC   REPH9+((JRNDR2-JRNLINED)+L'JRNDR2-L'CURTCUR-1)(L'CURTCURX        
               ),FORCURT+(CURTCUR-CURTABD)                                      
         MVC   REPH9+((JRNCR2-JRNLINED)+L'JRNCR2-L'CURTCUR-1)(L'CURTCURX        
               ),FORCURT+(CURTCUR-CURTABD)                                      
         MVC   OVRNARR,POSTNARR    SAVE STANDARD NARRATIVE                      
         LA    R0,JRNSPEC                                                       
         ST    R0,OVERSPEC         SAVE A(PRINT SPECS)                          
         LA    R0,ELEXTRA                                                       
         ST    R0,OVERXTRA         SAVE A(EXTRA ELEMENT AREA)                   
         LA    RF,JTACCUMS         CLEAR JOURNAL TOTALS                         
         LA    R0,JTACCUMN                                                      
         ZAP   0(L'JTACCUMS,RF),PZERO                                           
         LA    RF,L'JTACCUMS(RF)                                                
         BCT   R0,*-10                                                          
         ZAP   TOTITEM,PZERO       TOTAL ITEMS                                  
*                                                                               
         SR    RF,RF               SET SUB-PROGRAM NUMBER                       
         TM    TWAMODE,TWAMDRFT    TEST DRAFT JOURNAL MODE                      
         BZ    UPDF04                                                           
         LA    RF,1                                                             
         CLI   UPDACTN,ACTFILT     TEST FILTERED DRAFT JOURNAL                  
         BNE   UPDF04                                                           
         LA    RF,2                                                             
UPDF04   TM    CURIND1,CUR1SINC                                                 
         BZ    UPDF06                                                           
         LA    RF,6(RF)            CURRENCY SUB-PROGRAM NUMBERS                 
UPDF06   STC   RF,REPSUBPG                                                      
         B     UPDATEX                                                          
         EJECT                                                                  
UPDRCVF  OI    REPHEADI,REPHFRCE   FORCE NEW PAGE FOR NEW ACCOUNT               
         LA    R1,REPH4                                                         
         MVC   JRNHTXT(L'JRNRECV),JRNRECV                                       
         MVC   JRNHACT,RECVACT+1                                                
         MVC   JRNHACTN,RECVACTN                                                
         LA    R1,REPH5                                                         
         MVC   JRNHTXT(L'JRNBANK),JRNBANK                                       
         MVC   JRNHACT,BANK                                                     
         MVC   JRNHACTN,BANKNAME                                                
         LA    RF,JRACCUMS         CLEAR JOURNAL RECEIVABLE TOTALS              
         LA    R0,JRACCUMN                                                      
         ZAP   0(L'JRACCUMS,RF),PZERO                                           
         LA    RF,L'JRACCUMS(RF)                                                
         BCT   R0,*-10                                                          
         ZAP   RCVITEM,PZERO       RECEIVABLE TOTAL ITEMS                       
*                                                                               
         MVI   OVRBYTE,0           CLEAR COSTING POSTINGS INDICATOR             
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   *+8                                                              
         OI    OVRBYTE,NOCWOF      NO WRITE-OFF COSTING                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   *+8                                                              
         OI    OVRBYTE,NOCDSC      NO DISCOUNT COSTING                          
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BNH   *+8                                                              
         OI    OVRBYTE,NOCEXC      NO EXCHANGE DIFFERENCE COSTING               
         CLI   BCHACOST,C' '       TEST BANK CHARGE A/C ANALYSIS FLAG           
         BNH   *+8                                                              
         OI    OVRBYTE,NOCBCH      NO BANK CHARGE COSTING                       
         CLI   OVRBYTE,0           TEST ANY COSTING POSTINGS                    
         BE    UPDRCVFX                                                         
*                                                                               
         LA    R2,KEY                                                           
         USING ACTRECD,R2                                                       
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVCOST),RECVCOST                                     
         GOTO1 AGETACC,0           GET 1C COSTING ACCOUNT                       
         BNE   UPDRCVFX                                                         
         MVC   COSTCACN,RECNAME    SAVE 1C COSTING ACCOUNT NAME                 
*                                                                               
         MVC   THISCOST,WOFFCOST                                                
         GOTO1 SETCST,WOFF                                                      
         MVC   CWOFACTS(CWOFACTL),THISACTS                                      
         BNE   *+8                                                              
         NI    OVRBYTE,FF-(NOCWOF) SET WRITE-OFF COSTING OK                     
*                                                                               
         MVC   THISCOST,DISCCOST                                                
         GOTO1 SETCST,DISC                                                      
         MVC   CDSCACTS(CDSCACTL),THISACTS                                      
         BNE   *+8                                                              
         NI    OVRBYTE,FF-(NOCDSC) SET DISCOUNT COSTING OK                      
*                                                                               
         CLI   EXDFOPOK,0          TEST EXCHANGE DIFF OFFPOS IN KEY             
         BNE   UPDRCF04            RESOLVE ACCOUNTS LATER                       
         MVC   THISCOST,EXDFCOST                                                
         GOTO1 SETCST,EXDF                                                      
         MVC   CEXCACTS(CEXCACTL),THISACTS                                      
         BNE   UPDRCF04                                                         
         NI    OVRBYTE,FF-(NOCEXC) SET EXCHANGE DIFF COSTING OK                 
*                                                                               
UPDRCF04 CLI   BCHAOPOK,0          TEST BANK CHARGE OFFPOS IN KEY               
         BNE   UPDRCF08            RESOLVE ACCOUNTS LATER                       
         MVC   THISCOST,BCHACOST                                                
         GOTO1 SETCST,BCHA                                                      
         MVC   CBCHACTS(CBCHACTL),THISACTS                                      
         BNE   UPDRCF08                                                         
         NI    OVRBYTE,FF-(NOCBCH) SET BANK CHARGE COSTING OK                   
*                                                                               
UPDRCF08 DS    0H                                                               
*                                                                               
UPDRCVFX B     UPDATEX                                                          
         DROP  R1,R2                                                            
         EJECT                                                                  
UPDPROC  LA    RF,JRCVWOF          ADDRESS WRITE-OFF TOTALS                     
         LA    RE,JTOTWOF                                                       
         TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BO    UPDPRO18                                                         
         LA    RF,JRCVOFS          ADDRESS OFFSET TOTALS                        
         LA    RE,JTOTOFS                                                       
         TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BO    UPDPRO18                                                         
         LA    RF,JRCVTRF          ADDRESS TRANSFER TOTALS                      
         LA    RE,JTOTTRF                                                       
         TM    TSARINDS,TSARITRF   TEST TRANSFERRED                             
         BO    UPDPRO18                                                         
         AP    RECVAMT,TSARPOST    ADD TO RECEIVABLE TOTAL                      
         AP    RECVAM2,TSARPO2                                                  
*                                                                               
         LA    R1,OFFTAB1          POST AMOUNT TO RECEIVABLE TABLE              
         USING OFFTABD,R1                                                       
         LA    R0,OFFTMAX                                                       
UPDPRO02 CLI   OFFTABD,OFFTEOTQ    TEST FREE ENTRY                              
         BE    UPDPRO06                                                         
         CLC   OFFTOFFC,TSAROFFC   MATCH ON OFFICE CODE                         
         BNE   UPDPRO04                                                         
         AP    OFFTAMNT,TSARPOST   YES - ADD TO OFFICE POSTING                  
         AP    OFFTAM2,TSARPO2                                                  
         B     UPDPRO08                                                         
UPDPRO04 LA    R1,OFFTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDPRO02         DO FOR NUMBER OF TABLE ENTRIES               
         DC    H'0'                                                             
UPDPRO06 MVC   OFFTOFFC,TSAROFFC   CREATE NEW TABLE ENTRY                       
         ZAP   OFFTAMNT,TSARPOST                                                
         ZAP   OFFTAM2,TSARPO2                                                  
*                                                                               
UPDPRO08 LA    R1,OFFTAB2          POST AMOUNT TO ANALYSIS BANK TABLE           
         LA    R0,OFFTMAX                                                       
UPDPRO10 CLI   OFFTABD,OFFTEOTQ    TEST FREE ENTRY                              
         BE    UPDPRO14                                                         
         CLC   OFFTOFFC,TSAROFFC   MATCH ON OFFICE CODE                         
         BNE   UPDPRO12                                                         
         AP    OFFTAMNT,TSARPOST   YES - ADD TO OFFICE POSTING                  
         AP    OFFTAM2,TSARPO2                                                  
         B     UPDPRO16                                                         
UPDPRO12 LA    R1,OFFTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDPRO10         DO FOR NUMBER OF TABLE ENTRIES               
         DC    H'0'                                                             
UPDPRO14 MVC   OFFTOFFC,TSAROFFC   CREATE NEW TABLE ENTRY                       
         ZAP   OFFTAMNT,TSARPOST                                                
         ZAP   OFFTAM2,TSARPO2                                                  
         DROP  R1                                                               
*                                                                               
UPDPRO16 LA    RF,JRCVMRK          ADDRESS ALLOCATION TOTALS                    
         LA    RE,JTOTMRK                                                       
         CLI   TSARTYPE,TSARTOVR   TEST DIFFERENCE                              
         BNE   UPDPRO18                                                         
         LA    RF,JRCVDIF          ADDRESS DIFFERENCE TOTALS                    
         LA    RE,JTOTDIF                                                       
*                                                                               
UPDPRO18 AP    0(L'JRACCUMS,RF),TSARPOST                                        
         AP    0(L'JTACCUMS,RE),TSARPOST                                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+16                                                             
         AP    L'JRACCUMS(L'JRACCUMS,RF),TSARPO2                                
         AP    L'JTACCUMS(L'JTACCUMS,RE),TSARPO2                                
         MVC   POSTACT,RECVACT+1   BUILD RECEIVABLE/SOURCE POSTING              
         MVC   POSTACTN,RECVACTN                                                
         MVC   POSTCAC,TSARCAC                                                  
         MVC   POSTCACN,SPACES     NO CONTRA NAME (IT'S THE SOURCE)             
         MVC   POSTDATE,TSARDAT                                                 
         MVC   POSTREF,TSARREF                                                  
         MVI   POSTSTAT,TRNSAUTH                                                
         ZAP   POSTAM2,TSARPO2                                                  
         ZAP   POSTAMNT,TSARPOST                                                
         MVC   POSTOFFC,TSAROFFC                                                
         OI    UPDINDS,UPDIPRTS    PRINT STATUS ON JOURNAL                      
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BZ    UPDPRO20                                                         
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         TM    TSARINDS,TSARISDR   TEST DEBIT                                   
         BZ    *+8                                                              
         OI    POSTSTAT,TRNSDR     SET DEBIT                                    
         ZAP   DUB,POSTAM2                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   POSTAM2,DUB                                                      
         ZAP   DUB,POSTAMNT                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   POSTAMNT,DUB                                                     
         BAS   RE,BLDTNAR                                                       
         B     UPDPRO22                                                         
UPDPRO20 TM    TSARINDS,TSARIOFS   TEST OFFSET POSTING                          
         BZ    UPDPRO24                                                         
         MVC   POSTNARR,SPACES     SPECIAL NARRATIVE                            
         MVC   POSTNARR(L'TXTOFS),TXTOFS                                        
         LA    RF,POSTNARR+L'TXTOFS-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         MVC   0(L'TXTON,RF),TXTON                                              
         LA    RF,L'TXTON+1(RF)                                                 
         GOTO1 VDATCON,DMCB,(1,OFSDATE),(17,0(RF))                              
UPDPRO22 BAS   RE,BLDAFC           BUILD AFCEL IF REQUIRED                      
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         BAS   RE,PSTEXC           POST EXCHANGE DIFFERENCE IF REQUIRED         
         MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BNZ   UPDPRO42            PRODUCE TRANSFER POSTING                     
         B     UPDPROCX            EXIT                                         
*                                                                               
UPDPRO24 TM    TSARINDS,TSARIWOF   TEST WRITE-OFF POSTING                       
         BZ    *+8                                                              
         BAS   RE,BLDWNAR          BUILD WRITE-OFF NARRATIVE                    
         BAS   RE,BLDAFC           BUILD AFCEL IF REQUIRED                      
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         BAS   RE,PSTEXC           POST EXCHANGE DIFFERENCE IF REQUIRED         
*                                                                               
         TM    TSARINDS,TSARIWOF   IS THIS A WRITE-OFF POSTING?                 
         BZ    UPDPRO34                                                         
         MVC   POSTACT,WOFF        BUILD WRITE-OFF/RECEIVABLE POSTING           
         MVC   POSTACTN,WOFFNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         OC    WOFDATE,WOFDATE     TEST WRITE-OFF DATE (AND REFERENCE)          
         BZ    *+16                                                             
         MVC   POSTDATE,WOFDATE    WRITE-OFF DATE                               
         MVC   POSTREF,WOFREF      WRITE-OFF REFERENCE                          
         BAS   RE,BLDINAR          BUILD WRITE-OFF INVOICE NARRATIVE            
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         ZAP   POSTAMNT,TSARPOST                                                
         ZAP   OVRWOVAT,PZERO      CLEAR WRITE-OFF VAT ADJUSTMENT               
         OC    VAT,VAT                                                          
         BZ    UPDPRO26                                                         
         SR    RF,RF                                                            
         ICM   RF,3,VATRATE                                                     
         CVD   RF,DUB                                                           
         AP    DUB,=P'10000'                                                    
         ZAP   PKWK16,TSARPOST                                                  
         MP    PKWK16,=P'1000000'                                               
         DP    PKWK16,DUB                                                       
         SRP   PKWK16(L'PKWK16-L'DUB),64-2,5                                    
         ZAP   OVRWOVAT,TSARPOST                                                
         SP    OVRWOVAT,PKWK16(L'PKWK16-L'DUB)                                  
         SP    POSTAMNT,OVRWOVAT   TAKE IT FROM WRITE-OFF POSTING               
         SP    JRCVWOF,OVRWOVAT    TAKE FROM JOURNAL WRITE-OFF TOTALS           
         SP    JTOTWOF,OVRWOVAT                                                 
         AP    JRCVWVAT,OVRWOVAT   ADD TO JOURNAL WRITE-OFF VAT TOTALS          
         AP    JTOTWVAT,OVRWOVAT                                                
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDPRO26                                                         
         SR    RF,RF                                                            
         ICM   RF,3,VATRATE                                                     
         CVD   RF,DUB                                                           
         AP    DUB,=P'10000'                                                    
         ZAP   PKWK16,TSARPO2                                                   
         MP    PKWK16,=P'1000000'                                               
         DP    PKWK16,DUB                                                       
         SRP   PKWK16(L'PKWK16-L'DUB),64-2,5                                    
         ZAP   DUB2,TSARPO2                                                     
         SP    DUB2,PKWK16(L'PKWK16-L'DUB)                                      
         SP    JRCVWO2,DUB2        TAKE FROM JOURNAL WRITE-OFF TOTALS           
         SP    JTOTWO2,DUB2                                                     
         AP    JRCVWVA2,DUB2       ADD TO JOURNAL WRITE-OFF VAT TOTALS          
         AP    JTOTWVA2,DUB2                                                    
UPDPRO26 CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BE    UPDPRO28                                                         
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDPRO30            STANDARD POSTING TO SE/SQ                    
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPDPRO30            STANDARD POSTING TO SE/SQ                    
         GOTO1 ABLDTRN,DMCB,(0,RECVCOST),('TRNSDR',CWOFPACT),(X'FF',0)          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CWOF3ACT),CWOF3ACT                           
         MVC   POSTCACN,CWOF3ACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,DMCB,('TRNSDR',WOFF),('TRNSDR',CWOFPACT),       X        
               (X'FF',0)                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CWOFPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     UPDPRO32                                                         
*                                                                               
UPDPRO28 MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         MP    POSTAMNT,=P'-1'     REVERSE POSTING SIGN                         
UPDPRO30 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   UPDPRO32                                                         
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDPRO32                                                         
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPDPRO32                                                         
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CWOF2ACT),CWOF2ACT                           
         MVC   POSTCACN,CWOF2ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CWOF2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDPRO32 MVC   POSTDATE,TSARDAT    RESET TRANSACTION DATE                       
         MVC   POSTREF,TSARREF     RESET TRANSACTION REFERENCE                  
         OC    VAT,VAT             TEST VAT A/C PRESENT                         
         BZ    UPDPRO34                                                         
         CP    OVRWOVAT,PZERO      IF NON-ZERO AMOUNT                           
         BE    UPDPRO34                                                         
         MVC   POSTACT,VAT         BUILD VAT/RECEIVABLE POSTING                 
         MVC   POSTACTN,VATNAME                                                 
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         ZAP   POSTAMNT,OVRWOVAT                                                
         MP    POSTAMNT,=P'-1'     MINUS                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDPRO34 MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    UPDPROCX                                                         
         TM    TSARINDS,TSARIGRS   TEST GROSS POSTING - NO DISCOUNT             
         BO    UPDPROCX                                                         
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    TSARDI2,PZERO       TEST ANY POSTING (CURRENCY)                  
         BNE   *+14                                                             
         CP    TSARDISC,PZERO      TEST ANY POSTING TO MAKE                     
         BE    UPDPROCX                                                         
         TM    TSARINDS,TSARIOFS   TEST OFFSET (NO DISCOUNT)                    
         BNZ   UPDPRO36                                                         
         AP    JRCVDSC,TSARDISC    ACCUMULATE DISCOUNT ALLOC/WOFF ONLY          
         AP    JTOTDSC,TSARDISC                                                 
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDPRO36                                                         
         AP    JRCVDS2,TSARDI2                                                  
         AP    JTOTDS2,TSARDI2                                                  
UPDPRO36 MVC   POSTACT,RECVACT+1   BUILD 2ND RECEIVABLE/SOURCE POSTING          
         MVC   POSTACTN,RECVACTN                                                
         MVC   POSTCAC,TSARCAC                                                  
         MVC   POSTCACN,SPACES     NO CONTRA NAME (IT'S THE SOURCE)             
         MVC   POSTOFFC,TSAROFFC                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         ZAP   POSTAMNT,TSARDISC                                                
         ZAP   POSTAM2,TSARDI2                                                  
         BAS   RE,BLDAFC           BUILD AFCEL IF REQUIRED                      
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,DISC        BUILD DISCOUNT/RECEIVABLE POSTING            
         MVC   POSTACTN,DISCNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         CLC   DISCULSI,DISC       TEST SI DISCOUNT/SURCHARGE                   
         BE    UPDPRO38                                                         
         MP    POSTAMNT,=P'-1'     SE/SQ - REVERSE SIGN                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDPRO40            STANDARD POSTING TO SE/SQ                    
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDPRO40            STANDARD POSTING TO SE/SQ                    
         GOTO1 ABLDTRN,DMCB,(0,RECVCOST),('TRNSDR',CDSCPACT),(X'FF',0)          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CDSC3ACT),CDSC3ACT                           
         MVC   POSTCACN,CDSC3ACN                                                
         MVC   POSTDATE,TSARDAT    BILL DATE                                    
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,DMCB,('TRNSDR',DISC),('TRNSDR',CDSCPACT),       X        
               (X'FF',0)                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CDSCPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     UPDPROCX                                                         
*                                                                               
UPDPRO38 MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
UPDPRO40 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,DISC       TEST SI DISCOUNT/SURCHARGE                   
         BNE   UPDPROCX                                                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDPROCX                                                         
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDPROCX                                                         
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CDSC2ACT),CDSC2ACT                           
         MVC   POSTCACN,CDSC2ACN                                                
         MVC   POSTDATE,TSARDAT    BILL DATE                                    
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CDSC2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     UPDPROCX                                                         
*                                                                               
UPDPRO42 BAS   RE,BLDXFR           BUILD TRANSFER POSTING                       
         LH    R1,=H'-1'           SET RECORD ALREADY BUILT IN IO               
         GOTO1 ABLDTRN             ADD TRANSACTION                              
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDPROCX DS    0H                                                               
         B     UPDATEX                                                          
         EJECT                                                                  
UPDRCVL  CP    RECVAMT,PZERO       BUILD BANK/RECEIVABLE POSTING                
         BE    UPDRCL08            NOTHING TO POST                              
         OC    ANAL,ANAL                                                        
         BZ    UPDRCL02                                                         
         MVC   POSTACT,ANAL        USE ANALYSIS BANK IF PRESENT                 
         MVC   POSTACTN,ANALNAME                                                
         B     UPDRCL04                                                         
*                                                                               
UPDRCL02 MVC   POSTACT,BANK        ELSE BANK ACCOUNT                            
         MVC   POSTACTN,BANKNAME                                                
*                                                                               
UPDRCL04 MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
*                                                                               
         LA    R2,OFFTAB1                                                       
         USING OFFTABD,R2          R2=A(OFFICE TABLE)                           
         LA    R0,OFFTMAX                                                       
UPDRCL06 CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDRCL08                                                         
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         ZAP   POSTAM2,OFFTAM2                                                  
         BAS   RE,BLDAFC                                                        
         MVC   POSTOFFC,OFFTOFFC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDRCL06         DO FOR NUMBER OF TABLE ENTRIES               
         DROP  R2                                                               
*                                                                               
UPDRCL08 OC    PRTSUB,PRTSUB                                                    
         BZ    UPDRCVLX                                                         
*                                                                               
         OI    REPHEADI,REPHFRCE   NEW PAGE FOR RECEIVABLE TOTALS               
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVI   JRNDATE,C'-'                                                     
         MVC   JRNDATE+1(41),JRNDATE                                            
         MVC   JRNDATE+14(L'JRNRECV),JRNRECV                                    
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL10                                                         
         MVC   JRNDATE+1(69),JRNDATE                                            
         MVC   JRNDATE+30(L'JRNRECV),JRNRECV                                    
*                                                                               
UPDRCL10 LA    R2,L'REPP1(R2)      ALLOCATED                                    
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         AP    JRCVCRS,JRCVMRK                                                  
         AP    JRCVCR2,JRCVMR2                                                  
         CURED JRCVMRK,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL16                                                         
         CURED JRCVMR2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
*                                                                               
UPDRCL16 LA    R2,L'REPP1(R2)      SPECIALS                                     
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         AP    JRCVCRS,JRCVDIF                                                  
         AP    JRCVCR2,JRCVDI2                                                  
         CURED JRCVDIF,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL18                                                         
         CURED JRCVDI2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
*                                                                               
UPDRCL18 LA    R2,L'REPP1(R2)      PORTION OF CHEQUE                            
         MVC   JRNDATE(L'JRNCHQP),JRNCHQP                                       
         CURED RECVAMT,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL20                                                         
         CURED RECVAM2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDRCL20 LA    R2,L'REPP1(R2)      WRITTEN-OFF - SR                             
         MVC   JRNDATE(L'JRNWOFSR),JRNWOFSR                                     
         MVC   JRNDATE+L'JRNWOFSR+1(5),=C' - SR'                                
         ZAP   DUB,JRCVWOF         WRITTEN-OFF CRS NET OF VAT                   
         AP    DUB,JRCVWVAT        ADD BACK VAT TO WRITTEN-OFF SR CRS           
         AP    JRCVCRS,DUB                                                      
         ZAP   DUB2,JRCVWO2                                                     
         AP    DUB2,JRCVWVA2                                                    
         AP    JRCVCR2,DUB2                                                     
         CURED DUB,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                            
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL22                                                         
         CURED DUB2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                         
*                                                                               
UPDRCL22 LA    R2,L'REPP1(R2)      WRITE-OFF A/C                                
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JRCVWOF                                                      
         ZAP   DUB2,JRCVWO2                                                     
         LA    RF,JRNDR                                                         
         LA    RE,JRCVDRS                                                       
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   UPDRCL24                                                         
         LA    RF,JRNCR            CREDIT                                       
         LA    RE,JRCVCRS                                                       
         MP    DUB,=P'-1'          MINUS                                        
         MP    DUB2,=P'-1'                                                      
UPDRCL24 AP    0(L'JRCVDRS,RE),DUB                                              
         AP    L'JRCVDRS(L'JRCVDRS,RE),DUB2                                     
         CURED DUB,(L'JRNCR,(RF)),AGYCURT,MINUS=YES                             
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL26                                                         
         B     UPDRCL26            DON'T SHOW CURRENCY ON W/O A/C               
         LA    RF,JRNDR2                                                        
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+8                                                              
         LA    RF,JRNCR2           CREDIT                                       
         CURED DUB2,(L'JRNCR2,(RF)),FORCURT,MINUS=YES                           
*                                                                               
UPDRCL26 LA    R2,L'REPP1(R2)      WRITE-OFF VAT ADJ.                           
         MVC   JRNDATE(L'JRNWVAT),JRNWVAT                                       
         MP    JRCVWVAT,=P'-1'                                                  
         AP    JRCVCRS,JRCVWVAT                                                 
         MP    JRCVWVA2,=P'-1'                                                  
         AP    JRCVCR2,JRCVWVA2                                                 
         CURED JRCVWVAT,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                       
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL28                                                         
         B     UPDRCL28            DON'T SHOW CURRENCY ON W/O VAT A/C           
         CURED JRCVWVA2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                     
*                                                                               
UPDRCL28 TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    JRCVOF2,PZERO       OFFSETS (CURRENCY)                           
         BNE   *+14                                                             
         CP    JRCVOFS,PZERO       OFFSETS                                      
         BE    UPDRCL30                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         AP    JRCVCRS,JRCVOFS                                                  
         AP    JRCVCR2,JRCVOF2                                                  
         CURED JRCVOFS,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL30                                                         
         CURED JRCVOF2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
*                                                                               
UPDRCL30 LA    R2,L'REPP1(R2)      DIS/SCH/FEE - SR                             
         MVC   JRNDATE(L'JRNDSCSR),JRNDSCSR                                     
         MVC   JRNDATE+L'JRNDSCSR+1(5),=C' - SR'                                
         AP    JRCVDRS,JRCVDSC                                                  
         AP    JRCVDR2,JRCVDS2                                                  
         CURED JRCVDSC,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL32                                                         
         CURED JRCVDS2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDRCL32 LA    R2,L'REPP1(R2)      DIS/SCH/FEE A/C                              
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,JRCVDSC                                                      
         ZAP   DUB2,JRCVDS2                                                     
         CLC   DISCULSI,DISC       TEST SI DISCOUNT                             
         BNE   UPDRCL34                                                         
         LA    RF,JRNCR            SI - CREDIT                                  
         AP    JRCVCRS,DUB                                                      
         AP    JRCVCR2,DUB2                                                     
         B     UPDRCL36                                                         
UPDRCL34 LA    RF,JRNDR            SE/SQ - DEBIT                                
         MP    DUB,=P'-1'          DISCOUNT TYPE - REVERSE SIGN                 
         MP    DUB2,=P'-1'                                                      
         AP    JRCVDRS,DUB                                                      
         AP    JRCVDR2,DUB2                                                     
UPDRCL36 CURED DUB,(L'JRNCR,(RF)),AGYCURT,MINUS=YES                             
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL38                                                         
         B     UPDRCL38            DON'T SHOW CURRENCY ON DISCOUNT A/C          
         LA    RF,JRNDR2                                                        
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+8                                                              
         LA    RF,JRNCR2           CREDIT                                       
         CURED DUB2,(L'JRNCR2,(RF)),FORCURT,MINUS=YES                           
*                                                                               
UPDRCL38 TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    JRCVTR2,PZERO       TRANSFERRED (CURRENCY)                       
         BNE   *+14                                                             
         CP    JRCVTRF,PZERO                                                    
         BE    UPDRCL44                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JRCVTRF,(L'JRNDR,JRNDR+1),AGYCURT,MINUS=YES,BRACKET=Y            
         LA    R1,JRNDR                                                         
UPDRCL40 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDRCL40                                                         
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL44                                                         
         CURED JRCVTR2,(L'JRNDR2,JRNDR2+1),FORCURT,MINUS=YES,BRACKET=Y          
         LA    R1,JRNDR2                                                        
UPDRCL42 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDRCL42                                                         
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDRCL44 LA    R2,L'REPP1(R2)      TOTAL DEBITS AND CREDITS                     
         MVC   JRNDATE(L'JRNTOT),JRNTOT                                         
         AP    JTOTCRS,JRCVCRS                                                  
         AP    JTOTDRS,JRCVDRS     DON'T ADD RECVAMT TO JTOTDRS                 
         AP    JRCVDRS,RECVAMT                                                  
         AP    JTOTCR2,JRCVCR2                                                  
         AP    JTOTDR2,JRCVDR2                                                  
         AP    JRCVDR2,RECVAM2                                                  
         CURED JRCVCRS,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         CURED JRCVDRS,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDRCL46                                                         
         CURED JRCVCR2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
         CURED JRCVDR2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDRCL46 LA    R2,L'REPP1(R2)      ITEMS                                        
         CURED RCVITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
*                                                                               
         LA    R2,L'REPP1(R2)      COSTING WARNING MESSAGE                      
         TM    OVRBYTE,NOCWOF+NOCDSC                                            
         BZ    UPDRCL48            BOTH NOCWOF+NOCDSC BITS OFF                  
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         MVI   GTMAXL,JRNMSG2L                                                  
         LA    R0,JRNDATE                                                       
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$CSTAM)                                           
         GOTO1 VGETTXT,(R1)                                                     
         TM    OVRBYTE,NOCWOF+NOCDSC                                            
         BO    UPDRCL48            BOTH NOCWOF+NOCDSC BITS ON                   
*                                                                               
         MVC   JRNDATE,SPACES      ASSUME NOCWOF IS THE BIT ON                  
         MVC   GTMSGNO,=AL2(AS$WRTCM)                                           
         GOTO1 (RF),(R1)                                                        
         TM    OVRBYTE,NOCDSC      TEST NOCDSC BIT ON                           
         BZ    UPDRCL48                                                         
         MVC   JRNDATE,SPACES      DISCOUNT COSTING A/C(S) IN ERROR             
         MVC   GTMSGNO,=AL2(AS$DSFCM)                                           
         GOTO1 (RF),(R1)                                                        
         DROP  R1                                                               
*                                                                               
UPDRCL48 GOTO1 VREPORT,REPD                                                     
*                                                                               
UPDRCVLX AP    TOTITEM,RCVITEM     ADD RECEIVABLE ITEMS TO TOTAL                
         B     UPDATEX                                                          
         DROP  R2,R4                                                            
         EJECT                                                                  
UPDLAST  OI    REPHEADI,REPHFRCE   NEW PAGE FOR OVERALL POSTINGS                
         MVC   REPH4,SPACES                                                     
         LA    R4,RECVTAB          USE FIRST ENTRY FOR ANY COSTING              
         USING RECVTABD,R4                                                      
*                                                                               
         USING OFFTABD,R2          R2=A(OFFICE TABLE)                           
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    UPDLAS26                                                         
         LA    R2,OFFTAB2                                                       
         SR    R3,R3                                                            
         LA    R0,OFFTMAX                                                       
         ZAP   DUB,PZERO                                                        
         ZAP   DUB2,PZERO                                                       
         ZAP   DUB3,PZERO                                                       
UPDLAS02 CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDLAS04                                                         
         AP    DUB,OFFTAMNT                                                     
         AP    DUB2,OFFTAM2                                                     
         CP    DUB3,OFFTAMNT                                                    
         BH    *+12                                                             
         ZAP   DUB3,OFFTAMNT                                                    
         LR    R3,R2               POINT TO THIS TABLE ENTRY                    
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDLAS02         DO FOR NUMBER OF TABLE ENTRIES               
*                                                                               
UPDLAS04 TM    TWAMODE,TWAMDRFT    TEST DRAFT                                   
         BZ    *+14                                                             
         CP    DUB2,CHQAM2         TEST IN BALANCE IN CURRENCY                  
         BNE   UPDLAS26            CANNOT RESOLVE EXCHANGE DIFFERENCES          
         ZAP   DUB2,CHQAMT         TEST CHEQUE IN AGENCY CURRENCY               
         SP    DUB2,DUB                                                         
         BZ    UPDLAS26                                                         
         LR    R2,R3               R2=A(HIGHEST OFFICE AMOUNT)                  
         ZAP   JTOTEXD,DUB2        SET EXCHANGE DIFFERENCE                      
*                                                                               
         MVC   POSTOFFC,OFFTOFFC   SET OFFICE CODE FOR POSTING                  
         CLI   EXDFOPOK,0                                                       
         BE    UPDLAS05                                                         
         OI    OVRBYTE,NOCEXC      SET NO EXCHANGE DIFFERENCE COSTING           
         LA    R1,EXDF                                                          
         ICM   R1,8,EXDFOPOK                                                    
         BAS   RE,SETCST                                                        
         MVC   EXDF,THISULA        EXTRACT VALUES                               
         MVC   EXDFNAME,THISNAME                                                
         MVC   EXDFCOST,THISCOST                                                
         MVC   CEXCACTS(CEXCACTL),THISACTS                                      
         BNE   UPDLAS05                                                         
         NI    OVRBYTE,FF-(NOCEXC) SET EXCHANGE DIFFERENCE COSTING              
*                                                                               
UPDLAS05 MVC   POSTACT,BANK        BUILD BANK/EXCHANGE DIFF. POSTING            
         MVC   POSTACTN,BANKNAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'EXDF),EXDF                                   
         MVC   POSTCACN,EXDFNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         ZAP   POSTAMNT,JTOTEXD    SET AMOUNT & OFFICE CODE                     
         ZAP   POSTAM2,PZERO                                                    
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,EXDF        BUILD EXCHANGE DIFF./BANK POSTING            
         MVC   POSTACTN,EXDFNAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'BANK),BANK                                   
         MVC   POSTCACN,BANKNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         CLC   DISCULSI,EXDF       TEST SI EXCHANGE DIFFERENCE                  
         BE    UPDLAS22                                                         
         MP    POSTAMNT,=P'-1'     SE - REVERSE SIGN                            
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BNH   UPDLAS24            STANDARD POSTING TO SE                       
         TM    OVRBYTE,NOCEXC      TEST EXCHANGE COSTING POSTINGS OK            
         BNZ   UPDLAS24            STANDARD POSTING TO SE                       
         GOTO1 ABLDTRN,DMCB,(0,RECVCOST),('TRNSDR',CEXCPACT),(X'FF',0)          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CEXC3ACT),CEXC3ACT                           
         MVC   POSTCACN,CEXC3ACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,DMCB,('TRNSDR',EXDF),('TRNSDR',CEXCPACT),       X        
               (X'FF',0)                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CEXCPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     UPDLAS26                                                         
*                                                                               
UPDLAS22 MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
UPDLAS24 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,EXDF       TEST SI EXCHANGE DIFFERENCES                 
         BNE   UPDLAS26                                                         
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BNH   UPDLAS26                                                         
         TM    OVRBYTE,NOCEXC      TEST EXCHANGE COSTING POSTINGS OK            
         BNZ   UPDLAS26                                                         
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CEXC2ACT),CEXC2ACT                           
         MVC   POSTCACN,CEXC2ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CEXC2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
UPDLAS26 OC    ANAL,ANAL           TEST ANALYSIS BANK PRESENT                   
         BZ    UPDLAS30                                                         
         LA    R2,OFFTAB2                                                       
         LA    R0,OFFTMAX                                                       
UPDLAS28 CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDLAS30                                                         
         MVC   POSTACT,BANK        BUILD BANK/ANALYSIS BANK POSTING             
         MVC   POSTACTN,BANKNAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'ANAL),ANAL                                   
         MVC   POSTCACN,ANALNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         ZAP   POSTAM2,OFFTAM2                                                  
         BAS   RE,BLDAFC                                                        
         MVC   POSTOFFC,OFFTOFFC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,ANAL        BUILD ANALYSIS BANK/BANK POSTING             
         MVC   POSTACTN,ANALNAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'BANK),BANK                                   
         MVC   POSTCACN,BANKNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         ZAP   POSTAM2,OFFTAM2                                                  
         BAS   RE,BLDAFC                                                        
         MVC   POSTOFFC,OFFTOFFC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDLAS28         DO FOR NUMBER OF TABLE ENTRIES               
*                                                                               
UPDLAS30 CP    BCHAAMT,PZERO       TEST BANK CHARGES TO BE POSTED               
         BE    UPDLAS46                                                         
         LA    R0,OFFTMAX          APPORTION BANK CHARGES BY OFFICE             
         LA    R2,OFFTAB2                                                       
         SR    R3,R3                                                            
         ZAP   DUB,BCHAAMT                                                      
         ZAP   DUB2,BCHAAM2                                                     
         ZAP   DUB3,PZERO                                                       
         ZAP   OVRDUB1,CHQAMT                                                   
         ZAP   OVRDUB2,CHQAM2                                                   
         TM    TWAMODE,TWAMDRFT    IF DRAFT USE ALLOCATIONS, NOT CHEQUE         
         BZ    UPDLAS32                                                         
         ZAP   OVRDUB1,JTOTMRK     ALLOCATED                                    
         ZAP   OVRDUB2,JTOTMR2     ALLOCATED CURRENCY                           
         AP    OVRDUB1,JTOTDIF     SPECIALS                                     
         AP    OVRDUB2,JTOTDI2     SPECIALS CURRENCY                            
         AP    OVRDUB1,JTOTEXD     EXCHANGE DIFFERENCE                          
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   UPDLAS32                                                         
         SP    OVRDUB1,JTOTDSC     DISCOUNT                                     
         SP    OVRDUB2,JTOTDS2     DISCOUNT CURRENCY                            
UPDLAS32 CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDLAS36                                                         
         CP    DUB3,OFFTAMNT       TEST HIGH OFFICE AMOUNT                      
         BH    *+12                                                             
         ZAP   DUB3,OFFTAMNT       SAVE HIGH OFFICE AMOUNT                      
         LR    R3,R2               R3=A(HIGHEST OFFICE ENTRY)                   
         ZAP   PKWK16,BCHAAMT      TAKE BANK CHARGES AMOUNT                     
         MP    PKWK16,OFFTAMNT     MULTIPLY BY OFFICE AMOUNT                    
         LA    RE,7                                                             
         SR    RF,RF                                                            
         IC    RF,AGYCURT+(CURTDECP-CURTABD)                                    
         SR    RE,RF                                                            
         SRP   PKWK16,0(RE),0      SHIFT LEFT 7-DECP                            
         DP    PKWK16,OVRDUB1      DIVIDE BY CHEQUE AMOUNT                      
         LCR   RE,RE               REVERSE NUMBER OF PLACES                     
         SRP   PKWK16(8),64(RE),5  SHIFT RIGHT 7-DECP                           
         ZAP   OFFTAMNT,PKWK16(8)  FIRST 8 BYTES ARE QUOTIENT                   
         SP    DUB,OFFTAMNT        REDUCE AMOUNT LEFT                           
*                                                                               
         ZAP   PKWK16,BCHAAM2      TAKE BANK CHARGES AMOUNT (CURRENCY)          
         MP    PKWK16,OFFTAM2      MULTIPLY BY OFFICE AMOUNT (CURRENCY)         
         LA    RE,7                                                             
         SR    RF,RF                                                            
         IC    RF,FORCURT+(CURTDECP-CURTABD)                                    
         SR    RE,RF                                                            
         SRP   PKWK16,0(RE),0      SHIFT LEFT 7-DECP                            
         DP    PKWK16,OVRDUB2      DIVIDE BY CHEQUE AMOUNT (CURRENCY)           
         LCR   RE,RE               REVERSE NUMBER OF PLACES                     
         SRP   PKWK16(8),64(RE),5  SHIFT RIGHT 7-DECP                           
         ZAP   OFFTAM2,PKWK16(8)   FIRST 8 BYTES ARE QUOTIENT                   
         SP    DUB2,OFFTAM2        REDUCE AMOUNT LEFT (CURRENCY)                
*                                                                               
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDLAS32         DO FOR NUMBER OF TABLE ENTRIES               
*                                                                               
UPDLAS36 LTR   R3,R3               TEST ANY OFFICE POSTINGS                     
         BZ    UPDLAS46                                                         
         CP    DUB,PZERO           ACCOMMODATE ANY PENNY DIFFERENCES            
         BE    *+10                                                             
         AP    OFFTAMNT-OFFTABD(L'OFFTAMNT,R3),DUB                              
         CP    DUB2,PZERO                                                       
         BE    *+10                                                             
         AP    OFFTAM2-OFFTABD(L'OFFTAM2,R3),DUB2                               
         LA    R2,OFFTAB2          NOW MAKE BANK/BANK CHARGE POSTINGS           
         LA    R0,OFFTMAX                                                       
UPDLAS38 CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDLAS46                                                         
         MVC   POSTOFFC,OFFTOFFC   SET OFFICE CODE FOR POSTING                  
         CLI   BCHAOPOK,0                                                       
         BE    UPDLAS39                                                         
         OI    OVRBYTE,NOCBCH      SET NO BANK CHARGE COSTING                   
         LA    R1,BCHA                                                          
         ICM   R1,8,BCHAOPOK                                                    
         BAS   RE,SETCST                                                        
         MVC   BCHA,THISULA        EXTRACT VALUES                               
         MVC   BCHANAME,THISNAME                                                
         MVC   BCHACOST,THISCOST                                                
         MVC   CBCHACTS(CBCHACTL),THISACTS                                      
         BNE   UPDLAS39                                                         
         NI    OVRBYTE,FF-(NOCBCH) SET BANK CHARGE COSTING                      
*                                                                               
UPDLAS39 MVC   POSTACT,BANK        BUILD BANK/BANK CHARGE POSTING               
         MVC   POSTACTN,BANKNAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'BCHA),BCHA                                   
         MVC   POSTCACN,BCHANAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         ZAP   POSTAM2,OFFTAM2                                                  
         MP    POSTAMNT,=P'-1'     -DEBIT                                       
         MP    POSTAM2,=P'-1'                                                   
         BAS   RE,BLDAFC                                                        
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,BCHA        BUILD BANK CHARGE/BANK POSTING               
         MVC   POSTACTN,BCHANAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'BANK),BANK                                   
         MVC   POSTCACN,BANKNAME                                                
         OC    ANAL,ANAL                                                        
         BZ    *+16                                                             
         MVC   POSTCAC+L'COMPANY(L'ANAL),ANAL                                   
         MVC   POSTCACN,ANALNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         CLC   DISCULSI,BCHA       TEST SI BANK CHARGES                         
         BE    UPDLAS40                                                         
         MP    POSTAMNT,=P'-1'     SE - REVERSE SIGN                            
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         CLI   BCHACOST,C' '       TEST BANK CHARGES A/C ANALYSIS FLAG          
         BNH   UPDLAS42            STANDARD POSTING TO SE                       
         TM    OVRBYTE,NOCBCH      TEST BANK CHARGE COSTING POSTINGS OK         
         BNZ   UPDLAS42            STANDARD POSTING TO SE                       
         GOTO1 ABLDTRN,DMCB,(0,RECVCOST),('TRNSDR',CBCHPACT),(X'FF',0)          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CBCH3ACT),CBCH3ACT                           
         MVC   POSTCACN,CBCH3ACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,DMCB,('TRNSDR',BCHA),('TRNSDR',CBCHPACT),       X        
               (X'FF',0)                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CBCHPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     UPDLAS44                                                         
*                                                                               
UPDLAS40 MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
UPDLAS42 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,BCHA       TEST SI BANK CHARGES                         
         BNE   UPDLAS44                                                         
         CLI   BCHACOST,C' '       TEST BANK CHARGES A/C ANALYSIS FLAG          
         BNH   UPDLAS44                                                         
         TM    OVRBYTE,NOCBCH      TEST BANK CHARGE COSTING POSTINGS OK         
         BNZ   UPDLAS44                                                         
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CBCH2ACT),CBCH2ACT                           
         MVC   POSTCACN,CBCH2ACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CBCH2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
UPDLAS44 LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDLAS38         DO FOR NUMBER OF TABLE ENTRIES               
*                                                                               
UPDLAS46 LA    R0,OFFTMAX          CLEAR SECOND OFFICE TABLE                    
         LA    R2,OFFTAB2                                                       
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,*-10             DO FOR NUMBER OF TABLE ENTRIES               
         DROP  R2,R4                                                            
*                                                                               
         OC    PRTSUB,PRTSUB       TEST IF PRINTING                             
         BZ    UPDLASTX                                                         
         OI    REPHEADI,REPHFRCE   NEW PAGE FOR OVERALL TOTALS                  
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVI   JRNDATE,C'-'                                                     
         MVC   JRNDATE+1(41),JRNDATE                                            
         MVC   JRNDATE+14(L'JRNBTOT),JRNBTOT                                    
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+16                                                             
         MVC   JRNDATE+1(69),JRNDATE                                            
         MVC   JRNDATE+30(L'JRNBTOT),JRNBTOT                                    
*                                                                               
         LA    R2,L'REPP1(R2)      ALLOCATED                                    
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         CURED JTOTMRK,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS48                                                         
         CURED JTOTMR2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS48 LA    R2,L'REPP1(R2)      SPECIALS                                     
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JTOTDIF,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS50                                                         
         CURED JTOTDI2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS50 LA    R2,L'REPP1(R2)      CHEQUE                                       
         MVC   JRNDATE(L'JRNCHQ),JRNCHQ                                         
         ZAP   JTOTCHQ,CHQAMT                                                   
         AP    JTOTDRS,CHQAMT      ADD CHEQUE TO JTOTDRS, NOW                   
         ZAP   JTOTCH2,CHQAM2                                                   
         AP    JTOTDR2,CHQAM2                                                   
         CURED JTOTCHQ,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS51                                                         
         CURED JTOTCH2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS51 CP    JTOTEXD,PZERO       TEST EXCHANGE DIFFERENCE                     
         BE    UPDLAS52                                                         
         LA    R2,L'REPP1(R2)      CHEQUE                                       
         MVC   JRNDATE(L'JRNEXDIF),JRNEXDIF                                     
         ZAP   DUB,JTOTEXD         REVERSE (ADD BACK TO CHEQUE)                 
         MP    DUB,=P'-1'                                                       
         CURED DUB,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                            
*                                                                               
UPDLAS52 LA    R2,L'REPP1(R2)      WRITTEN-OFF - SR                             
         MVC   JRNDATE(L'JRNWOFSR),JRNWOFSR                                     
         MVC   JRNDATE+L'JRNWOFSR+1(5),=C' - SR'                                
         ZAP   DUB,JTOTWOF         WRITTEN-OFF CRS NET OF VAT                   
         AP    DUB,JTOTWVAT        ADD BACK VAT TO WRITTEN-OFF CRS              
         ZAP   DUB2,JTOTWO2                                                     
         AP    DUB2,JTOTWVA2                                                    
         CURED DUB,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                            
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS54                                                         
         CURED DUB2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                         
*                                                                               
UPDLAS54 LA    R2,L'REPP1(R2)      WRITE-OFF A/C                                
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JTOTWOF                                                      
         ZAP   DUB2,JTOTWO2                                                     
         LA    RF,JRNDR                                                         
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   UPDLAS56                                                         
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         MP    DUB2,=P'-1'                                                      
UPDLAS56 CURED DUB,(L'JRNCR,(RF)),AGYCURT,MINUS=YES                             
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS58                                                         
         B     UPDLAS58            DON'T SHOW CURRENCY ON W/O A/C               
         LA    RF,JRNDR2                                                        
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+8                                                              
         LA    RF,JRNCR2           CREDIT                                       
         CURED DUB2,(L'JRNCR2,(RF)),FORCURT,MINUS=YES                           
*                                                                               
UPDLAS58 LA    R2,L'REPP1(R2)      WRITE-OFF VAT ADJ.                           
         MVC   JRNDATE(L'JRNWVAT),JRNWVAT                                       
         MP    JTOTWVAT,=P'-1'                                                  
         MP    JTOTWVA2,=P'-1'                                                  
         CURED JTOTWVAT,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                       
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS60                                                         
         B     UPDLAS60            DON'T SHOW CURRENCY ON W/O VAT A/C           
         CURED JTOTWVA2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                     
*                                                                               
UPDLAS60 TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    JTOTOF2,PZERO       OFFSETS (CURRENCY)                           
         BNE   *+14                                                             
         CP    JTOTOFS,PZERO       OFFSETS                                      
         BE    UPDLAS62                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         CURED JTOTOFS,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS62                                                         
         CURED JTOTOF2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS62 LA    R2,L'REPP1(R2)      DIS/SCH/FEE - SR                             
         MVC   JRNDATE(L'JRNDSCSR),JRNDSCSR                                     
         MVC   JRNDATE+L'JRNDSCSR+1(5),=C' - SR'                                
         CURED JTOTDSC,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS64                                                         
         CURED JTOTDS2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS64 LA    R2,L'REPP1(R2)      DIS/SCH/FEE A/C                              
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,JTOTDSC                                                      
         ZAP   DUB2,JTOTDS2                                                     
         LA    RF,JRNCR            SI - CREDIT                                  
         CLC   DISCULSI,DISC       TEST SI DISCOUNT                             
         BE    UPDLAS66                                                         
         LA    RF,JRNDR            SE/SQ - DEBIT                                
         MP    DUB,=P'-1'          REVERSE SIGN                                 
         MP    DUB2,=P'-1'                                                      
UPDLAS66 CURED DUB,(L'JRNCR,(RF)),AGYCURT,MINUS=YES                             
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS68                                                         
         B     UPDLAS68            DON'T SHOW CURRENCY ON DISCOUNT A/C          
         LA    RF,JRNDR2                                                        
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+8                                                              
         LA    RF,JRNCR2           CREDIT                                       
         CURED DUB2,(L'JRNCR2,(RF)),FORCURT,MINUS=YES                           
*                                                                               
UPDLAS68 TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    *+14                                                             
         CP    JTOTTR2,PZERO       TRANSFERRED (CURRENCY)                       
         BNE   *+14                                                             
         CP    JTOTTRF,PZERO                                                    
         BE    UPDLAS74                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JTOTTRF,(L'JRNDR,JRNDR+1),AGYCURT,MINUS=YES,BRACKET=Y            
         LA    R1,JRNDR                                                         
UPDLAS70 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDLAS70                                                         
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS74                                                         
         CURED JTOTTR2,(L'JRNDR2,JRNDR2+1),FORCURT,MINUS=YES,BRACKET=Y          
         LA    R1,JRNDR2                                                        
UPDLAS72 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDLAS72                                                         
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDLAS74 LA    R2,L'REPP1(R2)      TOTAL DEBITS AND CREDITS                     
         MVC   JRNDATE(L'JRNTOT),JRNTOT                                         
         CURED JTOTCRS,(L'JRNCR,JRNCR),AGYCURT,MINUS=YES                        
         ZAP   DUB,JTOTDRS         TOTAL DEBITS                                 
         SP    DUB,JTOTEXD         SUBTRACT EXCHANGE DIFFERENCE                 
         CURED DUB,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                            
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS76                                                         
         CURED JTOTCR2,(L'JRNCR2,JRNCR2),FORCURT,MINUS=YES                      
         CURED JTOTDR2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS76 LA    R2,L'REPP1(R2)      BALANCE                                      
         MVC   JRNDATE(L'JRNBAL),JRNBAL                                         
         ZAP   JTOTBAL,JTOTCHQ                                                  
         SP    JTOTBAL,JTOTMRK                                                  
         SP    JTOTBAL,JTOTDIF                                                  
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    JTOTBAL,JTOTDSC                                                  
         SP    JTOTBAL,JTOTEXD     SUBTRACT EXCHANGE DIFFERENCE                 
         ZAP   JTOTBA2,JTOTCH2                                                  
         SP    JTOTBA2,JTOTMR2                                                  
         SP    JTOTBA2,JTOTDI2                                                  
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    JTOTBA2,JTOTDS2                                                  
         CURED JTOTBAL,(L'JRNDR,JRNDR),AGYCURT,MINUS=YES                        
         TM    CURIND1,CUR1SINC                                                 
         BZ    UPDLAS78                                                         
         CURED JTOTBA2,(L'JRNDR2,JRNDR2),FORCURT,MINUS=YES                      
*                                                                               
UPDLAS78 LA    R2,L'REPP1(R2)      ITEMS                                        
         CURED TOTITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
         CP    JTOTOFS,PZERO       TEST OFFSETS NOT IN BALANCE                  
         BNE   UPDLAS80                                                         
         CP    JTOTBAL,PZERO       TEST BALANCE WARNING REQ'D                   
         BNE   UPDLAS80                                                         
         TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZ    UPDLAS82                                                         
         CP    JTOTOF2,PZERO       OFFSETS CURRENCY)                            
         BNE   UPDLAS80                                                         
         CP    JTOTBA2,PZERO       TEST BALANCE WARNING REQ'D                   
         BE    UPDLAS82                                                         
UPDLAS80 LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         TM    CURIND1,CUR1SINC                                                 
         BZ    *+8                                                              
         MVI   GTMAXL,JRNMSG2L                                                  
         LA    R0,L'JRNITEM+9(RF)                                               
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$BATNB)                                           
         GOTO1 VGETTXT,(R1)                                                     
         DROP  R1                                                               
*                                                                               
UPDLAS82 GOTO1 VREPORT,REPD                                                     
*                                                                               
UPDLASTX B     UPDATEX                                                          
*                                                                               
UPDATEX  XIT1  ,                                                                
*                                                                               
UPDABEND LA    R2,PARM             ABEND - DEFINE GETTXT CONTROL BLOCK          
         USING GETTXTD,R2                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,FVMSGNO     ERROR SET BY CALLER                          
         CLI   GTMSGNO,X'FF'       TEST HOB INDICATES GENERAL MESSAGE           
         BNE   *+12                                                             
         MVI   GTMSGNO,0           CLEAR HOB                                    
         MVI   GTMSYS,X'FF'        SET GENERAL SYSTEM                           
         CLI   FVXTRA,0            TEST EXTRA MESSAGE/SUBSTITUTION TEXT         
         BE    UPDABE02                                                         
         LA    R1,FVXTRA                                                        
         CLI   FVXTRA,C' '         TEST EXTRA MESSAGE                           
         BH    *+12                                                             
         STCM  R1,7,GTASUBST       THIS IS A SUBSTITUTION STRING                
         B     UPDABE02                                                         
         STCM  R1,7,GTATXT         THIS IS AN EXTRA MESSAGE                     
         LA    RF,L'FVXTRA-1(R1)                                                
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         SR    RF,R1                                                            
         STC   RF,GTLTXT                                                        
*                                                                               
UPDABE02 XC    RECMSG,RECMSG       CLEAR MESSAGE FIELD                          
         GOTO1 VGETTXT,GETTXTD                                                  
         OI    RECMSGH+(FVOIND-FVIHDR),FVOXMT                                   
         OI    RECACTH+(FVOIND-FVIHDR),FVOCUR                                   
         MVC   RECACT,ACTNAME      RESTORE LAST ACTION NAME                     
         DC    H'0',C'$ABEND'                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD A TRANSFER DEBIT POSTING                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING RECVTABD,R4         R4=A(RECEIVABLES TABLE ENTRY)                
BLDXFR   NTR1  WORK=(R5,128)                                                    
         LA    R2,KEY              TRANSFER POSTING                             
         USING TRNRECD,R2                                                       
         MVC   TRNKEY,SPACES       RE-READ ORIGINAL SR DEBIT                    
         MVC   TRNKCULA,RECVACT    ACCOUNT                                      
         TM    COMPSTA4,CPYSOFF2   TEST NEW OFFICES IN USE                      
         BZ    *+18                                                             
         CLI   FILEFORM,VLISQ      TEST OLD FILE                                
         BE    *+10                                                             
         MVC   TRNKOFF,TSAROFFC    SET OFFICE IN KEY                            
         MVC   TRNKCULC,TSARCAC    CONTRA (SOURCE)                              
         MVC   TRNKDATE,TSARDAT    DATE                                         
         MVC   TRNKREF,TSARREF     REFERENCE                                    
         MVC   TRNKSBR,TSARSUBR    SUB-REFERENCE                                
         GOTO1 AIOREAD                                                          
         BE    *+6                                                              
         DC    H'0'                ORIGINAL SR DEBIT NOT FOUND                  
         LA    R2,IO                                                            
         MVC   TRNKACT,TSARTRFA    SUBSTITUTE TRANSFER ACCOUNT                  
         LR    R3,R2                                                            
         AH    R3,DATADISP         ADDRESS FIRST ELEMENT                        
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                TRANSACTION ELEMENT NOT FIRST                
         NI    TRNSTAT,FF-(TRNSREV)  ORIGINAL GOT REVERSED SO UNREVERSE         
         MVC   TRNMOS,BATMON       SUBSTITUTE BATCH REFERENCE                   
         MVC   TRNBREF,BATREF                                                   
         MVI   TRNTYPE,30          SUBSTITUTE BATCH TYPE                        
         SR    R0,R0                                                            
         IC    R0,TRNLN            TAKE L'TRANSACTION                           
         SR    RF,RF                                                            
         ICM   RF,3,TRNRLEN        RECORD LENGTH                                
         SR    RF,R0                                                            
         STCM  RF,3,TRNRLEN        REDUCE L'RECORD BY L'TRNEL                   
         SH    RF,DATADISP         SUBTRACT L'KEY                               
         STH   RF,HALF             SAVE L'RECORD BEYOND TRNEL                   
         LR    RE,R3               RE=A(TRNEL)                                  
         AR    RE,R0               RE=A(NEXT ELEMENT)                           
         LR    R1,RF               R1=RF=L'RECORD BEYOND TRANSACTION            
         LR    R0,R5               USE OVERLAY SAVE AREA                        
         MVCL  R0,RE               SAVE RECORD BEYOND TRANSACTION               
*                                                                               
BLDXFR2  MVC   TEMP,SPACES                                                      
         LA    RF,TEMP                                                          
         SR    RE,RE                                                            
         IC    RE,TRNLN                                                         
         SH    RE,=Y(TRNLN1Q+1)    MINUS L'FIXED PORTION -1 FOR EXECUTE         
         BM    BLDXFR4                                                          
         EX    RE,*+8                                                           
         BE    BLDXFR4                                                          
         CLC   TRNNARR(0),SPACES                                                
         EX    RE,*+4                                                           
         MVC   TEMP(0),TRNNARR                                                  
         LA    RF,0(RE,RF)         RF=LAST BYTE OF TRNNARR                      
         CLI   0(RF),C' '          SEEK LAST CHARACTER IN TRNNARR               
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
BLDXFR4  MVC   0(L'TXTTRFFR,RF),TXTTRFFR                                        
         LA    RF,L'TXTTRFFR+1(RF)                                              
         MVC   0(L'RECVACT-1,RF),RECVACT+1                                      
         LA    RF,L'RECVACT-2(RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(RF)                                                         
         MVC   0(L'TXTON,RF),TXTON                                              
         LA    RF,L'TXTON+1(RF)                                                 
         LR    R0,RF                                                            
         GOTO1 VDATCON,DMCB,(1,TODAYP),(17,0(RF))                               
         LR    RF,R0                                                            
         LA    RF,8(RF)            MAXIMUM L'DATE EXPRESSION                    
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,1(RF)                                                         
         LR    RE,RF                                                            
         LA    R0,TEMP                                                          
         SR    RF,R0               RF=NEW L'TRNNARR                             
         CLM   RF,1,=AL1(L'TRNNARR)                                             
         BNH   *+14                                                             
         MVC   TRNNARR,SPACES      ELSE CLEAR EXISTING NARRATIVE                
         B     BLDXFR2             AND RETURN TO BUILD NEW NARRATIVE            
*                                                                               
         BCTR  RF,0                L'TRNNARR -1 FOR EXECUTE                     
         EX    RF,*+4                                                           
         MVC   TRNNARR(0),TEMP                                                  
         LA    RF,TRNLN1Q+1(RF)    ADD L'FIXED PORTION +1                       
         STC   RF,TRNLN            SAVE NEW LENGTH                              
         ICM   R1,3,TRNRLEN        RECORD LENGTH (MINUS OLD TRNLN)              
         AR    R1,RF               ADD NEW TRNLN                                
         STCM  R1,3,TRNRLEN        UPDATE RECORD LENGTH                         
         LR    R0,RF               TAKE NEW TRNLN                               
         AR    R0,R3               R0=A(REMAINDER OF RECORD)                    
         LH    R1,HALF             R1=L'REMAINDER OF RECORD                     
         LR    RE,R5               RE=A(SAVED REMAINDER OF RECORD)              
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE REMAINDER OF RECORD                  
         SR    R0,R0                                                            
         USING TRSELD,R3                                                        
BLDXFR6  IC    R0,TRSLN            FIND TRANSACTION STATUS ELEMENT              
         AR    R3,R0                                                            
         CLI   0(R3),0             TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                TRSEL MISSING                                
         CLI   0(R3),TRSELQ        TEST TRANSACTION STATUS ELEMENT              
         BNE   BLDXFR6             TRY AGAIN                                    
         SR    RF,RF                                                            
         IC    RF,TRSLN                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    TRSDATE(0),TRSDATE  CLEAR ELEMENT VALUES                         
BLDXFRX  B     UPDATEX                                                          
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* BUILD 'TRANSFERRED TO' NARRATIVE FOR SR POSTING                     *         
***********************************************************************         
         SPACE 1                                                                
BLDTNAR  LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   POSTNARR,SPACES     TRANSFER NARRATIVE                           
         LA    R2,POSTNARR                                                      
         MVC   0(L'TXTTRFTO,R2),TXTTRFTO                                        
         LA    R2,L'TXTTRFTO+1(R2)                                              
         MVC   0(L'RECVUL,R2),RECVUL                                            
         MVC   L'RECVUL(L'TSARTRFA,R2),TSARTRFA                                 
         LA    R2,L'RECVUL+L'TSARTRFA-1(R2)                                     
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'TXTON,R2),TXTON                                              
         LA    R2,L'TXTON+1(R2)                                                 
         GOTO1 VDATCON,DMCB,(1,TODAYP),(17,0(R2))                               
         LR    RE,R0               RESTORE RETURN ADDRESS                       
         BR    RE                  RETURN TO CALLER                             
         SPACE 2                                                                
***********************************************************************         
* BUILD WRITE-OFF NARRATIVE FOR SR POSTING                            *         
***********************************************************************         
         SPACE 1                                                                
BLDWNAR  LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   POSTNARR,SPACES                                                  
         LA    R2,POSTNARR                                                      
         OC    WOFDATE,WOFDATE     TEST WRITE-OFF DATE (AND REFERENCE)          
         BNZ   BLDWNAR2                                                         
         MVC   0(L'TXTWOFD,R2),TXTWOFD                                          
         LA    R2,L'TXTWOFD+1(R2)                                               
         LA    RF,TODAYP                                                        
         B     BLDWNAR4                                                         
BLDWNAR2 MVC   0(L'TXTWOF,R2),TXTWOF                                            
         LA    R2,L'TXTWOF(R2)                                                  
         MVC   0(L'WOFREF,R2),WOFREF                                            
         LA    R2,L'WOFREF-1(R2)                                                
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'TXTDAT,R2),TXTDAT                                            
         LA    R2,L'TXTDAT+3(R2)                                                
         LA    RF,WOFDATE                                                       
BLDWNAR4 GOTO1 VDATCON,DMCB,(1,0(RF)),(17,0(R2))                                
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'TXTACC,R2),TXTACC                                            
         LA    R2,L'TXTACC+3(R2)                                                
         MVC   0(L'WOFF,R2),WOFF                                                
         LA    R2,L'WOFF+1(R2)                                                  
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'WOFFNAME,R2),WOFFNAME                                        
         LR    RE,R0               RESTORE RETURN ADDRESS                       
         BR    RE                  RETURN TO CALLER                             
         SPACE 2                                                                
***********************************************************************         
* BUILD WRITE-OFF INVOICE NARRATIVE FOR WRITE-OFF A/C POSTING         *         
***********************************************************************         
         SPACE 1                                                                
BLDINAR  LR    R0,RE               SAVE RETURN ADDRESS                          
         MVC   POSTNARR,SPACES                                                  
         LA    R2,POSTNARR                                                      
         MVC   0(L'TXTINV,R2),TXTINV                                            
         LA    R2,L'TXTINV(R2)                                                  
         MVC   0(L'TSARREF,R2),TSARREF                                          
         LA    R2,L'TSARREF-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'TXTDAT,R2),TXTDAT                                            
         LA    R2,L'TXTDAT+3(R2)                                                
         GOTO1 VDATCON,DMCB,(1,TSARDAT),(17,0(R2))                              
         LA    R2,9(R2)                                                         
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVC   2(L'WOFNARR,R2),WOFNARR                                          
         LR    RE,R0               RESTORE RETURN ADDRESS                       
         BR    RE                  RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* BUILD AFCEL IF NECESSARY                                            *         
***********************************************************************         
         SPACE 1                                                                
BLDAFC   TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZR   RE                                                               
         CP    POSTAM2,PZERO       TEST FOREIGN CURRENCY AMOUNT                 
         BER   RE                                                               
         LA    R1,ELEXTRA                                                       
         USING AFCELD,R1                                                        
         XC    AFCELD(AFCLNQ+1),AFCELD                                          
         MVI   AFCEL,AFCELQ                                                     
         MVI   AFCLN,AFCLNQ                                                     
         MVC   AFCCURR,FORCURT+(CURTCUR-CURTABD)                                
         ZAP   AFCAMNT,POSTAM2     POSTAM2=TSARPO2 OR TSARPO2 X -1              
         MVC   AFCX,ATLX           EXCHANGE RATE FOR THIS ALLOCATION            
         CLI   UPDMODE,UPDMPROC    TEST PROCESSING TSAR RECORDS                 
         BE    BLDAFC02                                                         
         TM    CURIND1,CUR1BNKC    TEST FOREIGN CURRENCY BANK ACCOUNT           
         BNZR  RE                                                               
         OI    AFCXSTAT,AFCXSMEM                                                
         BR    RE                                                               
BLDAFC02 TM    TSARINDS,TSARIWOF+TSARITRF+TSARIOFS                              
         BZR   RE                                                               
         MVC   AFCX,TSARAFCX       EXCHANGE RATE OF TRANSACTION                 
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* MAKE EXCHANGE RATE DIFFERENCE POSTINGS IF NECESSARY                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RECVTABD,R4         R4=A(RECEIVABLES TABLE ENTRY)                
PSTEXC   TM    CURIND1,CUR1SINC    TEST SINGLE CURRENCY ALLOCATION              
         BZR   RE                                                               
PSTEXCNT NTR1  ,                                                                
         ZAP   DUB,TSARDR2         TAKE DEBITS (CURRENCY)                       
         SP    DUB,TSARCR2         SUBTRACT CREDITS (CURRENCY)                  
         TM    TSARINDS,TSARIGRS   TEST ALLOCATED GROSS                         
         BNZ   *+10                                                             
         AP    DUB,TSARDI2         ADD DISCOUNT (CURRENCY)                      
         CP    DUB,TSARPO2         TEST ITEM NOW CLOSED IN CURRENCY             
         BNE   PSTEXC02                                                         
         ZAP   DUB,TSARDR          SET DIFFERENCE IN AGENCY CURRENCY            
         SP    DUB,TSARCR                                                       
         TM    TSARINDS,TSARIGRS                                                
         BNZ   *+10                                                             
         AP    DUB,TSARDISC                                                     
         SP    DUB,TSARPOST        CREDIT BALANCE IN AGENCY CURRENCY            
         MP    DUB,=P'-1'          REVERSE SIGN TO POST DEBIT                   
         B     PSTEXC04            CLOSE ITEM IN AGENCY CURRENCY                
*                                                                               
PSTEXC02 TM    TSARINDS,TSARIWOF+TSARITRF+TSARIOFS                              
         BNZ   PSTEXCX                                                          
         CLC   TSARAFCX,ATLX       TEST EXCHANGE RATES ARE THE SAME             
         BE    PSTEXCX                                                          
         LA    RF,TEMP                                                          
         USING AFCX,RF                                                          
         MVC   AFCX,ATLX                                                        
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         LA    RF,TEMP+L'AFCX                                                   
         MVC   AFCX,TSARAFCX                                                    
         XI    AFCXSTAT,AFCXSDIV   SWAP DIVIDE/MULTIPLY                         
         ICM   R0,8,AFCXSHFT       TAKE BINARY SIGNED SHIFT VALUE               
         SRA   R0,32-8             SHIFT AND PROPOGATE SIGN                     
         LCR   R0,R0                                                            
         STC   R0,AFCXSHFT         REVERSE SHIFT VALUE                          
         DROP  RF                                                               
         EXCHP TSARPO2,TEMP                                                     
         EXCHP TSARPO2,TEMP+L'AFCX,DUB=DUB2                                     
         SP    DUB,DUB2            REVERSE DIFFERENCE TO POST DEBIT             
*                                                                               
PSTEXC04 ZAP   POSTAMNT,DUB        BUILD RECEIVABLE/SOURCE POSTING              
         CP    POSTAMNT,PZERO      TEST ZERO POSTING                            
         BE    PSTEXCX             NO FURTHER ACTION                            
         BAS   RE,BLDFFT           BUILD ASSOCIATED CURRENCY ELEMENT            
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLI   EXDFOPOK,0                                                       
         BE    PSTEXC06                                                         
         OI    OVRBYTE,NOCEXC      SET NO EXCHANGE DIFFERENCE COSTING           
         LA    R1,EXDF                                                          
         ICM   R1,8,EXDFOPOK                                                    
         BAS   RE,SETCST                                                        
         MVC   EXDF,THISULA        EXTRACT VALUES                               
         MVC   EXDFNAME,THISNAME                                                
         MVC   EXDFCOST,THISCOST                                                
         MVC   CEXCACTS(CEXCACTL),THISACTS                                      
         BNE   PSTEXC06                                                         
         NI    OVRBYTE,FF-(NOCEXC) SET EXCHANGE DIFFERENCE COSTING              
PSTEXC06 MVC   POSTACT,EXDF        BUILD EXCHANGE/RECEIVABLE POSTING            
         MVC   POSTACTN,EXDFNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         BAS   RE,BLDFFT           BUILD ASSOCIATED CURRENCY ELEMENT            
         CLC   DISCULSI,EXDF       TEST SI EXCHANGE DIFFERENCES                 
         BE    PSTEXC08                                                         
         MP    POSTAMNT,=P'-1'     SE - REVERSE SIGN                            
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BNH   PSTEXC10            STANDARD POSTING TO SE                       
         TM    OVRBYTE,NOCEXC      TEST EXCHANGE COSTING POSTINGS OK            
         BNZ   PSTEXC10            STANDARD POSTING TO SE                       
         GOTO1 ABLDTRN,DMCB,(0,RECVCOST),('TRNSDR',CEXCPACT),(X'FF',0)          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CEXC3ACT),CEXC3ACT                           
         MVC   POSTCACN,CEXC3ACN                                                
         MVC   POSTDATE,TSARDAT    BILL DATE                                    
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,DMCB,('TRNSDR',EXDF),('TRNSDR',CEXCPACT),       X        
               (X'FF',0)                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CEXCPACT    BUILD 1P/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     PSTEXCX                                                          
*                                                                               
PSTEXC08 MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
PSTEXC10 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,EXDF       TEST SI EXCHANGE DIFFERENCES                 
         BNE   PSTEXCX                                                          
         CLI   EXDFCOST,C' '       TEST EXCHANGE DIFF A/C ANALYSIS FLAG         
         BNH   PSTEXCX                                                          
         TM    OVRBYTE,NOCEXC      TEST EXCHANGE COSTING POSTINGS OK            
         BNZ   PSTEXCX                                                          
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CEXC2ACT),CEXC2ACT                           
         MVC   POSTCACN,CEXC2ACN                                                
         MVC   POSTDATE,TSARDAT    BILL DATE                                    
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,CEXC2ACT    BUILD 12/1C COSTING POSTING                  
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'RECVCOST),RECVCOST                           
         MVC   POSTCACN,COSTCACN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
PSTEXCX  B     UPDATEX                                                          
         DROP  R4                                                               
*                                                                               
BLDFFT   LA    R1,ELEXTRA          BUILD ASSOCIATED CURRENCY ELEMENT            
         USING FFTELD,R1                                                        
         XC    FFTELD(FFTLN1Q+L'FFTDLEN+L'CURTCUR+1),FFTELD                     
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'CURTCUR                                
         MVI   FFTTYPE,FFTTACUR                                                 
         MVI   FFTDLEN,L'CURTCUR                                                
         MVC   FFTDATA(L'CURTCUR),FORCURT+(CURTCUR-CURTABD)                     
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A SET OF COSTING ACCOUNTS, WITH OPTIONAL OFFICE SUBSTITUTION  *         
* INTO ACCOUNT KEY                                                    *         
*                                                                     *         
* NTRY - R1 BYTE  0   - OFFICE POSITION IN KEY OR 0                   *         
*           BYTES 1-3 - A(U/L/ACCOUNT CODE)                           *         
***********************************************************************         
         SPACE 1                                                                
SETCST   NTR1  ,                                                                
         XC    THISACTS(THISACTL),THISACTS                                      
         LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,0(R1)                                                    
         SRL   R1,24                                                            
         LTR   R1,R1                                                            
         BZ    SETCST02            NO OFFICE SUBSTITUTION                       
         LA    R1,ACTKACT-1(R1)                                                 
         MVC   0(1,R1),POSTOFFC    OFFICE FOR THIS POSTING                      
         GOTO1 AGETACC,0                                                        
         BNE   UPDABEND            NO ACCOUNT FOR THIS OFFICE - ABEND           
         MVC   THISULA,ACTKULA                                                  
         MVC   THISNAME,RECNAME                                                 
         MVC   THISCOST,RECCSTG                                                 
SETCST02 CLI   THISCOST,C' '       TEST ACCOUNT ANALYSIS FLAG                   
         BNH   SETCSTN                                                          
         CLC   DISCULSI,ACTKUNT    TEST INCOME COSTING ANALYSIS                 
         BNE   SETCST04                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'THISCOST),THISCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   SETCSTN                                                          
         MVC   THIS2ACT,ACTKUNT    SAVE 12 COSTING U/L/ACCOUNT                  
         MVC   THIS2ACN,RECNAME    SAVE 12 COSTING ACCOUNT NAME                 
         B     SETCST06                                                         
*                                                                               
SETCST04 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'THISCOST),THISCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   SETCSTN                                                          
         MVC   THIS3ACT,ACTKUNT    SAVE 13 COSTING U/L/ACCOUNT                  
         MVC   THIS3ACN,RECNAME    SAVE 13 COSTING ACCOUNT NAME                 
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'THISCOST),THISCOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   SETCSTN                                                          
         MVC   THISPACT,ACTKUNT    SAVE 1P COSTING ACCOUNT                      
*                                                                               
SETCST06 DS    0H                                                               
         B     SETCSTY                                                          
         DROP  R2                                                               
*                                                                               
SETCSTY  CR    RB,RB                                                            
         B     UPDATEX                                                          
*                                                                               
SETCSTN  LTR   RB,RB                                                            
         B     UPDATEX                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
JRNSPEC  DS    0X                  ** SPECS FOR JOURNAL **                      
         SPROG 0,1,2,6,7,8                                                      
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPEC  H8,1,AC#ACC,7                                                    
         SPEC  H9,1,AC#ACC,7,LU                                                 
         SPEC  H8,16,AC#CTRA,10                                                 
         SPEC  H9,16,AC#CTRA,10,LU                                              
         SPEC  H8,32,AC#OFF,3                                                   
         SPEC  H9,32,AC#OFF,3,LU                                                
         SPEC  H8,36,AC#DATE,4                                                  
         SPEC  H9,36,AC#DATE,4,LU                                               
         SPEC  H8,45,AC#REFN,6                                                  
         SPEC  H9,45,AC#REFN,6,LU                                               
         SPEC  H8,52,AC#STT,3                                                   
         SPEC  H9,52,AC#STT,3,LU                                                
         SPEC  H8,60,AC#DRS,6                                                   
         SPEC  H9,60,AC#DRS,6,LU                                                
         SPEC  H8,71,AC#CRS,7                                                   
         SPEC  H9,71,AC#CRS,7,LU                                                
         SPROG 6,7,8                                                            
         SPEC  H8,86,AC#DRS,6                                                   
         SPEC  H8,99,AC#CRS,7                                                   
         SPROG 0                                                                
         SPEC  H1,41,AC#APOS,24                                                 
         SPEC  H2,41,AC#APOS,24,LU                                              
         SPROG 1                                                                
         SPEC  H1,38,AC#DPOS,30                                                 
         SPEC  H2,38,AC#DPOS,30,LU                                              
         SPROG 2                                                                
         SPEC  H1,33,AC#FPOS,39                                                 
         SPEC  H2,33,AC#FPOS,39,LU                                              
         SPROG 6                                                                
         SPEC  H1,41,AC#APOS,24                                                 
         SPEC  H2,41,AC#APOS,24,LU                                              
         SPROG 7                                                                
         SPEC  H1,38,AC#DPOS,30                                                 
         SPEC  H2,38,AC#DPOS,30,LU                                              
         SPROG 8                                                                
         SPEC  H1,33,AC#FPOS,39                                                 
         SPEC  H2,33,AC#FPOS,39,LU                                              
JRNSPECX DC    AL1(EOT)            SPEC END MARKER                              
         EJECT                                                                  
OFFTABD  DSECT                     DSECT COVERS OFFICE AMOUNT TABLE             
OFFTOFFC DS    CL(L'TRNOFFC)       OFFICE CODE                                  
OFFTEOTQ EQU   0                   END OF TABLE INDICATOR                       
OFFTAMNT DS    PL(L'TRNAMNT)       AMOUNT FOR BANK POSTING                      
OFFTAM2  DS    PL(L'AFCAMNT)       AMOUNT FOR BANK POSTING (CURRENCY)           
OFFTMAX  EQU   32                  MAXIMUM NUMBER OF ENTRIES IN TABLE           
OFFTABL  EQU   *-OFFTABD           LENGTH OF TABLE ENTRY                        
         SPACE 2                                                                
OVRWRKD  DSECT                     DSECT COVERS OVERLAY TEMPORARY W/S           
OVRDUB1  DS    D                                                                
OVRDUB2  DS    D                                                                
AUPDATE  DS    A                   A(OVERLAY UPDATE ROUTINE)                    
OVRWOVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT                     
OVRNARR  DS    CL(L'POSTNARR)      STANDARD NARRATIVE SAVED HERE                
OVRBYTE  DS    XL1                 FLAGGING BYTE                                
ACCERR   EQU   X'80'               GETACC RETURNED ERROR                        
NOCWOF   EQU   X'80'               WRITE-OFF COSTING A/C(S) MISSING             
NOCDSC   EQU   X'40'               DISCOUNT COSTING A/C(S) MISSING              
NOCEXC   EQU   X'20'               EXCHANGE COSTING A/C(S) MISSING              
NOCBCH   EQU   X'10'               BANK CHARGE COSTING A/C(S) MISSING           
*                                                                               
BANKDATP DS    PL3                 PWOS BANK DEPOSIT DATE                       
BANKDATC DS    XL2                 COMPRESSED BANK DEPOSIT DATE                 
*                                                                               
COSTCACN DS    CL(L'RECNAME)       1C COSTING ACCOUNT NAME                      
*                                                                               
CWOFACTS DS    0X                  WRITE-OFF COSTING ANALYSIS A/CS              
CWOF2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CWOF2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CWOF3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CWOF3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CWOFPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
CWOFACTL EQU   *-CWOFACTS                                                       
*                                                                               
CDSCACTS DS    0X                  DISCOUNT COSTING ANALYSIS A/CS               
CDSC2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CDSC2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CDSC3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CDSC3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CDSCPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
CDSCACTL EQU   *-CDSCACTS                                                       
*                                                                               
CEXCACTS DS    0X                  EXCHANGE DIFF COSTING ANALYSIS A/CS          
CEXC2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CEXC2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CEXC3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CEXC3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CEXCPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
CEXCACTL EQU   *-CEXCACTS                                                       
*                                                                               
CBCHACTS DS    0X                  BANK CHARGE COSTING ANALYSIS A/CS            
CBCH2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CBCH2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CBCH3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CBCH3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CBCHPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
CBCHACTL EQU   *-CBCHACTS                                                       
*                                                                               
THISULA  DS    CL(L'ACTKULA)       THIS POSTING ACCOUNT U/L/ACCOUNT             
THISNAME DS    CL(L'RECNAME)       THIS POSTING ACCOUNT NAME                    
THISCOST DS    CL(L'RECCSTG)       THIS POSTING ACCOUNT COSTING BYTE            
*                                                                               
THISACTS DS    0X                                                               
THIS2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
THIS2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
THIS3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
THIS3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
THISPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
THISACTL EQU   *-THISACTS                                                       
*                                                                               
JRACCUMS DS    0PL6                ** JOURNAL RECEIVABLE TOTALS **              
JRCVMRK  DS    PL6                 ALLOCATION AMOUNT                            
JRCVMR2  DS    PL6                 ALLOCATION AMOUNT                            
JRCVWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JRCVWO2  DS    PL6                 WRITE-OFF AMOUNT                             
JRCVOFS  DS    PL6                 OFFSET AMOUNT                                
JRCVOF2  DS    PL6                 OFFSET AMOUNT                                
JRCVDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JRCVDI2  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JRCVDSC  DS    PL6                 DISCOUNT/SURCHARGE AMOUNT                    
JRCVDS2  DS    PL6                 DISCOUNT/SURCHARGE AMOUNT                    
JRCVTRF  DS    PL6                 TRANSFER AMOUNT                              
JRCVTR2  DS    PL6                 TRANSFER AMOUNT                              
JRCVWVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JRCVWVA2 DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JRCVDRS  DS    PL6                 DEBITS                                       
JRCVDR2  DS    PL6                 DEBITS                                       
JRCVCRS  DS    PL6                 CREDITS                                      
JRCVCR2  DS    PL6                 CREDITS                                      
JRACCUMN EQU   (*-JRACCUMS)/L'JRACCUMS                                          
*                                                                               
JTACCUMS DS    0PL6                ** JOURNAL TOTALS **                         
JTOTMRK  DS    PL6                 ALLOCATION AMOUNT                            
JTOTMR2  DS    PL6                 ALLOCATION AMOUNT                            
JTOTWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JTOTWO2  DS    PL6                 WRITE-OFF AMOUNT                             
JTOTOFS  DS    PL6                 OFFSET AMOUNT                                
JTOTOF2  DS    PL6                 OFFSET AMOUNT                                
JTOTDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JTOTDI2  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JTOTDSC  DS    PL6                 DISCOUNT/SURCHARGE AMOUNT                    
JTOTDS2  DS    PL6                 DISCOUNT/SURCHARGE AMOUNT                    
JTOTTRF  DS    PL6                 TRANSFER AMOUNT                              
JTOTTR2  DS    PL6                 TRANSFER AMOUNT                              
JTOTWVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JTOTWVA2 DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JTOTCHQ  DS    PL6                 CHEQUE AMOUNT                                
JTOTCH2  DS    PL6                 CHEQUE AMOUNT                                
JTOTEXD  DS    PL6                 EXCHANGE DIFFERENCE                          
JTOTDRS  DS    PL6                 DEBITS                                       
JTOTDR2  DS    PL6                 DEBITS                                       
JTOTCRS  DS    PL6                 CREDITS                                      
JTOTCR2  DS    PL6                 CREDITS                                      
JTOTBAL  DS    PL6                 BALANCE                                      
JTOTBA2  DS    PL6                 BALANCE                                      
JTACCUMN EQU   (*-JTACCUMS)/L'JTACCUMS                                          
*                                                                               
RCVITEM  DS    PL4                 RECEIVABLE ITEM COUNT                        
TOTITEM  DS    PL4                 TOTAL ITEM COUNT                             
*                                                                               
ELEXTRA  DS    XL200               AREA FOR EXTRA ELEMENTS                      
*                                                                               
OFFTAB1  DS    (OFFTMAX)XL(OFFTABL)                                             
OFFTAB2  DS    (OFFTMAX)XL(OFFTABL)                                             
         EJECT                                                                  
DOUTLST  DS    0C                  ** DICTIONARY OUTPUT LIST **                 
TXTOFS   DS    CL9                                                              
*                                                                               
TXTACC   DS    CL3                                                              
TXTDAT   DS    CL4                                                              
TXTWOF   DS    CL11                                                             
TXTWOFD  DS    CL14                                                             
TXTINV   DS    CL9                                                              
*                                                                               
TXTTRFFR DS    CL16                                                             
TXTTRFTO DS    CL14                                                             
TXTON    DS    CL2,CL1                                                          
*                                                                               
JRNRECV  DS    CL14                                                             
JRNBANK  DS    CL12                                                             
JRNNARR  DS    CL9                                                              
JRNBTOT  DS    CL12                                                             
JRNMRK   DS    CL9                                                              
JRNDIF   DS    CL8                                                              
JRNCHQP  DS    CL14                                                             
JRNCHQ   DS    CL13                                                             
JRNWOFSR DS    CL11                                                             
JRNWOF   DS    CL13                                                             
JRNWVAT  DS    CL14                                                             
JRNOFS   DS    CL9                                                              
JRNDSCSR DS    CL11                                                             
JRNDSC   DS    CL15                                                             
JRNTRF   DS    CL11                                                             
JRNTOT   DS    CL11                                                             
JRNBAL   DS    CL7                                                              
JRNITEM  DS    CL5                                                              
JRNEXDIF DS    CL20                                                             
*                                                                               
OV@DEF   DS    CL12                                                             
OV@FTRAT DS    CL12                                                             
OV@CALC  DS    CL12                                                             
*                                                                               
OVLITS   DS    0X                  ** SEE COVLITS IN PROGRAM AREA **            
*                                                                               
BANKULS  DS    CL2                 VALID LEDGERS FOR BANK                       
         DS    CL2                                                              
         DS    CL2                                                              
         DS    CL2                                                              
         DS    XL1                                                              
*                                                                               
ANALULS  DS    CL2                 VALID LEDGERS FOR ANAL BANK                  
         DS    XL1                                                              
*                                                                               
DISCULS  DS    CL2                 VALID LEDGERS FOR DISCOUNT                   
DISCULSI DS    CL2                                                              
         DS    CL2                                                              
         DS    XL1                                                              
*                                                                               
RECVULS  DS    CL2                 VALID LEDGERS FOR RECEIVABLES                
         DS    XL1                                                              
*                                                                               
WOFFULS  DS    CL2                 VALID LEDGERS FOR WRITE-OFF                  
         DS    CL2                                                              
         DS    CL2                                                              
         DS    XL1                                                              
*                                                                               
COST2    DS    CL2                 12 COSTING U/L                               
COST3    DS    CL2                 13 COSTING U/L                               
COSTP99  DS    CL4                 1P COSTING U/L/DEPT                          
*                                                                               
OVLITSL  EQU   *-OVLITS                                                         
*                                                                               
OVRWRKL  EQU   *-OVRWRKD                                                        
         ORG   OVRWRKD+L'OVERWORK                                               
         EJECT                                                                  
JRNHEDD  DSECT                     DSECT COVERS A HEADLINE                      
JRNHTXT  DS    CL(L'JRNRECV)                                                    
         DS    CL2                                                              
JRNHACT  DS    CL(L'RECVACT-1)                                                  
         DS    CL2                                                              
JRNHACTN DS    CL(L'RECVACTN)                                                   
         EJECT                                                                  
       ++INCLUDE ACRECWRK                                                       
TWAD     DSECT                                                                  
         ORG   RECOLAYH                                                         
       ++INCLUDE ACRECE1D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'217ACREC11   05/01/02'                                      
         END                                                                    
