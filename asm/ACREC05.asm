*          DATA SET ACREC05    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T60805A                                                                  
ACREC05  TITLE '- DUTCH OVERLAY TO HANDLE HEADER SCREEN AND POSTINGS'           
ACREC05  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REC5**,RA,R9                                                 
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
         LH    R8,=Y(OVERWORK-WORKD)                                            
         LA    R8,WORKD(R8)                                                     
         USING OVRWRKD,R8                                                       
         USING SAVED,R7                                                         
         USING TWAD,R6                                                          
         SPACE 2                                                                
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
         B     UPDATE              UPDATE                                       
         B     UPDATE              DRAFT UPDATE                                 
         B     UPDATE              FILTERED DRAFT UPDATE                        
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     VALHED              CHANGE (HEADER)                              
         B     RELAST              RESTORE HEADER (AFTER UPDATE)                
         EJECT                                                                  
***********************************************************************         
* INITIALISE - SET Y-TYPES FOR VALID LEDGER LISTS                     *         
***********************************************************************         
         SPACE 1                                                                
SETLST   DS    0H                                                               
         GOTO1 VDICTATE,DMCB,C'LL  ',DINLST,DOUTLST                             
         GOTO1 (RF),(R1),,TINLST,TOUTLST                                        
         MVI   TOTALSX,EOT                                                      
         MVC   BANKLIST,=Y(BANKULS-ACREC05)                                     
         MVC   DISCLIST,=Y(DISCULS-ACREC05)                                     
         MVC   RECVLIST,=Y(RECVULS-ACREC05)                                     
         MVC   VATLIST,=Y(RECVULS-ACREC05)                                      
         MVC   WOFFLIST,=Y(WOFFULS-ACREC05)                                     
         MVC   SHEADTOT,=Y(RECHTOTH-RECOLAYH)                                   
         MVC   SHEADPFK,=Y(RECHPFKH-RECOLAYH)                                   
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
VALBNK   NI    TWAMODE2,255-TWA2NALL                                            
         GOTO1 AVALBNK,RECBNKH                                                  
         BH    EXIT                ERROR                                        
         BE    *+12                FIELD IS PRESENT                             
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALBNKX                                                          
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',BANKUL),                X        
               (X'C0',RECNDSP),AIO1,(L'BANKNAME,BANKNAME)                       
VALBNKX  DS    0H                                                               
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
         EJECT                                                                  
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
         MVC   BANKDATE,CHQDATE                                                 
VALCDTX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CHEQUE (CONTROL) AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCHQ   ZAP   CHQAMT,PZERO        CLEAR CHEQUE AMOUNT                          
         OC    BANK,BANK           TEST BANK A/C PRESENT                        
         BZ    *+8                                                              
         MVI   FVMINL,1            YES - REQUIRED FIELD                         
         GOTO1 AFVAL,RECCHQH                                                    
         BH    EXIT                ERROR                                        
         BE    *+12                FIELD PRESENT                                
         OI    TWAMODE2,TWA2NALL   ABSENT - SET NO ALLOCATION                   
         B     VALCHQ2                                                          
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         GOTO1 VCASHVAL,DMCB,(X'82',RECCHQ),(R0)                                
         CLI   0(R1),0                                                          
         BNE   EXIT                INVALID AMOUNT                               
         ZAP   CHQAMT,4(8,R1)                                                   
VALCHQ2  CURED CHQAMT,(L'RECCHQ,RECCHQ),2,ALIGN=LEFT,FLOAT=-                    
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
VALCHQX  DS    0H                                                               
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
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    VALWCP2                                                          
         CLC   DISCULSI,WOFF       TEST WRITE-OFFS TO SI                        
         BE    VALWCP2                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BH    VALWCP2                                                          
         CLC   DISCULSI,DISC       TEST DISCOUNTS TO SI                         
         BNE   *+8                                                              
VALWCP2  MVI   FVMINL,1            WRITE-OFF CLI/PRO REQUIRED                   
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
VALCOST  CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BH    *+12                                                             
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   VALCOSTX            NO COSTING ANALYSIS POSTINGS                 
         LA    R3,RECVTAB          R3=A(RECEIVABLE A/C TABLE)                   
         USING RECVTABD,R3                                                      
         LA    R2,KEY              R2=A(KEY)                                    
         USING ACTRECD,R2                                                       
         LA    R1,RECRCVH          SET A(RECEIVABLE A/C(S) FIELD)               
         ST    R1,FVADDR                                                        
*                                                                               
VALCOS2  CLI   RECVNDX,X'FF'       TEST E-O-T                                   
         BE    VALCOS4             FINISHED WITH RECEIVABLE A/C(S)              
         MVC   FVINDX,RECVNDX      SET MULTIPLE FIELD INDEX FOR ERROR           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'RECVCOST),RECVCOST                                     
         GOTO1 AGETACC,0           GET 1C COSTING ACCOUNT                       
         BNE   EXIT                                                             
         LA    R3,RECVTABL(R3)     NEXT TABLE ENTRY                             
         B     VALCOS2             TEST NEXT ACCOUNT                            
*                                                                               
VALCOS4  MVI   FVINDX,0            CLEAR MULTIPLE FIELD INDEX FOR ERROR         
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   VALCOS10            NO - DEAL WITH DISCOUNT A/C FLAG             
         LA    R1,RECWACH          SET A(WRITE-OFF A/C FIELD)                   
         ST    R1,FVADDR                                                        
         CLC   DISCULSI,WOFF       TEST INCOME COSTING ANALYSIS                 
         BNE   VALCOS6                                                          
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS8                                                          
*                                                                               
VALCOS6  MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
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
         B     VALCOS8                                                          
         EJECT                                                                  
VALCOS8  CLI   DISCCOST,C' '       NOW TEST DISCOUNT A/C ANALYSIS FLAG          
         BNH   VALCOS14                                                         
*                                                                               
VALCOS10 B     VALCOS12                                                         
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   EXIT                                                             
         B     VALCOS14                                                         
*                                                                               
VALCOS12 MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
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
*                                                                               
VALCOS14 DS    0H                                                               
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
         OC    WOFF,WOFF           PROT WRITE-OFF A/C IF PRESENT                
         BZ    *+8                                                              
         OI    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   WCPCNT,0            PROT W-O CLI/PRO IF PRESENT                  
         BE    *+8                                                              
         OI    RECWCPH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECVATH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECDATH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECMOSH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECBNRH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECBSOH+(FVATRB-FVIHDR),FVAPROT                                  
RESHEDX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* UPDATE - MAKE POSTINGS VIA ACUPDATE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RECVTABD,R4         R4=A(RECEIVABLES TABLE ENTRY)                
         USING REPD,R5             R5=A(REPORT WORKING STORAGE)                 
UPDATE   SR    RF,RF                                                            
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
         BO    UPDF2                                                            
         GOTO1 VDATCON,DMCB,(0,BANKDATE),(1,BANKDATP)                           
         MVC   OVRNARR,SPACES                                                   
         LA    R2,OVRNARR                                                       
         MVC   0(L'JRNPAYR,R2),JRNPAYR PAYMENT REFERENCE                        
         LA    R2,L'JRNPAYR+1(R2)                                               
         MVC   0(L'BANKREF,R2),BANKREF                                          
         LA    R2,L'BANKREF-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'JRNDTED,R2),JRNDTED DATED                                    
         LA    R2,L'JRNDTED+1(R2)                                               
         GOTO1 VDATCON,DMCB,(0,BANKDATE),(8,(R2))                               
         CLI   0(R2),C' '          TEST LEADING SPACE                           
         BNE   *+14                                                             
         MVC   0(9,R2),1(R2)       SHUFFLE ALONG                                
         MVI   9(R2),C' '          AND CLEAR LAST BYTE                          
         MVC   POSTNARR,OVRNARR                                                 
*                                                                               
         LA    R1,REPH6                                                         
         USING JRNHEDD,R1                                                       
         MVC   JRNHTXT(L'JRNNARR),JRNNARR                                       
         MVC   JRNHACT(L'REPH6-(JRNHACT-JRNHEDD)),POSTNARR                      
UPDF2    MVC   OVRNARR,POSTNARR    SAVE STANDARD NARRATIVE                      
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
         TM    TWAMODE,TWAMDRFT    TEST DRAFT JOURNAL MODE                      
         BZ    UPDFRSTX                                                         
         MVI   REPSUBPG,1          SET SUB-PROGRAM NUMBER                       
         CLI   UPDACTN,ACTFILT     TEST FILTERED DRAFT JOURNAL                  
         BNE   UPDFRSTX                                                         
         MVI   REPSUBPG,2          SET SUB-PROGRAM NUMBER                       
UPDFRSTX B     EXIT                                                             
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
         OI    OVRBYTE,NOCWOF      PRESET NO WRITE-OFF COSTING POSTINGS         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   *+8                                                              
         OI    OVRBYTE,NOCDSC      PRESET NO DISCOUNT COSTING POSTINGS          
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
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDRCF4                                                          
*                                                                               
         CLC   DISCULSI,WOFF       TEST INCOME COSTING ANALYSIS                 
         BNE   UPDRCF2                                                          
*                                                                               
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   UPDRCF4                                                          
         MVC   CWOF2ACT,ACTKUNT    SAVE 12 COSTING U/L/ACCOUNT                  
         MVC   CWOF2ACN,RECNAME    SAVE 12 COSTING ACCOUNT NAME                 
         NI    OVRBYTE,255-NOCWOF  SET WRITE-OFF COSTING OK                     
         B     UPDRCF4                                                          
*                                                                               
UPDRCF2  MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'WOFFCOST),WOFFCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   UPDRCF4                                                          
         MVC   CWOF3ACT,ACTKUNT    SAVE 13 COSTING U/L/ACCOUNT                  
         MVC   CWOF3ACN,RECNAME    SAVE 13 COSTING ACCOUNT NAME                 
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'WOFFCOST),WOFFCOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   UPDRCF4                                                          
         MVC   CWOFPACT,ACTKUNT    SAVE 1P COSTING ACCOUNT                      
         NI    OVRBYTE,255-NOCWOF  SET WRITE-OFF COSTING OK                     
*                                                                               
UPDRCF4  CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDRCVFX                                                         
*                                                                               
         CLC   DISCULSI,DISC       TEST INCOME COSTING ANALYSIS                 
         BNE   UPDRCF6                                                          
*                                                                               
         MVC   KEY,SPACES          INCOME COSTING ANALYSIS                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST2),COST2                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0           GET 12 COSTING ACCOUNT                       
         BNE   UPDRCVFX                                                         
         MVC   CDSC2ACT,ACTKUNT    SAVE 12 COSTING U/L/ACCOUNT                  
         MVC   CDSC2ACN,RECNAME    SAVE 12 COSTING ACCOUNT NAME                 
         NI    OVRBYTE,255-NOCDSC  SET DISCOUNT COSTING OK                      
         B     UPDRCVFX                                                         
*                                                                               
UPDRCF6  MVC   KEY,SPACES          EXPENSE COSTING ANALYSIS                     
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COST3),COST3                                           
         MVC   ACTKACT(L'DISCCOST),DISCCOST                                     
         GOTO1 AGETACC,0           GET 13 COSTING ACCOUNT                       
         BNE   UPDRCVFX                                                         
         MVC   CDSC3ACT,ACTKUNT    SAVE 13 COSTING U/L/ACCOUNT                  
         MVC   CDSC3ACN,RECNAME    SAVE 13 COSTING ACCOUNT NAME                 
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(L'COSTP99),COSTP99                                       
         MVC   ACTKUNT+L'COSTP99(L'DISCCOST),DISCCOST                           
         GOTO1 AGETACC,0           GET 1P COSTING ACCOUNT                       
         BNE   UPDRCVFX                                                         
         MVC   CDSCPACT,ACTKUNT    SAVE 1P COSTING ACCOUNT                      
         NI    OVRBYTE,255-NOCDSC  SET DISCOUNT COSTING OK                      
*                                                                               
UPDRCVFX B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
UPDPROC  LA    RF,JRCVWOF          ADDRESS WRITE-OFF TOTALS                     
         LA    RE,JTOTWOF                                                       
         TM    TSARINDS,TSARIWOF   TEST WRITE-OFF                               
         BO    UPDPRO2                                                          
         LA    RF,JRCVOFS          ADDRESS OFFSET TOTALS                        
         LA    RE,JTOTOFS                                                       
         TM    TSARINDS,TSARIOFS   TEST OFFSET                                  
         BO    UPDPRO2                                                          
         LA    RF,JRCVTRF          ADDRESS TRANSFER TOTALS                      
         LA    RE,JTOTTRF                                                       
         TM    TSARINDS,TSARITRF   TEST TRANSFERRED                             
         BO    UPDPRO2                                                          
         AP    RECVAMT,TSARPOST    ADD TO RECEIVABLE TOTAL                      
*                                                                               
         LA    R1,OFFTAB1          POST AMOUNT TO RECEIVABLE TABLE              
         USING OFFTABD,R1                                                       
         LA    R0,OFFTMAX                                                       
UPDPRO00 CLI   OFFTABD,OFFTEOTQ    TEST FREE ENTRY                              
         BE    UPDPRO01                                                         
         CLC   OFFTOFFC,TSAROFFC   MATCH ON OFFICE CODE                         
         BNE   *+14                                                             
         AP    OFFTAMNT,TSARPOST   YES - ADD TO OFFICE POSTING                  
         B     UPDPRO02                                                         
         LA    R1,OFFTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDPRO00         DO FOR NUMBER OF TABLE ENTRIES               
         DC    H'0'                                                             
UPDPRO01 MVC   OFFTOFFC,TSAROFFC   CREATE NEW TABLE ENTRY                       
         ZAP   OFFTAMNT,TSARPOST                                                
*                                                                               
UPDPRO02 LA    R1,OFFTAB2          POST AMOUNT TO ANALYSIS BANK TABLE           
         LA    R0,OFFTMAX                                                       
UPDPRO03 CLI   OFFTABD,OFFTEOTQ    TEST FREE ENTRY                              
         BE    UPDPRO04                                                         
         CLC   OFFTOFFC,TSAROFFC   MATCH ON OFFICE CODE                         
         BNE   *+14                                                             
         AP    OFFTAMNT,TSARPOST   YES - ADD TO OFFICE POSTING                  
         B     UPDPRO05                                                         
         LA    R1,OFFTABL(R1)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDPRO03         DO FOR NUMBER OF TABLE ENTRIES               
         DC    H'0'                                                             
UPDPRO04 MVC   OFFTOFFC,TSAROFFC   CREATE NEW TABLE ENTRY                       
         ZAP   OFFTAMNT,TSARPOST                                                
         DROP  R1                                                               
*                                                                               
UPDPRO05 LA    RF,JRCVMRK          ADDRESS ALLOCATION TOTALS                    
         LA    RE,JTOTMRK                                                       
         CLI   TSARTYPE,TSARTOVR   TEST DIFFERENCE                              
         BNE   UPDPRO2                                                          
         LA    RF,JRCVDIF          ADDRESS DIFFERENCE TOTALS                    
         LA    RE,JTOTDIF                                                       
*                                                                               
UPDPRO2  AP    0(L'JRACCUMS,RF),TSARPOST                                        
         AP    0(L'JTACCUMS,RE),TSARPOST                                        
         MVC   POSTACT,RECVACT+1   BUILD RECEIVABLE/SOURCE POSTING              
         MVC   POSTACTN,RECVACTN                                                
         MVC   POSTCAC,TSARCAC                                                  
         MVC   POSTCACN,SPACES     NO CONTRA NAME (IT'S THE SOURCE)             
         MVC   POSTDATE,TSARDAT                                                 
         MVC   POSTREF,TSARREF                                                  
         MVI   POSTSTAT,TRNSAUTH                                                
         ZAP   POSTAMNT,TSARPOST                                                
         MVC   POSTOFFC,TSAROFFC                                                
         OI    UPDINDS,UPDIPRTS    PRINT STATUS ON JOURNAL                      
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BZ    UPDPRO3                                                          
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         TM    TSARINDS,TSARISDR   TEST DEBIT                                   
         BZ    *+8                                                              
         OI    POSTSTAT,TRNSDR     SET DEBIT                                    
         ZAP   DUB,POSTAMNT                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   POSTAMNT,DUB                                                     
         BAS   RE,BLDTNAR                                                       
         B     UPDPRO4                                                          
UPDPRO3  TM    TSARINDS,TSARIOFS   TEST OFFSET POSTING                          
         BZ    UPDPRO5                                                          
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
UPDPRO4  GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BNZ   UPDPRO30            PRODUCE TRANSFER POSTING                     
         B     UPDPROCX            EXIT                                         
*                                                                               
UPDPRO5  TM    TSARINDS,TSARIWOF   TEST WRITE-OFF POSTING                       
         BZ    *+8                                                              
         BAS   RE,BLDWNAR          BUILD WRITE-OFF NARRATIVE                    
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         TM    TSARINDS,TSARIWOF   IS THIS A WRITE-OFF POSTING?                 
         BZ    UPDPRO14                                                         
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
         ZAP   OVRWOVAT,=P'0'      CLEAR WRITE-OFF VAT ADJUSTMENT               
         OC    VAT,VAT                                                          
         BZ    UPDPRO6                                                          
         SR    RF,RF                                                            
         ICM   RF,3,VATRATE                                                     
         CVD   RF,DUB                                                           
         AP    DUB,=P'10000'                                                    
         ZAP   OVRPKWK,TSARPOST                                                 
         MP    OVRPKWK,=P'1000000'                                              
         DP    OVRPKWK,DUB                                                      
         SRP   OVRPKWK(L'OVRPKWK-L'DUB),64-2,5                                  
         ZAP   OVRWOVAT,TSARPOST                                                
         SP    OVRWOVAT,OVRPKWK(L'OVRPKWK-L'DUB)                                
         SP    POSTAMNT,OVRWOVAT   TAKE IT FROM WRITE-OFF POSTING               
         SP    JRCVWOF,OVRWOVAT    TAKE FROM JOURNAL WRITE-OFF TOTALS           
         SP    JTOTWOF,OVRWOVAT                                                 
         AP    JRCVWVAT,OVRWOVAT   ADD TO JOURNAL WRITE-OFF VAT TOTALS          
         AP    JTOTWVAT,OVRWOVAT                                                
UPDPRO6  CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BE    UPDPRO8                                                          
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDPRO10            STANDARD POSTING TO SE/SQ                    
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPDPRO10            STANDARD POSTING TO SE/SQ                    
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
         B     UPDPRO12                                                         
*                                                                               
UPDPRO8  MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         MP    POSTAMNT,=P'-1'     REVERSE POSTING SIGN                         
UPDPRO10 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   UPDPRO12                                                         
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C ANALYSIS FLAG             
         BNH   UPDPRO12                                                         
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BNZ   UPDPRO12                                                         
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
UPDPRO12 MVC   POSTDATE,TSARDAT    RESET TRANSACTION DATE                       
         MVC   POSTREF,TSARREF     RESET TRANSACTION REFERENCE                  
         OC    VAT,VAT             TEST VAT A/C PRESENT                         
         BZ    UPDPRO14                                                         
         CP    OVRWOVAT,=P'0'      IF NON-ZERO AMOUNT                           
         BE    UPDPRO14                                                         
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
UPDPRO14 MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
         OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    UPDPROCX                                                         
         TM    TSARINDS,TSARIGRS   TEST GROSS POSTING - NO DISCOUNT             
         BO    UPDPROCX                                                         
         CP    TSARDISC,PZERO      TEST ANY POSTING TO MAKE                     
         BE    UPDPROCX                                                         
         TM    TSARINDS,TSARIOFS   TEST OFFSET (NO DISCOUNT)                    
         BNZ   *+16                                                             
         AP    JRCVDSC,TSARDISC    ACCUMULATE DISCOUNT ALLOC/WOFF ONLY          
         AP    JTOTDSC,TSARDISC                                                 
         MVC   POSTACT,RECVACT+1   BUILD 2ND RECEIVABLE/SOURCE POSTING          
         MVC   POSTACTN,RECVACTN                                                
         MVC   POSTCAC,TSARCAC                                                  
         MVC   POSTCACN,SPACES     NO CONTRA NAME (IT'S THE SOURCE)             
         MVC   POSTOFFC,TSAROFFC                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         ZAP   POSTAMNT,TSARDISC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,DISC        BUILD DISCOUNT/RECEIVABLE POSTING            
         MVC   POSTACTN,DISCNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
         CLC   DISCULSI,DISC       TEST SI DISCOUNT/SURCHARGE                   
         BE    UPDPRO16                                                         
         MP    POSTAMNT,=P'-1'     SE/SQ - REVERSE SIGN                         
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDPRO18            STANDARD POSTING TO SE/SQ                    
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDPRO18            STANDARD POSTING TO SE/SQ                    
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
UPDPRO16 MVC   POSTCAC,RECVPCP     SI - SET CLIENT/PRODUCT CONTRA               
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
UPDPRO18 GOTO1 ABLDTRN,0                                                        
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
UPDPRO30 BAS   RE,BLDXFR           BUILD TRANSFER POSTING                       
         LH    R1,=H'-1'           SET RECORD ALREADY BUILT IN IO               
         GOTO1 ABLDTRN             ADD TRANSACTION                              
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDPROCX DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
UPDRCVL  CP    RECVAMT,PZERO       BUILD BANK/RECEIVABLE POSTING                
         BE    UPDRCL2             NOTHING TO POST                              
         OC    ANAL,ANAL                                                        
         BZ    UPDRCL00                                                         
         MVC   POSTACT,ANAL        USE ANALYSIS BANK IF PRESENT                 
         MVC   POSTACTN,ANALNAME                                                
         B     UPDRCL01                                                         
*                                                                               
UPDRCL00 MVC   POSTACT,BANK        ELSE BANK ACCOUNT                            
         MVC   POSTACTN,BANKNAME                                                
*                                                                               
UPDRCL01 MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH  DEBIT AUTH'D                           
*                                                                               
         LA    R2,OFFTAB1                                                       
         USING OFFTABD,R2          R2=A(OFFICE TABLE)                           
         LA    R0,OFFTMAX                                                       
UPDRCL02 CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDRCL2                                                          
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
         MVC   POSTOFFC,OFFTOFFC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDRCL02         DO FOR NUMBER OF TABLE ENTRIES               
         DROP  R2                                                               
*                                                                               
UPDRCL2  OC    PRTSUB,PRTSUB                                                    
         BZ    UPDRCVLX                                                         
*                                                                               
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNDATE(42),=42C'-'                                              
         MVC   JRNDATE+14(L'JRNRECV),JRNRECV                                    
*                                                                               
         LA    R2,L'REPP1(R2)      ALLOCATED                                    
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         AP    JRCVCRS,JRCVMRK                                                  
         CURED JRCVMRK,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      SPECIALS                                     
         AP    JRCVCRS,JRCVDIF                                                  
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JRCVDIF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      PORTION OF CHEQUE                            
         MVC   JRNDATE(L'JRNCHQP),JRNCHQP                                       
         CURED RECVAMT,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      WRITTEN-OFF - SR                             
         ZAP   DUB,JRCVWOF         WRITTEN-OFF CRS NET OF VAT                   
         AP    DUB,JRCVWVAT        ADD BACK VAT TO WRITTEN-OFF SR CRS           
         AP    JRCVCRS,DUB                                                      
         MVC   JRNDATE(L'JRNWOFSR),JRNWOFSR                                     
         MVC   JRNDATE+L'JRNWOFSR+1(5),=C' - SR'                                
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
*                                                                               
         LA    R2,L'REPP1(R2)      WRITE-OFF A/C                                
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JRCVWOF                                                      
         LA    RF,JRNDR                                                         
         LA    RE,JRCVDRS                                                       
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   UPDRCL3                                                          
         LA    RF,JRNCR            CREDIT                                       
         LA    RE,JRCVCRS                                                       
         MP    DUB,=P'-1'          MINUS                                        
UPDRCL3  AP    0(L'JRCVDRS,RE),DUB                                              
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         LA    R2,L'REPP1(R2)      WRITE-OFF VAT ADJ.                           
         MVC   JRNDATE(L'JRNWVAT),JRNWVAT                                       
         MP    JRCVWVAT,=P'-1'                                                  
         AP    JRCVCRS,JRCVWVAT                                                 
         CURED JRCVWVAT,(L'JRNCR,JRNCR),2,MINUS=YES                             
*                                                                               
         CP    JRCVOFS,=P'0'       OFFSETS                                      
         BE    UPDRCL4                                                          
         LA    R2,L'REPP1(R2)                                                   
         AP    JRCVCRS,JRCVOFS                                                  
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         CURED JRCVOFS,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
UPDRCL4  LA    R2,L'REPP1(R2)      DIS/SCH/FEE - SR                             
         MVC   JRNDATE(L'JRNDSCSR),JRNDSCSR                                     
         MVC   JRNDATE+L'JRNDSCSR+1(5),=C' - SR'                                
         AP    JRCVDRS,JRCVDSC                                                  
         CURED JRCVDSC,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      DIS/SCH/FEE A/C                              
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,JRCVDSC                                                      
         CLC   DISCULSI,DISC       TEST SI DISCOUNT                             
         BNE   UPDRCL6                                                          
         LA    RF,JRNCR            SI - CREDIT                                  
         AP    JRCVCRS,DUB                                                      
         B     UPDRCL8                                                          
UPDRCL6  LA    RF,JRNDR            SE/SQ - DEBIT                                
         MP    DUB,=P'-1'          DISCOUNT TYPE - REVERSE SIGN                 
         AP    JRCVDRS,DUB                                                      
UPDRCL8  CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         CP    JRCVTRF,PZERO                                                    
         BE    UPDRCL12                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JRCVTRF,(L'JRNDR,JRNDR+1),2,MINUS=YES,BRACKET=Y                  
         LA    R1,JRNDR                                                         
UPDRCL10 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDRCL10                                                         
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDRCL12 LA    R2,L'REPP1(R2)      TOTAL DEBITS AND CREDITS                     
         MVC   JRNDATE(L'JRNTOT),JRNTOT                                         
         AP    JTOTCRS,JRCVCRS                                                  
         AP    JTOTDRS,JRCVDRS     DON'T ADD RECVAMT TO JTOTDRS                 
         AP    JRCVDRS,RECVAMT                                                  
         CURED JRCVCRS,(L'JRNCR,JRNCR),2,MINUS=YES                              
         CURED JRCVDRS,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      ITEMS                                        
         CURED RCVITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
*                                                                               
         LA    R2,L'REPP1(R2)      COSTING WARNING MESSAGE                      
         TM    OVRBYTE,NOCWOF+NOCDSC                                            
         BZ    UPDRCL14            BOTH NOCWOF+NOCDSC BITS OFF                  
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         LA    R0,JRNDATE                                                       
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$CSTAM)                                           
         GOTO1 VGETTXT,(R1)                                                     
         TM    OVRBYTE,NOCWOF+NOCDSC                                            
         BO    UPDRCL14            BOTH NOCWOF+NOCDSC BITS ON                   
*                                                                               
         MVC   JRNDATE,SPACES      ASSUME NOCWOF IS THE BIT ON                  
         MVC   GTMSGNO,=AL2(AS$WRTCM)                                           
         GOTO1 (RF),(R1)                                                        
         TM    OVRBYTE,NOCDSC      TEST NOCDSC BIT ON                           
         BZ    UPDRCL14                                                         
         MVC   JRNDATE,SPACES      DISCOUNT COSTING A/C(S) IN ERROR             
         MVC   GTMSGNO,=AL2(AS$DSFCM)                                           
         GOTO1 (RF),(R1)                                                        
*                                                                               
UPDRCL14 GOTO1 VREPORT,REPD                                                     
*                                                                               
UPDRCVLX AP    TOTITEM,RCVITEM     ADD RECEIVABLE ITEMS TO TOTAL                
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
UPDLAST  OI    REPHEADI,REPHFRCE   NEW PAGE FOR OVERALL POSTINGS                
         MVC   REPH4,SPACES                                                     
         OC    ANAL,ANAL           BUILD BANK/ANALYSIS BANK POSTING             
         BZ    UPDLAS4             ANALYSIS BANK A/C ABSENT                     
*                                                                               
         LA    R2,OFFTAB2                                                       
         USING OFFTABD,R2          R2=A(OFFICE TABLE)                           
         LA    R0,OFFTMAX                                                       
UPDLAS2  CLI   OFFTABD,OFFTEOTQ    TEST E-O-T                                   
         BE    UPDLAS4                                                          
*                                                                               
         MVC   POSTACT,BANK        BUILD BANK/ANALYSIS BANK POSTING             
         MVC   POSTACTN,BANKNAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'ANAL),ANAL                                   
         MVC   POSTCACN,ANALNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         ZAP   POSTAMNT,OFFTAMNT   SET AMOUNT & OFFICE CODE                     
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
         MVC   POSTOFFC,OFFTOFFC                                                
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
         XC    OFFTABD(OFFTABL),OFFTABD                                         
         LA    R2,OFFTABL(R2)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,UPDLAS2          DO FOR NUMBER OF TABLE ENTRIES               
         DROP  R2                                                               
*                                                                               
UPDLAS4  OC    PRTSUB,PRTSUB       TEST IF PRINTING                             
         BZ    UPDLASTX                                                         
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNDATE(42),=42C'-'                                              
         MVC   JRNDATE+14(L'JRNBTOT),JRNBTOT                                    
*                                                                               
         LA    R2,L'REPP1(R2)      ALLOCATED                                    
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         CURED JTOTMRK,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      SPECIALS                                     
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JTOTDIF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      CHEQUE                                       
         MVC   JRNDATE(L'JRNCHQ),JRNCHQ                                         
         ZAP   JTOTCHQ,CHQAMT                                                   
         AP    JTOTDRS,CHQAMT      ADD CHEQUE TO JTOTDRS, NOW                   
         CURED JTOTCHQ,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      WRITTEN-OFF - SR                             
         MVC   JRNDATE(L'JRNWOFSR),JRNWOFSR                                     
         MVC   JRNDATE+L'JRNWOFSR+1(5),=C' - SR'                                
         ZAP   DUB,JTOTWOF         WRITTEN-OFF CRS NET OF VAT                   
         AP    DUB,JTOTWVAT        ADD BACK VAT TO WRITTEN-OFF CRS              
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
*                                                                               
         LA    R2,L'REPP1(R2)      WRITE-OFF A/C                                
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JTOTWOF                                                      
         LA    RF,JRNDR                                                         
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+14                                                             
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         LA    R2,L'REPP1(R2)      WRITE-OFF VAT ADJ.                           
         MVC   JRNDATE(L'JRNWVAT),JRNWVAT                                       
         MP    JTOTWVAT,=P'-1'                                                  
         CURED JTOTWVAT,(L'JRNCR,JRNCR),2,MINUS=YES                             
*                                                                               
         CP    JTOTOFS,=P'0'       OFFSETS                                      
         BE    UPDLAS6                                                          
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         CURED JTOTOFS,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
UPDLAS6  LA    R2,L'REPP1(R2)      DIS/SCH/FEE - SR                             
         MVC   JRNDATE(L'JRNDSCSR),JRNDSCSR                                     
         MVC   JRNDATE+L'JRNDSCSR+1(5),=C' - SR'                                
         CURED JTOTDSC,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      DIS/SCH/FEE A/C                              
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,JTOTDSC                                                      
         CLC   DISCULSI,DISC       TEST SI DISCOUNT                             
         BNE   *+12                                                             
         LA    RF,JRNCR            SI - CREDIT                                  
         B     *+14                                                             
         LA    RF,JRNDR            SE/SQ - DEBIT                                
         MP    DUB,=P'-1'          REVERSE SIGN                                 
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         CP    JTOTTRF,PZERO                                                    
         BE    UPDLAS12                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JTOTTRF,(L'JRNDR,JRNDR+1),2,MINUS=YES,BRACKET=Y                  
         LA    R1,JRNDR                                                         
UPDLAS10 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'          REPLACE ( WITH *                             
         CLI   0(R1),C')'                                                       
         BNE   UPDLAS10                                                         
         MVI   0(R1),C'*'          REPLACE ) WITH *                             
*                                                                               
UPDLAS12 LA    R2,L'REPP1(R2)      TOTAL DEBITS AND CREDITS                     
         MVC   JRNDATE(L'JRNTOT),JRNTOT                                         
         CURED JTOTCRS,(L'JRNCR,JRNCR),2,MINUS=YES                              
         CURED JTOTDRS,(L'JRNDR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      BALANCE                                      
         MVC   JRNDATE(L'JRNBAL),JRNBAL                                         
         ZAP   JTOTBAL,JTOTCHQ                                                  
         SP    JTOTBAL,JTOTMRK                                                  
         SP    JTOTBAL,JTOTDIF                                                  
         TM    DISCINDS,DISCIADD   TEST ADDING DISCOUNT TO BALANCE              
         BNZ   *+10                                                             
         AP    JTOTBAL,JTOTDSC                                                  
         CURED JTOTBAL,(L'JRNCR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)      ITEMS                                        
         CURED TOTITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
         CP    JTOTOFS,PZERO       TEST OFFSETS NOT IN BALANCE                  
         BNE   *+14                                                             
         CP    JTOTBAL,PZERO       TEST BALANCE WARNING REQ'D                   
         BE    UPDLAS14                                                         
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         LA    R0,L'JRNITEM+9(RF)                                               
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         MVC   GTMSGNO,=AL2(AS$BATNB)                                           
         GOTO1 VGETTXT,(R1)                                                     
*                                                                               
UPDLAS14 GOTO1 VREPORT,REPD                                                     
*                                                                               
UPDLASTX B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* LAST - RESTORE HEADER SCREEN AFTER UPDATE                           *         
***********************************************************************         
         SPACE 1                                                                
RELAST   DS    0H                                                               
         NI    RECBMONH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBREFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBNKH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECRCVH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWACH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWCPH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECVATH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECDATH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECMOSH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECBNRH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECBSOH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
RELASTX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT TO ROOT                                                        *         
***********************************************************************         
         SPACE 1                                                                
EXIT     XIT1  ,                                                                
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
         NI    TRNSTAT,255-TRNSREV ORIGINAL GOT REVERSED SO UNREVERSE           
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
BLDXFRX  B     EXIT                                                             
         DROP  R2,R3                                                            
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
         LTORG                                                                  
         EJECT                                                                  
BANKULS  DC    C'GB'               VALID LEDGERS FOR BANK                       
         DC    C'GP'                                                            
         DC    C'SC'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
ANALULS  DC    C'SC'               VALID LEDGERS FOR ANAL BANK                  
         DC    AL1(EOT)                                                         
*                                                                               
DISCULS  DC    C'SE'               VALID LEDGERS FOR DISCOUNT                   
DISCULSI DC    C'SI'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
RECVULS  DC    C'SR'               VALID LEDGERS FOR RECEIVABLES                
         DC    AL1(EOT)                                                         
*                                                                               
WOFFULS  DC    C'SE'               VALID LEDGERS FOR WRITE-OFF                  
         DC    C'SI'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
COST2    DC    C'12'               12 COSTING U/L                               
COST3    DC    C'13'               13 COSTING U/L                               
COSTP99  DC    C'1P99'             1P COSTING U/L/DEPT                          
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
*                                                                               
         DCDDL AC#DSFA,15                                                       
         DCDDL AC#XFRD,11                                                       
         DCDDL AC#TDC,11                                                        
         DCDDL AC#BAL,7                                                         
         DCDDL AC#ITEMS,5                                                       
         DCDDL AC#PAYRF,17                                                      
         DCDDL AC#DATED,5                                                       
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
JRNSPEC  DS    0X                  ** SPECS FOR JOURNAL **                      
         SPROG 0,1,2                                                            
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
         SPROG 0                                                                
         SPEC  H1,29,AC#APOS,24                                                 
         SPEC  H2,29,AC#APOS,24,LU                                              
         SPROG 1                                                                
         SPEC  H1,26,AC#DPOS,30                                                 
         SPEC  H2,26,AC#DPOS,30,LU                                              
         SPROG 2                                                                
         SPEC  H1,21,AC#FPOS,39                                                 
         SPEC  H2,21,AC#FPOS,39,LU                                              
JRNSPECX DC    AL1(EOT)            SPEC END MARKER                              
         EJECT                                                                  
OFFTABD  DSECT                     DSECT COVERS OFFICE AMOUNT TABLE             
OFFTOFFC DS    CL(L'TRNOFFC)       OFFICE CODE                                  
OFFTEOTQ EQU   0                   END OF TABLE INDICATOR                       
OFFTAMNT DS    PL(L'TRNAMNT)       AMOUNT FOR BANK POSTING                      
OFFTMAX  EQU   32                  MAXIMUM NUMBER OF ENTRIES IN TABLE           
OFFTABL  EQU   *-OFFTABD           LENGTH OF TABLE ENTRY                        
         SPACE 2                                                                
OVRWRKD  DSECT                     DSECT COVERS OVERLAY TEMPORARY W/S           
*                                                                               
OVRPKWK  DS    PL16                LARGE PACKED WORK AREA                       
OVRWOVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT                     
OVRNARR  DS    CL(L'POSTNARR)      STANDARD NARRATIVE SAVED HERE                
OVRBYTE  DS    XL1                 FLAGGING BYTE                                
NOCWOF   EQU   X'80'               WRITE-OFF COSTING A/C(S) MISSING             
NOCDSC   EQU   X'40'               DISCOUNT COSTING A/C(S) MISSING              
*                                                                               
BANKDATP DS    PL3                 PWOS BANK DEPOSIT DATE                       
*                                                                               
COSTCACN DS    CL(L'RECNAME)       1C COSTING ACCOUNT NAME                      
*                                  WRITE-OFF COSTING ANALYSIS A/CS              
CWOF2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CWOF2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CWOF3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CWOF3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CWOFPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
*                                  DISCOUNT COSTING ANALYSIS A/CS               
CDSC2ACT DS    CL(L'ACTKCULA-1)    12 COSTING UNIT/LEDGER/ACCOUNT               
CDSC2ACN DS    CL(L'RECNAME)       12 COSTING ACCOUNT NAME                      
CDSC3ACT DS    CL(L'ACTKCULA-1)    13 COSTING UNIT/LEDGER/ACCOUNT               
CDSC3ACN DS    CL(L'RECNAME)       13 COSTING ACCOUNT NAME                      
CDSCPACT DS    CL(L'ACTKCULA-1)    1P COSTING UNIT/LEDGER/ACCOUNT               
*                                                                               
JRACCUMS DS    0PL6                ** JOURNAL RECEIVABLE TOTALS **              
JRCVMRK  DS    PL6                 ALLOCATION AMOUNT                            
JRCVWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JRCVOFS  DS    PL6                 OFFSET AMOUNT                                
JRCVDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JRCVDSC  DS    PL6                 DISCOUNT/SURCHARGE AMOUNT                    
JRCVTRF  DS    PL6                 TRANSFER AMOUNT                              
JRCVWVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JRCVDRS  DS    PL6                 DEBITS                                       
JRCVCRS  DS    PL6                 CREDITS                                      
JRACCUMN EQU   (*-JRACCUMS)/L'JRACCUMS                                          
*                                                                               
JTACCUMS DS    0PL6                ** JOURNAL TOTALS **                         
JTOTMRK  DS    PL6                 ALLOCATION AMOUNT                            
JTOTWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JTOTOFS  DS    PL6                 OFFSET AMOUNT                                
JTOTDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JTOTDSC  DS    PL6                 DISCOUNT/SURCHARGE AMOUNT                    
JTOTTRF  DS    PL6                 TRANSFER AMOUNT                              
JTOTWVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JTOTCHQ  DS    PL6                 CHEQUE AMOUNT                                
JTOTDRS  DS    PL6                 DEBITS                                       
JTOTCRS  DS    PL6                 CREDITS                                      
JTOTBAL  DS    PL6                 BALANCE                                      
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
JRNPAYR  DS    CL17                                                             
JRNDTED  DS    CL5                                                              
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
       ++INCLUDE ACRECF5D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREC05   05/01/02'                                      
         END                                                                    
