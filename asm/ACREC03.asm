*          DATA SET ACREC03    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T60803A                                                                  
         SPACE 2                                                                
***********************************************************************         
* DBOW   3     LENGTHEN SOME DICTIONARY REFERENCES                    *         
***********************************************************************         
         SPACE 2                                                                
ACREC03  TITLE '- GERMAN OVERLAY TO HANDLE HEADER SCREEN AND POSTINGS'          
ACREC03  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REC3**,RA                                                    
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
         GOTO1 (RF),(R1),C'LU  ',SINLST,SOUTLST                                 
         MVI   SOURCESX,EOT                                                     
         MVC   BANKLIST,=Y(BANKULS-ACREC03)                                     
         MVC   DISCLIST,=Y(DISCULS-ACREC03)                                     
         MVC   ICPYLIST,=Y(ICPYULS-ACREC03)                                     
         MVC   RECVLIST,=Y(RECVULS-ACREC03)                                     
         MVC   VATLIST,=Y(VATULS-ACREC03)                                       
         MVC   WOFFLIST,=Y(WOFFULS-ACREC03)                                     
         MVC   SHEADTOT,=Y(RECHTOTH-RECOLAYH)                                   
         MVC   SHEADPFK,=Y(RECHPFKH-RECOLAYH)                                   
         MVC   SRECWACT,=Y(RECWACTH-RECOLAYH)                                   
         MVC   SRECWAC,=Y(RECWACH-RECOLAYH)                                     
         MVC   SRECVATT,=Y(RECVATTH-RECOLAYH)                                   
         MVC   SRECVAT,=Y(RECVATH-RECOLAYH)                                     
         MVC   SRECDSCT,=Y(RECDSCTH-RECOLAYH)                                   
         MVC   SRECDSC,=Y(RECDSCH-RECOLAYH)                                     
         MVC   SRECDAMT,=Y(RECDAMTH-RECOLAYH)                                   
         MVC   SRECDAM,=Y(RECDAMH-RECOLAYH)                                     
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
VALBNK   TM    RECBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALBNKX                                                          
         MVI   FVMINL,1                                                         
         GOTO1 AVALBNK,RECBNKH                                                  
         BNE   EXIT                                                             
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',BANKUL),                X        
               (X'C0',RECNDSP),AIO1,(L'BANKNAME,BANKNAME)                       
VALBNKX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK REFERENCE NUNBER (CHEQUE NUMBER)                      *         
***********************************************************************         
         SPACE 1                                                                
VALREF   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECREFH                                                    
         BNE   EXIT                                                             
         MVC   BANKREF,FVIFLD                                                   
VALREFX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATE CHEQUE WAS DEPOSITED                                  *         
***********************************************************************         
         SPACE 1                                                                
VALDEP   MVI   FVMINL,1                                                         
         GOTO1 AVALDEP,RECDEPH                                                  
         BNE   EXIT                                                             
VALDEPX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CHEQUE (CONTROL) AMOUNT                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCHQ   MVI   FVMINL,1                                                         
         GOTO1 AFVAL,RECCHQH                                                    
         BNE   EXIT                                                             
         MVC   FVMSGNO,=AL2(EAAMTINV)                                           
         SR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         GOTO1 VCASHVAL,DMCB,(X'82',RECCHQ),(R0)                                
         CLI   0(R1),0                                                          
         BNE   EXIT                                                             
         ZAP   CHQAMT,4(8,R1)                                                   
         CURED CHQAMT,(L'RECCHQ,RECCHQ),2,ALIGN=LEFT,FLOAT=-                    
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
VALCHQX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECEIVING ACCOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALRCV   TM    RECRCVH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALRCVX                                                          
         GOTO1 AVALRCV,RECRCVH                                                  
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
         CLI   WOFFCOST,C' '       TEST WRITE-OFF A/C COSTING FLAG              
         BH    VALWCP2                                                          
         CLC   DISCULSI,WOFF       TEST WRITE-OFFS TO SI                        
         BE    VALWCP2                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C COSTING FLAG               
         BH    VALWCP2                                                          
         CLC   DISCULSI,DISC       TEST DISCOUNTS TO SI                         
         BNE   *+8                                                              
VALWCP2  MVI   FVMINL,1            WRITE-OFF CLI/PRO REQUIRED                   
         GOTO1 AVALWCP,RECWCPH                                                  
         BH    EXIT                INVALID OR REQUIRED AND NOT INPUT            
VALWCPX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INTERCOMPANY ACCOUNT FOR INTER-OFFICE POSTINGS             *         
***********************************************************************         
         SPACE 1                                                                
VALICO   TM    RECICOH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALICOX                                                          
         GOTO1 AVALICO,RECICOH                                                  
         BH    EXIT                                                             
         BL    VALICOX                                                          
         MVC   WORK,SPACES         IOAREA NOT INTACT                            
         MVC   WORK(L'COMPANY),COMPANY                                          
         MVC   WORK+L'COMPANY(L'ICPY),ICPY                                      
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,0,                            X        
               (X'C0',RECNDSP),WORK,(L'ICPYNAME,ICPYNAME)                       
VALICOX  DS    0H                                                               
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
* VALIDATE DISCOUNT ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   TM    RECDSCH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDSCX                                                          
         GOTO1 AVALDSC,RECDSCH                                                  
         BH    EXIT                                                             
         BL    VALDSCX                                                          
         MVC   FVMSGNO,=AL2(EAACCMLT)                                           
         CLI   RECVCNT,1           NOT ALLOWED FOR MULTI-RECEIVABLES            
         BNE   EXIT                EXIT WITH CC SET NEQ                         
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   RECOFFC,RECVOFF     MUST BE SAME OFFICE, TOO                     
         BNE   EXIT                EXIT WITH CC SET NEQ                         
         CLC   DISCULSI,DISC       IF DISCOUNT A/C IS INCOME                    
         BE    *+12                                                             
         CLI   DISCCOST,C' '       OR DISCOUNT A/C FLAGGED FOR COSTING          
         BNH   VALDSC2                                                          
         CLI   WCPCNT,0            CHECK FOR A PRODUCTION CLI/PRO               
         BNE   VALDSC2                                                          
         MVC   FVMSGNO,=AL2(EAWCPREQ)                                           
         LA    R1,RECWCPH          SET CURSOR TO WRITE-OFF CLI/PRO              
         ST    R1,FVADDR                                                        
         B     VALHEDE             EXIT SETS CC NEQ                             
VALDSC2  GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,0,                            X        
               (X'C0',RECNDSP),AIO1,(L'DISCNAME,DISCNAME)                       
VALDSCX  DS    0H                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DISCOUNT AMOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
VALDAM   MVI   OVRBYTE,0           CLEAR INDICATOR                              
         TM    RECDAMH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALDAMX                                                          
         OC    DISC,DISC                                                        
         BZ    *+8                                                              
         MVI   FVMINL,1                                                         
         GOTO1 AVALAMT,DMCB,RECDAMH,OVRPL6                                      
         BH    EXIT                INVALID OR REQUIRED AND NOT INPUT            
         BL    VALDAMX             NOT REQUIRED AND NOT INPUT                   
         OC    DISC,DISC           IF INPUT TEST ACCOUNT                        
         BZ    VALHEDE             EXIT SETS CC NEQ                             
         ZAP   DISCAMT,OVRPL6                                                   
         MVI   OVRBYTE,CALCVAT     INDICATE CALCULATE VAT THIS TIME             
VALDAMX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE VAT ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
VALVAT   TM    RECVATH+(FVATRB-FVIHDR),FVAPROT                                  
         BO    VALVAT4                                                          
         OC    WOFF,WOFF           IF WRITE-OFF A/C PRESENT                     
         BZ    VALVAT2                                                          
         CLI   PROFWVAT,C'Y'       AND PROFILE TO WRITE-OFF VAT SET             
         BNE   *+8                                                              
         MVI   FVMINL,1            VAT A/C COMPULSORY                           
VALVAT2  GOTO1 AVALVAT,RECVATH                                                  
         BH    EXIT                INVALID OR REQUIRED AND NOT INPUT            
         BL    VALVATX             NOT REQUIRED AND NOT INPUT                   
         MVC   FVMSGNO,=AL2(EAACCMLT)                                           
         CLI   RECVCNT,1           NOT ALLOWED FOR MULTI-RECEIVABLES            
         BNE   EXIT                EXIT WITH CC SET NEQ                         
         MVC   FVMSGNO,=AL2(EAOFFDIF)                                           
         CLC   RECOFFC,RECVOFF     MUST BE SAME OFFICE, TOO                     
         BNE   EXIT                EXIT WITH CC SET NEQ                         
         GOTO1 VACSRCHC,DMCB,FVADDR,TWAD,(C'*',VATUL)                  X        
               (X'C0',RECNDSP),AIO1,(L'VATNAME,VATNAME)                         
         ZAP   VATAMT,PZERO        CLEAR DISCOUNT VAT ADJUSTMENT                
VALVAT4  OC    VAT,VAT             TEST VAT A/C PRESENT                         
         BZ    VALVATX                                                          
         OC    DISC,DISC           TEST DISCOUNT A/C ENTERED                    
         BZ    VALVATX                                                          
         CLI   OVRBYTE,CALCVAT     TEST CALCULATE VAT AMOUNT                    
         BNE   VALVATX                                                          
         NI    TOTPROF,255-PROFTVAT                                             
         TM    PROFTOT,PROFTVAT    TEST VAT TOTAL REQUIRED                      
         BZ    *+8                                                              
         OI    TOTPROF,PROFTVAT    SET VAT TOTAL CONTROL BIT ON                 
         SR    RF,RF                                                            
         ICM   RF,3,VATRATE                                                     
         CVD   RF,DUB                                                           
         AP    DUB,=P'10000'                                                    
         ZAP   OVRPKWK,DISCAMT                                                  
         MP    OVRPKWK,=P'1000000'                                              
         DP    OVRPKWK,DUB                                                      
         SRP   OVRPKWK(L'OVRPKWK-L'DUB),64-2,5                                  
         ZAP   VATAMT,DISCAMT                                                   
         SP    VATAMT,OVRPKWK(L'OVRPKWK-L'DUB)  SAVE DISCOUNT VAT ADJ.          
         SP    DISCAMT,VATAMT      ADJUST DISCOUNT AMOUNT BY VAT AMOUNT         
VALVATX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ALL COSTING ACCCOUNTS WHICH MIGHT BE NEEDED                *         
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
VALCOS4  MVI   FVINDX,0            RESET MULTIPLE FIELD INDEX FOR ERROR         
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
*                                                                               
VALCOS8  CLI   DISCCOST,C' '       NOW TEST DISCOUNT A/C ANALYSIS FLAG          
         BNH   VALCOS14                                                         
*                                                                               
VALCOS10 LA    R1,RECDSCH          SET A(DISCOUNT A/C FIELD)                    
         ST    R1,FVADDR                                                        
         CLC   DISCULSI,DISC       TEST INCOME COSTING ANALYSIS                 
         BNE   VALCOS12                                                         
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
RESHED   DS    0H                  PROTECT SOME FIELDS                          
         OI    RECBMONH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECBREFH+(FVATRB-FVIHDR),FVAPROT                                 
         OI    RECBNKH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECRCVH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OC    WOFF,WOFF           PROT WRITE-OFF A/C IF PRESENT                
         BZ    *+8                                                              
         OI    RECWACH+(FVATRB-FVIHDR),FVAPROT                                  
         CLI   WCPCNT,0            PROT WRITE-OFF CLI/PRO IF PRESENT            
         BE    *+8                                                              
         OI    RECWCPH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OI    RECICOH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OC    DISC,DISC          PROT DISCOUNT A/C & AMOUNT IF PRESENT         
         BNZ   *+12                                                             
         CLI   RECVCNT,1          OR IF MULTIPLE RECEIVABLE A/CS                
         BNH   *+12                                                             
         OI    RECDSCH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    RECDAMH+(FVATRB-FVIHDR),FVAPROT                                  
*                                                                               
         OC    VAT,VAT             PROT VAT A/C IF PRESENT                      
         BZ    *+8                                                              
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
UPDFRST  ZAP   ICPYAMT,CHQAMT      INTERCOMPANY=CHEQUE                          
         GOTO1 VDATCON,DMCB,(0,BANKDATE),(1,BANKDATP)                           
         MVC   OVRNARR,SPACES                                                   
         LA    R2,OVRNARR                                                       
         MVC   0(L'TXTREF,R2),TXTREF  KONTOAUSZUGS-NR                           
         LA    R2,L'TXTREF+1(R2)                                                
         MVC   0(L'BANKREF,R2),BANKREF                                          
         LA    R2,L'BANKREF-1(R2)                                               
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,2(R2)                                                         
         MVC   0(L'TXTDAT,R2),TXTDAT  AUSZUGSDATUM                              
         LA    R2,L'TXTDAT+1(R2)                                                
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
         LA    RF,JRCVMRK          ADDRESS ALLOCATION TOTALS                    
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
UPDPRO4  GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         MVC   POSTNARR,OVRNARR    RESET STANDARD NARRATIVE                     
         TM    TSARINDS,TSARITRF   TEST TRANSFER POSTING                        
         BNZ   UPDPRO30            PRODUCE TRANSFER POSTING                     
         B     UPDPROCX            EXIT                                         
*                                                                               
UPDPRO5  GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         TM    TSARINDS,TSARIWOF   IS THIS A WRITE-OFF POSTING?                 
         BZ    UPDPROCX                                                         
         MVC   POSTACT,WOFF        BUILD WRITE-OFF/RECEIVABLE POSTING           
         MVC   POSTACTN,WOFFNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         ZAP   POSTAMNT,TSARPOST                                                
         ZAP   OVRWOVAT,PZERO      CLEAR WRITE-OFF VAT ADJUSTMENT               
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
         MVC   POSTDATE,BANKDATP   BANK DEPOSIT DATE                            
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
UPDPRO8  MVC   POSTCAC,RECVPCP     YES - SET CLIENT/PRODUCT CONTRA              
         MVC   POSTCACN,RECVPCPN                                                
         MVI   POSTSTAT,TRNSAUTH   CREDIT AUTH'D                                
         MP    POSTAMNT,=P'-1'     REVERSE POSTING SIGN                         
UPDPRO10 GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFFS                           
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
         MVC   POSTDATE,BANKDATP   BANK DEPOSIT DATE                            
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
         OC    VAT,VAT             TEST VAT A/C PRESENT                         
         BZ    UPDPROCX                                                         
         CP    OVRWOVAT,PZERO      IF NON-ZERO AMOUNT                           
         BE    UPDPROCX                                                         
         MVC   POSTACT,VAT         BUILD VAT/RECEIVABLE POSTING                 
         MVC   POSTACTN,VATNAME                                                 
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVI   POSTSTAT,0          CREDIT                                       
         ZAP   POSTAMNT,OVRWOVAT                                                
         MP    POSTAMNT,=P'-1'     MINUS                                        
         LA    R1,ELEXTRA                                                       
         USING SCIELD,R1                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGLEV    TYPE N NET BILLING (GROSS LESS VAT)          
         ZAP   SCIAMNT,TSARPOST                                                 
         SP    SCIAMNT,OVRWOVAT                                                 
         MP    SCIAMNT,=P'-1'                                                   
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
         B     UPDPROCX                                                         
*                                                                               
UPDPRO30 BAS   RE,BLDXFR           BUILD TRANSFER POSTING                       
         LH    R1,=H'-1'           SET RECORD ALREADY BUILT IN IO               
         GOTO1 ABLDTRN                                                          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDPROCX B     EXIT                                                             
         EJECT                                                                  
UPDRCVL  OC    ICPY,ICPY           TEST INTERCOMPANY ACCOUNT PRESENT            
         BZ    UPDRCL1                                                          
         CP    ICPYAMT,PZERO       TEST ANY INTERCOMPANY TO POST                
         BE    UPDRCL1                                                          
         MVC   POSTACT,ICPY        BUILD INTERCPY/RECEIVABLE POSTING            
         MVC   POSTACTN,ICPYNAME                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         ZAP   POSTAMNT,RECVAMT    TOTAL ALLOCATED AGAINST CHEQUE               
         SP    POSTAMNT,DISCAMT    MINUS DISCOUNT                               
         SP    POSTAMNT,VATAMT     MINUS VAT                                    
         MVC   POSTOFFC,RECVOFF    RECEIVABLE A/C OFFICE                        
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                  BUILD BANK/RECEIVABLE POSTING                
UPDRCL1  ZAP   POSTAMNT,RECVAMT    TOTAL ALLOCATED AGAINST CHEQUE               
         SP    POSTAMNT,DISCAMT    MINUS DISCOUNT                               
         SP    POSTAMNT,VATAMT     MINUS VAT                                    
         CP    POSTAMNT,PZERO                                                   
         BE    UPDRCL2             NOTHING TO POST                              
         MVC   POSTACT,BANK                                                     
         MVC   POSTACTN,BANKNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,TRNSDR+TRNSAUTH                                         
         MVC   POSTOFFC,BANKOFF                                                 
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDRCL2  OC    DISC,DISC           TEST DISCOUNT ACCOUNT PRESENT                
         BZ    UPDRCL8                                                          
         CP    DISCAMT,PZERO       TEST ANY POSTING TO MAKE                     
         BE    UPDRCL8                                                          
         MVC   POSTACT,DISC        BUILD DISCOUNT/RECEIVABLE POSTING            
         MVC   POSTACTN,DISCNAME                                                
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         ZAP   POSTAMNT,DISCAMT                                                 
         MVC   POSTOFFC,RECVOFF    RECEIVABLE A/C OFFICE                        
         MVI   POSTSTAT,TRNSDR     DEBIT                                        
         CLC   DISCULSI,DISC       TEST IF SI DISCOUNT                          
         BE    UPDRCL4                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDRCL6             STANDARD POSTING TO SE/SQ                    
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDRCL6             STANDARD POSTING TO SE/SQ                    
         GOTO1 ABLDTRN,DMCB,(0,RECVCOST),('TRNSDR',CDSCPACT),(X'FF',0)          
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         MVC   POSTACT,RECVCOST    SE - BUILD 1C/13 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CDSC3ACT),CDSC3ACT                           
         MVC   POSTCACN,CDSC3ACN                                                
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
         B     UPDRCL8                                                          
*                                                                               
UPDRCL4  MVI   POSTSTAT,0          CREDIT                                       
         MP    POSTAMNT,=P'-1'     MINUS                                        
UPDRCL6  GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
         CLC   DISCULSI,DISC       TEST IF SI DISCOUNT                          
         BNE   UPDRCL8                                                          
         CLI   DISCCOST,C' '       TEST DISCOUNT A/C ANALYSIS FLAG              
         BNH   UPDRCL8                                                          
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BNZ   UPDRCL8                                                          
*                                                                               
         MVC   POSTACT,RECVCOST    SI - BUILD 1C/12 COSTING POSTING             
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'CDSC2ACT),CDSC2ACT                           
         MVC   POSTCACN,CDSC2ACN                                                
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
*                                                                               
UPDRCL8  OC    VAT,VAT             TEST VAT ACCOUNT PRESENT                     
         BZ    UPDRCL10                                                         
         CP    VATAMT,PZERO        TEST ANY POSTING TO MAKE                     
         BE    UPDRCL10                                                         
         MVC   POSTACT,VAT         BUILD VAT/RECEIVABLE POSTING                 
         MVC   POSTACTN,VATNAME                                                 
         MVC   POSTCAC,RECVACT                                                  
         MVC   POSTCACN,RECVACTN                                                
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         MVI   POSTSTAT,0          CREDIT                                       
         ZAP   POSTAMNT,VATAMT                                                  
         MP    POSTAMNT,=P'-1'     MINUS                                        
         MVC   POSTOFFC,RECVOFF                                                 
         LA    R1,ELEXTRA                                                       
         USING SCIELD,R1                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITGLEV    TYPE N NET BILLING (DISC LESS VAT)           
         ZAP   SCIAMNT,DISCAMT                                                  
         MP    SCIAMNT,=P'-1'                                                   
         GOTO1 ABLDTRN,0                                                        
         AP    RCVITEM,=P'1'                                                    
*                                                                               
UPDRCL10 OC    PRTSUB,PRTSUB                                                    
         BZ    UPDRCVLX                                                         
         CLI   RECVCNT,1           TEST MULTIPLE RECEIVABLE A/CS                
         BE    UPDRCVLX                                                         
*                                                                               
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNDATE(42),=42C'-'                                              
         MVC   JRNDATE+13(L'JRNRECV),JRNRECV                                    
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         CURED JRCVMRK,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JRCVWOF                                                      
         LA    RF,JRNDR                                                         
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+14                                                             
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         CP    JRCVOFS,PZERO                                                    
         BE    UPDRCL12                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         CURED JRCVOFS,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
UPDRCL12 LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JRCVDIF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         CP    JRCVTRF,PZERO                                                    
         BE    UPDRCL16                                                         
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JRCVTRF,(L'JRNDR,JRNDR),2,MINUS=YES,BRACKET=YES                  
         LA    R1,JRNDR                                                         
UPDRCL14 LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'                                                       
         CLI   0(R1),C')'                                                       
         BNE   UPDRCL14                                                         
         MVI   0(R1),C'*'                                                       
*                                                                               
UPDRCL16 LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,DISCAMT                                                      
         LA    RF,JRNDR                                                         
         CLC   DISCULSI,DISC       TEST SI DISCOUNT                             
         BNE   *+14                                                             
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNVAT),JRNVAT                                         
         ZAP   DUB,VATAMT                                                       
         MP    DUB,=P'-1'                                                       
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNWVAT),JRNWVAT                                       
         ZAP   DUB,JRCVWVAT                                                     
         MP    DUB,=P'-1'                                                       
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         CURED RCVITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         LA    R0,JRNDATE                                                       
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BZ    UPDRCL18                                                         
*                                                                               
         MVC   GTMSGNO,=AL2(AS$CSTAM)                                           
         GOTO1 VGETTXT,(R1)                                                     
         LA    R2,L'REPP1(R2)                                                   
*                                                                               
UPDRCL18 TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BZ    UPDRCL20                                                         
         MVC   GTMSGNO,=AL2(AS$WRTCM)                                           
         GOTO1 (RF),(R1)                                                        
*                                                                               
UPDRCL20 GOTO1 VREPORT,REPD                                                     
         OI    REPHEADI,REPHFRCE   SKIP TO NEW PAGE NEXT TIME                   
*                                                                               
UPDRCVLX AP    TOTITEM,RCVITEM     ADD RECEIVABLE ITEMS TO TOTAL                
         B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
UPDLAST  MVC   REPH4,SPACES                                                     
         OC    ICPY,ICPY           TEST INTERCOMPANY ACCOUNT PRESENT            
         BZ    UPDLAS2                                                          
         CP    ICPYAMT,PZERO       TEST ANY POSTING TO MAKE                     
         BE    UPDLAS2                                                          
         MVC   POSTACT,ICP2        BUILD INTERCP2/BANK POSTING                  
         MVC   POSTACTN,ICP2NAME                                                
         MVC   POSTCAC(L'COMPANY),COMPANY                                       
         MVC   POSTCAC+L'COMPANY(L'BANK),BANK                                   
         MVC   POSTCACN,BANKNAME                                                
         MVI   POSTSTAT,0                                                       
         MVC   POSTDATE,BANKDATP                                                
         MVC   POSTREF,BANKREF                                                  
         ZAP   POSTAMNT,ICPYAMT                                                 
         MVC   POSTOFFC,BANKOFF                                                 
         GOTO1 ABLDTRN,0                                                        
         AP    TOTITEM,=P'1'                                                    
*                                                                               
UPDLAS2  DS    0H                                                               
         OC    PRTSUB,PRTSUB                                                    
         BZ    UPDLASTX                                                         
         LA    R2,REPP2                                                         
         USING JRNLINED,R2                                                      
         MVC   JRNDATE(42),=42C'-'                                              
         MVC   JRNDATE+13(L'JRNBTOT),JRNBTOT                                    
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNMRK),JRNMRK                                         
         CURED JTOTMRK,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNWOF),JRNWOF                                         
         ZAP   DUB,JTOTWOF                                                      
         LA    RF,JRNDR                                                         
         CLC   DISCULSI,WOFF       TEST SI WRITE-OFF                            
         BNE   *+14                                                             
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         CP    JTOTOFS,PZERO                                                    
         BE    UPDLAS4                                                          
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNOFS),JRNOFS                                         
         CURED JTOTOFS,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
UPDLAS4  LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNDIF),JRNDIF                                         
         CURED JTOTDIF,(L'JRNCR,JRNCR),2,MINUS=YES                              
*                                                                               
         CP    JTOTTRF,PZERO                                                    
         BE    UPDLAS8                                                          
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNTRF),JRNTRF                                         
         CURED JTOTTRF,(L'JRNDR,JRNDR),2,MINUS=YES,BRACKET=YES                  
         LA    R1,JRNDR                                                         
UPDLAS6  LA    R1,1(R1)                                                         
         CLI   0(R1),C'('                                                       
         BNE   *+8                                                              
         MVI   0(R1),C'*'                                                       
         CLI   0(R1),C')'                                                       
         BNE   UPDLAS6                                                          
         MVI   0(R1),C'*'                                                       
*                                                                               
UPDLAS8  LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNDSC),JRNDSC                                         
         ZAP   DUB,DISCAMT                                                      
         LA    RF,JRNDR                                                         
         CLC   DISCULSI,DISC       TEST SI DISCOUNT                             
         BNE   *+14                                                             
         LA    RF,JRNCR            CREDIT                                       
         MP    DUB,=P'-1'          MINUS                                        
         CURED DUB,(L'JRNCR,(RF)),2,MINUS=YES                                   
*                                                                               
         LA    R2,L'REPP1(R2)      DISCOUNT VAT ADJ.                            
         MVC   JRNDATE(L'JRNVAT),JRNVAT                                         
         ZAP   DUB,VATAMT                                                       
         MP    DUB,=P'-1'                                                       
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
*                                                                               
         LA    R2,L'REPP1(R2)      WRITTEN-OFF VAT ADJ.                         
         MVC   JRNDATE(L'JRNWVAT),JRNWVAT                                       
         ZAP   DUB,JTOTWVAT                                                     
         MP    DUB,=P'-1'                                                       
         CURED DUB,(L'JRNCR,JRNCR),2,MINUS=YES                                  
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNCHQ),JRNCHQ                                         
         ZAP   JTOTCHQ,CHQAMT                                                   
         CURED JTOTCHQ,(L'JRNCR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         MVC   JRNDATE(L'JRNBAL),JRNBAL                                         
         ZAP   JTOTBAL,JTOTCHQ                                                  
         SP    JTOTBAL,JTOTMRK                                                  
         SP    JTOTBAL,JTOTDIF                                                  
         AP    JTOTBAL,DISCAMT                                                  
         AP    JTOTBAL,VATAMT      INCLUDE DISCOUNT VAT ADJ.                    
         CURED JTOTBAL,(L'JRNCR,JRNDR),2,MINUS=YES                              
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         CURED TOTITEM,(8,JRNDATE),0,ALIGN=LEFT                                 
         LA    RF,JRNDATE                                                       
         AR    RF,R0                                                            
         MVC   1(L'JRNITEM,RF),JRNITEM                                          
*                                                                               
         LA    R2,L'REPP1(R2)                                                   
         CLI   RECVCNT,1           TEST MULTIPLE RECEIVABLE A/CS                
         BNE   UPDLAS12                                                         
         LA    R1,DMCB                                                          
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,JRNMSGL      MAXIMUM LENGTH                               
         LA    R0,JRNDATE                                                       
         STCM  R0,7,GTAOUT         A(OUTPUT)                                    
         MVI   GTMTYP,GTMSCR                                                    
         MVI   GT1INDS,GT1NOREF+GT1OWRK                                         
         TM    OVRBYTE,NOCWOF      TEST WRITE-OFF COSTING POSTINGS OK           
         BZ    UPDLAS10                                                         
*                                                                               
         MVC   GTMSGNO,=AL2(AS$CSTAM)                                           
         GOTO1 VGETTXT,(R1)                                                     
*                                                                               
UPDLAS10 LA    R2,L'REPP1(R2)                                                   
         TM    OVRBYTE,NOCDSC      TEST DISCOUNT COSTING POSTINGS OK            
         BZ    UPDLAS12                                                         
         MVC   GTMSGNO,=AL2(AS$CSTAM)                                           
         GOTO1 VGETTXT,(R1)                                                     
         LA    R2,L'REPP1(R2)                                                   
*                                                                               
UPDLAS12 CP    JTOTOFS,PZERO       TEST OFFSETS NOT IN BALANCE                  
         BNE   *+14                                                             
         CP    JTOTBAL,PZERO       TEST BALANCE WARNING REQ'D                   
         BE    UPDLAS14                                                         
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
         XC    RECOPT,RECOPT                                                    
         OI    RECOPTH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    RECBMONH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBREFH+(FVATRB-FVIHDR),X'FF'-FVAPROT                           
         NI    RECBNKH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         XC    RECCHQ,RECCHQ                                                    
         OI    RECCHQH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    RECRCVH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWACH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECWCPH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECICOH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         NI    RECDSCH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         XC    RECDSC,RECDSC                                                    
         OI    RECDSCH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    RECDAMH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         XC    RECDAM,RECDAM                                                    
         OI    RECDAMH+(FVOIND-FVIHDR),FVOXMT                                   
         NI    RECVATH+(FVATRB-FVIHDR),X'FF'-FVAPROT                            
         XC    RECVAT,RECVAT                                                    
         OI    RECVATH+(FVOIND-FVIHDR),FVOXMT                                   
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
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BANKULS  DC    C'GB'               VALID LEDGERS FOR BANK                       
         DC    C'GP'                                                            
         DC    C'SC'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
DISCULS  DC    C'SE'               VALID LEDGERS FOR DISCOUNT                   
DISCULSI DC    C'SI'                                                            
         DC    C'SQ'                                                            
         DC    AL1(EOT)                                                         
*                                                                               
ICPYULS  DC    C'SQ'               VALID LEDGERS FOR INTERCOMPANY               
         DC    AL1(EOT)                                                         
*                                                                               
RECVULS  DC    C'SR'               VALID LEDGERS FOR RECEIVABLES                
         DC    AL1(EOT)                                                         
*                                                                               
VATULS   DC    C'SG'               VALID LEDGERS FOR VAT                        
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
*                                                                               
SINLST   DCDDL AC#UAPL,12          ** BILLING SOURCES INPUT LIST **             
         DCDDL AC#OTHRS,12                                                      
         DCDDL AC#SRCRG,12                                                      
         DCDDL AC#OVPNT,12                                                      
SINLSTX  DC    AL1(EOT)                                                         
*                                                                               
TINLST   DS    0C                  ** TOTALS HEADINGS INPUT LIST **             
         DCDDL AC#BAL,12                                                        
         DCDDL AC#PAYAM,12                                                      
         DCDDL AC#ALCT2,12                                                      
         DCDDL AC#SPCL,12                                                       
         DCDDL AC#DISS,12                                                       
         DCDDL AC#WRTFS,12                                                      
         DCDDL AC#CTRD,12                                                       
         DCDDL AC#VATAM,12                                                      
TINLSTX  DC    AL1(EOT)                                                         
*                                                                               
DINLST   DS    0C                  ** DICTIONARY INPUT LIST **                  
         DCDDL AC#BNKSN,15                                                      
         DCDDL AC#DPSDT,12                                                      
         DCDDL AC#CTRD,12                                                       
         DCDDL AC#XFRFR,13                                                      
         DCDDL AC#XFRTO,13                                                      
         DCDDL AC#ON,3                                                          
         DCDDL AC#RCVA,16                                                       
         DCDDL AC#BNKA,9                                                        
         DCDDL AC#NRTV,4                                                        
         DCDDL AC#BATTO,11                                                      
         DCDDL AC#ALCTD,9                                                       
         DCDDL AC#WRTF,10                                                       
         DCDDL AC#CTRD,12                                                       
         DCDDL AC#SPCL,7                                                        
         DCDDL AC#XFRD,9                                                        
         DCDDL AC#VATAM,11                                                      
         DCDDL AC#WRTOV,17                                                      
         DCDDL AC#DISS,8                                                        
         DCDDL AC#PAYAM,14                                                      
         DCDDL AC#BAL,7                                                         
         DCDDL AC#ITEMS,10                                                      
*                                                                               
DINLSTX  DC    AL1(EOT)                                                         
         EJECT                                                                  
JRNSPEC  DS    0X                  ** SPECS FOR JOURNAL **                      
         SPROG 0,1,2                                                            
         SPEC  H1,1,RUN                                                         
         SPEC  H1,71,PAGE                                                       
         SPEC  H8,1,AC#ACC,8                                                    
         SPEC  H9,1,AC#ACC,8,LU                                                 
         SPEC  H8,16,AC#CTRA,10                                                 
         SPEC  H9,16,AC#CTRA,10,LU                                              
         SPEC  H8,32,AC#OFF,3                                                   
         SPEC  H9,32,AC#OFF,3,LU                                                
         SPEC  H8,36,AC#DATE,5                                                  
         SPEC  H9,36,AC#DATE,5,LU                                               
         SPEC  H8,45,AC#REFN,6                                                  
         SPEC  H9,45,AC#REFN,6,LU                                               
         SPEC  H8,52,AC#STT,3                                                   
         SPEC  H9,52,AC#STT,3,LU                                                
         SPEC  H8,61,AC#DRS,5,R                                                 
         SPEC  H9,61,AC#DRS,5,RU                                                
         SPEC  H8,72,AC#CRS,6,R                                                 
         SPEC  H9,72,AC#CRS,6,RU                                                
         SPROG 0                                                                
         SPEC  H1,32,AC#APOS,18                                                 
         SPEC  H2,32,AC#APOS,18,LU                                              
         SPROG 1                                                                
         SPEC  H1,27,AC#DPOS,28                                                 
         SPEC  H2,27,AC#DPOS,28,LU                                              
         SPROG 2                                                                
         SPEC  H1,23,AC#DPOS,39                                                 
         SPEC  H2,23,AC#DPOS,39,LU                                              
JRNSPECX DC    AL1(EOT)            SPEC END MARKER                              
         EJECT                                                                  
OVRWRKD  DSECT                     DSECT COVERS OVERLAY TEMPORARY W/S           
*                                                                               
OVRPL6   DS    PL6                 GENERAL PL6                                  
OVRPKWK  DS    PL16                LARGE PACKED WORK AREA                       
OVRWOVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT                     
OVRNARR  DS    CL(L'POSTNARR)      STANDARD NARRATIVE SAVED HERE                
OVRBYTE  DS    XL1                 FLAGGING BYTE                                
CALCVAT  EQU   X'01'               CALCULATE VAT AMOUNT                         
NOCWOF   EQU   X'80'               WRITE-OFF COSTING A/C(S) MISSING             
NOCDSC   EQU   X'40'               DISCOUNT COSTING A/C(S) MISSING              
*                                                                               
BANKDATP DS    XL3                 PWOS DATE FOR COSTING POSTINGS               
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
JRCVTRF  DS    PL6                 TRANSFER AMOUNT                              
JRCVWVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JRACCUMN EQU   (*-JRACCUMS)/L'JRACCUMS                                          
*                                                                               
JTACCUMS DS    0PL6                ** JOURNAL TOTALS **                         
JTOTMRK  DS    PL6                 ALLOCATION AMOUNT                            
JTOTWOF  DS    PL6                 WRITE-OFF AMOUNT                             
JTOTOFS  DS    PL6                 OFFSET AMOUNT                                
JTOTDIF  DS    PL6                 SPECIAL SCREEN AMOUNT                        
JTOTTRF  DS    PL6                 TRANSFER AMOUNT                              
JTOTWVAT DS    PL6                 WRITE-OFF VAT ADJUSTMENT AMOUNT              
JTOTCHQ  DS    PL6                 CHEQUE AMOUNT                                
JTOTBAL  DS    PL6                 BALANCE                                      
JTACCUMN EQU   (*-JTACCUMS)/L'JTACCUMS                                          
*                                                                               
RCVITEM  DS    PL4                 RECEIVABLE ITEM COUNT                        
TOTITEM  DS    PL4                 TOTAL ITEM COUNT                             
*                                                                               
ELEXTRA  DS    XL200               AREA FOR EXTRA ELEMENTS                      
         EJECT                                                                  
DOUTLST  DS    0C                  ** DICTIONARY OUTPUT LIST **                 
TXTREF   DS    CL15                                                             
TXTDAT   DS    CL12                                                             
TXTOFS   DS    CL12                                                             
*                                                                               
TXTTRFFR DS    CL13                                                             
TXTTRFTO DS    CL13                                                             
TXTON    DS    CL3                                                              
         EJECT                                                                  
JRNRECV  DS    CL16                                                             
JRNBANK  DS    CL9                                                              
JRNNARR  DS    CL4                                                              
JRNBTOT  DS    CL11                                                             
JRNMRK   DS    CL9                                                              
JRNWOF   DS    CL10                                                             
JRNOFS   DS    CL12                                                             
JRNDIF   DS    CL7                                                              
JRNTRF   DS    CL9                                                              
JRNVAT   DS    CL11                                                             
JRNWVAT  DS    CL17                                                             
JRNDSC   DS    CL8                                                              
JRNCHQ   DS    CL14                                                             
JRNBAL   DS    CL7                                                              
JRNITEM  DS    CL10                                                             
         SPACE 2                                                                
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
       ++INCLUDE ACRECF3D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREC03   05/01/02'                                      
         END                                                                    
