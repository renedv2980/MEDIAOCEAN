*          DATA SET ACREPRU02  AT LEVEL 010 AS OF 05/01/02                      
*PHASE ACRU02A,+0                                                               
*INCLUDE DATVAL                                                                 
*INCLUDE CHOPPER                                                                
         TITLE 'CREATE POSTING FILE FROM CLIENT RECEIVABLE TAPE'                
         PRINT NOGEN                                                            
ACRU02   CSECT                                                                  
         NMOD1 0,**ACRU**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACRUD,RC                                                         
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         L     R5,ADBXAREA                                                      
         USING BOXD,R5                                                          
         MVC   BOXWIDTH,=F'198'                                                 
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQF100                                                          
         RELOC RELO                                                             
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX                                                         
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         ST    R2,ABXHOOK                                                       
         L     R2,=A(BOXMID)                                                    
         ST    R2,ABOXMID                                                       
         L     R2,=A(BOXBOT)                                                    
         ST    R2,ABOXBOT                                                       
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)                                                         
         BE    XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        AT REQUEST FIRST INITIALIZE COUNTS AND SET UP WORKER FILE  *           
*                                                                   *           
*-------------------------------------------------------------------*           
REQF100  CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
*                                                                               
         MVC   SAVOPT1,QOPT1                                                    
         MVI   BADTAGN,0                INITIALIZE BAD REC INDICATOR            
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                     SET UP HEADERS                    
         GOTO1 ABXHOOK                                                          
*                                                                               
         LA    R2,RECTAB                      INITIALIZE RECORD                 
         LA    R3,RECTNUM                     COUNTERS                          
RECINIT  ZAP   0(L'RECIN,R2),=P'0'                                              
         LA    R2,L'RECIN(R2)                                                   
         BCT   R3,RECINIT                                                       
*                                                                               
         LA    R2,CASHTAB                     INITIALIZE CASH                   
         LA    R3,CASHTNUM                    ACCUMULATORS                      
CASHINIT ZAP   0(L'CSHIN,R2),=P'0'                                              
         LA    R2,L'CSHIN(R2)                                                   
         BCT   R3,CASHINIT                                                      
*                                                                               
*              SET UP WORKER FILE                                               
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'ARU'                WORKER FILE ID                    
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
*        OI    ID+12,X'01'                    ALLOW DUPLICATE KEYS              
         MVC   COMMAND,=CL6'OPEN'                                               
         BAS   RE,FILE                        OPEN POSTING FILE                 
         OPEN  (TAPE,(INPUT))                 OPEN TAPE                         
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              READ AND VALIDATE INPUT RECORDS                      *           
*                                                                   *           
*-------------------------------------------------------------------*           
TAPVALI  DS    0H                                                               
         XC    TRECORD(L'TRECORD),TRECORD                                       
         GET   TAPE,TRECORD                                                     
         BAS   RE,AGENCHK          CHECK AGENCY IN JCL AGAINST THAT             
         TM    BADTAGN,BADAGEN     PASSED ON TAPE, IF NO MATCH PRINT            
         BNO   TAPV100             ERROR MESSAGE, CLOSE FILE, AND EXIT          
         MVI   FORCEHED,C'Y'                                                    
         MVI   SPACING,2                                                        
         MVC   XP+2(L'AGCERR1),AGCERR1                                          
         MVC   XPSECOND+2(L'AGCERR2),AGCERR2                                    
         BAS   RE,REPORT                                                        
         B     EOJ                                                              
*                                                                               
TAPV100  AP    RECIN,=P'1'              COUNT NUM OF RECS READ IN               
         MVI   ERRSW1,0                 INITIALIZE ERROR SWITCH                 
         MVI   ERRSW2,0                 INITIALIZE ERROR SWITCH                 
         MVI   ERRSW3,0                 INITIALIZE ERROR SWITCH                 
*                                                                               
         LA    R2,PSTWORK               CLEAR POSTING WORK SPACE                
         LA    R3,PSTWKEND                                                      
PSTCLR   MVI   0(R2),0                                                          
         LA    R2,1(R2)                                                         
         BCT   R3,PSTCLR                                                        
*                                                                               
         CLI   TRECORD+2,C'R'           IS IT RECEIVABLE DEBIT                  
         BNE   TAPV150                                                          
         AP    RECINR,=P'1'             COUNT NUM OF RECS READ IN               
         BAS   RE,TAPDEBT               DEBIT ROURTINE                          
         B     TAPVALI                  GET NEXT RECORD                         
*                                                                               
TAPV150  CLI   TRECORD+2,C'C'           THEN MUST BE RECEIVABLE CREDIT          
         BNE   TAPV200                  MUST BE ONE OR THE OTHER                
         AP    RECINC,=P'1'             COUNT NUM OF RECS READ IN               
         BAS   RE,TAPCRED               CREDIT ROUTINE                          
         B     TAPVALI                  GET NEXT RECORD                         
*                                                                               
TAPV200  DS    0H                                                               
         OI    ERRSW1,BADRTYP                                                   
         BAS   RE,ERPRT                                                         
         B     TAPVALI                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS RECEIVABLE DEBITS                                  *           
*                                                                   *           
*-------------------------------------------------------------------*           
TAPDEBT  NTR1                                                                   
         CLC   TRINVNM,=C'000000'                                               
         BE    TAPD115                                                          
         CLC   TRINVNM,SPACES                                                   
         BH    *+8                                                              
TAPD115  OI    ERRSW1,MISINVN                                                   
*                                                                               
         MVC   WORK,SPACES         SHIFT INVOICE NUM TO THE LEFT                
         LA    R1,6                AND PAD WITH SPACES                          
         LA    R2,TRINVNM                                                       
TAPD120  CLI   0(R2),C' '                                                       
         BH    TAPD130                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,TAPD120                                                       
         B     TAPD140                                                          
TAPD130  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         MVC   PSTINVNM,WORK                                                    
*                                                                               
TAPD140  DS    0H                                                               
         LA    R2,TRINVDT               R2=INPUT DATE                           
         LA    R3,PSTINVDT              R3=RESULT FIELD                         
         MVI   BYTEV,0                  INPUT IS M/D/Y                          
         MVI   BYTE,1                   OUTPUT IS PWOS                          
         BAS   RE,VALDTE                VALIDATE DATE                           
         BE    TAPD142                                                          
         OI    ERRSW1,INVINVD                                                   
         B     TAPD150                                                          
TAPD142  GOTO1 DATCON,DMCB,(1,PSTINVDT),(5,INVDPRT)                             
*                                                                               
TAPD150  CLC   TRCLI,SPACES                                                     
         BH    *+8                                                              
         OI    ERRSW1,INVCLI                                                    
         MVC   PSTCLI,TRCLI                                                     
*                                                                               
         CLC   TRPRD,SPACES                                                     
         BH    *+8                                                              
         OI    ERRSW1,INVPRD                                                    
         MVC   PSTPROD,TRPRD                                                    
*                                                                               
         BAS   RE,CLIPRD                READ CLI/PRD REC                        
*                                                                               
         ZAP   BILLAMT,=P'0'                                                    
         OC    TRBLAMT,TRBLAMT                                                  
         BNZ   TAPD172                                                          
         OI    ERRSW2,MISBAMT                                                   
         B     TAPD174                                                          
TAPD172  PACK  BILLAMT,TRBLAMT          BILL (TRAN) AMT                         
         AP    BILIN,BILLAMT                                                    
         AP    CSHIN,BILLAMT                                                    
*                                                                               
TAPD174  ZAP   COMMAMT,=P'0'                                                    
         OC    TRCMAMT,TRCMAMT                                                  
         BZ    TAPD176                                                          
         PACK  COMMAMT,TRCMAMT          COMMISSION AMT                          
         AP    COMMIN,COMMAMT                                                   
         AP    CSHIN,COMMAMT                                                    
*                                                                               
TAPD176  ZAP   CDAMT,=P'0'                                                      
         OC    TRCD,TRCD                                                        
         BZ    TAPD178                                                          
         PACK  CDAMT,TRCD               CASH DISCOUNT                           
*                                                                               
TAPD178  ZAP   NETAMT,BILLAMT                                                   
         SP    NETAMT,COMMAMT                                                   
*                                                                               
         MVC   BSOURCE,SPACES                                                   
         MVC   SIACC,SPACES                                                     
         MVC   SIACCNM,SPACES                                                   
         OC    TRINUL(L'TRINUL+L'TRINACT),SPACES                                
***                                                                             
***      TEMPORARY FIX FOR TAPE ERROR                                           
         CLC   TRINACT(4),=C'UNAP'                                              
         BE    TAPD179                                                          
         B     TAPD180                                                          
***                                                                             
         CLC   TRINUL,SPACES                                                    
         BH    TAPD180                                                          
TAPD179  MVC   BSOURCE,TRINACT                                                  
         B     TAPD200                                                          
*                                                                               
TAPD180  DS    0H                                                               
         CLC   TRINACT,SPACES                                                   
         BNH   TAPD185                                                          
         LA    R4,IO                                                            
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
***      MVC   ACKEYACC+1(L'TRINUL),TRINUL                                      
         MVC   ACKEYACC+1(2),=C'SI'                                             
         MVC   ACKEYACC+3(L'TRINACT),TRINACT                                    
         MVC   SIACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID INCOME ACCT              
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
TAPD185  OI    ERRSW1,INVIACC           INVALID ACCOUNT                         
         B     TAPD200                                                          
*                                                                               
         LA    R4,IO                    GET ACCOUNT NAME                        
         BAS   RE,GETNME                                                        
         MVC   SIACCNM,WORK                                                     
         BAS   RE,CHOPIT                                                        
         MVC   BSOURCE(L'CHOPOUTA),CHOPOUTA                                     
*                                                                               
TAPD200  DS    0H                                                               
         CLC   TRCLUL,SPACES                                                    
         BNH   TAPD205                                                          
         CLC   TRCLACT,SPACES                                                   
         BH    TAPD210                                                          
TAPD205  OI    ERRSW1,MISCACC                                                   
         B     TAPD220                                                          
TAPD210  LA    R4,IO                                                            
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(L'TRCLUL),TRCLUL                                      
         MVC   ACKEYACC+3(L'TRCLACT),TRCLACT                                    
         MVC   SCACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID CLEARING ACCT            
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
         OI    ERRSW1,INVCACC           INVALID ACCOUNT                         
         B     TAPD220                                                          
*                                                                               
         MVC   SCACCNM,SPACES                                                   
         LA    R4,IO                    GET ACCOUNT NAME                        
         BAS   RE,GETNME                                                        
         MVC   SCACCNM,WORK                                                     
*                                                                               
TAPD220  OC    TRJOBN,SPACES                                                    
         OC    TRJOBDS,SPACES                                                   
*                                                                               
         CLI   TRPOSTI,C'Y'                                                     
         BNE   TAPD225                                                          
         CLC   TRINUL,=C'SI'                                                    
         BNE   TAPD223                                                          
         CLC   TRINACT,SPACES                                                   
         BH    TAPD225                                                          
TAPD223  OI    ERRSW3,MISINCAC                                                  
*                                  VALIDATE DUE DATE                            
TAPD225  ZAP   SPECAMT,=P'0'                                                    
*                                  VALIDATE DUE DATE                            
         CLC   TRDUEDT,SPACES                                                   
         BNH   TAPD230                                                          
         LA    R2,TRDUEDT          R2=INPUT DATE                                
         LA    R3,DUEDTE           R3=RESULT FIELD                              
         MVI   BYTEV,0                  INPUT IS M/D/Y                          
         MVI   BYTE,2              OUTPUT IS COMPRESSED                         
         BAS   RE,VALDTE           VALIDATE DATE                                
         BE    *+8                                                              
         OI    ERRSW2,INVDUED                                                   
         GOTO1 DATCON,DMCB,(2,DUEDTE),(5,DUEDPRT)                               
*                                                                               
TAPD230  CLC   TRMONACT,SPACES                                                  
         BH    TAPD240                                                          
         MVC   PSTMOA,PSTINVDT                                                  
         B     TAPD245                                                          
TAPD240  LA    R2,TRMONACT                                                      
         LA    R3,PSTMOA                                                        
         MVI   BYTEV,2                  INPUT IS M/D/Y                          
         MVI   BYTE,1                                                           
         BAS   RE,VALDTE                                                        
         BE    TAPD245                                                          
         OI    ERRSW2,INVMOA                                                    
         B     TAPD250                                                          
TAPD245  MVC   PPSTMOA,PSTMOA           SAVE PACKED WITHOUT SIGN                
         BAS   RE,CNVMOS                CONVERT MOS TO 2 CHAR FORMAT            
*                                                                               
TAPD250  OC    TRSPEC,TRSPEC                                                    
         BZ    TAPD260                                                          
         PACK  SPECAMT,TRSPEC           SPECIAL AMT (MEMO GROSS)                
*                                                                               
TAPD260  DS    0H                                                               
         CLC   TRRECAC,SPACES                                                   
         BNH   TAPD900                                                          
         LA    R4,IO                                                            
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SR'                                             
         CLC   TRRECUL,SPACES                                                   
         BNH   *+10                                                             
         MVC   ACKEYACC+1(L'TRRECUL),TRRECUL                                    
         MVC   ACKEYACC+3(L'TRRECAC),TRRECAC                                    
         MVC   SRACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID RECBLE ACCT              
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
         OI    ERRSW2,INVRACC           INVALID ACCOUNT                         
         B     TAPD900                                                          
*                                                                               
         MVI   ELCODE,X'32'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         OI    ERRSW2,INVRACC                                                   
         B     TAPD900                                                          
         LA    R4,IO                    GET ACCOUNT NAME                        
         BAS   RE,GETNME                                                        
         MVC   SRACCNM,WORK                                                     
*                                                                               
TAPD900  DS    0H                                                               
         CLI   LINE,52                  MAKE SURE ENOUGH ROOM TO PRINT          
         BNH   *+8                      AN ENTIRE ENTRY ON THIS PAGE            
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRTDEB                PRINT ENTRY ON REPORT                   
         CLI   ERRSW1,0                 WAS THERE AN ERROR                      
         BNE   TAPD930                                                          
         CLI   ERRSW2,0                 WAS THERE AN ERROR                      
         BNE   TAPD930                                                          
         CLI   ERRSW3,0                 WAS THERE AN ERROR                      
         BE    TAPD950                                                          
TAPD930  BAS   RE,ERPRT                 PRINT ERROR MESSAGES                    
         CLI   LINE,52                  MAKE SURE ENOUGH ROOM TO PRINT          
         BH    XIT                      AN ENTIRE ENTRY ON THIS PAGE            
         GOTO1 ABOXMID                  FLOAT IN MIDLINE                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
TAPD950  DS    0H                                                               
         CLI   LINE,52                  MAKE SURE ENOUGH ROOM TO PRINT          
         BH    TAPD970                  AN ENTIRE ENTRY ON THIS PAGE            
         GOTO1 ABOXMID                  FLOAT IN MIDLINE                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
TAPD970  AP    RECOUTR,=P'1'                                                    
         BAS   RE,POSTDEB               CREATE POSTING                          
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        PROCESS RECEIVABLE CREDITS                                 *           
*                                                                   *           
*-------------------------------------------------------------------*           
TAPCRED  NTR1                                                                   
         CLC   TRINVNM,=C'000000'                                               
         BE    TAPC115                                                          
         CLC   TRINVNM,SPACES                                                   
         BH    *+8                                                              
TAPC115  OI    ERRSW1,MISINVN                                                   
*                                                                               
         MVC   WORK,SPACES         SHIFT INVOICE NUM TO THE LEFT                
         LA    R1,6                AND PAD WITH SPACES                          
         LA    R2,TCINVNM                                                       
TAPC120  CLI   0(R2),C' '                                                       
         BH    TAPC130                                                          
         LA    R2,1(R2)                                                         
         BCT   R1,TAPC120                                                       
         B     TAPC140                                                          
TAPC130  BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R2)                                                    
         MVC   PSTINVNM,WORK                                                    
*                                                                               
TAPC140  DS    0H                                                               
         LA    R2,TCINVDT               R2=INPUT DATE                           
         LA    R3,PSTINVDT              R3=RESULT FIELD                         
         MVI   BYTEV,0                  INPUT IS M/D/Y                          
         MVI   BYTE,1                   OUTPUT IS PWOS                          
         BAS   RE,VALDTE                VALIDATE DATE                           
         BE    TAPC142                                                          
         OI    ERRSW1,INVINVD                                                   
         B     TAPC150                                                          
TAPC142  GOTO1 DATCON,DMCB,(1,PSTINVDT),(5,INVDPRT)                             
*                                                                               
         LA    R2,TCINVDT                                                       
         LA    R3,PSTMOA                                                        
         MVI   BYTEV,0                  INPUT IS M/D/Y                          
         MVI   BYTE,1                                                           
         BAS   RE,VALDTE                                                        
         MVC   PPSTMOA,PSTMOA                                                   
         BAS   RE,CNVMOS                CONVERT MOS TO 2 CHAR FORMAT            
*                                                                               
TAPC150  CLC   TCCLI,SPACES                                                     
         BH    *+8                                                              
         OI    ERRSW1,INVCLI                                                    
         MVC   PSTCLI,TCCLI                                                     
*                                                                               
         CLC   TCPROD,SPACES                                                    
         BH    *+8                                                              
         OI    ERRSW1,INVPRD                                                    
         MVC   PSTPROD,TCPROD                                                   
*                                                                               
         BAS   RE,CLIPRD                READ CLI/PRD REC                        
*                                                                               
         OC    TCBLAMT,TCBLAMT                                                  
         BNZ   TAPC160                                                          
         OI    ERRSW2,MISBAMT                                                   
         B     TAPC170                                                          
TAPC160  PACK  BILLAMT,TCBLAMT          BILL (TRAN) AMT                         
         AP    CHEKIN,BILLAMT                                                   
         AP    CSHIN,BILLAMT                                                    
*                                                                               
TAPC170  MVC   BSOURCE,SPACES                                                   
         MVC   SIACC,SPACES                                                     
         MVC   SIACCNM,SPACES                                                   
         OC    TCINUL(L'TCINUL+L'TCINACT),SPACES     INCOME UNIT/LEDGER         
***                                                                             
***      TEMPORARY FIX FOR TAPE ERROR                                           
         CLC   TCINACT(4),=C'UNAP'                                              
         BE    TAPC179                                                          
         B     TAPC180                                                          
***                                                                             
         CLC   TCINUL,SPACES                                                    
         BH    TAPC180                                                          
TAPC179  MVC   BSOURCE,TCINACT                                                  
         B     TAPC200                                                          
*                                                                               
TAPC180  DS    0H                                                               
         CLC   TCINACT,SPACES                                                   
         BNH   TAPC185                                                          
         LA    R4,IO                                                            
         USING ACKEYD,R4                VALIDATE INCOME ACCOUNT                 
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
***      MVC   ACKEYACC+1(L'TCINUL),TCINUL                                      
         MVC   ACKEYACC+1(2),=C'SI'                                             
         MVC   ACKEYACC+3(L'TCINACT),TCINACT                                    
         MVC   SIACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID INCOME ACCT              
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
TAPC185  OI    ERRSW1,INVIACC           INVALID ACCOUNT                         
         B     TAPC200                                                          
*                                                                               
         LA    R4,IO                                                            
         BAS   RE,GETNME                                                        
         MVC   SIACCNM,WORK                                                     
         BAS   RE,CHOPIT                                                        
         MVC   BSOURCE,CHOPOUT                                                  
*                                                                               
TAPC200  DS    0H                                                               
         CLC   TCCLUL,SPACES                                                    
         BNH   TAPC205                                                          
         CLC   TCCLACT,SPACES                                                   
         BH    TAPC210                                                          
TAPC205  OI    ERRSW1,MISCACC                                                   
         B     TAPC220                                                          
TAPC210  LA    R4,IO                    VALIDATE CLEARING ACCOUNT               
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(L'TRCLUL),TCCLUL                                      
         MVC   ACKEYACC+3(L'TRCLACT),TCCLACT                                    
         MVC   SCACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID CLEARING ACCT            
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
         OI    ERRSW1,MISCACC           INVALID ACCOUNT                         
         B     TAPC900                                                          
*                                                                               
         MVC   SCACCNM,SPACES                                                   
         LA    R4,IO                    GET ACCOUNT NAME                        
         BAS   RE,GETNME                                                        
         MVC   SCACCNM,WORK                                                     
*                                                                               
*                                                                               
TAPC220  DS    0H                                                               
         CLC   TCRECAC,SPACES                                                   
         BNH   TAPC240                                                          
         LA    R4,IO                    VALIDATE RECEIVABLE ACCOUNT             
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SR'                                             
         CLC   TCRECUL,SPACES                                                   
         BNH   *+10                                                             
         MVC   ACKEYACC+1(L'TCRECUL),TCRECUL                                    
         MVC   ACKEYACC+3(L'TCRECAC),TCRECAC                                    
         MVC   SRACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID RECBLE ACCT              
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
         OI    ERRSW2,INVRACC           INVALID ACCOUNT                         
         B     TAPC240                                                          
*                                                                               
         MVI   ELCODE,X'32'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+12                                                             
         OI    ERRSW2,INVRACC                                                   
         B     TAPC240                                                          
         LA    R4,IO                    GET ACCOUNT NAME                        
         BAS   RE,GETNME                                                        
         MVC   SRACCNM,WORK                                                     
*                                                                               
TAPC240  DS    0H                                                               
         CLC   TCCHKDT,SPACES                                                   
         BNH   TAPC260                                                          
         LA    R2,TCCHKDT               R2=INPUT DATE                           
         LA    R3,CHKDTE                R3=RESULT FIELD                         
         MVI   BYTEV,0                  INPUT IS M/D/Y                          
         MVI   BYTE,1                   OUTPUT IS PWOS                          
         BAS   RE,VALDTE                VALIDATE DATE                           
         BE    *+12                                                             
         OI    ERRSW2,INVCHKD                                                   
         B     TAPC260                                                          
         GOTO1 DATCON,DMCB,(1,CHKDTE),(5,CHKDTEPT)                              
*                                                                               
TAPC260  DS    0H                                                               
         CLC   TCDEPDT,SPACES                                                   
         BNH   TAPC900                                                          
         CLC   TCDEPDT,=C'000000'                                               
         BE    TAPC900                                                          
         LA    R2,TCDEPDT               R2=INPUT DATE                           
         LA    R3,DEPDTE                R3=RESULT FIELD                         
         MVI   BYTEV,0                  INPUT IS M/D/Y                          
         MVI   BYTE,1                   OUTPUT IS PWOS                          
         BAS   RE,VALDTE                VALIDATE DATE                           
         BE    *+12                                                             
         OI    ERRSW2,INVDEPD                                                   
         B     TAPC900                                                          
         GOTO1 DATCON,DMCB,(1,DEPDTE),(5,DEPDTEPT)                              
*                                                                               
TAPC900  DS    0H                                                               
         CLI   LINE,52                  MAKE SURE ENOUGH ROOM TO PRINT          
         BNH   *+8                      AN ENTIRE ENTRY ON THIS PAGE            
         MVI   FORCEHED,C'Y'            IF NOT START NEW PAGE                   
         BAS   RE,PRTCRED               PRINT ENTRY ON REPORT                   
         CLI   ERRSW1,0                 WAS THERE AN ERROR                      
         BNE   TAPC930                                                          
         CLI   ERRSW2,0                 WAS THERE AN ERROR                      
         BNE   TAPC930                                                          
         CLI   ERRSW3,0                 WAS THERE AN ERROR                      
         BE    TAPC950                                                          
TAPC930  BAS   RE,ERPRT                 PRINT ERROR MESSAGES                    
         CLI   LINE,52                  MAKE SURE ENOUGH ROOM TO PRINT          
         BH    XIT                      AN ENTIRE ENTRY ON THIS PAGE            
         GOTO1 ABOXMID                  FLOAT IN MIDLINE                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
         B     XIT                                                              
*                                                                               
TAPC950  DS    0H                                                               
         CLI   LINE,52                  MAKE SURE ENOUGH ROOM TO PRINT          
         BH    TAPC970                  AN ENTIRE ENTRY ON THIS PAGE            
         GOTO1 ABOXMID                  FLOAT IN MIDLINE                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
TAPC970  AP    RECOUTC,=P'1'                                                    
         BAS   RE,POSTCRD               CREATE POSTING                          
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              SET UP POSTING RECORD AND POST TO WORKER FILE        *           
*              DEBIT                                                *           
*-------------------------------------------------------------------*           
POSTDEB  NTR1                                                                   
         USING PSHEADD,R2                                                       
         LA    R2,POSTAREA                                                      
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC,SRACC                                                    
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC+3(L'BSOURCE),BSOURCE                                    
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDSBNM(L'BSOURCE),BSOURCE                                      
         MVC   PSHDANAL,SPACES                                                  
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRANSD,R2                                                        
         MVI   TRNSEL,X'44'                                                     
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,PSTINVDT                                                
         MVC   TRNSREF(6),PSTINVNM                                              
         MVI   TRNSSBRF,0                   SUB REFNO                           
         MVI   TRNSTYPE,26                                                      
         MVI   TRNSSTAT,X'80'               DEBITS                              
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),PSTMOA           MONTH OF SERVICE                    
         ZAP   TRNSAMNT,BILLAMT             AMOUNT                              
         MVC   TRNSOFFC,PSTOFFC             WHERE DOES OFFICE COME FROM         
         AP    BILOUT,TRNSAMNT              TOTAL CASH POSTED                   
         AP    CSHOUTD,TRNSAMNT             TOTAL CASH POSTED                   
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                        BUILD OTHERS ELEMENT                
         USING ACOTHERD,R2                                                      
         XC    0(ACOTLNQ1,R2),0(R2)                                             
         MVI   ACOTEL,ACOTELQ                                                   
         MVI   ACOTLEN,ACOTLNQ1                                                 
         MVC   ACOTPROF,SPACES                                                  
         MVC   ACOTPROF(1),TRSYS                                                
         MVC   ACOTDATE,PPSTMOA                                                 
         MVC   ACOTNUM(L'PSTPROD),PSTPROD                                       
         MVC   ACOTNUM+L'PSTPROD(L'TRJOBN),TRJOBN                               
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                        BUILD 1A ELEMENT                    
         USING ACMTD,R2                                                         
         XC    0(ACMTLNQ,R2),0(R2)                                              
         MVI   ACMTEL,ACMTELQ                                                   
         MVI   ACMTLEN,ACMTLNQ                                                  
         MVC   ACMTSYS,TRSYS                                                    
         MVC   ACMTCLI,TRCLI                                                    
         MVC   ACMTPRD,TRPRD                                                    
         MVC   ACMTJOB,TRJOBN                                                   
         MVC   ACMTDSCP,TRJOBDS                                                 
         MVC   ACMTMOS,PPSTMOA                                                  
         CVB   R0,SPECAMT                   SPECIAL AMOUNT                      
         STCM  R0,15,ACMTGRS                                                    
         CVB   R0,NETAMT                    NET AMOUNT                          
         STCM  R0,15,ACMTNET                                                    
         CVB   R0,COMMAMT                   COMMISSION AMOUNT                   
         STCM  R0,15,ACMTCOM                                                    
         CVB   R0,CDAMT                     CASH DISCOUNT                       
         STCM  R0,15,ACMTCD                                                     
         CVB   R0,BILLAMT                   BILL AMOUNT                         
         STCM  R0,15,ACMTRECV                                                   
         MVC   ELEM1A,0(R2)                                                     
*                                                                               
         ZIC   R1,1(R2)                     SUBSIDIARY CASH ELEMENT             
         AR    R2,R1                                                            
         USING TRCASHD,R2                                                       
         XC    TRCSEL(TRCSLNQ1),TRCSEL                                          
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'I'                                                    
         ZAP   TRCSAMNT,COMMAMT                                                 
*                                                                               
         CP    CDAMT,=P'0'                                                      
         BE    PSTD310                                                          
         ZIC   R1,1(R2)                     SUBSIDIARY CASH ELEMENT             
         AR    R2,R1                                                            
         USING TRCASHD,R2                                                       
         XC    TRCSEL(TRCSLNQ1),TRCSEL                                          
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'D'                                                    
         ZAP   TRCSAMNT,CDAMT                                                   
*                                                                               
PSTD310  CLC   TRDUEDT,SPACES               IF NO DUE DATE DO NOT               
         BNH   PSTD320                      CREATE ELEMENT                      
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                        BUILD 1A ELEMENT                    
         USING TRDUED,R2                                                        
         MVI   TRDUEL,TRDUELQ               ELEM CODE                           
         MVI   TRDUEN,TRDULNQ               ELEM LENGTH                         
         MVC   TRDUDATE,DUEDTE              DUE DATE                            
         MVC   ELEM61,0(R2)                                                     
PSTD320  BAS   RE,POSTIT                    POST TO WORKER FILE                 
*                                                                               
*        NET POSTING                                                            
         USING PSHEADD,R2                                                       
         LA    R2,POSTAREA                                                      
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC,SCACC                                                    
         MVC   PSHDSBAC,SRACC                                                   
         MVC   PSHDSBNM,SRACCNM                                                 
         MVC   PSHDANAL,SPACES                                                  
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRANSD,R2                                                        
         MVI   TRNSEL,X'44'                                                     
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,PSTINVDT                                                
         MVC   TRNSREF(6),PSTINVNM                                              
         MVI   TRNSSBRF,0                   SUB REFNO                           
         MVI   TRNSTYPE,26                                                      
         MVI   TRNSSTAT,X'00'               CREDIT                              
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),PSTMOA           MONTH OF SERVICE                    
         ZAP   TRNSAMNT,NETAMT              NET AMOUNT                          
         MVC   TRNSOFFC,PSTOFFC                                                 
         AP    NETOUT,TRNSAMNT              TOTAL CASH POSTED                   
         BAS   RE,POSTIT                                                        
         AP    CSHOUTC,NETAMT                                                   
*                                                                               
*        COMMISSION POSTING                                                     
         CLI   TRPOSTI,C'Y'                                                     
         BNE   XIT                                                              
         TM    ERRSW3,MISINCAC                                                  
         BO    XIT                                                              
         USING PSHEADD,R2                                                       
         LA    R2,POSTAREA                                                      
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC,SIACC                                                    
         MVC   PSHDSBAC,SJACC                                                   
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDANAL,SPACES                                                  
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRANSD,R2                                                        
         MVI   TRNSEL,X'44'                                                     
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,PSTINVDT                                                
         MVC   TRNSREF(6),PSTINVNM                                              
         MVI   TRNSSBRF,0                   SUB REFNO                           
         MVI   TRNSTYPE,26                                                      
         MVI   TRNSSTAT,X'00'               CREDIT                              
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),PSTMOA           MONTH OF SERVICE                    
         ZAP   TRNSAMNT,COMMAMT             COMMISSION AMOUNT                   
         MVC   TRNSOFFC,PSTOFFC                                                 
         AP    COMMOUT,TRNSAMNT                                                 
         AP    CSHOUTC,COMMAMT                                                  
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                        BUILD 1A ELEMENT                    
         USING ACMTD,R2                                                         
         XC    0(ACMTLNQ,R2),0(R2)                                              
         MVC   0(ACMTLNQ,R2),ELEM1A                                             
*                                                                               
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         USING TRCASHD,R2                                                       
         XC    TRCSEL(TRCSLNQ1),TRCSEL                                          
         MVI   TRCSEL,TRCSELQ                                                   
         MVI   TRCSLEN,TRCSLNQ1                                                 
         MVI   TRCSTYPE,C'G'                                                    
         ZAP   TRCSAMNT,BILLAMT                                                 
         BAS   RE,POSTIT                                                        
         B     XIT                          GET ANOTHER RECORD                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              SET UP POSTING RECORD AND POST TO WORKER FILE        *           
*              CREDIT                                               *           
*-------------------------------------------------------------------*           
POSTCRD  NTR1                                                                   
         USING PSHEADD,R2                                                       
         LA    R2,POSTAREA                                                      
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC,SCACC                                                    
         MVC   PSHDSBAC,SRACC               CONTRA IS SOURCE                    
         MVC   PSHDSBNM,SRACC                                                   
         MVC   PSHDANAL,SPACES                                                  
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRANSD,R2                                                        
         MVI   TRNSEL,X'44'                                                     
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,PSTINVDT                                                
         MVC   TRNSREF(6),PSTINVNM                                              
         MVI   TRNSSBRF,0                   SUB REFNO                           
         MVI   TRNSTYPE,30                                                      
         MVI   TRNSSTAT,X'80'               DEBIT                               
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),PSTMOA           MONTH OF SERVICE                    
         ZAP   TRNSAMNT,BILLAMT             AMOUNT                              
         MVC   TRNSOFFC,PSTOFFC                                                 
         AP    CHEKOUT,TRNSAMNT             TOTAL CASH POSTED                   
         AP    CSHOUTD,TRNSAMNT             TOTAL CASH POSTED                   
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING RALELD,R2                    ALLOCATION ELEMENT)                 
         XC    RALELD(RALALCLQ),RALELD                                          
         MVI   RALEL,RALELQ                                                     
         MVI   RALLN,RALALCLQ                                                   
         MVI   RALTYPE,RALTALC                                                  
         MVC   RALAREF,TCCHKNM                                                  
         MVC   RALADAT,CHKDTE                                                   
         MVC   RALADEP,DEPDTE                                                   
         MVC   ELEMD9,0(R2)                                                     
         BAS   RE,POSTIT                                                        
*                                                                               
*                                                                               
         USING PSHEADD,R2                                                       
         LA    R2,POSTAREA                                                      
         MVC   PSHDEL(2),=X'5046'           BUILD POSTING HEADER                
         MVC   PSHDACC,SPACES                                                   
         MVC   PSHDACC,SRACC                                                    
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC+3(L'BSOURCE),BSOURCE                                    
         MVC   PSHDSBNM,SPACES                                                  
         MVC   PSHDANAL,SPACES                                                  
*                                                                               
         ZIC   R1,1(R2)                     BUMP TO NEXT ELEMENT                
         AR    R2,R1                                                            
         USING TRANSD,R2                                                        
         MVI   TRNSEL,X'44'                                                     
         MVI   TRNSLEN,TRNSLNQ                                                  
         MVC   TRNSDATE,PSTINVDT                                                
         MVC   TRNSREF(6),PSTINVNM                                              
         MVI   TRNSSBRF,0                   SUB REFNO                           
         MVI   TRNSTYPE,30                                                      
         MVI   TRNSSTAT,X'00'               CREDIT                              
         MVC   TRNSBTCH,SPACES                                                  
         MVC   TRNSBTCH(2),PSTMOA           MONTH OF SERVICE                    
         ZAP   TRNSAMNT,BILLAMT             AMOUNT                              
         MVC   TRNSOFFC,PSTOFFC                                                 
         BAS   RE,POSTIT                                                        
         AP    CSHOUTC,BILLAMT              TOTAL CASH POSTED                   
         B     XIT                          GET ANOTHER RECORD                  
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------------------*           
*        AFTER PROCESSING EACH INPUT RECORD PUT OUT TO REPORT       *           
*                                                                   *           
*-------------------------------------------------------------------*           
POSTIT   NTR1                                                                   
         CLI   SAVOPT1,C'L'                                                     
         BNE   PST150                                                           
         ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         MVI   0(R2),0                      SET END OF RECORD                   
         LA    R2,1(R2)                     COMPUTE RECORD LENGTH               
         LA    R1,POSTHEAD                                                      
         SR    R2,R1                        END LESS START                      
         XC    POSTHEAD,POSTHEAD                                                
         STH   R2,POSTHEAD                                                      
         MVC   COMMAND,=CL6'ADD'                                                
         BAS   RE,FILE                      POST TO WORKER FILE                 
PST150   AP    RECOUT,=P'1'                                                     
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        AFTER PROCESSING EACH INPUT RECORD PUT OUT TO REPORT       *           
*        DEBIT RECORDS                                              *           
*-------------------------------------------------------------------*           
PRTDEB   NTR1                                                                   
         LA    R4,XP                                                            
         USING TAPPRNT,R4                                                       
         EDIT  (P4,RECIN),(5,TPITEM),0                                          
         MVC   TPTYPE,TAPTYPR                                                   
         MVC   TPACCT,SRACC+1                                                   
         MVC   TPACNAME,SRACCNM                                                 
         MVC   TPINVNM,PSTINVNM                                                 
         MVC   TPDATE,INVDPRT                                                   
         LA    R2,TPDEBIT                                                       
         EDIT  (P8,BILLAMT),(16,0(R2)),2,MINUS=YES                              
*                                                                               
         CP    CDAMT,=P'0'                                                      
         BE    PRTR130                                                          
         LA    R2,TPMEMO                                                        
         LA    R2,L'TPMEMO(R2)                                                  
         EDIT  (P8,CDAMT),(16,0(R2)),2,ALIGN=LEFT,MINUS=YES                     
         MVC   TPMEMO(3),=C'CD='                                                
PRTR130  BAS   RE,REPORT                                                        
*                                                                               
         MVC   TPACCT2(L'BSOURCE),BSOURCE                                       
         MVC   TPACNAME,SPACES                                                  
         LA    R2,TPCREDIT                                                      
         EDIT  (P8,COMMAMT),(16,0(R2)),2,MINUS=YES                              
         CP    SPECAMT,=P'0'                                                    
         BE    PRTR150                                                          
         LA    R2,TPMEMO                                                        
         LA    R2,L'TPMEMO(R2)                                                  
         EDIT  (P8,SPECAMT),(16,0(R2)),2,ALIGN=LEFT,MINUS=YES                   
         MVC   TPMEMO(6),=C'GROSS='                                             
PRTR150  BAS   RE,REPORT                                                        
*                                                                               
         MVC   TPACCT2,SCACC+1                                                  
         MVC   TPACNAME,SCACCNM                                                 
         LA    R2,TPCREDIT                                                      
         EDIT  (P8,NETAMT),(16,0(R2)),2,MINUS=YES                               
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP(COMRLEN),COMMLNER                                             
         MVC   XP+(RCLIL)(L'PSTCLI),PSTCLI                                      
         MVC   XP+(RPRDL)(L'PSTPROD),PSTPROD                                    
         MVC   XP+(RDUEL)(L'DUEDPRT),DUEDPRT                                    
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+(RCLIL)(14),=C'BILL SOURCE = '                                
         MVC   XP+(RCLIL+14)(L'BSOURCE),BSOURCE                                 
         BAS   RE,REPORT                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        AFTER PROCESSING EACH INPUT RECORD PUT OUT TO REPORT       *           
*        DEBIT RECORDS                                              *           
*-------------------------------------------------------------------*           
PRTCRED  NTR1                                                                   
         LA    R4,XP                                                            
         USING TAPPRNT,R4                                                       
         EDIT  (P4,RECIN),(5,TPITEM),0                                          
         MVC   TPTYPE,TAPTYPR                                                   
         MVC   TPACCT,SCACC+1                                                   
         MVC   TPACNAME,SCACCNM                                                 
         MVC   TPINVNM,PSTINVNM                                                 
         MVC   TPDATE,INVDPRT                                                   
         LA    R2,TPDEBIT                                                       
         EDIT  (P8,BILLAMT),(16,0(R2)),2,MINUS=YES                              
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   TPACCT2,SRACC+1                                                  
         MVC   TPACNAME,SRACCNM                                                 
         LA    R2,TPCREDIT                                                      
         EDIT  (P8,BILLAMT),(16,0(R2)),2,MINUS=YES                              
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP(COMCLEN),COMMLNEC                                             
         MVC   XP+(CCLIL)(L'PSTCLI),PSTCLI                                      
         MVC   XP+(CPRDL)(L'PSTPROD),PSTPROD                                    
         MVC   XP+(CCHKL)(L'CHKDTEPT),CHKDTEPT                                  
         MVC   XP+(CDEPL)(L'DEPDTEPT),DEPDTEPT                                  
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+(CCLIL)(14),=C'BILL SOURCE = '                                
         MVC   XP+(CCLIL+14)(L'BSOURCE),BSOURCE                                 
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        COUNT ERRONEOUS RECORDS AND PRINT ERROR MESSAGES OUT ON    *           
*        REPORT                                                     *           
*                                                                   *           
*-------------------------------------------------------------------*           
ERPRT    NTR1                                                                   
         LA    R4,XP                                                            
         USING TAPPRNT,R4                                                       
         AP    ERCNT,=P'1'                                                      
*                                                                               
         LA    R2,ERRTAB1          R2=TABLE OF ERROR MESSAGES                   
         SR    R1,R1                                                            
ERP100   CLI   0(R2),0             END OF ERROR TABLE                           
         BE    ERP300              GET NEXT INPUT RECORD                        
         IC    R1,0(R2)            R1=ERROR BIT                                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    ERRSW1,0                                                         
         BNO   ERP200               NOT AN ERROR - SKIP TO NEXT                 
         IC    R1,1(R2)             LENGTH OF ENTRY                             
         SH    R1,=H'3'             R1=LENGTH OF MESSAGES FOR EX INST.          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TPACCT(0),2(R2)      MESSAGE TO PRINT                            
         BAS   RE,REPORT                                                        
ERP200   IC    R1,1(R2)             R0=LENGTH OF ENTRY                          
         AR    R2,R1                R2=NEXT ERRTAB1 ENTRY                       
         B     ERP100                                                           
*                                                                               
*              CHECK ERROR TABLE TWO                                            
ERP300   DS    0H                                                               
         LA    R2,ERRTAB2          R2=TABLE OF ERROR MESSAGES                   
         SR    R1,R1                                                            
ERP350   CLI   0(R2),0             END OF ERROR TABLE                           
         BE    ERP500              GET NEXT INPUT RECORD                        
         IC    R1,0(R2)            R1=ERROR BIT                                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    ERRSW2,0                                                         
         BNO   ERP400               NOT AN ERROR - SKIP TO NEXT                 
         IC    R1,1(R2)             LENGTH OF ENTRY                             
         SH    R1,=H'3'             R1=LENGTH OF MESSAGES FOR EX INST.          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TPACCT(0),2(R2)      MESSAGE TO PRINT                            
         BAS   RE,REPORT                                                        
ERP400   IC    R1,1(R2)             R0=LENGTH OF ENTRY                          
         AR    R2,R1                R2=NEXT ERRTAB1 ENTRY                       
         B     ERP350                                                           
*                                                                               
ERP500   DS    0H                                                               
         LA    R2,ERRTAB3          R2=TABLE OF ERROR MESSAGES                   
         SR    R1,R1                                                            
ERP550   CLI   0(R2),0             END OF ERROR TABLE                           
         BE    ERPXIT              GET NEXT INPUT RECORD                        
         IC    R1,0(R2)            R1=ERROR BIT                                 
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    ERRSW3,0                                                         
         BNO   ERP600               NOT AN ERROR - SKIP TO NEXT                 
         IC    R1,1(R2)             LENGTH OF ENTRY                             
         SH    R1,=H'3'             R1=LENGTH OF MESSAGES FOR EX INST.          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TPACCT(0),2(R2)      MESSAGE TO PRINT                            
         BAS   RE,REPORT                                                        
ERP600   IC    R1,1(R2)             R0=LENGTH OF ENTRY                          
         AR    R2,R1                R2=NEXT ERRTAB1 ENTRY                       
         B     ERP550                                                           
*                                                                               
ERPXIT   B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        AT END OF FILE CLOSE TAPE AND WORKER FILES                 *           
*                                                                   *           
*-------------------------------------------------------------------*           
EOJ      DS    0H                                                               
         GOTO1 ABOXBOT                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         CLOSE TAPE                                                             
         LA    R2,POSTAREA                                                      
         USING PSSUBFD,R2                                                       
         MVI   PSSBEL,PSSBELQ                                                   
         MVI   PSSBLEN,X'1D'                                                    
         MVC   PSSBDESC,=CL16'RECEIVBLE UPLOAD'       NEED NAME                 
         ZAP   PSSBRECS,RECOUT                 OUTPUT RECORDS                   
***      ZAP   CSHOUTD,BILOUT                                                   
***      AP    CSHOUTD,CHEKOUT                                                  
         ZAP   PSSBCASH,CSHOUTD                OUTPUT CASH                      
         SR    R3,R3                                                            
         IC    R3,PSSBLEN                                                       
         AR    R3,R2                                                            
         MVI   0(R3),X'00'                     END OF RECORD                    
         LA    R3,1(R3)                        COMPUTE RECORD LENGTH            
         LA    R1,POSTHEAD                                                      
         SR    R3,R1                           END LESS START                   
         XC    POSTHEAD,POSTHEAD                                                
         STH   R3,POSTHEAD                                                      
         MVC   COMMAND,=CL6'ADD'                                                
         BAS   RE,FILE                                                          
         MVC   COMMAND,=CL6'CLOSE'                                              
         BAS   RE,FILE                                                          
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         GOTO1 ABXHOOK                                                          
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(19),=C'TOTAL RECORDS INPUT'                                 
         EDIT  (P4,RECIN),(7,XP+47)                                             
         BAS   RE,REPORT                                                        
         MVC   XP+10(16),=C'TOTAL CASH INPUT'                                   
         EDIT  (P8,CSHIN),(14,XP+41),2,MINUS=YES                                
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(20),=C'TYPE R RECORDS INPUT'                                
         EDIT  (P4,RECINR),(7,XP+47)                                            
         BAS   RE,REPORT                                                        
         MVC   XP+10(17),=C'BILL AMOUNT INPUT'                                  
         EDIT  (P8,BILIN),(14,XP+41),2,MINUS=YES                                
         BAS   RE,REPORT                                                        
         MVC   XP+10(19),=C'INCOME AMOUNT INPUT'                                
         EDIT  (P8,COMMIN),(14,XP+41),2,MINUS=YES                               
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(20),=C'TYPE C RECORDS INPUT'                                
         EDIT  (P4,RECINC),(7,XP+47)                                            
         BAS   RE,REPORT                                                        
         MVC   XP+10(18),=C'CHECK AMOUNT INPUT'                                 
         EDIT  (P8,CHEKIN),(14,XP+41),2,MINUS=YES                               
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(20),=C'TOTAL RECORDS OUTPUT'                                
         EDIT  (P4,RECOUT),(7,XP+47)                                            
         BAS   RE,REPORT                                                        
         MVC   XP+10(18),=C'TOTAL DEBIT OUTPUT'                                 
         EDIT  (P8,CSHOUTD),(14,XP+41),2,MINUS=YES                              
         BAS   RE,REPORT                                                        
         MVC   XP+10(19),=C'TOTAL CREDIT OUTPUT'                                
         EDIT  (P8,CSHOUTC),(14,XP+41),2,MINUS=YES                              
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(24),=C'TYPE R RECORDS PROCESSED'                            
         EDIT  (P4,RECOUTR),(7,XP+47)                                           
         BAS   RE,REPORT                                                        
         MVC   XP+10(21),=C'BILL AMOUNT PROCESSED'                              
         EDIT  (P8,BILOUT),(14,XP+41),2,MINUS=YES                               
         BAS   RE,REPORT                                                        
         MVC   XP+10(23),=C'INCOME AMOUNT PROCESSED'                            
         EDIT  (P8,COMMOUT),(14,XP+41),2,MINUS=YES                              
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(24),=C'TYPE C RECORDS PROCESSED'                            
         EDIT  (P4,RECOUTC),(7,XP+47)                                           
         BAS   RE,REPORT                                                        
         MVC   XP+10(22),=C'CHECK AMOUNT PROCESSED'                             
         EDIT  (P8,CHEKOUT),(14,XP+41),2,MINUS=YES                              
         BAS   RE,REPORT                                                        
         MVI   SPACING,2                                                        
         BAS   RE,REPORT                                                        
*                                                                               
         MVC   XP+4(14),=C'       ERRORS '                                      
         EDIT  (P4,ERCNT),(7,XP+47),ZERO=NOBLANK                                
         BAS   RE,REPORT                                                        
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        READ SJ CLI/PRD REC                                        *           
*                                                                   *           
*-------------------------------------------------------------------*           
CLIPRD   NTR1                                                                   
         MVC   SRACC,SPACES                                                     
         MVC   SRACCNM,SPACES                                                   
         TM    ERRSW1,INVCLI            WAS THERE AN ERROR IN CLI/PRD           
         BO    CLIP200                  FIELDS - IF YES EXIT                    
         TM    ERRSW1,INVPRD            WAS THERE AN ERROR IN CLI/PRD           
         BO    CLIP200                  FIELDS - IF YES EXIT                    
         LA    R4,IO                                                            
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SJ'                                             
         MVC   ACKEYACC+3(L'TRCLI),TRCLI                                        
         MVC   ACKEYACC+6(L'TRPRD),TRPRD                                        
         MVC   SJACC,ACKEYACC                                                   
         BAS   RE,READACC               READ FOR VALID ACCOUNT                  
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
         OI    ERRSW2,INVCLPR           INVALID ACCOUNT                         
         B     CLIP200                                                          
*                                                                               
         MVI   ELCODE,X'24'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BNE   CLIP100                                                          
         USING ACPROFD,R4                                                       
         CLC   ACPRRECV,SPACES                                                  
         BNH   *+10                                                             
         MVC   SRACC,ACPRRECV                                                   
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   PSTOFFC,ACPROFFC                                                 
*                                                                               
CLIP100  DS    0H                                                               
         LA    R4,IO                                                            
         USING ACKEYD,R4                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SJ'                                             
         MVC   ACKEYACC+3(L'TRCLI),TRCLI                                        
         BAS   RE,READACC               READ FOR VALID RECBLE ACCT              
         CLI   DMCB+8,0                                                         
         BE    *+12                     ACCOUNT FOUND                           
         OI    ERRSW2,INVCLPR           INVALID ACCOUNT                         
         B     CLIP200                                                          
         LA    R4,IO                    GET ACCOUNT NAME                        
         BAS   RE,GETNME                                                        
         MVC   SJCLINM,WORK                                                     
*                                                                               
         MVI   ELCODE,X'24'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACPROFD,R4                                                       
         CLC   SRACC,SPACES                                                     
         BNE   CLIP180                                                          
         CLC   ACPRRECV,SPACES                                                  
         BNH   *+10                                                             
         MVC   SRACC,ACPRRECV                                                   
CLIP180  CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   PSTOFFC,ACPROFFC                                                 
*                                                                               
CLIP200  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CHECK 2 CHARACTER AGENCY CODE AGAINST THAT PASSED ON TAPE  *           
*                                                                   *           
*-------------------------------------------------------------------*           
CHOPIT   NTR1                                                                   
         MVC   CHOPOUT,SPACES                                                   
         GOTO1 =V(CHOPPER),DMCB,(L'WORK,WORK),(L'CHOPOUTA,CHOPOUT),    X        
               (12,4)                                                           
         B     XIT                                                              
*                                                                               
*                                                                               
*-------------------------------------------------------------------*           
*        CHECK 2 CHARACTER AGENCY CODE AGAINST THAT PASSED ON TAPE  *           
*                                                                   *           
*-------------------------------------------------------------------*           
AGENCHK  NTR1                                                                   
         L     R4,ADCOMP                                                        
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACCOMPD,R4                                                       
         CLC   ACMPALFA,TRAGENCY         TO AVOID DISASTER CHECK                
         BE    AGENXIT                   THAT AGENCY I'M GETTING TAPE           
         OI    BADTAGN,BADAGEN           FOR IS AGENCY IN JCL                   
AGENXIT  B     XIT                                                              
*-------------------------------------------------------------------*           
*              CONVERT MONTH OF SERVICE FORMAT FROM PSTMOA          *           
*-------------------------------------------------------------------*           
CNVMOS   NTR1                                                                   
         LA    R3,MOSTAB                                                        
         LA    R4,12                                                            
CNVM120  CLC   PSTMOA+1(1),0(R3)                                                
         BE    CNVM130                                                          
         LA    R3,2(R3)                                                         
         BCT   R4,CNVM120                                                       
CNVM130  MVC   PSTMOA+1(1),1(R3)                                                
*                                                                               
         MVZ   PSTMOA(1),=X'FF'                                                 
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              WORKER FILE INTERFACE                                *           
*                                                                   *           
*-------------------------------------------------------------------*           
FILE     NTR1                                                                   
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         CLI   SAVOPT1,C'L'                                                     
         BNE   XIT                                                              
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,POSTHEAD                             
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
*                                                                               
*-------------------------------------------------------------------*           
*        VALIDATE AND CONVERT DATES PASSED TO ROUTINE               *           
*              R2=INPUT DATE 6 CHAR MMDDYY, R3=OUTPUT ADDRESS WITH  *           
*              OUTPUT TYPE PASSED IN BYTE                           *           
*                                                                   *           
*-------------------------------------------------------------------*           
VALDTE   NTR1                                                                   
         MVC   WORK(2),0(R2)       MONTH                                        
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),2(R2)     DAY                                          
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),4(R2)      YEAR                                        
VALD140  GOTO1 DATVAL,DMCB,(BYTEV,WORK),TMPDAT1   DATE YYMMDD                   
         OC    DMCB(4),DMCB                   IS DATE VALID                     
         BZ    VALDTENO                       DATE ERROR                        
         GOTO1 DATCON,DMCB,(0,TMPDAT1),(BYTE,0(R3))                             
         SR    R1,R1                                                            
VALDTENO LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        ROUTINTE TO READ ACC FILE                                  *           
*-------------------------------------------------------------------*           
READACC  NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',IO,IO                        
XIT      XIT1                                                                   
*                                                                               
*                                                                               
*-------------------------------------------------------------------*           
*        ROUTINTE TO GET NAME FROM NAME ELEMENT                     *           
*-------------------------------------------------------------------*           
GETNME   NTR1                                                                   
         MVC   WORK(36),SPACES                                                  
         MVI   ELCODE,X'20'                   FOR NAME ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                           NO NAME                           
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME                                                 
*                                                                               
*                                                                               
*-------------------------------------------------------------------*           
*        REPORT ROUTINE                                             *           
*                                                                   *           
*-------------------------------------------------------------------*           
REPORT   NTR1                                                                   
         MVC   XHEAD3+125(9),=C'DRAFT RUN'                                      
         CLI   RCPOSTNG,C'N'                                                    
         BE    RPT120                                                           
         CLI   SAVOPT1,C'L'                                                     
         BNE   RPT120                                                           
         MVC   XHEAD3+125(8),=C'LIVE RUN'                                       
RPT120   GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                  *           
*-------------------------------------------------------------------*           
DATVAL   DC    V(DATVAL)                                                        
AGCERR1  DC    C'*** AGENCY ON TAPE DOES NOT MATCH AGENCY IN JCL ***'           
AGCERR2  DC    C'     ************ STOP PROCESSING ************     '           
*                                                                               
HALF128  DC    H'128'                                                           
*                                                                               
ERRTAB1  DS    0H                                                               
ERR01    DC    AL1(BADRTYP),AL1(ERR02-ERR01)                                    
         DC    C'** ERROR - BAD RECORD TYPE'                                    
ERR02    DC    AL1(MISINVN),AL1(ERR03-ERR02)                                    
         DC    C'** ERROR - MISSING INVOICE NUMBER'                             
ERR03    DC    AL1(INVINVD),AL1(ERR04-ERR03)                                    
         DC    C'** ERROR - INVALID INVOICE DATE'                               
ERR04    DC    AL1(INVCLI),AL1(ERR05-ERR04)                                     
         DC    C'** ERROR - MISSING CLIENT CODE'                                
ERR05    DC    AL1(INVPRD),AL1(ERR06-ERR05)                                     
         DC    C'** ERROR - MISSING PRODUCT CODE'                               
ERR06    DC    AL1(INVIACC),AL1(ERR07-ERR06)                                    
         DC    C'** ERROR - INVALID INCOME ACCOUNT'                             
ERR07    DC    AL1(INVCACC),AL1(ERR08-ERR07)                                    
         DC    C'** ERROR - INVALID CLEARING ACCOUNT'                           
ERR08    DC    AL1(MISCACC),AL1(ERR09-ERR08)                                    
         DC    C'** ERROR - MISSING CLEARING ACCOUNT'                           
ERR09    DS    0C                                                               
         DC    X'00'                                                            
*                                                                               
ERRTAB2  DS    0H                                                               
ERR01A   DC    AL1(INVDUED),AL1(ERR02A-ERR01A)                                  
         DC    C'** ERROR - INVALID DUE DATE'                                   
ERR02A   DC    AL1(INVMOA),AL1(ERR03A-ERR02A)                                   
         DC    C'** ERROR - INVALID MONTH OF ACTIVITY'                          
ERR03A   DC    AL1(INVRACC),AL1(ERR04A-ERR03A)                                  
         DC    C'** ERROR - INVALID RECEIVABLE ACCOUNT'                         
ERR04A   DC    AL1(MISCHKN),AL1(ERR05A-ERR04A)                                  
         DC    C'** ERROR - MISSING CHECK NUMBER'                               
ERR05A   DC    AL1(INVCHKD),AL1(ERR06A-ERR05A)                                  
         DC    C'** ERROR - INVALID CHECK DATE'                                 
ERR06A   DC    AL1(INVDEPD),AL1(ERR07A-ERR06A)                                  
         DC    C'** ERROR - INVALID DEPOSIT DATE'                               
ERR07A   DC    AL1(MISBAMT),AL1(ERR08A-ERR07A)                                  
         DC    C'** ERROR - MISSING BILLING AMOUNT'                             
ERR08A   DC    AL1(INVCLPR),AL1(ERR09A-ERR08A)                                  
         DC    C'** ERROR - INVALID CLIENT PRODUCT RECORD'                      
ERR09A   DS    0C                                                               
         DC    X'00'                                                            
*                                                                               
ERRTAB3  DS    0H                                                               
ERR01B   DC    AL1(MISINCAC),AL1(ERR09B-ERR01B)                                 
         DC    C'** ERROR - MISSING INCOME ACCOUNT'                             
ERR09B   DS    0C                                                               
         DC    X'00'                                                            
*                                                                               
COMMLNER DS    0C                                                               
         DC    13CL1' '                                                         
         DC    CL4'CLI='                                                        
RCLIL    EQU   *-COMMLNER                                                       
COMCLIR  DC    CL3'   '                                                         
         DC    CL2', '                                                          
         DC    CL4'PRD='                                                        
RPRDL    EQU   *-COMMLNER                                                       
COMPRDR  DC    CL3'   '                                                         
         DC    CL2', '                                                          
         DC    CL9'DUE DATE='                                                   
RDUEL    EQU   *-COMMLNER                                                       
COMDUEDR DC    CL8'        '                                                    
COMRLEN  EQU   *-COMMLNER                                                       
*                                                                               
COMMLNEC DS    0C                                                               
         DC    13CL1' '                                                         
         DC    CL4'CLI='                                                        
CCLIL    EQU   *-COMMLNEC                                                       
COMCLIC  DC    CL3'   '                                                         
         DC    CL2', '                                                          
         DC    CL4'PRD='                                                        
CPRDL    EQU   *-COMMLNEC                                                       
COMPRDC  DC    CL3'   '                                                         
         DC    CL2', '                                                          
         DC    CL11'CHECK DATE='                                                
CCHKL    EQU   *-COMMLNEC                                                       
CHEKDTEC DC    CL8'        '                                                    
         DC    CL2', '                                                          
         DC    CL13'DEPOSIT DATE='                                              
CDEPL    EQU   *-COMMLNEC                                                       
CDEPDTE  DC    CL8'        '                                                    
COMCLEN  EQU   *-COMMLNEC                                                       
*                                                                               
MOSTAB   DS    0H                                                               
         DC    X'01F1'                                                          
         DC    X'02F2'                                                          
         DC    X'03F3'                                                          
         DC    X'04F4'                                                          
         DC    X'05F5'                                                          
         DC    X'06F6'                                                          
         DC    X'07F7'                                                          
         DC    X'08F8'                                                          
         DC    X'09F9'                                                          
         DC    X'10C1'                                                          
         DC    X'11C2'                                                          
         DC    X'12C3'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*-------------------------------------------------------------------*           
*        BOX ROUTINE TO FLOAT IN MIDLINE                            *           
*-------------------------------------------------------------------*           
BOXMID   NMOD1 0,*BOXMID*                                                       
         L     RC,BOXRC                                                         
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         ZIC   R2,LINE                                                          
         LA    R3,BOXROWS                                                       
         AR    R3,R2                                                            
         MVI   0(R3),C'M'                                                       
         XMOD1 1                                                                
*-------------------------------------------------------------------*           
*        BOX ROUTINE TO FLOAT IN BOTTOM                             *           
*-------------------------------------------------------------------*           
BOXBOT   NMOD1 0,*BOXBOT*                                                       
         L     RC,BOXRC                                                         
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         ZIC   R2,LINE                                                          
         LA    R3,BOXROWS                                                       
         AR    R3,R2                                                            
         MVI   0(R3),C'B'                                                       
         XMOD1 1                                                                
*-------------------------------------------------------------------*           
*        BOX ROUTINE                                                *           
*-------------------------------------------------------------------*           
BXHOOK   NMOD1 0,*BXHOOK*                                                       
         L     RC,BOXRC                                                         
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         MVC   BOXROWS,XSPACES                                                  
         MVC   BOXCOLS,XSPACES                                                  
         MVC   BOXCOLSR,XSPACES                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
         CLI   RCSUBPRG,0                                                       
         BNE   BX300                                                            
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+7,C'C'                                                   
         MVI   BOXCOLS+12,C'C'                                                  
         MVI   BOXCOLS+74,C'C'                                                  
         MVI   BOXCOLS+83,C'C'                                                  
         MVI   BOXCOLS+93,C'C'                                                  
         MVI   BOXCOLS+112,C'C'                                                 
         MVI   BOXCOLS+129,C'C'                                                 
         MVI   BOXCOLS+154,C'R'                                                 
         B     BX500                                                            
*                                                                               
BX300    DS    0H                                                               
         MVI   BOXROWS+3,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+36,C'C'                                                  
         MVI   BOXCOLS+55,C'R'                                                  
*                                                                               
BX500    DS    0H                                                               
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
TAPE     DCB   DDNAME=TAPEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               MACRF=GM,                                               X        
               EODAD=EOJ                                                        
*                                                                               
POSTBUFF DS    0D                     POSTING BUFFER                            
         DC    4500X'00'                                                        
         EJECT                                                                  
*-------------------------------------------------------------------*           
*              WORKING STORAGE                                      *           
*-------------------------------------------------------------------*           
ACRUD    DSECT                                                                  
         DS    0D                                                               
TRECORD  DS    0CL200                                                           
TRAGENCY DS    CL2                                                              
TAPTYPR  DS    CL1                                                              
TRINVNM  DS    CL6                                                              
TRINVDT  DS    CL6                                                              
TRCLI    DS    CL3                                                              
TRPRD    DS    CL3                                                              
TRBLAMT  DS    CL12                                                             
TRCMAMT  DS    CL12                                                             
TRCD     DS    CL10                                                             
TRSYS    DS    CL1                                                              
TRMED    DS    CL1                                                              
TRINUL   DS    CL2                                                              
TRINACT  DS    CL12                                                             
TRCLUL   DS    CL2                                                              
TRCLACT  DS    CL12                                                             
TRJOBN   DS    CL6                                                              
TRJOBDS  DS    CL36                                                             
TRPOSTI  DS    CL1                                                              
TSRLENGT EQU   *-TRAGENCY                                                       
TRDUEDT  DS    CL6                                                              
TRMONACT DS    CL4                                                              
TRSPEC   DS    CL12                                                             
TRRECUL  DS    CL2                                                              
TRRECAC  DS    CL12                                                             
         DS    CL36                                                             
TLRLENGT EQU   *-TRAGENCY                                                       
*                                                                               
         ORG   TRECORD                                                          
TCAGENCY DS    CL2                                                              
TAPTYPC  DS    CL1                                                              
TCINVNM  DS    CL6                                                              
TCINVDT  DS    CL6                                                              
TCCLI    DS    CL3                                                              
TCPROD   DS    CL3                                                              
TCBLAMT  DS    CL12                                                             
TCINUL   DS    CL2                                                              
TCINACT  DS    CL12                                                             
TCCLUL   DS    CL2                                                              
TCCLACT  DS    CL12                                                             
TCRECUL  DS    CL2                                                              
TCRECAC  DS    CL12                                                             
TCCHKNM  DS    CL6                                                              
TCCHKDT  DS    CL6                                                              
TCDEPDT  DS    CL6                                                              
         DS    CL35                                                             
TSCLENGT EQU   *-TCAGENCY                                                       
         DS    CL72                                                             
TLCLENGT EQU   *-TCAGENCY                                                       
*                                                                               
         DS    CL200                                                            
*                                                                               
ERRSW1   DS    CL1                                                              
BADRTYP  EQU   X'80'               BAD RECORD TYPE                              
MISINVN  EQU   X'40'               MISSING INVOICE NUMBER                       
INVINVD  EQU   X'20'               INVALID INVOICE DATE                         
INVCLI   EQU   X'10'               MISSING/INVALID CLIENT                       
INVPRD   EQU   X'08'               MISSING/INVALID PRODUCT                      
INVIACC  EQU   X'04'               INVALID INCOME ACCOUNT                       
INVCACC  EQU   X'02'               INVALID CLEARING ACCOUNT                     
MISCACC  EQU   X'01'               MISSING CLEARING ACCOUNT                     
*                                                                               
ERRSW2   DS    CL1                                                              
INVDUED  EQU   X'80'               INVALID DUE DATE                             
INVMOA   EQU   X'40'               INVALID MONTH OF ACTIVITY                    
INVRACC  EQU   X'20'               INVALID RECEIVABLE ACCOUNT                   
MISCHKN  EQU   X'10'               MISSING CHECK NUMBER                         
INVCHKD  EQU   X'08'               INVALID CHECK DATE                           
INVDEPD  EQU   X'04'               INVALID DEPOSIT DATE                         
MISBAMT  EQU   X'02'               MISSING BILLING AMOUNT                       
INVCLPR  EQU   X'01'               INVALID CLI/PRD REC                          
*                                                                               
ERRSW3   DS    CL1                                                              
MISINCAC EQU   X'80'               MISSING SI ACCOUNT FOR COMM POSTING          
*                                                                               
BADTAGN  DS    CL1                                                              
BADAGEN  EQU   X'01'                                                            
*                                                                               
BYTEV    DS    CL1                                                              
SAVOPT1  DS    CL1                                                              
LONGSHRT DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
ID       DS    CL16                                                             
*                                                                               
BILLAMT  DS    D                   BILL AMOUNT                                  
NETAMT   DS    D                   NET AMOUNT                                   
COMMAMT  DS    D                   COMMISSION AMOUNT                            
CDAMT    DS    D                   CASH DISCOUNT AMOUNT                         
SPECAMT  DS    D                   CASH DISCOUNT AMOUNT                         
ABXHOOK  DS    A                                                                
ABOXMID  DS    A                                                                
ABOXBOT  DS    A                                                                
ADBOX    DS    A                                                                
SAVRA    DS    F                                                                
RELO     DS    F                                                                
*                                                                               
*                      *** THIS ENTIRE AREA WILL BE CLEARED AFTER EACH          
PSTWORK  DS    0C      *** TAPE RECORD READ                                     
TMPDAT1  DS    CL6                 TEMP STORAGE FOR DATVAL                      
PSTMOA   DS    XL2                 MONTH OF ACTIVITY                            
         DS    XL1                 NEED XTRA BYTE FOR DATE CONVERSION           
PPSTMOA  DS    CL3                 PACKED MONTH OF ACTIVITY                     
PSTINVDT DS    CL3                 INVOICE DATE                                 
PSTOFFC  DS    CL2                 OFFICE CODE                                  
INVDPRT  DS    CL8                 INVOICE DATE (PRINTABLE FORM)                
DUEDTE   DS    CL2                 DUE DATE                                     
DUEDPRT  DS    CL8                 DUE DATE (PRINTABLE FORM)                    
DEPDTE   DS    CL3                 DEPOSIT DATE PWOS                            
DEPDTEPT DS    CL8                 DEPOSIT DATE PRINTABLE                       
CHKDTE   DS    CL3                 CHECK DATE PWOS                              
CHKDTEPT DS    CL8                 CHECK DATE PRINTABLE                         
BSOURCE  DS    CL12                RECEIVABLE ACCOUNT                           
SRACC    DS    CL15                RECEIVABLE ACCOUNT                           
SRACCNM  DS    CL36                RECEIVABLE ACCT NAME                         
SCACC    DS    CL15                CLEARING ACCOUNT                             
SCACCNM  DS    CL36                CLEARING ACCT NAME                           
SIACC    DS    CL15                INCOME ACCOUNT(BILLING SOURCE)               
SIACCNM  DS    CL36                INCOME ACCT NAME                             
SJACC    DS    CL15                                                             
SJCLINM  DS    CL36                                                             
PSTINVNM DS    CL6                                                              
PSTCLI   DS    CL3                                                              
PSTPROD  DS    CL3                                                              
PSTWKEND EQU   *-PSTWORK                                                        
*                                                                               
CHOPOUT  DS    0CL48                                                            
CHOPOUTA DS    CL12                                                             
CHOPOUTB DS    CL12                                                             
CHOPOUTC DS    CL12                                                             
CHOPOUTD DS    CL12                                                             
*                                                                               
RECTAB   DS    0C                                                               
RECIN    DS    PL4                 NUMBER AND TOTAL RECORDS IN                  
RECOUT   DS    PL4                 POSTING RECORDS OUT                          
RECINR   DS    PL4                 TOTAL R TYPE IN                              
RECINC   DS    PL4                 TOTAL C TYPE IN                              
RECOUTR  DS    PL4                 TOTAL R TYPE OUT(PROCESSED)                  
RECOUTC  DS    PL4                 TOTAL C TYPE OUT(PROCESSED)                  
ERCNT    DS    PL4                 ERROR COUNT                                  
RECTNUM  EQU   (*-RECTAB)/L'RECIN                                               
*                                                                               
CASHTAB  DS    0C                                                               
CSHIN    DS    PL8                 CASH IN                                      
CSHOUTD  DS    PL8                 CASH OUT DEBIT                               
CSHOUTC  DS    PL8                 CASH OUT CREDIT                              
BILIN    DS    PL8                 BILL AMOUNT IN                               
COMMIN   DS    PL8                 COMMISSION AMOUNT IN                         
CHEKIN   DS    PL8                 CHECK AMOUNT IN                              
BILOUT   DS    PL8                 BILL AMOUNT OUT                              
NETOUT   DS    PL8                 NET AMOUNT OUT                               
COMMOUT  DS    PL8                 COMMISSION AMOUNT OUT                        
CHEKOUT  DS    PL8                 CHECK AMOUNT OUT                             
CASHTNUM EQU   (*-CASHTAB)/L'CSHIN                                              
*                                                                               
ELEM1A   DS    CL(ACMTLNQ)                                                      
ELEM61   DS    CL(TRDULNQ)                                                      
ELEMD9   DS    CL113                                                            
*                                                                               
POSTHEAD DS    F                   RECORD HEADER                                
POSTAREA DS    CL500               AREA FOR POSTING                             
*                                                                               
IO       DS    CL2000                                                           
*                                                                               
*                                                                               
TAPPRNT  DSECT                                                                  
TPPLINE  DS    0CL198                                                           
TPITEM   DS    CL6                                                              
         DS    CL1                                                              
         DS    CL2                                                              
TPTYPE   DS    CL1                                                              
         DS    CL3                                                              
TPACCT   DS    CL14                                                             
         DS    CL4                                                              
         ORG   TPACCT+2                                                         
TPACCT2  DS    CL14                                                             
         DS    CL2                                                              
TPACNAME DS    CL36                                                             
         DS    CL9                                                              
TPINVNM  DS    CL6                                                              
         DS    CL3                                                              
TPDATE   DS    CL8                                                              
         DS    CL2                                                              
TPDEBIT  DS    CL16                                                             
         DS    CL2                                                              
TPCREDIT DS    CL16                                                             
         DS    CL2                                                              
TPMEMO   DS    CL7                                                              
         DS    CL16                                                             
         DS    CL68                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
*        ACMASTD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACGENFILE                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        ACBIGPRNTD                                                             
*        DDLOGOD                                                                
*        DDREMOTED                                                              
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDBIGBOX                                                               
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPRU02 05/01/02'                                      
         END                                                                    
