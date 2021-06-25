*          DATA SET ACREPP602  AT LEVEL 045 AS OF 07/23/13                      
*PHASE ACP602C                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE ACLIST                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'JOB POSTING DETAILS'                                            
ACP602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP6**,R9,RR=RE                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=GENERAL W/S                               
         LA    RC,SPACEND                                                       
         USING ACP6D,RC            RC=PROGRAM W/S                               
         ST    RE,PRELOC                                                        
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   UJ10                                                             
         L     RF,=A(BUFFALOC)                                                  
         A     RF,PRELOC                                                        
         ST    RF,ADBUFC                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',ADBUFC                                      
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   EXIT                                                             
         DC    H'0'                                                             
         DROP  R3                                                               
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
UJ10     CLI   MODE,REQFRST                                                     
         BNE   UJ20                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   REQAUTH,=P'0'                                                    
         ZAP   REQTOT,=P'0'                                                     
         ZAP   REQBAUTH,=P'0'                                                   
         ZAP   REQBTOT,=P'0'                                                    
         MVC   REQTOTS,=9PL8'0'                                                 
         MVC   TOTCHREQ,=9PL8'0'                                                
         MVC   OPTBLOC,SPACES                                                   
         MVC   OPTBLOC(4),=C'NONE'                                              
         CLC   QOPT1(3),SPACES                                                  
         BE    EXIT                                                             
         LA    RE,OPTBLOC                                                       
         CLI   QOPT1,C' '                                                       
         BE    UJ12                                                             
         MVC   0(10,RE),=C'AUTHORISED'                                          
         CLI   QOPT1,C'A'                                                       
         BE    *+10                                                             
         MVC   0(12,RE),=C'UNAUTHORISED'                                        
         LA    RE,15(RE)                                                        
UJ12     CLI   QOPT2,C' '                                                       
         BE    UJ14                                                             
         MVC   0(4,RE),=C'HELD'                                                 
         CLI   QOPT2,C'H'                                                       
         BE    *+10                                                             
         MVC   0(6,RE),=C'UNHELD'                                               
         LA    RE,15(RE)                                                        
UJ14     CLI   QOPT3,C' '                                                       
         BE    EXIT                                                             
         MVC   0(6,RE),=C'BILLED'                                               
         CLI   QOPT3,C'B'                                                       
         BE    EXIT                                                             
         MVC   0(8,RE),=C'UNBILLED'                                             
         B     EXIT                                                             
         EJECT                                                                  
UJ20     CLI   MODE,LEVAFRST                                                    
         BNE   UJ25                                                             
         MVC   CLITOTS,=9PL8'0'                                                 
         MVC   TOTCHCLI,=9PL8'0'                                                
         USING RUNXTRAD,RF                                                      
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC   IF W/C LIST IN EFFECT                        
         BZ    *+8                                                              
         MVI   PROGPROF+1,C'N'     THEN FORCE NO DETAIL                         
         B     EXIT                                                             
         DROP  RF                                                               
         SPACE 2                                                                
UJ25     CLI   MODE,LEVBFRST                                                    
         BNE   UJ30                                                             
         MVC   PRDTOTS,=9PL8'0'                                                 
         MVC   TOTCHPRD,=9PL8'0'                                                
         B     EXIT                                                             
         EJECT                                                                  
UJ30     CLI   MODE,PROCACC                                                     
         BNE   UJ45                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         ZAP   AMOUNT,=P'0'                                                     
         ZAP   WCTOT,=P'0'                                                      
         ZAP   JOBTOT,=P'0'                                                     
         ZAP   JOBAUTH,=P'0'                                                    
         ZAP   ORDTOT,=P'0'                                                     
         ZAP   WCCT,=P'0'                                                       
         ZAP   WCAUTH,=P'0'                                                     
         LA    RF,12                                                            
         L     RE,ATABLE                                                        
         MVC   0(200,RE),SPACES                                                 
         LA    RE,200(RE)                                                       
         BCT   RF,*-10                                                          
         L     RE,ATABLE                                                        
         MVI   0(RE),X'FF'                                                      
         MVI   JACTIV,C'N'                                                      
         MVI   DRSW,C'N'                                                        
         MVI   WANT,C'Y'                                                        
         BAS   RE,FILTER                                                        
         CLI   WANT,C'Y'                                                        
         BNE   EXIT                                                             
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFC,(X'80',1)                          
         MVC   BUFACCS,=9PL8'0'                                                 
         MVC   SVANAL,SPACES                                                    
         MVC   JOBTOTS,=9PL8'0'                                                 
         MVC   TOTCHJOB,=9PL8'0'                                                
*                                                                               
         BAS   RE,LOOKUP                                                        
         MVI   HITS,0                                                           
         USING JBLOCK,R5                                                        
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BE    UJ34                YES                                          
         LH    R2,JBNROWS                                                       
*                                                                               
         USING JBCOLD,R3                                                        
UJ31     CLI   JBCOLTYP,JBCOLTWC                                                
         BNE   UJ32                                                             
         MVC   SVANAL,JBCOLWC                                                   
         MVC   LISTCODE,SVANAL                                                  
         BAS   RE,LISTCHK                  FILTER OUT UNWANTED CODES            
         CLI   DMCB,C'E'                                                        
         BE    UJ32                                                             
         MVI   HITS,1              MEANS MATCHING WC FOUND                      
         BAS   RE,GETWNAME                                                      
         MVC   BUFNAME,WORK                                                     
         MVC   BUFKEY,SVANAL       AND ADD TO BUFFALO FOR JOB SUMMARY           
         ZAP   BUFACCS1,JBCOLVAL                                                
         ZAP   BUFACCS2,JBCOLVAL+6(6)                                           
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         AP    JOBTOTS(8),BUFACCS1                                              
         AP    JOBTOTS+8(8),BUFACCS2                                            
*                                                                               
UJ32     AH    R3,JBLCOL                                                        
         BCT   R2,UJ31                                                          
*                                                                               
UJ33     MVC   SVANAL,SPACES                                                    
         CLC   QOPT1(3),SPACES     UNLESS FILTERING ON BILLED/HOLD              
         BNE   EXIT                ESTIMATE MEANS ACTIVE                        
         CLI   HITS,0              PROVIDED WE HAD A MATCH                      
         BE    EXIT                WITH LIST RECORD                             
         MVI   JACTIV,C'Y'                                                      
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
         USING MJETABD,R3                                                       
UJ34     CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    UJ33                YES                                          
         CLI   MJETTYP,MJETTWQ     LOOK FOR WORKCODES                           
         BNE   UJ35                                                             
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   UJ35                                                             
         MVC   SVANAL,MJETWCD                                                   
         MVC   LISTCODE,SVANAL                                                  
         BAS   RE,LISTCHK                  FILTER OUT UNWANTED CODES            
         CLI   DMCB,C'E'                                                        
         BE    UJ32                                                             
         MVI   HITS,1              MEANS MATCHING WC FOUND                      
         BAS   RE,GETWNAME                                                      
         MVC   BUFNAME,WORK                                                     
         MVC   BUFKEY,SVANAL       AND ADD TO BUFFALO FOR JOB SUMMARY           
         ZAP   BUFACCS1,MJETVAL                                                 
         ZAP   BUFACCS2,MJETVAL+6(6)                                            
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         AP    JOBTOTS(8),BUFACCS1                                              
         AP    JOBTOTS+8(8),BUFACCS2                                            
*                                                                               
UJ35     XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     UJ34                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
         USING TRANSD,R2                                                        
UJ45     CLI   MODE,ANALFRST                                                    
         BNE   UJ50                                                             
         CLI   WANT,C'Y'                                                        
         BNE   EXIT                                                             
         MVI   ANALPEND,0                                                       
         L     R2,ADTRANS                                                       
         MVC   LISTCODE,TRNSANAL                                                
         BAS   RE,LISTCHK                                                       
         CLI   DMCB,C'E'                                                        
         BE    EXIT                                                             
         MVC   BUFACCS,=9PL8'0'                                                 
         CLC   TRNSANAL,=C'99'                                                  
         BNE   *+10                                                             
         MVC   TOTCHJOB,JOBTOTS                                                 
         CLC   TRNSANAL,=C'98'                                                  
         BL    EXIT                                                             
         CLI   DRSW,C'Y'           ALREADY DONE DEBIT LINE                      
         BE    EXIT                                                             
         CLC   JOBTOT(12),=2PL6'0'                                              
         BE    EXIT                EXIT IF NO NET AND AUTH                      
         MVI   DRSW,C'Y'                                                        
         GOTO1 MYREPORT                                                         
         MVC   P+40(18),=C'TOTAL FOR INVOICES'                                  
         BAS   RE,DRLINE                                                        
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
UJ50     CLI   MODE,PROCTRNS                                                    
         BNE   UJ100                                                            
         CLI   WANT,C'Y'                                                        
         BNE   EXIT                                                             
         USING TRANSD,R2                                                        
         L     R2,ADTRANS                                                       
         CLI   0(R2),X'44'                                                      
         BNE   EXIT                                                             
         MVC   LISTCODE,TRNSANAL                                                
         BAS   RE,LISTCHK                                                       
         CLI   DMCB,C'E'                                                        
         BE    EXIT                                                             
UJ500    MVI   NUMOF77,0                                                        
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         MVI   BILLED,C'N'                                                      
         ZAP   BILLAMT,=P'0'                                                    
         AP    WCCT,=P'1'                                                       
         L     RF,ADSUBAC                                                       
         MVC   WORK,SPACES                                                      
         USING TRSUBHD,RF                                                       
         MVC   WORK(12),TRSBACNT+3 CHOP SUPPLIER NAME AND NO                    
         ZIC   RE,TRSBLEN                                                       
         SH    RE,=H'18'                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+13(0),TRSBNAME                                              
         GOTO1 =V(SQUASHER),DMCB,WORK,50,RR=PRELOC                              
         L     RF,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((RF),WORK),(22,PLSUPP),(C'P',2)                    
         MVC   SVANAL,TRNSANAL                                                  
         CLC   TRNSANAL,=C'**'     ORDER                                        
         BE    UJ60                                                             
         MVC   PLINV,TRNSREF                                                    
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,PLIDTE)                              
         TM    TRNSSTAT,X'80'      REVERSE SIGN OF NON-BILL CREDITS             
         BO    UJ50AA                                                           
         CLC   TRNSANAL,=C'99'                                                  
         BE    UJ50AA                                                           
         MP    TRNSAMNT,=P'-1'                                                  
UJ50AA   ZAP   AMOUNT,TRNSAMNT                                                  
         ZAP   BUFACCS5,TRNSAMNT                                                
         CLC   TRNSANAL,=C'99'     BRANCH IF NOT A BILL                         
         BNE   UJ51                                                             
         CLI   PROGPROF+1,C'Y'     AND UNLESS 'ALL-JOB' PROFILE IS SET          
         BE    UJ50AC                                                           
         CLI   JACTIV,C'Y'         IGNORE IF NO NON-BILL TRANSACTIONS           
         BNE   UJ96                HAVE BEEN SELECTED                           
UJ50AC   AP    REQBTOT,AMOUNT                                                   
         ZAP   BUFACCS6,TRNSNARR+15(6)  COMMISSION                              
         ZAP   BUFACCS7,BUFACCS6                                                
         AP    BUFACCS7,BUFACCS5        PLUS NET = GROSS                        
         ZAP   BUFACCS4,BUFACCS5   NET BILLED AMOUNT FOR 99                     
         MVC   PLINV,SPACES                                                     
         MVC   PLBILL,TRNSREF                                                   
         MVC   PLBDTE,PLIDTE                                                    
         MVC   PLIDTE,SPACES                                                    
         MVC   PLSUPP,SPACES                                                    
         MVC   PSECOND,SPACES                                                   
         L     RE,ATABLE           FIND BILL NO IN TABLE                        
UJ50A    CLI   0(RE),X'FF'         AND SHOW ALLOC AMOUNT                        
         BE    UJ52                                                             
         CLC   PLBILL,0(RE)                                                     
         BE    UJ50C                                                            
         LA    RE,12(RE)                                                        
         B     UJ50A                                                            
UJ50C    MVC   0(6,RE),SPACES      REMOVE ENTRY                                 
         EDIT  (P6,6(RE)),(10,PLALLOC),2,MINUS=YES                              
         AP    WCAUTH,DUB                                                       
         AP    REQBAUTH,DUB                                                     
         ZAP   DUB1,AMOUNT                                                      
         SP    DUB1,DUB                                                         
         EDIT  (P8,DUB1),(10,PLBAL),2,MINUS=YES                                 
         B     UJ52                                                             
*                                                                               
UJ51     ZAP   BUFACCS7,BUFACCS5                                                
         ZAP   BUFACCS6,=P'0'                                                   
         TM    TRNSSTAT,X'01'      NON-COMMISSIONABLE                           
         BO    UJ51A                                                            
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         MVC   GOSELWC,TRNSANAL                                                 
         L     R6,AMONACC                                                       
         USING ACMD,R6                                                          
         MVC   GOAKEY,ACMALTN                                                   
         DROP  R6                                                               
         GOTO1 GETOPT,DMCB,(R7)                                                 
         XC    GOAKEY,GOAKEY                                                    
         XC    GOSELWC,GOSELWC                                                  
         ZAP   PL13,BUFACCS5       NOW COMMISSION                               
         MP    PL13,GOAGYCOM                                                    
         SRP   PL13,64-6,5         RATE IS 4 DEC PL                             
         ZAP   BUFACCS6,PL13       COMMISSION                                   
UJ51A    AP    BUFACCS7,BUFACCS6   NET PLUS COMM = GROSS                        
         ZAP   BUFACCS8,BUFACCS5                                                
         LA    R4,P                                                             
*                                                                               
UJ52     CLI   QOPT1,C' '                                                       
         BE    UJ54                                                             
         CLI   QOPT1,C'A'          AUTH ONLY                                    
         BNE   UJ53                                                             
         TM    TRNSSTAT,X'08'                                                   
         BZ    UJ96                                                             
         B     UJ54                                                             
UJ53     TM    TRNSSTAT,X'08'      IGNORE AUTH                                  
         BO    UJ96                                                             
UJ54     TM    TRNSSTAT,X'08'                                                   
         BZ    *+8                                                              
         MVI   PLAPP,C'A'                                                       
*                                                                               
         CLI   QOPT2,C' '                                                       
         BE    UJ56                                                             
         CLI   QOPT2,C'H'          HELD ONLY                                    
         BNE   UJ55                                                             
         TM    TRNSSTAT,X'04'                                                   
         BZ    UJ96                                                             
         B     UJ56                                                             
UJ55     TM    TRNSSTAT,X'04'      IGNORE HELD                                  
         BO    UJ96                                                             
UJ56     TM    TRNSSTAT,X'04'                                                   
         BZ    *+8                                                              
         MVI   PLHOLD,C'H'                                                      
         EDIT  TRNSAMNT,(10,PLNET),2,MINUS=YES                                  
         B     UJ70                                                             
         SPACE 2                                                                
UJ60     CLI   QOPT6,C'S'          OPTION TO SUPPRESS JOBS                      
         BNE   UJ61                WITH OUTSATNDING ORDERS                      
         MVI   WANT,C'N'                                                        
         B     EXIT                                                             
UJ61     CLI   QOPT2,C'H'          ORDERS - DONT SHOW IF HELD ONLY              
         BE    EXIT                                                             
         CLI   QOPT3,C'B'          OR BILLED ONLY                               
         BE    EXIT                                                             
         MVC   PLORD,TRNSREF       ORDER DATA                                   
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(8,PLIDTE)                              
UJ62     CLI   0(R2),0                                                          
         BE    UJ90                                                             
         CLI   0(R2),X'67'                                                      
         BE    UJ66                                                             
         CLI   0(R2),X'68'                                                      
         BE    UJ68                                                             
UJ64     ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     UJ62                                                             
         USING ACORDRD,R2                                                       
UJ66     MVC   PLAUTH,ACORAUTH                                                  
         B     UJ64                                                             
*                                                                               
         USING ACOAMTD,R2                                                       
UJ68     SP    ACOAMT,ACOAIVAL     SHOW WHAT IS LEFT OF ORDER                   
         EDIT  ACOAMT,(10,PLNET),2,MINUS=YES,BRACKET=YES                        
         AP    ORDTOT,ACOAMT                                                    
         ZAP   BUFACCS3,ACOAMT                                                  
         MVC   PLINV(4),=C'W/C='   SHOW MAJOR WORK CODE                         
         MVC   PLINV+4(2),ACOAWC                                                
         B     UJ64                                                             
*                                                                               
UJ70     CLC   SVANAL,=C'99'                                                    
         BE    UJ94                                                             
         XC    SJHOURS,SJHOURS                                                  
         LA    R3,ALLOCS                                                        
         LA    R1,MAX77                                                         
         MVC   0(ALOCLEN,R3),SPACES                                             
         LA    R3,ALOCLEN(R3)                                                   
         BCT   R1,*-10                                                          
         XC    NUMOF77(9),NUMOF77                                               
UJ71     CLI   0(R2),0             LOOK FOR EXTRA ELEMENTS                      
         BE    UJ82                                                             
         CLI   0(R2),X'25'         ORDER NUMBER                                 
         BE    UJ74                                                             
         CLI   0(R2),X'4D'         AUTHORISER                                   
         BE    UJ76                                                             
         CLI   0(R2),X'50'         SJ HOURS IF TYPE IS 'T'                      
         BE    UJ78                                                             
         CLI   0(R2),X'23'         SUBREF ELEMENT                               
         BE    UJ80                                                             
UJ72     ZIC   RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     UJ71                                                             
*                                                                               
         USING ACNOD,R2                                                         
UJ74     MVC   PLORD,ACNO                                                       
         B     UJ72                                                             
*                                                                               
         USING TRAUTHD,R2                                                       
UJ76     MVC   PLAUTH,TRAUCODE                                                  
         B     UJ72                                                             
*                                                                               
         USING TRCASHD,R2                                                       
UJ78     CLI   TRCSTYPE,C'T'                                                    
         BNE   UJ72                                                             
         EDIT  (P6,TRCSAMNT),(10,SJHOURS),2,ALIGN=LEFT                          
         B     UJ72                                                             
*                                                                               
         USING ACOTHERD,R2                                                      
UJ80     MVC   PSECOND+24(6),ACOTNUM                                            
         B     UJ72                                                             
*                                                                               
         USING ACMD,R2                                                          
UJ82     L     R2,AMONACC                                                       
         L     R2,ACMAPRO2                                                      
         LA    R3,ALLOCS                                                        
*                                                                               
         USING PTAELD,R2                                                        
         USING ALLOCS,R3                                                        
UJ83     CLI   0(R2),PTAELQ        NO PTA'S OR ALL DONE                         
         BNE   UJ88                                                             
         CLI   PTASTAT1,PTASPEND   SKIP PENDING                                 
         BE    UJ84                                                             
         CLI   PTASTAT1,PTASREVS   SKIP REVERSED                                
         BE    UJ84                                                             
         CLI   PTASTAT1,PTASREVU   SKIP REVERSALS                               
         BE    UJ84                                                             
         CLI   PTATYPE,PTATWOF     TAKE WRITE-OFFS                              
         BE    UJ85                                                             
         CLI   PTATYPE,PTATWOFR    RECOVERIES                                   
         BE    UJ85                                                             
         CLI   PTATYPE,PTATRAL     AND BILLING                                  
         BE    UJ86                                                             
*                                                                               
UJ84     SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         AR    R2,RF                                                            
         B     UJ83                                                             
*                                                                               
UJ85     MVC   ALOCNUM,PTAWREF                                                  
         GOTO1 DATCON,DMCB,(1,PTAWDAT),(2,ALOCDTE)                              
         B     UJ87                                                             
*                                                                               
UJ86     MVC   ALOCNUM,PTARBLNO                                                 
         MVC   ALOCDTE,PTARBLDT                                                 
*                                                                               
UJ87     MVI   BILLED,C'Y'                                                      
         ZAP   ALOCAMT,PTANET                                                   
         AP    BILLAMT,PTANET                                                   
         SR    RF,RF                                                            
         IC    RF,NUMOF77                                                       
         LA    RF,1(RF)                                                         
         LA    RE,MAX77                                                         
         CR    RF,RE                                                            
         BH    UJ88                                                             
         STC   RF,NUMOF77                                                       
         LA    R3,ALOCLEN(R3)                                                   
         B     UJ84                                                             
         DROP  R3                                                               
*                                                                               
UJ88     ZAP   BUFACCS4,BILLAMT                                                 
         AP    WCAUTH,BILLAMT      WORK CODE AUTHORISED AMOUNT                  
         CP    BILLAMT,AMOUNT                                                   
         BE    UJ90                                                             
         CP    BILLAMT,=P'0'       ZERO = NO BILLING                            
         BE    UJ90                                                             
         MVI   BILLED,C'P'         PARTIAL                                      
         B     UJ90                                                             
*                                                                               
UJ90     CLI   QOPT3,C' '                                                       
         BE    UJ94                                                             
         CLI   QOPT3,C'B'          BILLED ONLY                                  
         BNE   UJ92                                                             
         CLI   BILLED,C'N'                                                      
         BE    UJ96                                                             
         B     UJ94                                                             
UJ92     CLI   BILLED,C'Y'         IGNORE BILLED                                
         BNE   UJ94                                                             
         SP    WCAUTH,BILLAMT      REMOVE WHAT WE HAVE ALREADY ADDED            
         B     UJ95A               AND ADD TO BILL TABLE                        
UJ94     CLI   ANALPEND,0          WORK CODE PRINT PENDING                      
         BNE   UJ95                                                             
         MVI   ANALPEND,1                                                       
         MVC   SAVEP,P                                                          
         MVC   P,SPACES                                                         
         MVC   SAVEP2,PSECOND                                                   
         MVC   PSECOND,SPACES                                                   
         GOTO1 MYREPORT                                                         
         MVC   P+1(2),SVANAL                                                    
         BAS   RE,GETWNAME                                                      
         MVC   P+4(15),WORK                                                     
         LA    RE,P+20                                                          
         LA    RF,PSECOND+20                                                    
         LA    R1,20                                                            
UJ94A    CLI   0(RE),C' '          UNDERLINE WORK CODE NAME                     
         BE    *+8                                                              
         MVI   0(RF),C'-'                                                       
         BCTR  RE,0                                                             
         BCTR  RF,0                                                             
         BCT   R1,UJ94A                                                         
         GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
*                                                                               
         MVC   P,SAVEP                                                          
         MVC   PSECOND,SAVEP2                                                   
UJ95     CLC   SVANAL,=C'99'                                                    
         BE    UJ954                                                            
         CLI   NUMOF77,0           IF NONE BILLED                               
         BE    UJ953               DON'T ADD IT UP                              
         LA    R3,ALLOCS                                                        
         USING ALLOCS,R3                                                        
         ZIC   R6,NUMOF77                                                       
         CLI   PROGPROF+2,C'Y'                                                  
         BE    UJ952                                                            
*                                  PRINT ONLY LATEST BILL                       
         XC    LATEST,LATEST                                                    
UJ951    CLC   LATEST+6(2),ALOCDTE                                              
         BH    UJ951A              GET LATEST BILL                              
         MVC   LATEST,ALOCNUM                                                   
         LA    R3,ALOCLEN(R3)                                                   
UJ951A   BCT   R6,UJ951                                                         
         ZAP   DUB,BILLAMT                                                      
         LA    RF,DUB              BILLED AMOUNT AND                            
         LA    R5,LATEST+6         DATE                                         
         BAS   RE,UJ95Z                                                         
         MVC   PLBILL,LATEST       BILL NO.                                     
         B     UJ953                                                            
*                                  PRINT ALL BILLS                              
UJ952    BCTR  R6,0                REDUCE COUNT...LAST ONE HAS BAL.             
         LA    RF,ALOCAMT                                                       
         LA    R5,ALOCDTE          BILL DATE                                    
         BAS   RE,UJ95Z                                                         
         MVC   PLBILL,ALOCNUM      BILL NUMBER                                  
         LA    R3,ALOCLEN(R3)                                                   
         LTR   R6,R6               JUST IN CASE THERE WAS                       
         BZ    UJ953               ONLY ONE '4B' ELEM.                          
         GOTO1 MYREPORT                                                         
         B     UJ952                                                            
UJ95Z    NTR1                                                                   
         EDIT  (P8,0(RF)),(10,PLALLOC),2,MINUS=YES                              
         GOTO1 DATCON,DMCB,(2,(R5)),(8,PLBDTE)                                  
         B     EXIT                                                             
         DROP  R3                                                               
*                                  PRINT ONLY/LAST LINE WITH BAL.               
UJ953    CLC   SVANAL,=C'**'                                                    
         BE    UJ954               NO BALANCE FOR ORDERS                        
         ZAP   DOUBLE,AMOUNT                                                    
         SP    DOUBLE,BILLAMT      SHOW UNBILLED                                
         EDIT  (P8,DOUBLE),(10,PLBAL),2,MINUS=YES                               
UJ954    GOTO1 MYREPORT                                                         
         CLI   PROGPROF+3,C'Y'     PRINT SJ HOURS ON NEXT LINE                  
         BNE   UJ954A                                                           
         OC    SJHOURS,SJHOURS     NONE ANYWAY                                  
         BZ    UJ954A                                                           
         MVC   P+1(6),=C'HOURS='                                                
         MVC   P+7(L'SJHOURS),SJHOURS                                           
         GOTO1 MYREPORT                                                         
         XC    SJHOURS,SJHOURS                                                  
UJ954A   AP    WCTOT,AMOUNT                                                     
         MVI   JACTIV,C'Y'                                                      
         CLI   BILLED,C'N'         POP BILL INTO TABLE                          
         BE    UJ96                                                             
UJ95A    LA    R3,ALLOCS                                                        
         USING ALLOCS,R3                                                        
         ZIC   R6,NUMOF77                                                       
UJ95A1   L     RE,ATABLE                                                        
         LA    RF,200              MAX NO OF BILLS PER JOB                      
UJ95B    CLI   0(RE),X'FF'                                                      
         BE    UJ95C                                                            
         CLC   ALOCNUM,0(RE)                                                    
         BE    UJ95E                                                            
         LA    RE,12(RE)                                                        
         BCT   RF,UJ95B                                                         
         B     UJ95F                                                            
UJ95C    MVI   12(RE),X'FF'        NEW ENTRY                                    
         MVC   0(6,RE),ALOCNUM                                                  
         ZAP   DUB,ALOCAMT                                                      
         ZAP   6(6,RE),DUB                                                      
         B     UJ95F                                                            
UJ95E    ZAP   DUB,ALOCAMT         EXISTING ENTRY                               
         AP    6(6,RE),DUB                                                      
UJ95F    LA    R3,ALOCLEN(R3)                                                   
         BCT   R6,UJ95A1           GO FOR NEXT BILL                             
         DROP  R3                                                               
UJ96     MVC   P,SPACES                                                         
         CLC   BUFACCS,=9PL8'0'                                                 
         BE    EXIT                                                             
         MVC   BUFKEY,SVANAL                                                    
         BAS   RE,GETWNAME                                                      
         MVC   BUFNAME,WORK                                                     
         CLC   SVANAL,=C'99'       EXCEPT FOR PREV BILLING                      
         BE    *+10                DEDUCE UNBILLED CHARGES                      
         SP    BUFACCS8,BUFACCS4                                                
         GOTO1 BUFFALO,DMCB,=C'PUT',ADBUFC,BUFREC                               
         LA    RE,JOBTOTS                                                       
         LA    R1,BUFACCS                                                       
         LA    RF,9                                                             
UJ98     CLC   SVANAL,=C'99'       ADD TO JOB TOTS                              
         BNE   UJ99                UNLESS BILLS - THEN SUBTRACT                 
         SP    0(8,RE),0(8,R1)                                                  
         B     UJ99A                                                            
UJ99     AP    0(8,RE),0(8,R1)                                                  
UJ99A    LA    RE,8(RE)                                                         
         LA    R1,8(R1)                                                         
         BCT   RF,UJ98                                                          
         MVC   BUFACCS,=9PL8'0'                                                 
         B     EXIT                                                             
         EJECT                                                                  
UJ100    CLI   MODE,ANALLAST                                                    
         BNE   UJ150                                                            
         CLI   WANT,C'Y'                                                        
         BNE   EXIT                                                             
         L     R2,ADTRANS                                                       
         MVC   LISTCODE,TRNSANAL-TRANSD(R2)                                     
         BAS   RE,LISTCHK                                                       
         CLI   DMCB,C'E'                                                        
         BE    EXIT                                                             
         CLC   SVANAL,=C'**'                                                    
         BNE   UJ101                                                            
         CP    ORDTOT,=P'0'                                                     
         BE    UJ106                                                            
         CLI   QOPT3,C'B'          CAN'T HAVE BILLED ORDER TOTAL                
         BE    EXIT                                                             
         B     UJ101A                                                           
UJ101    CP    WCTOT,=P'0'                                                      
         BNE   UJ101A                                                           
         CP    WCAUTH,=P'0'                                                     
         BE    UJ106                                                            
UJ101A   GOTO1 MYREPORT                                                         
         MVC   P+40(19),=C'TOTAL FOR WORK-CODE'                                 
         BAS   RE,GETWNAME                                                      
         MVC   P+61(2),SVANAL                                                   
         MVC   P+64(15),WORK                                                    
         CLC   SVANAL,=C'**'                                                    
         BE    UJ102                                                            
         ZAP   DOUBLE,WCAUTH                                                    
         CP    WCAUTH,=P'0'                                                     
         BE    UJ101C                                                           
         EDIT  WCAUTH,(10,P+90),2,MINUS=YES,BRACKET=YES                         
         CLC   SVANAL,=C'99'                                                    
         BNE   *+10                                                             
         MP    WCAUTH,=P'-1'                                                    
         AP    JOBAUTH,WCAUTH                                                   
         ZAP   WCAUTH,=P'0'                                                     
UJ101C   EDIT  WCTOT,(10,P+80),2,MINUS=YES,BRACKET=YES                          
         ZAP   DUB1,DUB            SHOW BALANCE                                 
         SP    DUB1,DOUBLE                                                      
         CLC   SVANAL,=C'99'                                                    
         BNE   *+10                                                             
         MP    DUB,=P'-1'                                                       
         ZAP   WCTOT,DUB                                                        
         AP    JOBTOT,WCTOT                                                     
         ZAP   WCTOT,=P'0'                                                      
         EDIT  (P8,DUB1),(10,P+100),2,MINUS=YES,BRACKET=YES                     
         B     UJ104                                                            
UJ102    EDIT  ORDTOT,(10,P+80),2,MINUS=YES,BRACKET=YES                         
         ZAP   ORDTOT,=P'0'                                                     
UJ104    DS    0H                                                               
         CP    WCCT,=P'1'                                                       
         BNH   UJ106                                                            
         GOTO1 MYREPORT                                                         
UJ106    MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         ZAP   WCCT,=P'0'                                                       
         B     EXIT                                                             
         EJECT                                                                  
UJ150    CLI   MODE,ACCLAST                                                     
         BNE   UJ200                                                            
         CLI   WANT,C'Y'                                                        
         BNE   EXIT                                                             
         CLI   JACTIV,C'N'         IGNORE INACTIVE JOBS UNLESS                  
         BNE   UJ151               THE PROFILE IS SET                           
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   EXIT                                                             
UJ151    L     R2,ATABLE           PRINT PHANTOM BILLS FROM TABLE               
         LA    R4,P                                                             
         USING PLINED,R4                                                        
         SR    R3,R3                                                            
UJ152    CLI   0(R2),X'FF'                                                      
         BE    UJ154                                                            
         LA    R3,1(R3)            COUNT ENTRIES                                
         LA    R2,12(R2)                                                        
         B     UJ152                                                            
UJ154    L     R2,ATABLE                                                        
         LTR   R3,R3                                                            
         BZ    UJ170                                                            
         GOTO1 MYREPORT                                                         
         GOTO1 XSORT,DMCB,(0,(R2)),(R3),12,6,0                                  
*                                                                               
UJ160    CLI   0(R2),X'FF'                                                      
         BE    UJ170                                                            
         CLC   0(6,R2),SPACES      USED ENTRY                                   
         BE    UJ162                                                            
         MVC   PLBILL,0(R2)                                                     
         EDIT  (P6,6(R2)),(10,PLALLOC),2,MINUS=YES                              
         SP    JOBAUTH,DUB                                                      
         GOTO1 MYREPORT                                                         
UJ162    LA    R2,12(R2)                                                        
         B     UJ160                                                            
UJ170    DS    0H                                                               
         AP    REQTOT,JOBTOT                                                    
         AP    REQAUTH,JOBAUTH                                                  
         CLI   QOPT3,C'B'          JOB LINE - MEANINGLESS ON BILLED OPT         
         BE    UJ172                                                            
         GOTO1 MYREPORT                                                         
         MVC   P+40(13),=C'TOTAL FOR JOB'                                       
         BAS   RE,DRLINE                                                        
         GOTO1 MYREPORT                                                         
         B     UJ174                                                            
UJ172    MVC   P,SPACES                                                         
UJ174    BAS   RE,WCSUM            DO WORK CODE SUMMARY                         
         BAS   RE,COMMPRT                                                       
         B     EXIT                                                             
         EJECT                                                                  
UJ200    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         MVI   RCSUBPRG,0                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         MVC   P+40(27),=C'TOTAL FOR REQUEST - BILLING'                         
         ZAP   JOBAUTH,REQBAUTH                                                 
         ZAP   JOBTOT,REQBTOT                                                   
         BAS   RE,DRLINE                                                        
         GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
         MVC   P+40(17),=C'TOTAL FOR REQUEST'                                   
         ZAP   JOBTOT,REQTOT                                                    
         ZAP   JOBAUTH,REQAUTH                                                  
         BAS   RE,DRLINE                                                        
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              WORK CODE SUMMARY                                                
         SPACE 2                                                                
WCSUM    NTR1                                                                   
         MVI   RCSUBPRG,1                                                       
         CLI   LINE,52                                                          
         BL    *+12                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     WCS10                                                            
         MVC   P+1(17),=C'WORK-CODE SUMMARY'                                    
         MVC   PSECOND+1(17),=17C'-'                                            
         GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
WCS10    MVI   FORCEMID,C'Y'                                                    
         GOTO1 MYREPORT                                                         
         MVI   CHRGPEND,C'Y'                                                    
         MVC   PRTSWS2,=9C'N'      CLEAR PRINT SWITCHES                         
         XC    BUFKEY,BUFKEY                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',ADBUFC,BUFKEY,1                            
WCS20    TM    DMCB+8,X'80'                                                     
         BO    WCS80                                                            
         CLC   BUFKEY,=C'99'                                                    
         BNE   WCS30                                                            
         GOTO1 MYREPORT                                                         
         LA    R2,TOTCHJOB                                                      
         MVC   P+2(15),=C'*TOTAL CHARGES*'                                      
         MVI   SPACING,2                                                        
         LA    R4,PRTSWS2          JOB SUMMARY PRINT SWITCHES                   
         BAS   RE,TOTALS                                                        
         MVI   CHRGPEND,C'N'                                                    
WCS30    CLI   QOPT3,C'U'          OR SUPPRESS BILLED ITEMS                     
         BNE   WCS50                                                            
WCS40    CLC   BUFACCS3(56),=7PL8'0' AND IF NO BALANCES ON JOB                  
         BNE   WCS50                                                            
         SP    JOBTOTS(8),BUFACCS1 THEN DO NOT INCLUDE IN SUMMARY               
         SP    JOBTOTS+8(8),BUFACCS2                                            
         SP    TOTCHJOB(8),BUFACCS1                                             
         SP    TOTCHJOB+8(8),BUFACCS2                                           
         B     WCS72                                                            
WCS50    MVC   P+1(2),BUFKEY                                                    
         MVC   P+4(15),BUFNAME                                                  
         LA    RF,P+19                                                          
         LA    R2,BUFACCS                                                       
         LA    R4,PRTSWS2          SUMMARY PRINT SWITCHES                       
         LA    RE,8                                                             
WCS60    CP    0(8,R2),=P'0'                                                    
         BE    WCS70                                                            
         MVI   0(R4),C'Y'          TURN ON PRINT SWITCH                         
         EDIT  (P8,0(R2)),(11,0(RF)),2,FLOAT=-                                  
WCS70    LA    RF,11(RF)                                                        
         LA    R2,8(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   RE,WCS60                                                         
*                                                                               
         GOTO1 MYREPORT                                                         
WCS72    GOTO1 BUFFALO,DMCB,=C'SEQ',ADBUFC,BUFKEY,1                             
         B     WCS20                                                            
*                                                                               
WCS80    GOTO1 MYREPORT                                                         
         CLI   CHRGPEND,C'Y'                                                    
         BNE   WCS90                                                            
         MVC   P+2(15),=C'*TOTAL CHARGES*'                                      
         LA    R2,TOTCHJOB                                                      
         MVI   SPACING,2                                                        
         LA    R4,PRTSWS2                                                       
         BAS   RE,TOTALS                                                        
WCS90    MVC   P+2(15),=C'**JOB BALANCE**'                                      
         MVI   BALANCE,C'Y'                                                     
         LA    R2,JOBTOTS                                                       
         LA    R4,PRTSWS2                                                       
         CLC   0(9*8,R2),=9PL8'0'                                               
         BNE   WCS94                                                            
         LA    R2,P+19                                                          
         LA    RF,8                                                             
WCS92    MVC   0(11,R2),=C'       NIL '                                         
         LA    R2,11(R2)                                                        
         BCT   RF,WCS92                                                         
         GOTO1 MYREPORT                                                         
         B     WCS96                                                            
WCS94    BAS   RE,TOTALS                                                        
WCS96    MVI   BALANCE,C'N'                                                     
         GOTO1 (RF),(R1),=C'CLEAR',ADBUFC,(X'80',1)                             
         MVI   RCSUBPRG,0                                                       
         B     EXIT                                                             
         EJECT                                                                  
*              PRINT HIGHER LEVEL TOTAL LINES                                   
         SPACE 2                                                                
TOTALS   NTR1                                                                   
         CLC   0(9*8,R2),=9PL8'0'                                               
         BE    TOTS9                                                            
         LA    RF,8                                                             
         LA    R3,P+19                                                          
TOTS2    CLI   0(R4),C'Y'          ANYTHING TO PRINT                            
         BNE   TOTS4                                                            
         EDIT  (P8,(R2)),(11,(R3)),2,FLOAT=-                                    
         CLI   MODE,REQLAST                                                     
         BE    TOTS4                                                            
         AP    9*8(8,R2),0(8,R2)    ADD TO NEXT LEVEL UP                        
         MVI   9(R4),C'Y'          TURN ON NEXT HIGHEST LEVEL                   
TOTS4    LA    R3,11(R3)                                                        
         LA    R2,8(R2)                                                         
         LA    R4,1(R4)                                                         
         BCT   RF,TOTS2                                                         
TOTS9    GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
DRLINE   NTR1                      DO JOB TOTALS (PRE AND POST BILLING)         
         EDIT  JOBTOT,(10,P+80),2,MINUS=YES                                     
         ZAP   DUB1,DUB                                                         
         CP    DUB,=P'0'                                                        
         BNE   *+10                                                             
         MVC   P+80(10),SPACES                                                  
         EDIT  JOBAUTH,(10,P+90),2,MINUS=YES                                    
         SP    DUB1,DUB                                                         
         CP    DUB,=P'0'                                                        
         BNE   *+10                                                             
         MVC   P+90(10),SPACES                                                  
         EDIT  (P8,DUB1),(10,P+100),2,MINUS=YES                                 
         CP    DUB,=P'0'                                                        
         BNE   *+10                                                             
         MVC   P+100(10),SPACES                                                 
         B     EXIT                                                             
         EJECT                                                                  
FILTER   NTR1                                                                   
         CLI   QOPT4,C' '          CLOSED OPTION                                
         BE    FLT10                                                            
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLI   QOPT4,C'S'                                                       
         BE    FLT2                                                             
         TM    ACSTSTAT,X'40'      CLOSED ONLY                                  
         BZ    FLTNO                                                            
         B     FLT10                                                            
FLT2     TM    ACSTSTAT,X'40'      SUPPRESS CLOSED                              
         BO    FLTNO                                                            
*                                                                               
FLT10    CLI   QOPT5,C' '          ZERO BALANCE OPTION                          
         BE    EXIT                                                             
         L     R2,ADACCBAL                                                      
         USING ACBALD,R2                                                        
         CLI   QOPT5,C'Z'                                                       
         BE    FLT20                                                            
         CP    ACBLDR,ACBLCR                                                    
         BE    FLTNO               SUPPRESS ZERO BALANCE JOBS                   
         B     EXIT                                                             
FLT20    CP    ACBLDR,ACBLCR                                                    
         BNE   FLTNO               ZERO BALANCE JOBS ONLY                       
         B     EXIT                                                             
*                                                                               
FLTNO    MVI   WANT,C'N'                                                        
         B     EXIT                                                             
         EJECT                                                                  
*              PRINTING OF COMMENTS                                             
         SPACE 2                                                                
COMMPRT  NTR1                                                                   
         L     R2,ADPROFIL         COMPOSITE PROFILE (X'24') EL                 
         USING ACPROFD,R2                                                       
         CLC   ACPRUNBL,SPACES                                                  
         BE    CM01                                                             
         OC    ACPRUNBL,ACPRUNBL                                                
         BZ    CM01                                                             
         BAS   RE,MYREPORT                                                      
         LA    R3,ACPRUNBL                                                      
         MVC   P+1(17),=C'UNBILLABLE CODES='                                    
         LA    R4,P+18                                                          
         LA    R5,6                                                             
         SPACE 1                                                                
CM01A    MVC   0(2,R4),0(R3)                                                    
         MVI   2(R4),C','                                                       
         CLI   1(R3),C' '                                                       
         BNE   *+12                                                             
         BCTR  R4,0                                                             
         MVC   2(2,R4),=C','                                                    
         LA    R4,3(R4)                                                         
         LA    R3,2(R3)                                                         
         CLI   0(R3),C' '                                                       
         BE    *+8                                                              
         BCT   R5,CM01A                                                         
         BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         BAS   RE,MYREPORT                                                      
         SPACE 1                                                                
CM01     CLI   PROGPROF,C'Y'                                                    
         BNE   EXIT                                                             
         CLI   ACPRBLPR,X'41'                                                   
         BL    CM02                                                             
         BAS   RE,MYREPORT                                                      
         MVC   P+1(14),=C'PRINT ON BILLS'                                       
         MVC   PSECOND+1(14),=19C'-'                                            
         BAS   RE,MYREPORT                                                      
         MVC   P+1(50),ACPRBLPR                                                 
         BAS   RE,MYREPORT                                                      
CM02     CLI   ACPRLEN,105         OTHER CHAT                                   
         BE    CM06                                                             
         BAS   RE,MYREPORT                                                      
         MVC   P+1(17),=C'OTHER INFORMATION'                                    
         MVC   PSECOND+1(17),=19C'-'                                            
         ZIC   RF,LINE             CHECK FOR ENOUGH ROOM                        
         LA    RF,6(RF)                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,MYREPORT                                                      
         MVC   P+1(50),ACPRNARR                                                 
         CLI   ACPRLEN,155                                                      
         BE    CM04                                                             
         MVC   PSECOND+1(50),ACPRNARR+50                                        
         CLI   ACPRLEN,205                                                      
         BE    CM04                                                             
         MVC   PTHIRD+1(50),ACPRNARR+100                                        
CM04     BAS   RE,MYREPORT                                                      
*                                                                               
CM06     MVI   COMMSW,C'N'                                                      
         MVI   CPSW,C'N'                                                        
         SR    R3,R3                                                            
         L     RF,ADACC            SAVE JOB KEY                                 
         MVC   SAVEKEY,0(RF)                                                    
         LR    R2,RF                                                            
         AH    R2,DATADISP                                                      
CM2      CLI   0(R2),0                                                          
         BE    CMXT                                                             
         CLI   0(R2),X'3E'                                                      
         BE    CM6                                                              
CM4      IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     CM2                                                              
         SPACE 1                                                                
         USING ACOMMD,R2                                                        
CM6      CLI   ACOMTYPE,C'M'       OLD TYPE                                     
         BE    CM8                                                              
         CLI   ACOMTYPE,0          BRANCH IF IT IS NOT PURE DATA                
         BNE   CM10                                                             
         SPACE 1                                                                
CM8      CLI   CPSW,C'Y'           HAVE I PRINTED HEADING                       
         BE    CM9                                                              
         BAS   RE,MYREPORT                                                      
         MVC   P+1(8),=C'COMMENTS'                                              
         MVC   PSECOND+1(8),=19C'-'                                             
         ZIC   RF,LINE             CHECK FOR ENOUGH ROOM                        
         LA    RF,6(RF)                                                         
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BL    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,MYREPORT                                                      
         MVI   CPSW,C'Y'                                                        
CM9      IC    R3,ACOMLEN                                                       
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),ACOMMENT                                                  
         BAS   RE,MYREPORT                                                      
         B     CM4                                                              
         SPACE 3                                                                
CM10     DS    0H                                                               
         CLI   COMMBYTE,1          BEFORE                                       
         BE    CM12                                                             
         TM    ACOMTYPE,X'04'                                                   
         BZ    CM4                                                              
         B     CM14                                                             
CM12     TM    ACOMTYPE,X'08'      BEFORE                                       
         BZ    CM4                                                              
CM14     DS    0H                                                               
         XC    COMMKEY,COMMKEY     READ A STANDARD COMMENT RECORD               
         MVI   COMMKEY,X'0C'                                                    
         MVC   COMMKEY+1(1),QCOMPANY                                            
         MVC   COMMKEY+2(6),ACOMMENT                                            
         MVI   COMMSW,C'Y'                                                      
         L     R4,=A(COMMBUFF)                                                  
         A     R4,PRELOC                                                        
         MVC   KEYSAVE,COMMKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',COMMKEY,(R4)                     
         CLC   KEYSAVE,0(R4)                                                    
         BNE   CM4                 DIDN'T READ IT                               
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
         LR    RE,R4                                                            
         SR    RF,RF               NOW COUNT THE NUMER OF LINES                 
CM15     CLI   0(RE),0             IN THE COMMENT                               
         BE    CM16                                                             
         CLI   0(RE),X'3E'                                                      
         BNE   *+8                                                              
         LA    RF,1(RF)                                                         
         IC    R3,1(RE)                                                         
         AR    RE,R3                                                            
         B     CM15                                                             
         SPACE 1                                                                
CM16     IC    R3,MAXLINES         SEE IF ALL THE COMMENT WILL FIT              
         SR    RE,RE                                                            
         IC    RE,LINE                                                          
         SR    R3,RE                                                            
         CR    RF,R3               NEEDED VS WHAT IS LEFT                       
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
CM22     SR    R3,R3                                                            
         CLI   0(R4),0                                                          
         BE    CM4                                                              
         CLI   0(R4),X'3E'                                                      
         BE    CM26                                                             
CM24     IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     CM22                                                             
         SPACE 1                                                                
CM26     CLI   CPSW,C'Y'           HAVE I PRINTED HEADING                       
         BE    CM27                                                             
         MVC   P+1(8),=C'COMMENTS'                                              
         MVC   PSECOND+1(8),=19C'-'                                             
         BAS   RE,MYREPORT                                                      
         MVI   CPSW,C'Y'                                                        
         DROP  R2                                                               
         USING ACOMMD,R4                                                        
CM27     IC    R3,ACOMLEN                                                       
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),ACOMMENT                                                  
         BAS   RE,MYREPORT                                                      
         B     CM24                GET NEXT ELEMENT                             
         SPACE 2                                                                
CMXT     CLI   COMMSW,C'N'         DID WE ACTUALLY READ 'ACCOUNT'               
         BE    CMXT2                                                            
         L     R4,=A(COMMBUFF)                                                  
         A     R4,PRELOC                                                        
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEKEY,(R4)                     
CMXT2    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*              FILL HEADLINES AND PRINT                                         
         SPACE 2                                                                
MYREPORT NTR1                                                                   
         CLI   MODE,REQLAST                                                     
         BE    MYRPTX                                                           
         GOTO1 =V(ACCDIV),DMCB,ADLDGHIR,ADACC,ACDIVWK,RR=RB                     
         LA    RF,ACDIVWK                                                       
         MVC   HEAD5+10(6),1(RF)   CLIENT CODE                                  
         MVC   HEAD6+10(6),14(RF)  PRODUCT CODE                                 
         MVC   HEAD7+10(6),27(RF)  JOB CODE                                     
*                                                                               
         L     R2,ADLVANAM                                                      
         LA    R3,HEAD5+17                                                      
         BAS   RE,GETNAME                                                       
         L     R2,ADLVBNAM                                                      
         LA    R3,HEAD6+17                                                      
         BAS   RE,GETNAME                                                       
         L     R2,ADACCNAM                                                      
         LA    R3,HEAD7+17                                                      
         BAS   RE,GETNAME                                                       
         LA    RE,OPTBLOC                                                       
         LA    RF,3                                                             
         LA    R1,HEAD5+87                                                      
MYRPT2   MVC   0(15,R1),0(RE)                                                   
         LA    RE,15(RE)                                                        
         LA    R1,132(R1)                                                       
         BCT   RF,MYRPT2                                                        
MYRPTX   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 2                                                                
GETNAME  ZIC   RF,1(R2)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R3),2(R2)                                                    
         EJECT                                                                  
*              DIG OUT WORK-CODE NAME AND PUT IN WORK                           
         SPACE 2                                                                
GETWNAME NTR1                                                                   
         L     RF,ADLEDGER                                                      
         AH    RF,DATADISP                                                      
         SR    RE,RE                                                            
         MVC   WORK(15),=CL15'MISSING'                                          
GETW2    CLI   0(RF),0                                                          
         BE    EXIT                                                             
         CLI   0(RF),X'12'                                                      
         BE    GETW6                                                            
GETW4    IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     GETW2                                                            
*                                                                               
         USING ACANALD,RF                                                       
GETW6    CLC   SVANAL,ACANCODE                                                  
         BNE   GETW4                                                            
         MVC   WORK(15),ACANDESC                                                
         B     EXIT                                                             
         SPACE 2                                                                
         GETEL R2,DATADISP,ELCODE                                               
         EJECT                                                                  
         USING RUNXTRAD,RF                                                      
LISTCHK  NTR1                                                                   
         MVI   DMCB,0                                                           
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC           ANY LIST RECORD                      
         BZ    EXIT                                                             
         GOTO1 =V(ACLIST),DMCB,VLISTREC,LISTCODE,RR=RB                          
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                        BAD LIST RECORD                      
         B     EXIT                                                             
         EJECT                                                                  
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'OE,CE'                                                         
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
ATABLE   DC    A(TABLE)                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 1                                                                
TABLE    DS    200CL12                                                          
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
ACP6D    DSECT                                                                  
DUB1     DS    D                                                                
PRELOC   DS    F                                                                
ADBUFC   DS    A                                                                
BUFREC   DS    0CL89                                                            
BUFKEY   DS    CL2                                                              
BUFNAME  DS    CL15                                                             
BUFACCS  DS    0CL72                                                            
BUFACCS1 DS    PL8                 ORIGINAL ESTIMATE                            
BUFACCS2 DS    PL8                 PRESENT ESTIMATE                             
BUFACCS3 DS    PL8                 ORDERED                                      
BUFACCS4 DS    PL8                 NET(US) - BILLED(UK)                         
BUFACCS5 DS    PL8                 COMM(US) - NET(UK)                           
BUFACCS6 DS    PL8                 GROSS(US) - COMM(UK)                         
BUFACCS7 DS    PL8                 CD(US) - GROSS(UK)                           
BUFACCS8 DS    PL8                 BILLED(US) - UNBILLED CHARGES(UK)            
BUFACCS9 DS    PL8                 UNBILLED CHARGES(US) - NOT USED (UK)         
ACDIVWK  DS    CL52                FOR ACCDIV                                   
REQTOT   DS    PL6                                                              
REQAUTH  DS    PL6                                                              
REQBTOT  DS    PL6                                                              
REQBAUTH DS    PL6                                                              
AMOUNT   DS    PL6                                                              
WCTOT    DS    PL6                                                              
JOBTOT   DS    PL6                                                              
JOBAUTH  DS    PL6                                                              
ORDTOT   DS    PL6                                                              
JOBTOTS  DS    CL72                                                             
PRDTOTS  DS    CL72                                                             
CLITOTS  DS    CL72                                                             
REQTOTS  DS    CL72                                                             
TOTCHJOB DS    CL72                                                             
TOTCHPRD DS    CL72                                                             
TOTCHCLI DS    CL72                                                             
TOTCHREQ DS    CL72                                                             
SVANAL   DS    CL2                                                              
JACTIV   DS    CL1                                                              
WCCT     DS    PL3                                                              
ELCODE   DS    CL1                                                              
COMMSW   DS    CL1                                                              
COMMBYTE DS    CL1                                                              
CPSW     DS    CL1                                                              
SAVEKEY  DS    CL49                                                             
COMMKEY  DS    CL49                                                             
BILLED   DS    CL1                                                              
OPTBLOC  DS    CL45                                                             
ANALPEND DS    CL1                                                              
SAVEP2   DS    CL132                                                            
SAVEP    DS    CL132                                                            
WCAUTH   DS    PL6                                                              
BILLNO   DS    CL6                                                              
BILLAMT  DS    PL6                                                              
WANT     DS    CL1                                                              
DRSW     DS    CL1                                                              
CHRGPEND DS    CL1                                                              
BALANCE  DS    CL1                                                              
PRTSWS2  DS    CL9                                                              
PL13     DS    PL13                                                             
SAVER3   DS    A                                                                
NUMOF77  DS    C                                                                
LATEST   DS    CL8             LATEST BILL NO. AND DATE                         
ALLOCS   DS    0H                                                               
ALOCNUM  DS    CL6             BILL NO.                                         
ALOCDTE  DS    CL2             BILL DATE                                        
ALOCAMT  DS    PL8             BILL AMOUNT                                      
ALOCLEN  EQU   *-ALLOCS                                                         
         ORG   ALLOCS                                                           
MAX77    EQU   20                                                               
         DS    (MAX77)XL(ALOCLEN)                                               
SJHOURS  DS    CL10                                                             
LISTCODE DS    CL2                                                              
HITS     DS    C                                                                
         EJECT                                                                  
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
*              DSECT FOR PRINT-LINE                                             
         SPACE 2                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PLSUPP   DS    CL22                                                             
         DS    CL1                                                              
PLINV    DS    CL6                                                              
         DS    CL1                                                              
PLIDTE   DS    CL7                                                              
         DS    CL1                                                              
PLORD    DS    CL6                                                              
         DS    CL1                                                              
PLAUTH   DS    CL15                                                             
         DS    CL1                                                              
PLAPP    DS    CL1                                                              
         DS    CL1                                                              
PLHOLD   DS    CL1                                                              
         DS    CL1                                                              
PLBILL   DS    CL6                                                              
         DS    CL1                                                              
PLBDTE   DS    CL7                                                              
PLNET    DS    CL10                                                             
PLALLOC  DS    CL10                                                             
PLBAL    DS    CL10                                                             
         EJECT                                                                  
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
         BUFF  LINES=200,ROWS=1,COLUMNS=9,FLAVOR=PACKED,KEYLIST=(2,A),CX        
               OMMENT=15                                                        
         SPACE 1                                                                
COMMBUFF CSECT                                                                  
         DS    1000C                                                            
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACREPP602 07/23/13'                                      
         END                                                                    
