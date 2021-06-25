*          DATA SET ACREPBU02  AT LEVEL 013 AS OF 12/11/09                      
*PHASE ACBU02A                                                                  
*INCLUDE CONVMOS                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE BUFFERIN                                                               
ACBU02   TITLE '- BUCKET UPDATER'                                               
ACBU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACBU**,R9,R8                                                 
         USING ACWORKD,RA                RA=A(GLOBAL W/S)                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC                  RC=A(LOCAL W/S)                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCACC                                                     
         BE    ACCF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST FOR REQUEST                                                   *         
***********************************************************************         
REQF     LA    R1,HOOK                   A(HEADLINE HOOK ROUTINE)               
         ST    R1,HEADHOOK                                                      
         L     R1,ADBXAREA                                                      
         USING BOXD,R1                   INITIALISE BOX                         
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXCOLS+(LINBXL-LIND),C'L'                                       
         MVI   BOXCOLS+(LINBX1-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX2-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX3-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX4-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX5-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX6-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX7-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX8-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX9-LIND),C'C'                                       
         MVI   BOXCOLS+(LINBX10-LIND),C'C'                                      
         MVI   BOXCOLS+(LINBXR-LIND),C'R'                                       
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+06,C'T'                                                  
         MVI   BOXROWS+09,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         DROP  R1                                                               
*                                                                               
         MVI   RCSUBPRG,0                SET SUBPROGRAM                         
         MVI   FLGS,0                                                           
         MVI   FCRESET,C'Y'                                                     
*                                                                               
         L     R2,ADCMPEL                                                       
         USING CPYELD,R2                                                        
         TM    CPYSTAT4,CPYSOFF2         TEST NEW OFFICES                       
         BZ    *+8                                                              
         OI    FLGS,OFF2                                                        
*                                                                               
         CLI   QOPT1,C'Y'                PRINT ALL BUCKETS                      
         BNE   *+8                                                              
         OI    FLGS,PRNTALL                                                     
*                                                                               
         CLI   QOPT2,C'Y'                PRINT NET CHANGE LINE                  
         BNE   *+8                                                              
         OI    FLGS,PRNTNET                                                     
*                                                                               
         CLI   QOPT3,C'N'                IGNORE SPECIAL BUCKETS                 
         BNE   *+8                                                              
         OI    FLGS,NOSPCL                                                      
*                                                                               
         XC    STRTMOS,STRTMOS           DEFAULT MOS RANGE                      
         MVI   ENDMOS,X'FF'                                                     
         CLC   QMOSSTRT,SPACES                                                  
         BE    REQF3                                                            
         MVC   WORK(4),QMOSSTRT                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   STRTMOS,WORK+6                                                   
*                                                                               
REQF3    CLC   QMOSEND,SPACES                                                   
         BE    REQF5                                                            
         MVC   WORK(4),QMOSEND                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,WORK+6)                                  
         MVC   ENDMOS,WORK+6                                                    
*                                                                               
REQF5    DS    0H                                                               
         MVI   LENCON,0                                                         
         CLC   QCUL,SPACES          ANY CONTRA FILTER                           
         BE    REQF7                                                            
         LA    R2,QCACCT+L'QCACCT-1                                             
         LA    R1,14                                                            
         CLI   0(R2),C' '                                                       
         BH    *+12                                                             
         BCTR  R2,0                                                             
         BCT   R1,*-10                                                          
         DC    H'0'                                                             
         STC   R1,LENCON            SAVE LENGTH FOR COMPARE                     
*                                                                               
REQF7    B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
LDGF     MVI   FORCEHED,C'Y'                                                    
         MVI   LDGINDS,0                                                        
         L     R4,ADLEDGER                                                      
         USING LDGRECD,R4                                                       
*                                                                               
LDGF01   CLC   LDGKUNT(2),PRODUL         PRODUCTION                             
         BNE   *+8                                                              
         OI    LDGINDS,LDGWC             WORKCODE IN KEY                        
         CLC   LDGKUNT(2),PCTUL          PROJECT CONTROL                        
         BNE   *+8                                                              
         OI    LDGINDS,LDGWC             WORKCODE IN KEY                        
*                                                                               
         CLC   LDGKUNT(2),COSTUL         COST LEDGER                            
         BNE   *+8                                                              
         OI    LDGINDS,LDGCOST                                                  
         CLC   LDGKUNT(2),PERSUL         PERSON LEDGER                          
         BNE   *+8                                                              
         OI    LDGINDS,LDGPERS                                                  
*                                                                               
         L     R4,ADLDGEL                                                       
         USING LDGELD,R4                                                        
         TM    LDGSTAT,LDGSCOKE                                                 
         BZ    *+8                                                              
         OI    LDGINDS,LDGCOKE           SET COKE LEDGER                        
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST FOR ACCOUNT                                                   *         
***********************************************************************         
ACCF     GOTO1 BUFFERIN,DMCB,('BUFFAINI',BUFF),(0,0),ADCOMFAC                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,ABUKBUF                CLEAR BUCKET BUFFER                    
         XC    0(BUKBHDRL,R1),0(R1)                                             
*                                                                               
         MVI   PRTINDS,0                                                        
         XC    BBFDATE,BBFDATE           ACCOUNT BBF DATE                       
*                                                                               
         ICM   R2,15,ADACCBAL                                                   
         BZ    XIT                                                              
         USING ABLELD,R2                                                        
*                                                                               
         ICM   R2,15,ADACCSTA            ACCOUNT STATUS                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R2                                                        
         MVC   BBFDATE,RSTBDATE          SAVE BBF DATE                          
*                                                                               
         LA    R2,DKEY                                                          
         USING ACTRECD,R2                                                       
         L     R1,ADACC                                                         
         MVC   ACTKEY,0(R1)                                                     
         BAS   RE,DMRD                   READ ACCOUNT                           
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    LKEY,LKEY                 CLEAR LAST KEY                         
*                                                                               
ACCF03   BAS   RE,DMSEQ                                                         
         LA    R2,DIR                                                           
         CLC   ACTKCULA,DKEY             TEST SAME ACCOUNT                      
         BNE   ACCF15                                                           
         USING OFARECD,R2                TEST FOR OFFICE ACCOUNT                
         CLC   OFAKEY+OFAKEND(L'OFAKEY-OFAKEND),SPACES                          
         BE    ACCF03                    SKIP OFFICE ACCOUNTS                   
*                                                                               
         USING CACRECD,R2                                                       
         CLC   CACKOFF,=C'**'            IGNORE ORDERS                          
         BE    ACCF03                                                           
         CLC   CACKUNT(2),COSTUL         FOR 1C                                 
         BNE   *+14                                                             
         CLC   CACKCUNT(2),=C'14'        SKIP 14, 15, 16..                      
         BNL   ACCF03                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,LENCON                                                      
         BZ    ACCF04                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CACKULC(0),QCUL                                                  
         BNE   ACCF03                                                           
*                                                                               
ACCF04   OC    LKEY,LKEY                 TEST FIRST CONTRA                      
         BZ    ACCF05                                                           
         CLC   CACKEY(CACKSPAC-CACKEY),LKEY TEST CHANGE OF CONTRA               
         BE    ACCF07                                                           
         BAS   RE,TBUK                   POST TRANSACTION TOTALS                
*                                                                               
ACCF05   MVC   LKEY,CACKEY                                                      
         L     R1,ABUKBUF                CLEAR BUCKET BUFFER                    
         XC    0(BUKBHDRL,R1),0(R1)                                             
*                                                                               
ACCF07   MVC   AIO,AIO1                                                         
         BAS   RE,DMGETR                 GET THE RECORD                         
         L     R2,AIO                                                           
         LA    R3,CACRFST                                                       
         CLI   0(R3),BUKELQ              LOOK FOR BUCKET                        
         BE    ACCF09                                                           
         CLI   0(R3),PBKELQ              OR PRIOR BUCKET                        
         BNE   ACCF11                                                           
ACCF09   CLC   CACKSTYP,SPACES           TEST BUCKET SUB-TYPE                   
         BNE   ACCF03                    YES, CAN'T HANDLE IT                   
         BAS   RE,CAC                    POST BUCKET ELEMENTS                   
         B     ACCF03                                                           
*                                                                               
         USING TRNRECD,R2                                                       
ACCF11   L     R2,AIO                                                           
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
         CLI   TRNEL,TRNELQ                                                     
         BNE   ACCF13                                                           
         BAS   RE,TRNS                   PROCESS A TRANSACTION                  
         B     ACCF03                                                           
*                                                                               
ACCF13   BAS   RE,TMS                    PROCESS A TIME RECORD                  
         B     ACCF03                                                           
*                                                                               
ACCF15   BAS   RE,TBUK                   PROCESS LAST CONTRA                    
         BAS   RE,GETBUF                 GET RECORDS FROM BUFFERIN              
         TM    PRTINDS,PRTCBOX           DID WE PRINT ANYTHING ?                
         BZ    XIT                       NO, OK TO EXIT                         
         L     RE,ADBXAREA               LINE FOR END OF ACCOUNT                
         MVI   BOXREQ-BOXD(RE),C'B'                                             
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LAST FOR REQUEST                                                    *         
***********************************************************************         
REQL     TM    PRTINDS,PRTCBOX           DID WE PRINT ANYTHING ?                
         BZ    XIT                       NO, OK TO EXIT                         
         MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         GOTO1 ACREPORT                                                         
         L     RF,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RF),C'C'                                             
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* LAST FOR RUN                                                        *         
***********************************************************************         
RUNL     MVC   P,SPACES                                                         
         MVC   PSECOND,SPACES                                                   
         L     RF,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RF),C' '                                             
         MVI   BOXYORN-BOXD(RF),C'N'                                            
         GOTO1 ACREPORT                                                         
*                                                                               
RUNL1    MVI   RCSUBPRG,1                REMOVE BOXES AND HEADLINES             
         MVI   FORCEHED,C'Y'                                                    
         LA    R3,CNT                                                           
*                                                                               
RUNL3    MVC   P+1(30),4(R3)             EDIT THE RECORDS COUNTS                
         EDIT  (P4,0(R3)),(7,P+36),ZERO=NOBLANK                                 
         GOTO1 ACREPORT                                                         
         LA    R3,L'CNT(R3)                                                     
         CLI   0(R3),X'FF'                                                      
         BNE   RUNL3                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* POST BUCKET RECORD TO BUFFER                                        *         
***********************************************************************         
CAC      NTR1  ,                                                                
         L     R4,AIO                                                           
         USING CACRECD,R4                                                       
         TM    FLGS,NOSPCL              SPECIAL BUCKETS ?                       
         BNO   *+12                                                             
         CLI   CACKBTYP,C' '                                                    
         BH    XIT                                                              
         XC    WREC(WKLNQ),WREC          SET BUFFERIN KEY                       
         MVC   WCAC,CACKCULC             CONTRA ACCOUNT                         
         MVC   WOFC,CACKOFF              OFFICE                                 
         MVC   WTYP,CACKBTYP             TYPE                                   
         ZAP   WTDR,PZERO                                                       
         ZAP   WTCR,PZERO                                                       
         ZAP   WBDR,PZERO                                                       
         ZAP   WBCR,PZERO                                                       
*                                                                               
         SR    R0,R0                                                            
         LA    R2,CACRFST                                                       
CAC3     CLI   0(R2),0                   EOR ?                                  
         BE    XIT                                                              
         CLI   0(R2),BUKELQ                                                     
         BNE   CAC5                                                             
         USING BUKELD,R2                                                        
         MVC   WPERD,BUKYEAR             YEAR/MONTH                             
         XC    WPERDH,WPERDH                                                    
         ZAP   WBDR,BUKDR                DEBIT                                  
         ZAP   WBCR,BUKCR                CREDITS                                
         B     CAC7                                                             
*                                                                               
CAC5     CLI   0(R2),PBKELQ              PRIOR BUCKET ?                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PBKELD,R2                                                        
         MVC   WPERD,PBKLOW              LOW MONTH                              
         MVC   WPERDH,PBKHI              HI MONTH                               
         ZAP   WBDR,PBKDR                DEBIT                                  
         ZAP   WBCR,PBKCR                CREDITS                                
*                                                                               
CAC7     GOTO1 BUFFERIN,DMCB,('BUFFAPUT',BUFF),(0,WREC),ADCOMFAC                
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    R0,1(R2)                  BUMP TO NEXT ELEMENT                   
         AR    R2,R0                                                            
         B     CAC3                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* POST TRANSACTION TO BUCKET BUFFERS                                  *         
***********************************************************************         
TRNS     NTR1  ,                                                                
         L     R2,AIO                                                           
         USING TRNRECD,R2                                                       
         MVC   OFC,TRNKOFF                                                      
         TM    TRNRSTAT,TRNSDRFT         SKIP DRAFT TRANSACTIONS                
         BNZ   TRNSX                                                            
         TM    TRNRSTA2,TRNSPEEL         AND PEELED TRANSACTIONS                
         BNZ   TRNSX                                                            
         LA    R3,TRNRFST                                                       
         USING TRNELD,R3                                                        
*                                                                               
         MVC   MOS,TRNDATE                                                      
         CLI   TRNKUNT,C'1'              FOR UNITS 1 AND 2                      
         BL    TRNS2                                                            
         CLI   TRNKUNT,C'2'                                                     
         BH    TRNS2                                                            
         CLI   TRNTYPE,2                 SOME BATCH TRANSFER TYPES              
         BE    TRNS6                                                            
         CLI   TRNTYPE,5                                                        
         BE    TRNS6                                                            
         CLI   TRNTYPE,18                                                       
         BL    TRNS2                                                            
         CLI   TRNTYPE,20                                                       
         BNH   TRNS6                     TRANSACTION DATE IS MOS                
*                                                                               
TRNS2    GOTO1 CONVMOS,DMCB,(X'FE',TRNELD),MOS                                  
*                                                                               
TRNS6    LA    R4,BUKELEM                                                       
         USING BUKELD,R4                 BUILD BUCKET ELEMENT                   
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,MOS                                                       
         ZAP   BUKDR,PZERO                                                      
         ZAP   BUKCR,PZERO                                                      
*                                                                               
         MVI   BUKTYPE,C' '                                                     
         LA    R1,BUKDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    R1,BUKCR                                                         
         AP    0(L'BUKDR,R1),TRNAMNT                                            
         BAS   RE,UPDBUK                 UPDATE BUCKET ELEMENT                  
*                                                                               
TRNS8    LA    R5,TRNELD                                                        
         USING SCIELD,R5                                                        
TRNS10   SR    R0,R0                                                            
         IC    R0,SCILN                  BUMP TO NEXT ELEMENT                   
         AR    R5,R0                                                            
         CLI   SCIEL,0                   TEST E-O-R                             
         BE    TRNSX                                                            
         CLI   SCIEL,SCIELQ                                                     
         BNE   TRNS10                                                           
         TM    FLGS,NOSPCL               IGNORE SPECIAL BUCKETS                 
         BO    TRNS10                                                           
*                                                                               
         ZAP   BUKDR,PZERO               SUBSIDIARY CASH ELEMENTS               
         ZAP   BUKCR,PZERO                                                      
         L     R1,ABUKTAB                                                       
         USING BUKTABD,R1                R1=A(BUCKET TYPE TABLE)                
TRNS12   CLI   BUKTCTRY,BUKTEOTQ         TEST E-O-T                             
         BE    TRNS10                                                           
         CLI   BUKTCTRY,CTRYANY          TEST ANY COUNTRY VALID                 
         BE    TRNS16                                                           
         TM    BUKTCTRY,CTRYNOT          TEST 'NOT' COUNTRY                     
         BZ    TRNS14                                                           
         MVC   BYTE,BUKTCTRY             EXTRACT COUNTRY FROM TABLE             
         NI    BYTE,255-CTRYNOT          TURN OFF 'NOT' BIT                     
         CLC   BYTE,RCCTRY               MATCH ON COUNTRY CODE                  
         BNE   TRNS16                    CARRY ON IF NOT EQUAL                  
         B     TRNS18                                                           
TRNS14   CLC   BUKTCTRY,RCCTRY           MATCH ON COUNTRY CODE                  
         BNE   TRNS18                                                           
TRNS16   CLC   BUKTITYP,SCITYPE          MATCH ON BUCKET TYPE                   
         BNE   TRNS18                                                           
         CLC   BUKTLEDG,SPACES           SPECIAL LEDGER IN TABLE?               
         BE    TRNS20                                                           
         CLC   BUKTLEDG,TRNKUNT          YES - MATCH ON LEDGER                  
         BE    TRNS20                                                           
TRNS18   LA    R1,BUKTABL(R1)            BUMP TO NEXT TABLE ENTRY               
         B     TRNS12                                                           
*                                                                               
TRNS20   CLI   BUKTOTYP,0                CREATE BUCKET RECORD?                  
         BE    TRNS10                                                           
         MVC   BUKTYPE,BUKTOTYP          SET BUCKET TYPE FOR RECORD             
         SR    RF,RF                                                            
         ICM   RF,3,BUKTROUT             CREATE BUCKET?                         
         BZ    TRNS10                                                           
         LA    RF,ACBU02(RF)                                                    
         BASR  RE,RF                     SUBSIDIARY CASH ROUTINE                
         B     TRNS10                    RETURNS AT 0(RE) TO SKIP               
         BAS   RE,UPDBUK                 RETURNS AT 4(RE) TO UPDATE             
         B     TRNS10                                                           
*                                                                               
TRNSX    B     XIT                                                              
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* HANDLE SUBSIDIARY CASH ELEMENTS FOR BUCKET UPDATING                 *         
***********************************************************************         
BTGRNT   DS    0H                       GROSS/NET FOR US COKE EXP.              
         ZAP   BUKDR,SCIGRS             COKE GROSS=DEBIT,NET=CREDIT             
         CLI   SCILN,SCILN2Q                                                    
         BL    *+10                                                             
         ZAP   BUKCR,SCINET                                                     
         B     4(RE)                                                            
*                                                                               
BTVEHI   DS    0H                        VEHICLES                               
BTHOUR   DS    0H                        HOURS                                  
BTGRSS   DS    0H                        GROSS                                  
BTIVAT2P DS    0H                        VAT                                    
BTGLEV   DS    0H                        NET (GROSS LESS VAT)                   
*&&UK                                                                           
BTHOUR1R DS    0H                                                               
*&&                                                                             
         LA    R1,BUKDR                  POST DEBIT/CREDIT                      
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    R1,BUKCR                                                         
         ZAP   0(L'BUKCR,R1),SCIAMNT                                            
         B     4(RE)                                                            
*&&US                                                                           
BTHOUR1R DS    0H                        HOURS (1R ONLY)                        
*&&                                                                             
BTLOAT1R DS    0H                        LEAVE OF ABSENCE HOURS                 
         LA    R1,BUKCR                                                         
         CP    TRNAMNT,PZERO                                                    
         BE    *+8                                                              
         LA    R1,BUKDR                                                         
         ZAP   0(L'BUKDR,R1),SCIAMNT                                            
         B     4(RE)                                                            
*                                                                               
BTGRSS1J DS    0H                        GROSS (US 1J LEDGER ONLY)              
BTHOUR1J DS    0H                        HOURS (US 1J LEDGER ONLY)              
BTVEHI1J DS    0H                        VEHICLES (US 1J LEDGER ONLY)           
BTFEEA   DS    0H                        FEE ADJUSTMENTS                        
BTFEES   DS    0H                        FEES                                   
BTANAL   DS    0H                        ANALYSIS                               
         ZAP   BUKDR,SCIAMNT             AMOUNT TO DEBIT                        
         CLI   SCILN,SCILN2Q                                                    
         BL    *+10                                                             
         ZAP   BUKCR,SCIADMN             ADMIN TO CREDIT(IF PRESENT)            
         B     4(RE)                                                            
*                                                                               
BTDEPR41 DS    0H                        ASSET DEPRECIATION                     
BTBENE   DS    0H                        BENEFITS(1R LEDGER ONLY)               
         ZAP   BUKDR,SCIAMNT             BENEFIT                                
         ZAP   BUKCR,SCIADMN             ADMINISTRATIVE                         
         B     4(RE)                                                            
*                                                                               
BTIAMTST DS    0H                        INSURABLE AMOUNT (UK)                  
BTFORDST DS    0H                        INCOME DEBIT (UK)                      
BTJHRSST DS    0H                        NET DEBIT (UK)                         
         ZAP   BUKDR,SCIAMNT                                                    
         B     4(RE)                                                            
*                                                                               
BTINCAST DS    0H                        INCOME CREDIT (UK)                     
BTGLEVST DS    0H                        NET CREDIT (UK)                        
         ZAP   BUKCR,SCIAMNT                                                    
         B     4(RE)                                                            
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* POST TIME X'8B' ELEMENTS TO BUCKET BUFFERS                          *         
***********************************************************************         
TMS      NTR1  ,                                                                
         L     R2,AIO                                                           
         USING TIMRECD,R2                                                       
         TM    TIMRSTAT,TIMSFAPP         IS IT FULLY APPROVED ?                 
         BNO   TMSX                                                             
*                                                                               
         LA    R3,TIMRFST                                                       
         USING TIMELD,R3                                                        
         MVC   OFC,TIMKOFF                                                      
*                                                                               
TMS10    CLI   TIMEL,TIMELQ              TIME ELEMENT                           
         BNE   TMS20                                                            
         CLI   TIMETYP,TIMEINP           INPUT DETAIL TYPE ELEM                 
         BE    TMS30                                                            
*                                                                               
TMS20    SR    R0,R0                                                            
         IC    R0,TIMLN                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    TMSX                                                             
         B     TMS10                                                            
*                                                                               
TMS30    LA    R4,BUKELEM                                                       
         USING BUKELD,R4                 BUILD BUCKET ELEMENT                   
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLNQ                                                     
         MVC   BUKMOS,TIMMOA                                                    
         ZAP   BUKDR,PZERO                                                      
         ZAP   BUKCR,TIMHRS                                                     
*                                                                               
         MVI   BUKTYPE,SCITHOUR          BUILD BUCKET ELEMENT(HOURS)            
         BAS   RE,UPDBUK                 UPDATE BUCKET ELEMENT                  
         B     TMS20                                                            
*                                                                               
TMSX     B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
***********************************************************************         
* POST BUCKET ELEMENTS TO STORAGE BUFFER                              *         
***********************************************************************         
UPDBUK   ST    RE,SAVRE                                                         
         MVC   BUKOFC,SPACES             ADD WITHOUT OFFICE                     
         TM    LDGINDS,LDGWC             LEDGER HAS W/C IN KEY                  
         BZ    *+10                                                             
         MVC   BUKOFC,OFC                ADD WITH WC                            
         BAS   RE,UPDBUK2                                                       
         TM    LDGINDS,LDGWC             LEDGER HAS W/C IN KEY                  
         BNZ   UPDBUKX                                                          
*                                                                               
         TM    FLGS,OFF2                 NEW OFFICES                            
         BNO   UPDBUKX                                                          
         L     RE,ADLEDGER                                                      
         CLC   LDGKUNT-LDGKEY(2,RE),=C'1C'                                      
         BE    UPDBUK1                                                          
         CLI   LDGKUNT-LDGKEY(RE),C'G'                                          
         BE    UPDBUK1                                                          
         CLI   LDGKUNT-LDGKEY(RE),C'S'                                          
         BNE   UPDBUKX                                                          
UPDBUK1  MVC   BUKOFC,OFC                OFFICE (WC)  IN KEY                    
         BAS   RE,UPDBUK2                                                       
UPDBUKX  L     RE,SAVRE                                                         
         BR    RE                                                               
*                                                                               
UPDBUK2  L     R1,ABUKBUF                                                       
         USING BUKBUFD,R1                R1=A(BUCKET BUFFER)                    
         ICM   RF,15,BUKBHNUM            RF=NUMBER OF ENTRIES IN TABLE          
         LA    R1,BUKBHDRL(R1)           BUMP OVER TABLE HEADER                 
         LTR   R0,RF                     TEST TABLE EMPTY                       
         BZ    UPDBUK4                                                          
*                                                                               
UPDBUK3  CLI   BUKB,BUKBEOTQ             TEST END OF TABLE                      
         BE    UPDBUK4                                                          
         CLC   BUKBOFC(L'BUKBOFC+L'BUKBTYPE),BUKOFC MATCH OFFICE & TYPE         
         BNE   *+14                                                             
         CLC   BUKBMOS,BUKELEM+(BUKMOS-BUKELD) MATCH MONTH                      
         BE    UPDBUK6                                                          
         LA    R1,BUKBUFL(R1)            BUMP TO NEXT BUFFER ENTRY              
         BCT   R0,UPDBUK3                                                       
*                                                                               
UPDBUK4  LA    RF,1(RF)                  INCREMENT NUMBER OF ENTRIES            
         CHI   RF,BUKBMAXN               TEST SPACE FOR NEW ENTRY               
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   BUKBOFC,BUKOFC                                                   
         MVC   BUKBTYPE,BUKTYPE                                                 
         MVC   BUKBMOS,BUKELEM+(BUKMOS-BUKELD)                                  
         ZAP   BUKBDR,BUKELEM+(BUKDR-BUKELD)(L'BUKDR)                           
         ZAP   BUKBCR,BUKELEM+(BUKCR-BUKELD)(L'BUKCR)                           
         MVI   BUKBUFD+BUKBUFL,BUKBEOTQ                                         
         L     R1,ABUKBUF                                                       
         STCM  RF,15,BUKBHNUM                                                   
         B     UPDBUKXX                                                         
*                                                                               
UPDBUK6  AP    BUKBDR,BUKELEM+(BUKDR-BUKELD)(L'BUKDR)                           
         AP    BUKBCR,BUKELEM+(BUKCR-BUKELD)(L'BUKCR)                           
*                                                                               
UPDBUKXX BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* POST TRANSACTION BUCKETS ON CHANGE OF CONTRA                        *         
***********************************************************************         
TBUK     NTR1  ,                                                                
         L     R1,ABUKBUF                R1=A(FIRST BUCKET ELEMENT)             
         ICM   R0,15,0(R1)               R0=NUMBER OF BUCKET ELEMENTS           
         BZ    XIT                                                              
         LA    R1,4(R1)                                                         
         USING BUKBUFD,R1                                                       
         SHI   R0,1                                                             
         BZ    TBUK7                                                            
*                                                                               
TBUK3    LA    RF,BUKBUFL(R1)            SORT INTO ASCENDING SEQUENCE           
         LR    RE,R0                                                            
*                                                                               
TBUK5    CLC   BUKBUFD(BUKBKEYL),0(RF)                                          
         BNH   *+22                                                             
         XC    0(BUKBUFL,R1),0(RF)                                              
         XC    0(BUKBUFL,RF),0(R1)                                              
         XC    0(BUKBUFL,R1),0(RF)                                              
         LA    RF,BUKBUFL(RF)                                                   
         BCT   RE,TBUK5                                                         
*                                                                               
         LA    R1,BUKBUFL(R1)                                                   
         BCT   R0,TBUK3                                                         
         DROP  R1                                                               
*                                                                               
TBUK7    L     R2,ABUKBUF                                                       
         ICM   R3,15,0(R2)               R3=NUMBER OF BUFFER ENTRIES            
         LA    R2,4(R2)                                                         
         USING BUKBUFD,R2                R2=A(BUFFER ENTRY)                     
*                                                                               
TBUK9    XC    WREC(WLNQ),WREC           LOOK FOR RANGE RECORD                  
         MVC   WOFC,BUKBOFC              OFFICE                                 
         MVC   WCAC,LKEY+(CACKCULC-CACKEY) CONTRA                               
         MVC   WTYP,BUKBTYPE             TYPE                                   
         ZAP   WTDR,BUKBDR               TRANSACTION DEBITS                     
         ZAP   WTCR,BUKBCR                           CREDITS                    
         ZAP   WBDR,PZERO                                                       
         ZAP   WBCR,PZERO                                                       
         MVC   WPERD,BUKBMOS             MONTH                                  
*                                                                               
         BAS   RE,TSTRNG                 TEST FOR RANGE/ADD TO BUFFER           
*                                                                               
         LA    R2,BUKBUFL(R2)                                                   
         BCT   R3,TBUK9                                                         
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* TEST FOR RANGE BEFORE ADDING NEW RECORDS                            *         
***********************************************************************         
TSTRNG   NTR1  ,                                                                
         MVC   SVBFR,WREC                SAVE KEY                               
         XC    SVBFR+(WPERD-WREC)(L'WPERD),SVBFR+(WPERD-WREC)                   
         XC    SVBFR+(WPERDH-WREC)(L'WPERDH),SVBFR+(WPERDH-WREC)                
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',BUFF),(0,SVBFR),ADCOMFAC               
TSTRNG3  CLI   4(R1),0                                                          
         BNE   TSTRNG7                                                          
         CLC   WREC(WPERD-WREC),SVBFR TEST KEY                                  
         BNE   TSTRNG7                                                          
         OC    SVBFR+(WPERDH-WREC)(L'WPERDH),SVBFR+(WPERDH-WREC)                
         BZ    TSTRNG4                   NO RANGE - GET ANOTHER                 
         CLC   WPERD,SVBFR+(WPERD-WREC)                                         
         BL    TSTRNG4                                                          
         CLC   WPERD,SVBFR+(WPERDH-WREC)                                        
         BNH   TSTRNG5                                                          
TSTRNG4  GOTO1 BUFFERIN,DMCB,('BUFFASEQ',BUFF),(0,SVBFR),ADCOMFAC               
         B     TSTRNG3                                                          
*                                                                               
TSTRNG5  MVC   WPERD,SVBFR+(WPERD-WREC) USE RANGE                               
         MVC   WPERDH,SVBFR+(WPERDH-WREC)                                       
*                                                                               
TSTRNG7  GOTO1 BUFFERIN,DMCB,('BUFFAPUT',BUFF),(0,WREC),ADCOMFAC                
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* READ FROM BUFFERIN AND BUILD NEW RECORDS                            *         
***********************************************************************         
GETBUF   NTR1  ,                                                                
         ZAP   ACCDR,PZERO               KEEP TRACK OF RECORD CHANGES           
         ZAP   ACCCR,PZERO                                                      
         LA    R7,P                                                             
         USING LIND,R7                                                          
*                                                                               
         XC    OFCONTYP,OFCONTYP         CLEAR OFFICE/CONTRA                    
         XC    WREC(WKLNQ),WREC                                                 
*                                                                               
         GOTO1 BUFFERIN,DMCB,('BUFFARDH',BUFF),(0,WREC),ADCOMFAC                
         CLI   4(R1),0                                                          
         BE    GETBUF9                   PROCESS FIRST                          
         B     XIT                                                              
*                                                                               
GETBUF3  GOTO1 BUFFERIN,DMCB,('BUFFASEQ',BUFF),(0,WREC),ADCOMFAC                
         CLI   4(R1),0                                                          
         BE    *+8                                                              
         MVI   WREC,X'FF'                                                       
*                                                                               
         CLC   OFCONTYP,WOFC             TEST CHANGE OF KEY                     
         BE    GETBUF11                                                         
*                                                                               
         BAS   RE,PROCR                  PROCESS BUCKET RECORD                  
*                                                                               
         CLC   OF,WOFC                   TEST SAME OFFICE                       
         BE    GETBUF9                                                          
         TM    PRTINDS,PRTNET            PRINT NET CHANGES LINE ?               
         BNO   GETBUF5                                                          
         ZAP   DUB,ACCDR                 CHANGES TO DEBITS                      
         SP    DUB,ACCCR                                                        
         CP    DUB,=P'0'                                                        
         BE    GETBUF5                                                          
         MVC   LINMOS(5),=C'*NET*'                                              
         CURED DUB,(L'LINODR,LINODR),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
*                                                                               
GETBUF5  ZAP   ACCDR,PZERO               CLEAR ACCOUNT CHANGES                  
         ZAP   ACCCR,PZERO                                                      
         NI    PRTINDS,ALL-(PRTNET)                                             
*                                                                               
GETBUF9  CLI   WREC,X'FF'                EOF ?                                  
         BE    XIT                                                              
         L     R2,AIO3                   FIRST TIME                             
         XC    0(100,R2),0(R2)           CLEAR IO3                              
         MVC   OFCONTYP,WOFC             SAVE NEW HEADER KEY                    
*                                                                               
GETBUF11 BAS   RE,BUKR                   BUILD A BUCKET RECORD                  
         B     GETBUF3                                                          
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A BUCKET RECORD                                               *         
***********************************************************************         
BUKR     NTR1  ,                                                                
         L     R2,AIO3                                                          
         USING CACRECD,R2                                                       
         OC    CACKEY,CACKEY             TEST FIRST FOR RECORD                  
         BNZ   BUKR3                     YES,                                   
         MVC   CACKEY,SPACES             BUILD RECORD KEY                       
         L     RF,ADACC                                                         
         MVC   CACKCULA,0(RF)            ACCOUNT                                
         MVC   CACKOFF,WOFC              OFFICE                                 
         MVC   CACKCULC,WCAC             CONTRA                                 
         MVC   CACKBTYP,WTYP             BUCKET TYPE                            
         CLI   CACKBTYP,C'G'                                                    
         BNE   *+10                                                             
         XC    CACKSTYP,CACKSTYP                                                
         XC    CACKSTA(30),CACKSTA                                              
         MVI   BUKCNTN,0                 SET BUCKET COUNT                       
*                                                                               
BUKR3    ZAP   DR,WBDR                   ASSUME BUCKET AMOUNTS                  
         ZAP   CR,WBCR                                                          
         CLC   WPERD,STRTMOS             TEST BEFORE START                      
         BL    BUKR5                     YES, KEEP BUCKET AMOUNT                
         LA    RF,WPERD                                                         
         OC    WPERDH,WPERDH                                                    
         BZ    *+8                                                              
         LA    RF,WPERDH                                                        
         CLC   0(2,RF),ENDMOS            TEST AFTER END                         
         BH    BUKR5                     YES, KEEP BUCKET AMOUNT                
*                                                                               
         CLC   BBFDATE,WPERD             BBF DATE VS. PERIOD                    
         BH    BUKR5                     IF HIGH KEEP THE BUCKET                
         ZAP   DR,WTDR                   NO, USE TRANSACTION TOTALS             
         ZAP   CR,WTCR                                                          
*                                                                               
BUKR5    CP    DR,PZERO                                                         
         BNE   *+14                                                             
         CP    CR,PZERO                                                         
         BE    XIT                                                              
         CLC   WPERD,WPERDH              TEST LOW DATE = HIGH DATE              
         BE    *+14                                                             
         OC    WPERDH,WPERDH             TEST PERIOD RANGE                      
         BNZ   BUKR15                    YES, MUST GO BACK AS RANGE             
*                                                                               
         LA    R1,BUKELEM1               MAKE A BUCKET ELEMENT                  
         USING BUKELD,R1                                                        
         MVI   BUKEL,BUKELQ                                                     
         MVI   BUKLN,BUKLN7Q                                                    
         MVC   BUKMOS,WPERD              YEAR/MONTH                             
         ZAP   BUKDR7,DR                                                        
         ZAP   BUKCR7,CR                                                        
         GOTO1 ADDBUK,CACRECD                                                   
         B     XIT                                                              
*                                                                               
BUKR15   GOTO1 HELLO,ELIST,(C'G',ACCMST),('PBKELQ',AIO3),0                      
         CLI   ELERR,0                                                          
         BNE   BUKR17                                                           
         L     R4,ELADDR                 UPDATE PRIOR BUCKET ELEMENT            
         USING PBKELD,R4                                                        
         CLC   PBKLOW,WPERD                                                     
         BL    *+10                                                             
         MVC   PBKLOW,WPERD                                                     
         CLC   PBKHI,WPERDH                                                     
         BH    *+10                                                             
         MVC   PBKHI,WPERDH                                                     
         AP    PBKDR,DR                                                         
         AP    PBKCR,CR                                                         
         B     XIT                                                              
*                                                                               
BUKR17   XC    ELEMENT,ELEMENT           CREATE PRIOR BUCKET ELEMENT            
         LA    R4,ELEMENT                                                       
         MVI   PBKEL,PBKELQ                                                     
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,WPERD                                                     
         MVC   PBKHI,WPERDH                                                     
         ZAP   PBKDR,DR                                                         
         ZAP   PBKCR,CR                                                         
         GOTO1 HELLO,ELIST,(C'P',ACCMST),AIO3,ELEMENT,0                         
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                      CAN'T ADD THE ELEMENT                  
         B     XIT                                                              
         DROP  R1,R2,R4                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A BUCKET ELEMENT TO A BUCKET RECORD                  *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD BUCKET INTO)                              *         
*        BUKELEM1 CONTAINS BUCKET ELEMENT                             *         
***********************************************************************         
ADDBUK   NTR1  ,                                                                
         LR    R4,R1                                                            
                                                                                
         USING ACCRECD,R4          R4=A(RECORD CONTAINING BUCKETS)              
BUK1     USING BUKELD,BUKELEM1     BUKELEM1=BUCKET ELEMENT                      
                                                                                
ADDBUK02 GOTOR HELLO,ELIST,(C'G',ACCMST),('BUKELQ',ACCRECD),           *        
               (L'BUKMOS,BUK1.BUKMOS)                                           
         CLI   ELERR,0             TEST ANY MORE MATCHING ELEMENTS              
         BNE   ADDBUK08            NO                                           
         L     R1,ELADDR                                                        
REC      USING BUKELD,R1                                                        
ADDBUK04 AP    BUK1.BUKDR7,REC.BUKDR                                            
         AP    BUK1.BUKCR7,REC.BUKCR                                            
         MVI   REC.BUKEL,DELELQ                                                 
         GOTOR HELLO,ELIST,(C'D',ACCMST),('DELELQ',ACCRECD),0                   
         B     ADDBUK02                                                         
         DROP  REC                                                              
                                                                                
BUK2     USING BUKELD,BUKELEM2                                                  
ADDBUK08 XC    BUK2.BUKELD(BUKLN7Q),BUK2.BUKELD                                 
         CLI   BUK1.BUKDR7,0       TEST VALUES FIT IN PL6 ACCUMS                
         BNE   *+12                                                             
         CLI   BUK1.BUKCR7,0                                                    
         BE    ADDBUK12            YES - DON'T SPILT                            
                                                                                
         LA    RF,ACCRECD                                                       
         AH    RF,NEWDISP          POINT TO FIRST BUCKET ELEMENT                
REC      USING BUKELD,RF                                                        
         CLI   REC.BUKEL,BUKELQ    TEST BUCKET ELEMENT                          
         BNE   ADDBUK09            OK TO SPLIT                                  
         CLC   BUK1.BUKMOS,REC.BUKMOS  TEST TRYING TO SPLIT LOW BUCKET          
         BNL   ADDBUK09                NO - OK TO SPLIT                         
SAV      USING BUKELD,BUKSAVE                                                   
         MVC   SAV.BUKELD(BUKDR-BUKELD),BUK1.BUKELD                             
         ZAP   SAV.BUKDR7,BUK1.BUKDR7                                           
         ZAP   SAV.BUKCR7,BUK1.BUKCR7                                           
         B     ADDBUK24                                                         
         DROP  REC,SAV                                                          
                                                                                
ADDBUK09 MVC   BUK2.BUKELD(BUKDR-BUKELD),BUK1.BUKELD                            
         ZAP   BUK2.BUKDR7,PZERO                                                
         ZAP   BUK2.BUKCR7,PZERO                                                
         CLI   BUK1.BUKDR7,0       TEST DEBIT BUCKET OVERFLOW                   
         BE    ADDBUK10            NO                                           
         ZAP   BUK2.BUKDR7,BUK1.BUKDR7                                          
         ZAP   DUB,PL6MAX                                                       
         CP    BUK2.BUKDR7,PZERO                                                
         BH    *+10                                                             
         MP    DUB,PMINUS1                                                      
         ZAP   BUK1.BUKDR7,DUB                                                  
         SP    BUK2.BUKDR7,DUB                                                  
                                                                                
ADDBUK10 CLI   BUK1.BUKCR7,0       TEST CREDIT BUCKET OVERFLOW                  
         BE    ADDBUK12            NO                                           
         ZAP   BUK2.BUKCR7,BUK1.BUKCR7                                          
         ZAP   DUB,PL6MAX                                                       
         CP    BUK2.BUKCR7,PZERO                                                
         BH    *+10                                                             
         MP    DUB,PMINUS1                                                      
         ZAP   BUK1.BUKCR7,DUB                                                  
         SP    BUK2.BUKCR7,DUB                                                  
                                                                                
ADDBUK12 LA    RF,ACCRECD                                                       
         AH    RF,NEWDISP          POINT TO FIRST CONTRA-HEADER ELEMENT         
         SR    RE,RE               RE=A(OLDEST BUCKET ELEMENT)                  
         SR    R1,R1               R1=NUMBER OF BUCKET ELEMENTS                 
         SR    R0,R0                                                            
REC      USING BUKELD,RF                                                        
ADDBUK14 CLI   REC.BUKEL,0         TEST EOR                                     
         BE    ADDBUK18                                                         
         CLI   REC.BUKEL,BUKELQ    TEST BUCKET ELEMENT                          
         BNE   ADDBUK16                                                         
         AHI   R1,1                BUMP NUMBER OF BUCKETS                       
         LTR   RE,RE               TEST FIRST BUCKET ELEMENT                    
         BNZ   ADDBUK16                                                         
         LA    RE,REC.BUKELD       YES - SAVE IT'S ADDRESS                      
ADDBUK16 IC    R0,REC.BUKLN        BUMP TO NEXT ELEMENT                         
         AR    RF,R0                                                            
         B     ADDBUK14                                                         
         DROP  REC                                                              
                                                                                
ADDBUK18 CHI   R1,BUKMAXN          TEST ENOUGH ROOM FOR NEW BUCKET(S)           
         BNH   ADDBUK26            YES                                          
                                                                                
BLD      USING BUKELD,WORK         WORK=BUCKET BUILD AREA                       
                                                                                
         LTR   RE,RE               POINT TO OLDEST (FIRST) BUCKET               
         BNZ   *+6                                                              
         DC    H'0'                                                             
OLD      USING BUKELD,RE                                                        
SAV      USING BUKELD,BUKSAVE                                                   
         MVC   SAV.BUKELD(BUKDR-BUKELD),OLD.BUKELD                              
         ZAP   SAV.BUKDR7,OLD.BUKDR                                             
         ZAP   SAV.BUKCR7,OLD.BUKCR                                             
                                                                                
ADDBUK20 MVI   OLD.BUKEL,DELELQ    MARK ELEMENT FOR DELETION                    
         IC    R0,OLD.BUKLN        BUMP TO NEXT ELEMENT                         
         AR    RE,R0                                                            
         CLC   OLD.BUKELD(BUKDR-BUKELD),SAV.BUKELD                              
         BNE   ADDBUK22                                                         
         AP    SAV.BUKDR7,OLD.BUKDR                                             
         AP    SAV.BUKCR7,OLD.BUKCR                                             
         B     ADDBUK20                                                         
                                                                                
ADDBUK22 GOTOR HELLO,ELIST,(C'D',ACCMST),('DELELQ',ACCRECD),0                   
                                                                                
         GOTOR BLDBUK              BUILD BUCKET ELEMENT IN WORK                 
         GOTOR HELLO,ELIST,(C'P',ACCMST),ACCRECD,BLD.BUKEL,0                    
         CLI   ELERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
         GOTOR HELLO,ELIST,(C'G',ACCMST),('PBKELQ',ACCRECD),0                   
         CLI   ELERR,0                                                          
         BNE   ADDBUK24                                                         
         L     R3,ELADDR           UPDATE PRIOR BUCKET ELEMENT                  
         USING PBKELD,R3                                                        
         CLC   PBKLOW,SAV.BUKYEAR                                               
         BL    *+10                                                             
         MVC   PBKLOW,SAV.BUKYEAR                                               
         CLC   PBKHI,SAV.BUKYEAR                                                
         BH    *+10                                                             
         MVC   PBKHI,SAV.BUKYEAR                                                
         AP    PBKDR,SAV.BUKDR7                                                 
         AP    PBKCR,SAV.BUKCR7                                                 
         B     ADDBUK28                                                         
                                                                                
ADDBUK24 XC    ELEMENT,ELEMENT     CREATE PRIOR BUCKET ELEMENT                  
         LA    R3,ELEMENT                                                       
         MVI   PBKEL,PBKELQ                                                     
         MVI   PBKLN,PBKLNQ                                                     
         MVC   PBKLOW,SAV.BUKYEAR                                               
         MVC   PBKHI,SAV.BUKYEAR                                                
         ZAP   PBKDR,SAV.BUKDR7                                                 
         ZAP   PBKCR,SAV.BUKCR7                                                 
         GOTOR HELLO,ELIST,(C'P',ACCMST),ACCRECD,PBKELD,0                       
         CLI   ELERR,0                                                          
         BE    ADDBUK28                                                         
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
ADDBUK26 GOTOR BLDBUK              BUILD BUCKET ELEMENT IN WORK                 
         GOTOR HELLO,ELIST,(C'P',ACCMST),ACCRECD,BLD.BUKEL,0                    
         CLI   ELERR,0                                                          
         BE    ADDBUK28                                                         
         DC    H'0'                CAN'T ADD THE ELEMENT                        
                                                                                
ADDBUK28 CLI   BUK2.BUKEL,0        TEST ANY REMAINDER                           
         BE    ADDBUKX                                                          
         MVC   BUK1.BUKELD(BUKLN7Q),BUK2.BUKELD                                 
         B     ADDBUK08            CAN'T ADD THE ELEMENT                        
         DROP  BUK2                                                             
                                                                                
ADDBUKX  J     XIT                                                              
                                                                                
BLDBUK   MVC   BLD.BUKELD(BUKDR-BUKELD),BUK1.BUKELD                             
         ZAP   BLD.BUKDR,BUK1.BUKDR7                                            
         ZAP   BLD.BUKCR,BUK1.BUKCR7                                            
         MVI   BLD.BUKLN,BUKLNQ                                                 
         BR    RE                                                               
         DROP  R3,R4,BLD,SAV,OLD                                                
         EJECT                                                                  
***********************************************************************         
* PROCESS THE BUCKET RECORD                                           *         
***********************************************************************         
PROCR    NTR1  ,                                                                
         L     R4,AIO2                   IO FOR OLD RECORD                      
         XC    0(100,R4),0(R4)                                                  
         L     R5,AIO3                   NEW RECORDS                            
         USING CACRECD,R5                                                       
         MVC   DKEY,0(R5)                                                       
         MVI   CACINDS,CACADD            ASSUME NEW RECORD                      
         NI    PRTINDS,ALL-PRTCAC                                               
         BAS   RE,DMHGH                                                         
         CLC   DKEY,DIR                  IS RECORD ON FILE ?                    
         BNE   PROCR3                                                           
         MVI   CACINDS,0                 RECORD ON FILE                         
         MVC   AIO,AIO2                                                         
         BAS   RE,DMGETR                 GET OLD RECORD INTO IO2                
*                                                                               
PROCR3   BAS   RE,COMP                   COMPARE OLD VS. NEW                    
         MVC   AIO,AIO3                                                         
         L     R5,AIO                                                           
         TM    CACINDS,CACADD            ADD NEW RECORD ?                       
         BNO   PROCR5                                                           
         CLI   CACRFST,0                 ANY ELEMENTS ?                         
         BE    XIT                                                              
         BAS   RE,DMADDR                                                        
         AP    CNTBRADD,PONE                                                    
         B     XIT                                                              
*                                                                               
PROCR5   TM    CACINDS,CACPUT            RECORD CHANGED ?                       
         BNO   XIT                                                              
         CLI   CACRFST,0                 ANY ELEMENTS ?                         
         BNE   *+14                                                             
         OI    CACRSTAT,CACSDELT         NO, SET RECORD DELETED                 
         MVC   CACRLEN,=Y(CACRFST-CACKEY) AND MINIMUM LENGTH                    
*                                                                               
         BAS   RE,DMPUTR                                                        
*                                                                               
         TM    CACRSTAT,CACSDELT         RECORD DELETED ?                       
         BO    *+14                                                             
         AP    CNTBRCHA,PONE             COUNT RECORDS CHANGED                  
         B     XIT                                                              
*                                                                               
         AP    CNTBRDEL,PONE             COUNT RECORDS DELETED                  
         MVC   DKEY,CACKEY               GET DIRECTORY                          
         BAS   RE,DMRD                                                          
         CLC   DKEY,DIR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,DIR                                                           
         OI    CACKSTAT,CACSDELT         DELETE DIRECTORY                       
         BAS   RE,DMWRTR                                                        
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* COMPARE NEW BUCKET RECORD TO OLD                                    *         
***********************************************************************         
COMP     NTR1  ,                                                                
         LA    R7,P                                                             
         USING LIND,R7                                                          
         ZAP   RECDR,PZERO               KEEP TRACK OF RECORD CHANGES           
         ZAP   RECCR,PZERO                                                      
*                                                                               
         L     R2,AIO3                   NEW RECORD IS IN IO3                   
         USING CACRECD,R2                                                       
         LA    R3,CACRFST                                                       
*                                                                               
COMP3    CLI   0(R3),0                   END OF NEW RECORD                      
         BE    COMP23                    TEST REMAINDER OF OLD RECORD           
         CLI   0(R3),BUKELQ                                                     
         BNE   COMP13                                                           
         USING BUKELD,R3                                                        
         TM    CACINDS,CACADD            NEW RECORD                             
         BO    COMP4                                                            
         LA    R0,L'BUKMOS                                                      
         LA    RF,BUKMOS                                                        
         GOTO1 HELLO,ELIST,(C'G',ACCMST),('BUKELQ',AIO2),((R0),(RF))            
         CLI   ELERR,0                                                          
         BE    COMP5                                                            
         OI    CACINDS,CACPUT            SET RECORD CHANGED                     
COMP4    BAS   RE,BUKNEW                 EDIT NEW BUKELD                        
         AP    RECDR,BUKDR                                                      
         AP    RECCR,BUKCR                                                      
         B     COMP21                                                           
*                                                                               
COMP5    L     R5,ELADDR                                                        
O        USING BUKELD,R5                                                        
         MVI   O.BUKEL,X'FF'             DON'T USE IT AGAIN                     
         CP    BUKDR,O.BUKDR                                                    
         BNE   COMP7                                                            
         CP    BUKCR,O.BUKCR                                                    
         BNE   COMP7                                                            
         BAS   RE,BUKSAM                 BUKEL'S ARE THE SAME                   
         B     COMP21                                                           
*                                                                               
COMP7    OI    CACINDS,CACPUT            SET RECORD CHANGED                     
         BAS   RE,BUKDIF                 DIFFERENT BUCKET ELEMENTS              
         AP    RECDR,BUKDR               ADD NEW                                
         AP    RECCR,BUKCR                                                      
         SP    RECDR,O.BUKDR             SUBTRACT OLD                           
         SP    RECCR,O.BUKCR                                                    
         B     COMP21                                                           
         DROP  O,R3                                                             
*                                                                               
COMP13   CLI   0(R3),PBKELQ              CHECK PRIOR BUCKETS                    
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PBKELD,R3                                                        
         TM    CACINDS,CACADD            NEW RECORD                             
         BO    COMP14                                                           
         LA    R0,L'PBKLOW+L'PBKHI                                              
         LA    RF,PBKLOW                                                        
         GOTO1 HELLO,ELIST,(C'G',ACCMST),('PBKELQ',AIO2),((R0),(RF))            
         CLI   ELERR,0                                                          
         BE    COMP15                                                           
         OI    CACINDS,CACPUT            SET RECORD CHANGED                     
COMP14   BAS   RE,PBKNEW                 EDIT NEW PBKELD                        
         AP    RECDR,PBKDR                                                      
         AP    RECCR,PBKCR                                                      
         B     COMP21                                                           
*                                                                               
COMP15   L     R5,ELADDR                                                        
O        USING PBKELD,R5                                                        
         MVI   O.PBKEL,X'FF'             DON'T USE IT AGAIN                     
         CP    PBKDR,O.PBKDR                                                    
         BNE   COMP17                                                           
         CP    PBKCR,O.PBKCR                                                    
         BNE   COMP17                                                           
         BAS   RE,PBKSAM                 PBKEL'S ARE THE SAME                   
         B     COMP21                                                           
*                                                                               
COMP17   OI    CACINDS,CACPUT            SET RECORD CHANGED                     
         BAS   RE,PBKDIF                 DIFFERENT BUCKET ELEMENTS              
         AP    RECDR,PBKDR               ADD NEW                                
         AP    RECCR,PBKCR                                                      
         SP    RECDR,O.PBKDR             SUBTRACT OLD                           
         SP    RECCR,O.PBKCR                                                    
*                                                                               
COMP21   SR    R0,R0                     GET NEXT ELEMENT ON NEW RECORD         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     COMP3                                                            
*                                                                               
COMP23   TM    CACINDS,CACADD            IS IT A NEW RECORD ?                   
         BO    COMP29                                                           
*                                                                               
         L     R2,AIO2                   LOOK FOR REMAINING ELEMENTS            
         LA    R5,CACRFST                                                       
         SR    R0,R0                                                            
COMP25   CLI   0(R5),0                   END OF OLD RECORD                      
         BE    COMP29                    ALL DONE                               
         CLI   0(R5),BUKELQ                                                     
O        USING BUKELD,R5                                                        
         BNE   COMP26                                                           
         CP    O.BUKDR,PZERO             DON'T DELETE THE ZERO BUCKET           
         BNE   *+14                                                             
         CP    O.BUKCR,PZERO                                                    
         BE    COMP27                                                           
         OI    CACINDS,CACPUT            SET CHANGED BIT                        
         BAS   RE,BUKDEL                 DELETED BUKEL                          
         SP    RECDR,O.BUKDR             SUBTRACT OLD                           
         SP    RECCR,O.BUKCR                                                    
         B     COMP27                                                           
*                                                                               
COMP26   CLI   0(R5),PBKELQ                                                     
O        USING PBKELD,R5                                                        
         BNE   COMP27                                                           
         CP    O.PBKDR,PZERO             DON'T DELETE THE ZERO BUCKET           
         BNE   *+14                                                             
         CP    O.PBKCR,PZERO                                                    
         BE    COMP27                                                           
         OI    CACINDS,CACPUT            SET CHANGED BIT                        
         BAS   RE,PBKDEL                 DELETED PBKEL                          
         SP    RECDR,O.PBKDR             SUBTRACT OLD                           
         SP    RECCR,O.PBKCR                                                    
*                                                                               
COMP27   SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     COMP25                                                           
*                                                                               
COMP29   AP    ACCDR,RECDR                                                      
         AP    ACCCR,RECCR                                                      
         TM    FLGS,PRNTALL              PRINT ALL CONTRAS ?                    
         BO    *+12                                                             
         TM    CACINDS,CACADD+CACPUT     ANY CHANGES ?                          
         BZ    XIT                                                              
         BAS   RE,PRTDTL                                                        
         TM    FLGS,PRNTNET              PRINT NET CHANGES LINE ?               
         BNO   XIT                                                              
         ZAP   DUB,RECDR                 CHANGES TO DEBITS                      
         SP    DUB,RECCR                                                        
         CP    DUB,=P'0'                                                        
         BE    XIT                                                              
         MVC   LINMOS(5),=C'*NET*'                                              
         CURED DUB,(L'LINODR,LINODR),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
         OI    PRTINDS,PRTNET                                                   
         B     XIT                                                              
         DROP  O,R2,R3                                                          
         EJECT                                                                  
*                                                                               
         USING BUKELD,R3                                                        
O        USING BUKELD,R5                                                        
BUKDEL   ST    RE,SAVRE                                                         
         AP    CNTBLDEL,PONE             COUNT DELETED BUCKETS                  
         MVC   LINACT,=CL16'BUCKET DELETED'                                     
         MVC   YRMO,O.BUKMOS                                                    
         GOTO1 DATCON,DMCB,(1,YRMO),(18,LINMOS)                                 
         B     BUKSAM3                                                          
*                                                                               
BUKDIF   ST    RE,SAVRE                                                         
         AP    CNTBLCHA,PONE             COUNT ELEMENTS FIXED                   
         MVC   LINACT,=CL16'BUCKET CORRECTED'                                   
         B     BUKSAM3                                                          
*                                                                               
BUKSAM   TM    FLGS,PRNTALL              OPTION TO PRINT ALL                    
         BNOR  RE                                                               
         ST    RE,SAVRE                                                         
         MVC   LINACT,=CL16'BUCKET UNCHANGED'                                   
BUKSAM3  CURED O.BUKDR,(L'LINODR,LINODR),2,MINUS=YES                            
         CURED O.BUKCR,(L'LINOCR,LINOCR),2,MINUS=YES                            
         B     BUKNEW3                                                          
*                                                                               
BUKNEW   ST    RE,SAVRE                                                         
         AP    CNTBLADD,PONE             COUNT NEW ELEMENTS                     
         MVC   LINACT,=CL16'BUCKET ADDED'                                       
BUKNEW3  BAS   RE,PRTDTL                 PRINT RECORD DETAILS                   
         CLI   0(R3),0                                                          
         BE    BUKNEW5                                                          
         MVC   YRMO,BUKMOS                                                      
         GOTO1 DATCON,DMCB,(1,YRMO),(18,LINMOS)                                 
         CURED BUKDR,(L'LINNDR,LINNDR),2,MINUS=YES                              
         CURED BUKCR,(L'LINNCR,LINNCR),2,MINUS=YES                              
BUKNEW5  GOTO1 ACREPORT                                                         
         OI    PRTINDS,PRTCBOX           SET CLOSE BOX                          
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  O,R3                                                             
         EJECT                                                                  
         USING PBKELD,R3                                                        
O        USING PBKELD,R5                                                        
PBKDEL   ST    RE,SAVRE                                                         
         AP    CNTBLDEL,PONE            COUNT DELETED BUCKETS                   
         MVC   LINACT,=CL16'BUCKET DELETED'                                     
         MVC   YRMO,O.PBKLOW                                                    
         GOTO1 DATCON,DMCB,(1,YRMO),(18,LINLO)                                  
         MVC   YRMO,O.PBKHI                                                     
         GOTO1 DATCON,DMCB,(1,YRMO),(18,LINHI)                                  
         B     PBKSAM3                                                          
*                                                                               
PBKDIF   ST    RE,SAVRE                                                         
         AP    CNTBLCHA,PONE             COUNT ELEMENTS FIXED                   
         MVC   LINACT,=CL16'BUCKET CHANGED'                                     
         B     PBKSAM3                                                          
*                                                                               
PBKSAM   TM    FLGS,PRNTALL              OPTION TO PRINT ALL                    
         BNOR  RE                                                               
         ST    RE,SAVRE                                                         
         MVC   LINACT,=CL16'BUCKET UNCHANGED'                                   
PBKSAM3  CURED O.PBKDR,(L'LINODR,LINODR),2,MINUS=YES                            
         CURED O.PBKCR,(L'LINOCR,LINOCR),2,MINUS=YES                            
         B     PBKNEW3                                                          
*                                                                               
PBKNEW   ST    RE,SAVRE                                                         
         AP    CNTBLADD,PONE                                                    
         MVC   LINACT,=CL16'BUCKET ADDED'                                       
PBKNEW3  BAS   RE,PRTDTL                 PRINT RECORD DETAILS                   
         CLI   0(R3),0                                                          
         BE    PBKNEW5                                                          
         MVC   YRMO,PBKLOW                                                      
         GOTO1 DATCON,DMCB,(1,YRMO),(18,LINLO)                                  
         MVC   YRMO,PBKHI                                                       
         GOTO1 DATCON,DMCB,(1,YRMO),(18,LINHI)                                  
         CURED PBKDR,(L'LINNDR,LINNDR),2,MINUS=YES                              
         CURED PBKCR,(L'LINNCR,LINNCR),2,MINUS=YES                              
PBKNEW5  GOTO1 ACREPORT                                                         
         OI    PRTINDS,PRTCBOX           SET CLOSE BOX                          
         L     RE,SAVRE                                                         
         BR    RE                                                               
         DROP  O,R3                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP THE OFFICE CONTRA DETAIL FOR PRINTING                        *         
***********************************************************************         
PRTDTL   NTR1  ,                                                                
         OI    PRTINDS,PRTCBOX           SET CLOSE BOX                          
         LA    R7,P                                                             
         USING LIND,R7                                                          
         L     RE,ADACC                                                         
         TM    PRTINDS,PRTACC            PRINT RECORD INFO                      
         BO    PRTDTL3                                                          
         OI    PRTINDS,PRTACC                                                   
         MVC   LINACC,1(RE)              ACCOUNT                                
         LA    R3,LINACC+L'P             R3=PSECOND                             
         MVC   0(4,R3),=C'BBF='                                                 
         MVC   FULL(2),BBFDATE                                                  
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(10,4(R3))                                  
*                                                                               
PRTDTL3  MVC   LINTYP,TYP                TYPE                                   
         TM    PRTINDS,PRTCAC            CONTRA DETAIL                          
         BO    PRTDTLX                                                          
         OI    PRTINDS,PRTCAC                                                   
*                                                                               
         MVC   LINOFC,OF                 OFFICE                                 
         MVC   LINCAC,ULC                CONTRA                                 
         CLC   CULC,SPACES                                                      
         BH    PRTDTLX                                                          
         MVC   LINCAC,SPACES                                                    
         MVC   LINCAC(L'CON),CON                                                
PRTDTLX  B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
DMRD     ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,(X'08',DMREAD),ACCDIR,DKEY,DIR                      
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMHGH    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMSEQ    ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         B     DMERR                                                            
*                                                                               
DMGETR   ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMWRTR   CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         B     DMERR                                                            
*                                                                               
DMADDR   CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,AIO,DMWORK                         
         B     DMERR                                                            
*                                                                               
DMPUTR   CLI   RCWRITE,C'Y'                                                     
         BNER  RE                                                               
         ST    RE,SAVRE                                                         
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,AIO,DMWORK                         
*                                                                               
DMERR    MVC   BYTE,8(R1)                                                       
         NI    BYTE,X'FF'-(X'10'+X'02') IGNORE RNF/RECORD DELETED               
         CLI   BYTE,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* OTHER ROUTINES                                                      *         
***********************************************************************         
         GETEL R2,DATADISP,ELCODE                                               
GETELN   AH    R2,NEWDISP                                                       
         J     FIRSTEL                                                          
         EJECT                                                                  
***********************************************************************         
* HEADLINE HOOK ROUTINE                                               *         
***********************************************************************         
HOOK     DS    0H                                                               
         MVC   HEAD5+69(13),=CL13'DRAFT REQUEST'                                
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   HEAD5+69(13),=CL13'LIVE REQUEST'                                 
HOOKX    BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
CONVMOS  DC    V(CONVMOS)                                                       
HELLO    DC    V(HELLO)                                                         
BUFFERIN DC    V(BUFFERIN)                                                      
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
*                                                                               
ABUKBUF  DC    A(BUKBUF)                                                        
ABUKTAB  DC    A(BUKTAB)                                                        
*                                                                               
YRMO     DC    XL2'00'                                                          
DAY      DC    X'01'                                                            
*                                                                               
PRODUL   DC    C'SJ'                                                            
PCTUL    DC    C'1J'                                                            
COSTUL   DC    C'1C'                                                            
PERSUL   DC    C'1R'                                                            
*                                                                               
NEWDISP  DC    Y(ACCRFST-ACCKEY)                                                
ACCMST   DC    C'ACCMST '                                                       
ACCDIR   DC    C'ACCDIR '                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
*                                                                               
DELELQ   EQU   X'FF'               FOR ELEMENT DELETION                         
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
PMINUS1  DC    P'-1'                                                            
PL6MAX   DC    PL6'99999999999'                                                 
*                                                                               
CTRYANY  EQU   0                                                                
CTRYNOT  EQU   X'80'                                                            
*                                                                               
CNT      DS    0XL34                                                            
CNTBRADD DC    PL4'0',CL30'BUCKET RECORDS ADDED'                                
CNTBRCHA DC    PL4'0',CL30'BUCKET RECORDS CORRECTED'                            
CNTBRDEL DC    PL4'0',CL30'BUCKET RECORDS DELETED'                              
CNTBLADD DC    PL4'0',CL30'BUCKET ELEMENTS ADDED'                               
CNTBLCHA DC    PL4'0',CL30'BUCKET ELEMENTS CORRECTED'                           
CNTBLDEL DC    PL4'0',CL30'BUCKET ELEMENTS DELETED'                             
CNTX     DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
BUKTAB   DS    0X                  ** CONTRA BUCKET TABLE **                    
         DC    AL1(CTRYUSA),AL1(SCITGRNT,SCITDFLT),C'SE'                        
         DC    AL2(BTGRNT-ACBU02)                                               
         DC    AL1(CTRYUSA),AL1(SCITCOKE,SCITGRNT),C'SE'                        
         DC    AL2(BTGRNT-ACBU02)                                               
         DC    AL1(CTRYUSA),AL1(SCITGRNT,SCITGRNT),C'  '                        
         DC    AL2(BTGRNT-ACBU02)                                               
         DC    AL1(CTRYGBR),AL1(SCITGLEV,SCITGLEV),C'ST'                        
         DC    AL2(BTGLEVST-ACBU02)                                             
         DC    AL1(CTRYGBR),AL1(SCITINCA,SCITINCA),C'ST'                        
         DC    AL2(BTINCAST-ACBU02)                                             
         DC    AL1(CTRYGBR),AL1(SCITFORD,SCITGLEV),C'ST'                        
         DC    AL2(BTFORDST-ACBU02)                                             
         DC    AL1(CTRYGBR),AL1(SCITJHRS,SCITINCA),C'ST'                        
         DC    AL2(BTJHRSST-ACBU02)                                             
         DC    AL1(CTRYGBR),AL1(SCITIAMT,SCITIAMT),C'ST'                        
         DC    AL2(BTIAMTST-ACBU02)                                             
         DC    AL1(CTRYNOT+CTRYUSA),AL1(SCITIVAT,SCITIVAT),C'2P'                
         DC    AL2(BTIVAT2P-ACBU02)                                             
         DC    AL1(CTRYNOT+CTRYUSA),AL1(SCITDEPR,SCITDEPR),C'41'                
         DC    AL2(BTDEPR41-ACBU02)                                             
         DC    AL1(CTRYANY),AL1(SCITVEHI,SCITVEHI),C'1J'                        
         DC    AL2(BTVEHI1J-ACBU02)                                             
         DC    AL1(CTRYANY),AL1(SCITHOUR,0),C'1C'                               
         DC    AL2(0)                                                           
         DC    AL1(CTRYANY),AL1(SCITHOUR,SCITHOUR),C'1J'                        
         DC    AL2(BTHOUR1J-ACBU02)                                             
         DC    AL1(CTRYANY),AL1(SCITGRSS,SCITGRSS),C'1J'                        
         DC    AL2(BTGRSS1J-ACBU02)                                             
         DC    AL1(CTRYANY),AL1(SCITGRSS,SCITGRSS),C'  '                        
         DC    AL2(BTGRSS-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITBENE,SCITBENE),C'  '                        
         DC    AL2(BTBENE-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITJHRS,SCITHOUR),C'  '                        
         DC    AL2(BTHOUR-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITHOUR,SCITHOUR),C'1R'                        
         DC    AL2(BTHOUR1R-ACBU02)                                             
         DC    AL1(CTRYGBR),AL1(SCITLOAT,SCITLOAT),C'1R'                        
         DC    AL2(BTLOAT1R-ACBU02)                                             
         DC    AL1(CTRYANY),AL1(SCITHOUR,SCITHOUR),C'  '                        
         DC    AL2(BTHOUR-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITGLEV,SCITGLEV),C'  '                        
         DC    AL2(BTGLEV-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITVEHI,SCITVEHI),C'  '                        
         DC    AL2(BTVEHI-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITFEEA,SCITFEEA),C'  '                        
         DC    AL2(BTFEEA-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITFEES,SCITFEES),C'  '                        
         DC    AL2(BTFEES-ACBU02)                                               
         DC    AL1(CTRYANY),AL1(SCITANAL,SCITANAL),C'SI'                        
         DC    AL2(BTANAL-ACBU02)                                               
BUKTABX  DC    AL1(BUKTEOTQ)                                                    
         EJECT                                                                  
BUFF     BUFFD TYPE=P,                                                 *        
               KEYLEN=WKLNQ,                                           *        
               COLUMNS=4,                                              *        
               REPCOM=NO,                                              *        
               FILE=BUFFWK                                                      
*                                                                               
BUKBUF   DC    ((BUKBMAXN*BUKBUFL)+BUKBHDRL)X'00'                               
*                                                                               
         DS    F                                                                
IO1      DS    XL2000              I/O AREA                                     
         DS    F                                                                
IO2      DS    XL2000              I/O AREA 2                                   
         DS    F                                                                
IO3      DS    XL2000              I/O AREA 3                                   
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
WREC     DS    0X                  BUFFERIN WORK RECORD                         
WOFC     DS    CL2                 OFFICE                                       
WCAC     DS    CL15                CONTRA ACCOUNT                               
WTYP     DS    CL1                 BUCKET TYPE                                  
WPERD    DS    XL2                 MONTH/YEAR                                   
WPERDH   DS    XL2                 HIGH RANGE (MONTH/YEAR)                      
WKLNQ    EQU   *-WREC                                                           
WTDR     DS    PL8                 TRANSACTION DEBITS                           
WBDR     DS    PL8                 BUCKET DEBITS                                
WTCR     DS    PL8                 TRANSACTION CREDITS                          
WBCR     DS    PL8                 BUCKET CREDITS                               
WLNQ     EQU   *-WREC                                                           
*                                                                               
SVBFR    DS    XL(WLNQ)                                                         
*                                                                               
BBFDATE  DS    XL2                 ACCOUNT PEELED DATE                          
*                                                                               
DR       DS    PL8                                                              
CR       DS    PL8                                                              
RECDR    DS    PL8                                                              
RECCR    DS    PL8                                                              
ACCDR    DS    PL8                                                              
ACCCR    DS    PL8                                                              
*                                                                               
AIO      DS    A                                                                
*                                                                               
DKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
DIR      DS    CL64                DIRECTORY RECORD                             
*                                                                               
DA       DS    F                                                                
SAVRE    DS    F                                                                
*                                                                               
BUKSAVE  DS    XL(BUKLN7Q)                                                      
BUKELEM1 DS    XL(BUKLN7Q)                                                      
BUKELEM2 DS    XL(BUKLN7Q)                                                      
*                                                                               
ELIST    DS    3A                  HELLO PARAMETER LIST                         
ELERR    DS    0XL1                HELLO ERROR RETURN BYTE                      
ELADDR   DS    A                   HELLO ELEMENT ADDRESS (GET)                  
ELADDR2  DS    A                   HELLO ELEMENT ADDRESS (ADD)                  
ELADDR3  DS    A                                                                
ELADDR4  DS    A                                                                
*                                                                               
ALL      EQU   X'FF'                                                            
*                                                                               
FLGS     DS    XL1                 FLAGS                                        
OFF2     EQU   X'80'               NEW OFFICES                                  
PRNTALL  EQU   X'40'               PRINT ALL BUCKETS                            
PRNTNET  EQU   X'20'               PRINT NET DIFFERENCE                         
NOSPCL   EQU   X'10'               IGNORE SPECIAL BUCKETS                       
*                                                                               
LDGINDS  DS    XL1                 LEDGER INDICATORS                            
LDGWC    EQU   X'80'               WORKCODE IS IN THE KEY                       
LDGCOKE  EQU   X'40'               COKE LEDGER                                  
LDGCOST  EQU   X'20'               COST LEDGER                                  
LDGPERS  EQU   X'10'               PERSON LEDGER                                
*                                                                               
PRTINDS  DS    XL1                 PRINT SWITCHES                               
PRTACC   EQU   X'80'               ACCOUNT CODE PRINTED                         
PRTCAC   EQU   X'40'               CONTRA DETAIL PRINTED                        
PRTCACA  EQU   X'20'               CONTRA HEADER ADDED MESSAGE PRINTED          
PRTNET   EQU   X'10'               NET HAS BEEN PRINTED                         
PRTCBOX  EQU   X'01'               CLOSE BOX                                    
*                                                                               
CACINDS  DS    XL1                 CAC RECORD INDICATORS                        
CACADD   EQU   X'80'               CACRECD ADD                                  
CACPUT   EQU   X'40'               CACRECD CHANGE                               
*                                                                               
ELCODE   DS    XL1                                                              
OFC      DS    CL2                 OFFICE CODE                                  
*                                                                               
MOS      DS    XL2                 TRANSACTION MONTH OF SERVICE                 
STRTMOS  DS    XL2                 BUCKET START MONTH                           
ENDMOS   DS    XL2                 BUCKET END RANGE                             
*                                                                               
BUKCNTN  DS    XL1                 NUMBER OF BUCKET ELEMENTS ON RECORD          
BUKMAXN  EQU   108                 MAX NUMBER OF BUKEL'S ON A RECORD            
*                                                                               
BUKOFC   DS    CL2                 OFFICE                                       
BUKTYPE  DS    XL1                 BUCKET TYPE                                  
BUKELEM  DS    XL(BUKLNQ)          BUCKET ELEMENTS BUILT HERE                   
*                                                                               
LENCON   DS    XL1                                                              
LKEY     DS    XL(L'CACKEY)        SAVED LAST CONTRA-ACCOUNT KEY                
*                                                                               
OFCONTYP DS    XL(L'WOFC+L'WCAC+L'WTYP)                                         
         ORG   OFCONTYP                                                         
OF       DS    CL(L'WOFC)                                                       
CULA     DS    CL(L'WCAC)                                                       
TYP      DS    CL(L'WTYP)                                                       
         ORG   OF                                                               
OFCON    DS    XL(L'WOFC+L'WCAC)                                                
         ORG   CULA                                                             
CULC     DS    CL3                                                              
         ORG   CULC+1                                                           
ULC      DS    CL14                                                             
         ORG   ULC+2                                                            
CON      DS    CL12                                                             
         ORG   OFCONTYP+L'OFCONTYP                                              
*                                                                               
ELEMENT  DS    XL256                                                            
*                                                                               
         EJECT                                                                  
BUKTABD  DSECT                     ** CONTRA BUCKET TABLE DSECT **              
BUKTCTRY DS    XL1                 COUNTRY CODE (SEE EQUATES)                   
BUKTEOTQ EQU   255                 END OF TABLE INDICATOR                       
BUKTITYP DS    CL1                 INPUT BUCKET TYPE (IN ELEMENT)               
BUKTOTYP DS    CL1                 OUTPUT BUCKET TYPE (IN RECORD)               
BUKTLEDG DS    CL2                 UNIT/LEDGER CODE (SPACES=ANY)                
BUKTROUT DS    AL2                 DISPLACEMENT TO ROUTINE                      
BUKTABL  EQU   *-BUKTABD                                                        
         SPACE 1                                                                
BUKBUFD  DSECT                     ** LIST OF BUCKETS DSECT **                  
BUKBHDR  DS    0X                  TABLE HEADER                                 
BUKBHNUM DS    XL4                 NUMBER OF ENTRIES IN TABLE                   
BUKBHDRL EQU   *-BUKBUFD                                                        
         ORG   BUKBHDR                                                          
BUKB     DS    0X                                                               
BUKBEOTQ EQU   255                 END OF TABLE INDICATOR                       
BUKBOFC  DS    CL2                 OFFICE                                       
BUKBTYPE DS    CL1                 BUCKET TYPE VALUE                            
BUKBMOS  DS    PL2                 BUCKET MONTH OF SERVICE (YYMM)               
BUKBKEYL EQU   *-BUKBUFD           LENGTH OF KEY                                
BUKBDR   DS    PL8                 BUCKET DEBIT AMOUNT                          
BUKBCR   DS    PL8                 BUCKET CREDIT AMOUNT                         
BUKBUFL  EQU   *-BUKBUFD                                                        
BUKBMAXN EQU   512                 MAXIMUM NUMBER OF BUCKET ENTRIES             
         EJECT                                                                  
LIND     DSECT                     ** PRINT LINE LAYOUT **                      
LIN      DS    0CL(L'P)                                                         
LINBXL   DS    CL1                                                              
LINACC   DS    CL(L'CACKULA)       UNIT/LEDGER ACCOUNT CODE                     
LINBX1   DS    CL1                                                              
LINOFC   DS    CL2                 OFFICE                                       
LINBX2   DS    CL1                                                              
LINCAC   DS    CL(L'CACKULC)       CONTRA UNIT/LEDGER ACCOUNT CODE              
LINBX3   DS    CL1                                                              
LINTYP   DS    CL(L'CACKBTYP)      BUCKET TYPE                                  
LINBX4   DS    CL1                                                              
LINMOS   DS    CL6                 MONTH OF SERVICE                             
         ORG   LINMOS                                                           
LINLO    DS    CL6                 LOW MOS                                      
LINBX5   DS    CL1                                                              
LINHI    DS    CL6                 HI MOS                                       
LINBX6   DS    CL1                                                              
LINODR   DS    CL15                OLD DEBITS                                   
LINBX7   DS    CL1                                                              
LINNDR   DS    CL15                NEW DEBITS                                   
LINBX8   DS    CL1                                                              
LINOCR   DS    CL15                OLD CREDITS                                  
LINBX9   DS    CL1                                                              
LINNCR   DS    CL15                NEW CREDITS                                  
LINBX10  DS    CL1                                                              
LINACT   DS    CL16                ACTION                                       
LINBXR   DS    CL1                                                              
         ORG   LIN                                                              
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
                                                                                
BUKELD   DSECT                                                                  
         ORG   BUKDR                                                            
BUKDR7   DS    PL7                                                              
BUKCR7   DS    PL7                                                              
BUKLN7Q  EQU   *-BUKELD                                                         
                                                                                
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
* DDCTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPBU02 12/11/09'                                      
         END                                                                    
