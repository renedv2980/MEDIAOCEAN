*          DATA SET ACREPID02S AT LEVEL 022 AS OF 05/01/02                      
*PHASE ACID02A                                                                  
*INCLUDE DATVAL                                                                 
         TITLE 'W.B. DONER INTERFACE TAPE'                                      
ACID02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACID**,R9       R9=2ND BASE REGISTER                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING LWSD,RC             RC=A(SAVE W/S)                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST        RUN FIRST                                    
         BE    RUNF                                                             
         CLI   MODE,REQFRST        REQ FIRST                                    
         BE    REQF                                                             
         CLI   MODE,PROCTRNS       PROCESS TRANSACTIONS                         
         BE    PROC                                                             
         CLI   MODE,REQLAST        REQ LAST                                     
         BE    REQL                                                             
EXIT     XMOD1 1                                                                
         EJECT                                                                  
******************************************************************              
*        INITIALIZE ROUTINE                                      *              
******************************************************************              
*                                                                               
RUNF     DS    0H                                                               
         LA    RE,VTYPES           MOVE VTYPES TO W/S                           
         LA    R0,ADCONS                                                        
         LA    RF,VTYPLNQ                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(BXHOOK)                                                    
         ST    R2,HEADHOOK                                                      
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        INITIALIZE BUFFALO                                      *              
******************************************************************              
*                                                                               
REQF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   TAPECNT,=P'0'                                                    
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         GOTO1 DATCON,DMCB,(0,QSTART),(2,STARTDT)                               
         GOTO1 DATCON,DMCB,(0,QEND),(2,ENDDT)                                   
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        PROCESS INCOMING TRANSACTIONS - PUT OUT REC TO BUFFALO  *              
******************************************************************              
*                                                                               
         USING ACKEYD,R5                                                        
PROC     L     R5,ADACC            R5 -> ACCOUNT REC                            
*                                                                               
         L     R1,ALDGTBL          ONLY PROCESS U/L'S IN TABLE                  
PROC100  CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(LDGTLNQ,R1),ACKEYACC+1                                         
         BE    PROC200                                                          
         LA    R1,LDGTLNQ(R1)                                                   
         B     PROC100                                                          
*                                                                               
PROC200  L     R5,ADTRANS          ELIMINATE ITEMS MARKED AS OFFSET             
         SH    R5,DATADISP                                                      
         MVI   ELCODE,X'60'        STATUS ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   *+12                                                             
         TM    TRSTATUS-TRSTATD(R5),X'08'                                       
         BO    EXIT                                                             
*                                                                               
         USING ACKEYD,R5                                                        
         L     R5,ADTRANS          *** MUST BE MARKED USED ***                  
         SH    R5,DATADISP                                                      
         OC    ACDTUSED,ACDTUSED                                                
         BZ    EXIT                                                             
*                                                                               
         USING TRANSD,R5                                                        
         L     R5,ADTRANS          *** SKIP DEBITS ***                          
         TM    TRNSSTAT,X'80'                                                   
         BO    EXIT                                                             
*                                                                               
* HANDLE CREDIT POSTINGS                                                        
*                                                                               
         USING ACKEYD,R5                                                        
PROC1000 L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         CLC   ACDTUSED,STARTDT                                                 
         BL    EXIT                *** CANT BE < START DATE ***                 
         CLC   ACDTUSED,ENDDT                                                   
         BH    EXIT                *** CANT BE > END DATE ***                   
*                                                                               
         MVC   BUFREC,XSPACES      HANDLE CREDITS                               
         ZAP   PAYAMT,=P'0'        CLEAR TEMP ACCUMS                            
         ZAP   CSHAMT,=P'0'                                                     
         ZAP   CDAMT,=P'0'                                                      
         ZAP   GSTAMT,=P'0'                                                     
         ZAP   QSTAMT,=P'0'                                                     
         ZAP   HSTAMT,=P'0'                                                     
*                                                                               
         USING ACKEYD,R5           *** FIND CONVERTED ACCOUNT ***               
         L     R5,ADACC            R5 -> ACCOUNT REC                            
         USING ACCTBD,R4                                                        
         L     R4,AACCTBL          R4 -> ACCOUNT CONVERSION TABLE               
*                                                                               
PROC1100 DS    0H                                                               
         MVC   BUFLEDG,ACKEYACC+2  SAVE LEDGER                                  
         MVC   BUFACCT,ACCCNV      SAVE CONVERTED ACCT                          
         CLC   ACCULM,ACKEYACC+1   COMPARE FOR U/L/MEDIA                        
         BE    PROC1200                                                         
         CLI   ACCULM,X'FF'        SKIP IF NOT IN TABLE                         
         BE    EXIT                                                             
         LA    R4,ACCTBQ(R4)                                                    
         B     PROC1100                                                         
*                                                                               
PROC1200 DS    0H                                                               
         L     R5,ADTRANS                                                       
         SH    R5,DATADISP                                                      
         MVC   BUFCLI,XSPACES                                                   
         CLI   BUFACCT,C'2'        ONLY PUT CLIENT IN '2XXX' ACCTS              
         BNE   *+10                                                             
         MVC   BUFCLI(3),ACKEYCON+12                                            
*                                                                               
         USING OFFCD,R4            *** GET OFFICE ***                           
         L     R4,AOFFDEF                                                       
         USING TRANSD,R5                                                        
         L     R5,ADTRANS                                                       
PROC1300 MVC   BUFOFF,OFFCNV                                                    
         CLC   OFFANAL,TRNSOFFC                                                 
         BE    PROC1400                                                         
         CLI   OFFANAL,X'FF'       END OF TABLE                                 
         BE    EXIT                                                             
         LA    R4,OFFCQ(R4)                                                     
         B     PROC1300                                                         
*                                                                               
PROC1400 DS    0H                  *** GET CASH AMOUNT ***                      
         ZAP   CSHAMT,TRNSAMNT     CASH AMOUNT=NET+GST+QST-CD                   
*                                                                               
PROC1500 ZIC   R1,1(R5)            *** GET CD & GST AMOUNTS ***                 
         AR    R5,R1                                                            
         CLI   0(R5),X'46'         EXTRA PAY ELEMENT                            
         BE    PROC1550                                                         
         CLI   0(R5),X'50'         SUBSIDIARY CASH ELEM                         
         BE    PROC1600                                                         
         CLI   0(R5),X'64'         MANUAL PAYMENT ELEMENT                       
         BE    PROC1700                                                         
         CLI   0(R5),0                                                          
         BE    PROC2000                                                         
         B     PROC1500                                                         
*                                                                               
         USING TRPAYD,R5           EXTRA PAY ELEMENT                            
PROC1550 DS    0H                                                               
         ZAP   CDAMT,TRPYCD        CASH DISCOUNT                                
         B     PROC1500                                                         
*                                                                               
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -            
         USING SCIELD,R5                                                        
PROC1600 DS    0H                                                               
         CLI   SCITYPE,SCITCDSC    ADD UP CASH DISCOUNT C'D'                    
         BNE   *+10                                                             
         ZAP   CDAMT,SCIAMNT                                                    
         CLI   SCITYPE,SCITTAXP    ADD UP GST C'T'                              
         BNE   *+10                                                             
         AP    GSTAMT,SCIAMNT                                                   
*                                                                               
         CLI   SCITYPE,SCITTQST    ADD UP QST OR HST C'Q'                       
         BNE   PROC1500                                                         
         CLI   SCILN,SCILN3Q       CHECK THE LENGTH                             
         BNE   PROC1650              IF NOT THAT LONG THEN QST                  
         CLC   SCISUBPR,=C'NB'     NEW BRUNSWICK?                               
         BE    PROC1625              THEN HST                                   
         CLC   SCISUBPR,=C'NS'     NOVA SCOTIA?                                 
         BE    PROC1625              THEN HST                                   
         CLC   SCISUBPR,=C'NF'     NEWFOUNDLAND?                                
         BNE   PROC1650              IF NOT EQUAL THEN QST, ELSE HST            
PROC1625 AP    HSTAMT,SCIAMNT                                                   
         B     PROC1500                                                         
PROC1650 AP    QSTAMT,SCIAMNT                                                   
         B     PROC1500                                                         
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -            
PROC1700 DS    0H                                                               
         USING MPYELD,R5                                                        
         MVC   BUFCNUM,MPYNO                       CHECK NUMBER                 
         GOTO1 DATCON,DMCB,(2,MPYDTE),(0,BUFCDTE)  CHECK DATE                   
         B     PROC1500                                                         
*                                                                               
PROC2000 ZAP   PAYAMT,CSHAMT       NET=CASH+CD-GST-QST                          
         AP    PAYAMT,CDAMT                                                     
         SP    PAYAMT,GSTAMT                                                    
         SP    PAYAMT,QSTAMT                                                    
         SP    PAYAMT,HSTAMT                                                    
*                                                                               
* PUT RECORDS TO BUFFALO  - BUFOFF/BUFACCT ALREADY SET FROM ABOVE               
*                                                                               
         MVC   BUFDESC,XSPACES                                                  
         MVI   BUFTYPE,C'1'                                                     
         MVI   BUFSTAT,BUFPAYQ     *** PAYEE RECORD TO SORTER ***               
         ZAP   BUFPAY,PAYAMT                                                    
         ZAP   BUFCSH,=P'0'                                                     
         ZAP   BUFCD,=P'0'                                                      
         ZAP   BUFGST,=P'0'                                                     
         ZAP   BUFQST,=P'0'                                                     
         ZAP   BUFHST,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         MVI   BUFSTAT,BUFGSTQ     *** GST RECORD TO SORTER ***                 
         MVC   BUFACCT,GSTACCT     MOVE IN HARDCODED GST ACCT                   
         MVC   BUFCLI,XSPACES                                                   
         ZAP   BUFPAY,=P'0'                                                     
         ZAP   BUFCSH,=P'0'                                                     
         ZAP   BUFCD,=P'0'                                                      
         ZAP   BUFGST,GSTAMT                                                    
         ZAP   BUFQST,=P'0'                                                     
         ZAP   BUFHST,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         MVI   BUFSTAT,BUFQSTQ     *** QST RECORD TO SORTER ***                 
         MVC   BUFOFF,=C'ML'       HARDCODED FOR QST ACCOUNT                    
         MVC   BUFACCT,QSTACCT     MOVE IN HARDCODED QST ACCT                   
         MVC   BUFCLI,XSPACES                                                   
         ZAP   BUFPAY,=P'0'                                                     
         ZAP   BUFCSH,=P'0'                                                     
         ZAP   BUFCD,=P'0'                                                      
         ZAP   BUFGST,=P'0'                                                     
         ZAP   BUFQST,QSTAMT                                                    
         ZAP   BUFHST,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
         MVI   BUFSTAT,BUFHSTQ     *** HST RECORD TO SORTER ***                 
         MVC   BUFOFF,=C'ML'       HARDCODED FOR HST ACCOUNT                    
         MVC   BUFACCT,HSTACCT     MOVE IN HARDCODED HST ACCT                   
         MVC   BUFCLI,XSPACES                                                   
         ZAP   BUFPAY,=P'0'                                                     
         ZAP   BUFCSH,=P'0'                                                     
         ZAP   BUFCD,=P'0'                                                      
         ZAP   BUFGST,=P'0'                                                     
         ZAP   BUFQST,=P'0'                                                     
         ZAP   BUFHST,HSTAMT                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
*                                                                               
         MVI   BUFSTAT,BUFCDQ      *** CD RECORD TO SORTER ***                  
         MVC   BUFOFF,AGENCY       MOVE IN HARDCODED FOR AGENCY LEVEL           
         MVC   BUFACCT,CDACCT      MOVE IN HARDCODED CD ACCT                    
         ZAP   BUFPAY,=P'0'                                                     
         ZAP   BUFCSH,=P'0'                                                     
         ZAP   BUFCD,CDAMT                                                      
         ZAP   BUFGST,=P'0'                                                     
         ZAP   BUFQST,=P'0'                                                     
         ZAP   BUFHST,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
         MVI   BUFSTAT,BUFCSHQ     *** CASH RECORD TO SORTER ***                
         MVC   BUFOFF,AGENCY       MOVE IN HARDCODED FOR AGENCY LEVEL           
         MVC   BUFACCT,CSHACCT     MOVE IN HARDCODED CASH ACCT                  
         ZAP   BUFPAY,=P'0'                                                     
         ZAP   BUFCSH,CSHAMT                                                    
         ZAP   BUFCD,=P'0'                                                      
         ZAP   BUFGST,=P'0'                                                     
         ZAP   BUFQST,=P'0'                                                     
         ZAP   BUFHST,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
* CHECK TOTAL                                                                   
*                                                                               
         MVC   BUFDESC,=CL10'CHECK'                                             
         MVC   BUFCDTE(L'BUFKEY-(BUFCDTE-BUFKEY)),EFFS                          
         MVI   BUFSTAT,BUFALL                                                   
         ZAP   BUFPAY,PAYAMT                                                    
         ZAP   BUFCSH,CSHAMT                                                    
         ZAP   BUFCD,CDAMT                                                      
         ZAP   BUFGST,GSTAMT                                                    
         ZAP   BUFQST,QSTAMT                                                    
         ZAP   BUFHST,HSTAMT                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
* LEDGER TOTAL                                                                  
*                                                                               
         MVC   BUFDESC,=CL10'LEDGER'                                            
         MVC   BUFCNUM(L'BUFKEY-(BUFCNUM-BUFKEY)),EFFS                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
*                                                                               
* REQ TOTAL                                                                     
*                                                                               
         MVC   BUFDESC,=CL10'REQUEST'                                           
         MVC   BUFLEDG(L'BUFKEY-(BUFLEDG-BUFKEY)),EFFS                          
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFREC                                
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        RETRIEVE RECORDS FROM BUFFALO                           *              
******************************************************************              
*                                                                               
REQL     DS    0H                                                               
         MVC   BUFREC,XSPACES                                                   
         BAS   RE,INITTAPE                                                      
         GOTO1 BUFFALO,DMCB,=C'HIGH',(0,ABUFF),BUFREC,1                         
*                                                                               
REQL100  TM    DMCB+8,X'80'        EOF?                                         
         BO    REQLX                                                            
         BAS   RE,REPORT                                                        
         BAS   RE,BLDTAPE                                                       
         GOTO1 BUFFALO,DMCB,=C'SEQ',(C'1',ABUFF),BUFREC,1                       
         B     REQL100                                                          
*                                                                               
REQLX    DS    0H                                                               
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
         MVC   XP,XSPACES                                                       
         BAS   RE,PRNTXP                                                        
         MVC   PDESC,=CL24'TOTAL TAPE RECS:'                                    
         LA    R2,PDESC+17                                                      
         EDIT  (P8,TAPECNT),(12,(R2)),ALIGN=LEFT                                
         BAS   RE,PRNTXP                                                        
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        PRINT REPORT                                            *              
******************************************************************              
*                                                                               
REPORT   NTR1                                                                   
         MVC   CURLEDG,BUFLEDG     STORE LEDGER FOR HEADLINES                   
         USING PLINED,R7                                                        
         LA    R7,XP                                                            
         MVC   XP,XSPACES                                                       
         CLI   BUFSTAT,BUFALL                                                   
         BNE   REP50                                                            
         BAS   RE,PRNTXP                                                        
         MVC   PDESC(10),=C'TOTAL FOR '                                         
         MVC   PDESC+10(L'BUFDESC),BUFDESC                                      
         B     REP100                                                           
*                                                                               
REP50    MVC   PCNUM,BUFCNUM       CHECK NUMBER                                 
         GOTO1 DATCON,DMCB,(0,BUFCDTE),(X'20',PCDTE) CHK DTE PRTBL FRM          
         MVI   PCMP,C'2'           DSP COMPANY CODE                             
         MVC   POFF,BUFOFF                                                      
         MVC   PACCT,BUFACCT                                                    
         MVC   PDESC,=CL24'CHECK PAYMENT INTERFACE'                             
         MVC   PREF1,=CL2'17'                                                   
         MVC   PREF2,=CL2'01'                                                   
         MVC   PDDSCLI,BUFCLI                                                   
*                                                                               
REP100   TM    BUFSTAT,BUFPAYQ                                                  
         BZ    REP200                                                           
         EDIT  (P8,BUFPAY),(12,PPAY),2,FLOAT=-,ZERO=BLANK                       
*                                                                               
REP200   TM    BUFSTAT,BUFGSTQ                                                  
         BZ    REP250                                                           
         EDIT  (P8,BUFGST),(12,PGST),2,FLOAT=-,ZERO=BLANK                       
*                                                                               
REP250   TM    BUFSTAT,BUFQSTQ                                                  
         BZ    REP275                                                           
         EDIT  (P8,BUFQST),(12,PQST),2,FLOAT=-,ZERO=BLANK                       
*                                                                               
REP275   TM    BUFSTAT,BUFHSTQ                                                  
         BZ    REP300                                                           
         EDIT  (P8,BUFHST),(12,PHST),2,FLOAT=-,ZERO=BLANK                       
*                                                                               
REP300   TM    BUFSTAT,BUFCDQ                                                   
         BZ    REP400                                                           
         EDIT  (P8,BUFCD),(12,PCD),2,FLOAT=-,ZERO=BLANK                         
*                                                                               
REP400   TM    BUFSTAT,BUFCSHQ                                                  
         BZ    REP500                                                           
         EDIT  (P8,BUFCSH),(12,PCSH),2,FLOAT=-,ZERO=BLANK                       
*                                                                               
REP500   BAS   RE,PRNTXP                                                        
         CLI   BUFSTAT,BUFALL                                                   
         BNE   EXIT                EXTRA LINE AFTER TOTAL                       
         BAS   RE,PRNTXP                                                        
         CLC   BUFDESC,=CL10'LEDGER'                                            
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'       HEADUP AFTER LEDGER TOTAL                    
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        PRINT A LINE                                            *              
******************************************************************              
*                                                                               
PRNTXP   ST    RE,SAVERE                                                        
         MVC   XHEAD5+1(7),=C'LEDGER:'                                          
         MVC   XHEAD5+9(1),CURLEDG                                              
         GOTO1 ACREPORT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
******************************************************************              
*        BUILD TAPE RECORD                                       *              
******************************************************************              
*                                                                               
BLDTAPE  NTR1                                                                   
         CLI   BUFSTAT,BUFALL      NO TOTAL RECORD AS OUTPUT                    
         BE    EXIT                                                             
         AP    TAPECNT,=P'1'                                                    
         CLI   QOPT1,C'Y'          Y=OUTPUT A TAPE                              
         BNE   EXIT                                                             
         LA    R5,TAPEREC                                                       
         USING TPRECD,R5                                                        
         LA    RE,TAPEREC          CLEAR OUT TAPE AREA                          
         LH    RF,=Y(TPLNQ)                                                     
         LA    R0,XSPACES                                                       
         SR    R1,R1                                                            
         ICM   R1,8,XSPACES                                                     
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TPCNUM,BUFCNUM      CHECK NUMBER                                 
         GOTO1 DATCON,DMCB,(0,BUFCDTE),(X'20',TPCDTE) CHK DTE PRTBL             
         MVI   TPCMP,C'2'          DSP COMPANY CODE                             
         MVC   TPOFF,BUFOFF        OFFICE                                       
         MVC   TPACCT,BUFACCT      ACCOUNT                                      
*                                                                               
         TM    BUFSTAT,BUFPAYQ                                                  
         BZ    *+10                                                             
         ZAP   DUB,BUFPAY                                                       
         TM    BUFSTAT,BUFCSHQ                                                  
         BZ    *+10                                                             
         ZAP   DUB,BUFCSH                                                       
         TM    BUFSTAT,BUFGSTQ                                                  
         BZ    *+10                                                             
         ZAP   DUB,BUFGST                                                       
         TM    BUFSTAT,BUFQSTQ                                                  
         BZ    *+10                                                             
         ZAP   DUB,BUFQST                                                       
         TM    BUFSTAT,BUFHSTQ                                                  
         BZ    *+10                                                             
         ZAP   DUB,BUFHST                                                       
         TM    BUFSTAT,BUFCDQ                                                   
         BZ    *+10                                                             
         ZAP   DUB,BUFCD                                                        
         UNPK  TPAMNT,DUB          AMOUNT                                       
*                                                                               
         MVC   TPDESC,=CL24'CHECK PAYMENT INTERFACE'                            
         MVC   TPREF1,=CL12'17'                                                 
         MVC   TPREF2,=CL12'01'                                                 
         MVC   TPDDSCLI,XSPACES                                                 
         MVC   TPDDSCLI(L'BUFCLI),BUFCLI                                        
*                                                                               
         PUT   TAPE01,TAPEREC      PUT RECORD TO TAPE                           
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        INITIALIZE TAPE OUTPUT                                  *              
******************************************************************              
*                                                                               
INITTAPE NTR1                                                                   
         CLI   QOPT1,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 DYNALLOC,DMCB,(0,=CL8'TAPE01'),DSPARM                            
         OPEN  (TAPE01,OUTPUT)                                                  
         B     EXIT                                                             
         EJECT                                                                  
******************************************************************              
*        CONSTANT DECLARATIONS                                   *              
******************************************************************              
*                                                                               
         GETEL R5,DATADISP,ELCODE                                               
*                                                                               
DSPARM   DC    CL20'ACCTAPE.AC0IDDS1'                                           
EFFS     DC    30X'FF'                                                          
*                                                                               
CSHACCT  DC    C'1060'                                                          
GSTACCT  DC    C'1585'                                                          
QSTACCT  DC    C'2592'                                                          
HSTACCT  DC    C'1585'                                                          
CDACCT   DC    C'9350'                                                          
AGENCY   DC    C'AY'                                                            
         EJECT                                                                  
******************************************************************              
*        EXTERNAL ADDRESS LIST                                   *              
******************************************************************              
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(BUFFALOC)                                                      
         DC    V(DATVAL)                                                        
         DC    A(OFFDEF)                                                        
         DC    A(ACCTBL)                                                        
         DC    A(LDGTBL)                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
******************************************************************              
*        LITERAL DECLARATIONS                                    *              
******************************************************************              
*                                                                               
         LTORG                                                                  
*                                                                               
TAPE01   DCB   DSORG=PS,                                               X        
               MACRF=PM,                                               X        
               DDNAME=TAPE01,                                          X        
               RECFM=FB,                                               X        
               BLKSIZE=108,                                            X        
               LRECL=108                                                        
*                                                                               
         BUFF  FLAVOR=PACKED,                                          X        
               KEYLIST=(24,A),                                         X        
               COMMENT=10,                                             X        
               LINES=100,                                              X        
               COLUMNS=6,                                              X        
               ROWS=1                                                           
         EJECT                                                                  
******************************************************************              
*        BOX HOOK                                                *              
******************************************************************              
*                                                                               
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXCOLS(198),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+6,C'T'            SET ROWS                               
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
         MVI   BOXCOLS,C'L'              SET LH MARGIN                          
         MVI   BOXCOLS+9,C'C'                                                   
         MVI   BOXCOLS+18,C'C'                                                  
         MVI   BOXCOLS+22,C'C'                                                  
         MVI   BOXCOLS+27,C'C'                                                  
         MVI   BOXCOLS+34,C'C'                                                  
         MVI   BOXCOLS+61,C'C'                                                  
         MVI   BOXCOLS+66,C'C'                                                  
         MVI   BOXCOLS+72,C'C'                                                  
         MVI   BOXCOLS+79,C'C'                                                  
         MVI   BOXCOLS+92,C'C'                                                  
         MVI   BOXCOLS+105,C'C'                                                 
         MVI   BOXCOLS+118,C'C'                                                 
         MVI   BOXCOLS+131,C'C'                                                 
         MVI   BOXCOLS+144,C'C'                                                 
         MVI   BOXCOLS+157,C'R'                                                 
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*        OFFICE CODE TRANSLATION TABLE                           *              
******************************************************************              
*                                                                               
* CL2 - OFFICE IN DDS SYSTEM (TRNSANAL)                                         
* CL2 - OFFICE ON THEIR SYSTEM                                                  
*                                                                               
OFFDEF   DS    0F                                                               
         DC    C'1 ',C'AY'         AGENCY                                       
         DC    C'7 ',C'TO'         TORONTO                                      
         DC    C'9 ',C'ML'         MONTREAL                                     
         DC    X'FF'                                                            
         EJECT                                                                  
******************************************************************              
*        ACCOUNT CONVERSION TABLE                                *              
******************************************************************              
*                                                                               
LDGTBL   DS    0F                                                               
         DC    C'ST'                U/L=ST                                      
         DC    C'SQ'                U/L=SQ                                      
         DC    X'FF'                                                            
LDGTLNQ  EQU   2                   LENGTH OF EACH ENTRY                         
         SPACE 5                                                                
******************************************************************              
*        ACCOUNT CONVERSION TABLE                                *              
******************************************************************              
*                                                                               
ACCTBL   DS    0F                                                               
         DC    C'STN',C'2014'      NETWORK TV                                   
         DC    C'STT',C'2015'      SPOT TV                                      
         DC    C'STR',C'2017'      SPOT RADIO                                   
*                                                                               
         DC    C'SQM',C'2010'      MAGAZINE                                     
         DC    C'SQN',C'2011'      NEWSPAPER                                    
         DC    C'SQO',C'2013'      OUTDOOR                                      
         DC    X'FF'                                                            
         EJECT                                                                  
******************************************************************              
*        DSECT TO COVER SAVE W/S                                 *              
******************************************************************              
*                                                                               
LWSD     DSECT                                                                  
VTYPES   DS    0A                  EXTERNAL ADDRESSES                           
ABUFF    DS    A                                                                
DATVAL   DS    A                   DATVAL                                       
AOFFDEF  DS    A                   OFFICE TABLE                                 
AACCTBL  DS    A                   ACCOUNT TABLE                                
ALDGTBL  DS    A                   LEDGER TABLE                                 
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SAVERE   DS    A                   TEMP SAVE AREA FOR REG RE                    
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
ELCODE   DS    XL1                 USED IN GETEL ROUTINE                        
FIRSTSW  DS    CL1                 INDICATES FIRST TIME IN                      
STARTDT  DS    PL3                 YYMMDD START DATE                            
ENDDT    DS    PL3                 YYMMDD END DATE                              
DDSCLI   DS    CL3                 DDS CLIENT CODE FROM CONTRA+12(3)            
CURLEDG  DS    CL1                 HOLDS CURRENT LEDGER BEING PRINTED           
PAYAMT   DS    PL8                                                              
CSHAMT   DS    PL8                                                              
CDAMT    DS    PL8                                                              
GSTAMT   DS    PL8                                                              
QSTAMT   DS    PL8                                                              
HSTAMT   DS    PL8                                                              
TAPECNT  DS    PL8                                                              
MYWORK   DS    CL12                                                             
TAPEREC  DS    CL(TPLNQ)           TAPE BUFFER                                  
*                                                                               
BUFREC   DS    0CL82                                                            
BUFKEY   DS    0CL24                                                            
BUFTYPE  DS    CL1                 ALWAYS C'1'                                  
BUFLEDG  DS    CL1                 LEDGER                                       
BUFCNUM  DS    CL6                 CHECK NUMBER                                 
BUFCDTE  DS    CL6                 CHECK DATE YYMMDD                            
BUFOFF   DS    CL2                 OFFICE CODE                                  
BUFACCT  DS    CL4                 NATURAL ACCOUNT                              
BUFCLI   DS    CL3                 DDS CLIENT CODE                              
BUFSTAT  DS    CL1                 TYPE OF POSTING                              
*                                                                               
BUFDESC  DS    CL10                DESCRIPTION FOR TOTAL                        
BUFPAY   DS    PL8                 DR TO PAYEE                                  
BUFCSH   DS    PL8                 CR TO CASH                                   
BUFCD    DS    PL8                 CR TO INCOME - CD                            
BUFGST   DS    PL8                 DR TO GST                                    
BUFQST   DS    PL8                 DR TO QST                                    
BUFHST   DS    PL8                 DR TO HST                                    
BUFLNQ   EQU   *-BUFREC                                                         
*                                                                               
BUFPAYQ  EQU   X'01'               DR TO PAYEE                                  
BUFCSHQ  EQU   X'02'               CR TO CASH                                   
BUFCDQ   EQU   X'04'               CR TO INCOME - CD                            
BUFGSTQ  EQU   X'08'               DR TO GST                                    
BUFQSTQ  EQU   X'10'               DR TO QST                                    
BUFHSTQ  EQU   X'20'               DR TO HST                                    
BUFALL   EQU   X'FF'                                                            
         EJECT                                                                  
******************************************************************              
*        OFFICE TABLE DSECT                                      *              
******************************************************************              
*                                                                               
OFFCD    DSECT                                                                  
OFFANAL  DS    CL2                 OFFICE FROM DDS TRNSANAL                     
OFFCNV   DS    CL2                 OFFICE TO CONVERT TO                         
OFFCQ    EQU   *-OFFCD                                                          
         SPACE 5                                                                
******************************************************************              
*        ACCOUNT TABLE DSECT                                     *              
******************************************************************              
*                                                                               
ACCTBD   DSECT                                                                  
ACCULM   DS    CL3                 U/L/MEDIA ON DDS SYSTEM                      
ACCCNV   DS    CL4                 CONVERSION ACCT                              
ACCTBQ   EQU   *-ACCTBD                                                         
         EJECT                                                                  
******************************************************************              
*        PRINT LINE DSECT                                        *              
******************************************************************              
*                                                                               
PLINED   DSECT                                                                  
         DS    CL2                                                              
PCNUM    DS    CL6                 CHECK NUMBER                                 
         DS    CL3                                                              
PCDTE    DS    CL6                 CHECK DATE                                   
         DS    CL3                                                              
PCMP     DS    CL1                 ALWAYS C'2'                                  
         DS    CL3                                                              
POFF     DS    CL2                 OFFICE CODE                                  
         DS    CL3                                                              
PACCT    DS    CL4                 NATURAL ACCOUNT                              
         DS    CL3                                                              
PDESC    DS    CL24                DESCRIPTION                                  
         DS    CL3                                                              
PREF1    DS    CL2                                                              
         DS    CL4                                                              
PREF2    DS    CL2                                                              
         DS    CL3                                                              
PDDSCLI  DS    CL3                 DDS CLIENT CODE                              
         DS    CL3                                                              
PPAY     DS    CL12                                                             
         DS    CL1                                                              
PGST     DS    CL12                                                             
         DS    CL1                                                              
PQST     DS    CL12                                                             
         DS    CL1                                                              
PHST     DS    CL12                                                             
         DS    CL1                                                              
PCD      DS    CL12                                                             
         DS    CL1                                                              
PCSH     DS    CL12                                                             
         EJECT                                                                  
******************************************************************              
*        TAPE OUTPUT DSECT                                       *              
******************************************************************              
*                                                                               
TPRECD   DSECT                                                                  
TPKEY    DS    0CL24                                                            
TPCNUM   DS    CL6                 CHECK NUMBER                                 
TPCDTE   DS    CL6                 CHECK DATE YYMMDD                            
TPCMP    DS    CL1                 DSP IS COMPANY "2"                           
TPOFF    DS    CL2                 OFFICE CODE                                  
TPACCT   DS    CL4                 NATURAL ACCOUNT                              
         DS    CL17                RECORD FILLER                                
TPAMNT   DS    CL12                SIGNED FIELD                                 
TPDESC   DS    CL24                DESCRIPTION                                  
TPREF1   DS    CL12                DSP BANK CODE - "17"                         
TPREF2   DS    CL12                DSP CLASS NUM = "01"                         
TPDDSCLI DS    CL12                DDS CLIENT CODE                              
TPLNQ    EQU   *-TPRECD                                                         
         EJECT                                                                  
******************************************************************              
*        OTHER INCLUDES                                          *              
******************************************************************              
*                                                                               
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDCNTRL                                                                
*        ACBIGPRNTD                                                             
*        DDBOXEQUS                                                              
*        DDBIGBOX                                                               
*        DDLOGOD                                                                
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
*        ACMASTD                                                                
*        DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREPID02S05/01/02'                                      
         END                                                                    
