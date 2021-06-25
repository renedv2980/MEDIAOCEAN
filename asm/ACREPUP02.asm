*          DATA SET ACREPUP02  AT LEVEL 098 AS OF 05/01/02                      
*PHASE ACUP02A,+0                                                               
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'UNPEELING REPORT'                                               
ACUP02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACUP**                                                       
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACUPD,RC                                                         
         USING AMTTABD,R7                                                       
         SPACE 3                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
XIT      XMOD1                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              RUN FIRST                                             *          
*--------------------------------------------------------------------*          
RUNF     DS    0H                                                               
         L     R0,=AL4(MAXNTRS*AMTTLNQ*12)                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF               TEST IF STORAGE ACQUIRED                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AAMTTAB          ADDRESS OF AMOUNT TABLE                      
         A     R1,=AL4((MAXNTRS-1)*AMTTLNQ*12)                                  
         ST    R1,AAMTTABX         END OF AMOUNT TABLE                          
*                                                                               
         MVI   OFFICE2,C'N'        NOT 2 BYTE OFFICE COMPANY                    
         L     RF,ADCMPEL                                                       
         USING ACCOMPD,RF                                                       
         TM    ACMPSTA4,X'01'      2 BYTE OFFICE SUPPORTED?                     
         BZ    *+8                                                              
         MVI   OFFICE2,C'Y'                                                     
*                                                                               
         L     RF,=A(IO2)                                                       
         ST    RF,AIO2                                                          
*                                                                               
         ZAP   DRRUN,=P'0'         CLEAR ACCUMS FOR RUN                         
         ZAP   CRRUN,=P'0'                                                      
         B     XIT                                                              
*                                                                               
         DROP  RF                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              REQUEST FIRST                                         *          
*--------------------------------------------------------------------*          
REQF     DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,TODAY2)                                
         ZAP   DRREQ,=P'0'         CLEAR ACCUMS FOR REQUEST                     
         ZAP   CRREQ,=P'0'                                                      
         L     RE,AMONACC                                                       
         USING ACMD,RE                                                          
         XC    ACMFDTE,ACMFDTE    CLEAR FISCAL START SO MONACC                  
*                                 WON'T FILTER TRANS FOR P&L LEDGERS            
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              LEDGER FIRST                                          *          
*--------------------------------------------------------------------*          
LDGF     DS    0H                                                               
         MVI   PAYEESW,C'N'        TEST IF LEDGER IS A PAYEE LEDGER             
         L     RF,ADLEDGER                                                      
         CLI   1(RF),C'S'                                                       
         BNE   XIT                                                              
         CLI   2(RF),C'J'                                                       
         BNE   *+8                                                              
         MVI   FCREVOVR,FCREVTRN+FCREVTRY  INCLUDE REVERSALS                    
*                                                                               
*&&UK*&& LA    RE,UKTABLE                                                       
*&&US*&& LA    RE,USTABLE                                                       
LDGF600  CLI   0(RE),X'FF'                                                      
         BE    XIT                                                              
         CLC   2(1,RF),0(RE)                                                    
         BE    LDGF700                                                          
         LA    RE,1(RE)                                                         
         B     LDGF600                                                          
*                                                                               
LDGF700  MVI   PAYEESW,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              PROCESS ACCOUNT                                       *          
*--------------------------------------------------------------------*          
PACC     DS    0H                                                               
         L     R5,ADACCBAL         ADDR OF BAL ELEM (X'32')                     
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                CANNOT BE ZERO                               
         ZAP   DRTOT,=P'0'         CLEAR ACCUMS FOR ACCOUNT                     
         ZAP   CRTOT,=P'0'                                                      
         MVI   ACCTSW,0            FLAG BYTE FOR ACCOUNT                        
*                                                                               
         CLI   OFFICE2,C'Y'                                                     
         BNE   XIT                                                              
         LA    RF,*+10             SWITCH TO 31-BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         L     RE,AAMTTAB          CLEAR TABLE ON NEW ACCOUNT                   
         L     RF,=AL4(MAXNTRS*AMTTLNQ*12)                                      
         XCEFL                                                                  
         LA    RF,*+6              SET TO 24-BIT MODE                           
         BSM   0,RF                                                             
         MVC   TABPOINT,AAMTTAB    RESET POINTER                                
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              PROCESS TRANSACTION                                   *          
*--------------------------------------------------------------------*          
PTRN     DS    0H                                                               
         L     R6,ADTRANS          POINT TO TRANSACTION ELEM (X'44')            
         USING TRANSD,R6                                                        
         LTR   R6,R6                                                            
         BZ    XIT                                                              
         LR    R3,R6                                                            
         SH    R3,DATADISP         POINT TO KEY                                 
         USING ACKEYD,R3                                                        
         CLC   ACDTPEEL,TODAY2     PEEL DATE = DATECARD                         
         BNE   XIT                                                              
         BAS   RE,FINDMOS          FIND MONTH OF SERVICE FOR TRANS              
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   PT050                                                            
         BAS   RE,FINDLOC          FIND LOCATION IN TABLE                       
         BAS   RE,GETNTRY                                                       
*                                                                               
PT050    TM    TRNSSTAT,X'80'      1=DR  0=CR                                   
         BO    PT080                                                            
         AP    CRTOT,TRNSAMNT      ADD TO ACCOUNT TOTALS                        
         CLI   OFFICE2,C'Y'                                                     
         BNE   PT090                                                            
         AP    AMTTCR,TRNSAMNT                                                  
         BAS   RE,PUTNTRY                                                       
         B     PT090                                                            
PT080    AP    DRTOT,TRNSAMNT                                                   
         CLI   OFFICE2,C'Y'                                                     
         BNE   PT090                                                            
         AP    AMTTDR,TRNSAMNT                                                  
         BAS   RE,PUTNTRY                                                       
*                                                                               
PT090    XC    ACDTPEEL,ACDTPEEL   REMOVE PEEL DATE                             
         NI    TRNSSTAT,(X'FF'-X'10')   TURN OFF PEEL BIT                       
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+16                                                             
         CLI   QOPT1,C'D'          DRAFT RUN?                                   
         BE    *+8                                                              
         MVI   MODE,WRITRANS       SET MODE TO MARK THE FILE                    
         MVI   ACCTSW,X'FF'                                                     
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
* GET ENTRY FROM AMTTAB                                               *         
*---------------------------------------------------------------------*         
*                                                                               
GETNTRY  DS    0H                                                               
         LA    RF,*+10             SWITCH TO 31-BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         L     R7,TABPOINT                                                      
         MVC   AMTNTRY,0(R7)                                                    
*                                                                               
         LA    RF,*+6              SWITCH BACK TO 24-BIT MODE                   
         BSM   0,RF                                                             
         LA    R7,AMTNTRY          RETURN WITH R7 POINT TO ENTRY                
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* PUT ENTRY TO AMTTAB                                                 *         
*---------------------------------------------------------------------*         
*                                                                               
PUTNTRY  DS    0H                                                               
         LA    RF,*+10             SWITCH TO 31-BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         L     R7,TABPOINT                                                      
         MVC   0(L'AMTNTRY,R7),AMTNTRY                                          
*                                                                               
         LA    RF,*+6              SWITCH BACK TO 24-BIT MODE                   
         BSM   0,RF                                                             
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FINDS POSTING MOS FOR TRANSACTION                                   *         
*---------------------------------------------------------------------*         
         USING TRSELD,R6                                                        
FINDMOS  NTR1                      ASSUME R4 POINTS TO TRANS ELEM               
         SR    R1,R1                                                            
FINDM10  CLI   0(R6),0             EOR?                                         
         BE    XIT                 CAN'T FIND LOWEST MOS                        
         CLI   TRSEL,TRSELQ        TRANSACTION STATUS ELEMENT?                  
         BE    FINDM50                                                          
         IC    R1,TRSLN                                                         
         AR    R6,R1                                                            
         B     FINDM10             LOOP UNTIL WE FIND TRSEL                     
*                                                                               
FINDM50  MVC   POSTMOS,TRSPMOS                                                  
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
* FINDS LOCATION ON AMTTAB FOR ENTRY                                  *         
*---------------------------------------------------------------------*         
FINDLOC  DS    0H                                                               
         LA    RF,*+10             SWITCH TO 31 BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
*                                                                               
         L     R7,TABPOINT         NO, BUMP TO NEXT POSITION                    
         CLI   PAYEESW,C'Y'        IGNORE OFFICE IF PAYEE LEDGER                
         BE    FINDL05                                                          
         CLC   AMTTOFF(L'ACKEYWRK+L'ACKEYCON),ACKEYWRK                          
         BNE   *+14                                                             
FINDL05  CLC   AMTTMOS,POSTMOS     ARE OFFICE/CONTRA/MOS THE SAME?              
         BE    FINDL90             LEAVE                                        
*                                                                               
         L     R7,AAMTTAB                                                       
FINDL10  CLI   0(R7),0             END OF OFFICE TABLE                          
         BE    FINDL50             ADD NEW OFFICE HERE                          
         CLC   AMTTOFF(L'ACKEYWRK+L'ACKEYCON),ACKEYWRK                          
         BNE   FINDL30                                                          
         CLC   AMTTMOS,POSTMOS                                                  
         BE    FINDL90                                                          
FINDL30  LA    R7,AMTTLNQ(R7)                                                   
         B     FINDL10                                                          
*                                                                               
FINDL50  C     R7,AAMTTABX         EXCEEDED ALLOCATED SPACE?                    
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AMTTOFF,ACKEYWRK    OFFICE                                       
         MVC   AMTTCNT,ACKEYCON    CONTRA                                       
         MVC   AMTTMOS,POSTMOS     POSTING MOS                                  
         ZAP   AMTTDR,=P'0'                                                     
         ZAP   AMTTCR,=P'0'                                                     
*                                                                               
FINDL90  ST    R7,TABPOINT                                                      
         LA    RF,*+6              SET 24 BIT MODE                              
         BSM   0,RF                                                             
         BR    RE                                                               
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              ACCOUNT LAST                                          *          
*--------------------------------------------------------------------*          
         USING ACBALD,R5                                                        
ACCL     DS    0H                                                               
         CLI   ACCTSW,0                                                         
         BE    XIT                                                              
*                                                                               
         CLI   OFFICE2,C'Y'        2 BYTE OFFICE?                               
         BNE   AL40                                                             
         CLC   ACKEYACC+1(2),=C'SJ'                                             
         BE    AL40                SJ DOESN'T HAVE OFFICE/ACCOUNT RECS          
         BAS   RE,UPDTOFF                                                       
*                                                                               
AL40     L     R5,ADACCBAL         ADDR OF BAL ELEMENT                          
         LTR   R5,R5                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ADDRESS = NO GOOD                         
         CLI   0(R5),X'32'         MUST HAVE BAL ELEMENT                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         AP    ACBLDR,DRTOT        ADJUST DEBIT BAL                             
         SP    ACBLFRWD,DRTOT      ADJUST BAL FWRD                              
         AP    ACBLCR,CRTOT        ADJUST CREDIT BAL                            
         AP    ACBLFRWD,CRTOT      ADJUST BAL FWRD                              
         AP    DRREQ,DRTOT         ROLL FWRD ACCOUNT TOTALS                     
         AP    CRREQ,CRTOT                                                      
         L     R4,ADACC            POINT TO ACCOUNT                             
         MVC   P+1(12),3(R4)                                                    
         L     R4,ADACCNAM         POINT TO ACCOUNT NAME                        
         LA    R5,P+17                                                          
         BAS   RE,NAMOUT                                                        
         LA    R4,DRTOT                                                         
         BAS   RE,EDAMNT                                                        
         GOTO1 MYREPORT                                                         
         SPACE 1                                                                
         L     R3,ADACC                                                         
         MVI   ELCODE,X'33'        FIND PEEL ELEMENT (X'33')                    
         BAS   RE,GETEL                                                         
         BNE   AL50                                                             
         USING ACPEELD,R3                                                       
         GOTO1 DATCON,DMCB,(1,ACPEPLDT),(2,TODAY2P)                             
         CLC   TODAY2P,TODAY2      PEEL DATE = DATECARD DATE                    
         BNE   AL50                                                             
         ZAP   ACPEDR,=P'0'        'ADJUST' PEEL ELEMENT                        
         ZAP   ACPECR,=P'0'                                                     
         XC    ACPEPLDT,ACPEPLDT                                                
AL50     CLI   QOPT1,C'D'          DRAFT RUN?                                   
         BE    XIT                                                              
         CLI   RCWRITE,C'Y'                                                     
         BNE   XIT                                                              
         MVI   MODE,WRITACC                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* UPDTOFF - UPDATE OFFICE ACCOUNTS' LOWEST MOS DATES                  *         
*           AND CONTRA ACCOUNT'S BUCKET ELEMENTS                      *         
***********************************************************************         
         USING CHDRECD,R3                                                       
         USING BUKELD,R4                                                        
*                                                                               
UPDTOFF  NTR1                                                                   
         L     R3,ADACC                                                         
         CLC   =C'SJ',1(R3)        DON'T UPDATE SJ AND 1J                       
         BE    XIT                 DOESN'T HAVE OFFICE ACCOUNTS                 
         CLC   =C'1J',1(R3)                                                     
         BE    XIT                                                              
         MVC   DIRKEY,SPACES                                                    
         MVC   DIRKEY(L'CHDKCULA),0(R3)         COPY ACCOUNT KEY                
         MVC   SVKEY,DIRKEY                                                     
         MVI   RECCHNGD,C'N'                                                    
*                                                                               
         MVC   TABPOINT,AAMTTAB                                                 
UOFF005  BAS   RE,GETNTRY                                                       
         MVC   LASTOFF,AMTTOFF     INITIALIZE                                   
         XC    LASTCNT,LASTCNT                                                  
         ZAP   OFFDR,=P'0'                                                      
         ZAP   OFFCR,=P'0'                                                      
UOFF010  CLI   0(R7),0             EOT?                                         
         BE    UOFF100             YES, LEAVE                                   
*                                                                               
UOFF013  CLC   LASTOFF,AMTTOFF     CHANGE IN OFFICE?                            
         BE    UOFF015                                                          
         OC    LASTCNT,LASTCNT                                                  
         BZ    *+8                                                              
         BAS   RE,WRITEOC                                                       
         CLI   PAYEESW,C'Y'        PAYEE LEDGER, NO OFFICE                      
         BE    UOFF005                                                          
         BAS   RE,UOFF200          UPDATE OFFICE ACCOUNT                        
         B     UOFF005                                                          
*                                                                               
UOFF015  CLC   LASTCNT,AMTTCNT                                                  
         BE    UOFF018                                                          
         OC    LASTCNT,LASTCNT                                                  
         BZ    *+8                                                              
         BAS   RE,WRITEOC                                                       
*                                                                               
         BAS   RE,READOC           READ OFFICE / CONTRA                         
         MVC   LASTCNT,AMTTCNT                                                  
UOFF018  L     R3,AIO2                                                          
         LA    R4,CHDRFST                                                       
*                                                                               
UOFF020  CLI   0(R4),0             EOR, DON'T HAVE BUCKET ELEMENT               
         BNE   *+6                 SHOULD HAVE ONE                              
         DC    H'0'                                                             
         CLI   BUKEL,BUKELQ        BUCKET ELEMENT?                              
         BNE   *+14                                                             
         CLC   BUKMOS,AMTTMOS      WAS THERE ONE BEFORE?                        
         BE    UOFF040             YES, JUST UPDATE BUCKETS                     
         ZIC   R1,BUKLN            NO, BUMP UNTIL WE FIND IT                    
         AR    R4,R1                                                            
         B     UOFF020                                                          
*                                                                               
UOFF040  SP    BUKDR,AMTTDR        ADJUST DEBITS & CREDITS                      
         SP    BUKCR,AMTTCR                                                     
         AP    OFFDR,AMTTDR        OFFICE DEBIT TOTAL                           
         AP    OFFCR,AMTTCR        OFFICE CREDIT TOTAL                          
         MVI   RECCHNGD,C'Y'       RECORD HAS BEEN CHANGED                      
*                                                                               
UOFF054  CLI   QOPT7,C'X'          EXPAND OFFICE                                
         BNE   UOFF080                                                          
         MVC   P+57(2),AMTTOFF                                                  
         MVC   P+60(14),AMTTCNT+1                                               
         CURED AMTTDR,(15,P+72),2,ZERO=NOBLANK,FLOAT=-                          
         CURED AMTTCR,(15,P+87),2,ZERO=NOBLANK,FLOAT=-                          
         MVC   DUB(2),AMTTMOS                                                   
         MVI   DUB+2,X'01'                                                      
         GOTO1 DATCON,DMCB,(1,DUB),(14,P+105),0                                 
         GOTO1 MYREPORT                                                         
*                                                                               
UOFF080  DS    0H                                                               
         LA    RF,*+10             SWITCH TO 31-BIT MODE                        
         O     RF,=X'80000000'                                                  
         BSM   0,RF                                                             
         L     R7,TABPOINT         BUMP TO NEXT OFFICE                          
         LA    R7,AMTTLNQ(R7)                                                   
         ST    R7,TABPOINT                                                      
         LA    RF,*+6              SET TO 24-BIT MODE                           
         BSM   0,RF                                                             
*                                                                               
         BAS   RE,GETNTRY                                                       
         B     UOFF010             LOOP UNTIL ALL DONE                          
*                                                                               
UOFF100  OC    LASTOFF,LASTOFF                                                  
         BZ    UOFF150                                                          
         OC    LASTCNT,LASTCNT                                                  
         BZ    *+8                                                              
         BAS   RE,WRITEOC          UPDATE OFFICE / CONTRA                       
*                                                                               
         CLI   PAYEESW,C'Y'                                                     
         BE    UOFF150                                                          
         BAS   RE,UOFF200          UPDATE OFFICE ACCOUNT FOR LAST ONE           
*                                                                               
UOFF150  B     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
* READ OFFICE / CONTRA RECORD                                         *         
*---------------------------------------------------------------------*         
         USING CHDRECD,R3                                                       
READOC   NTR1                                                                   
         LA    R3,DIRKEY                                                        
         MVC   DIRKEY,SVKEY                                                     
         MVC   CHDKOFF,AMTTOFF     OFFICE                                       
         MVC   CHDKCULC,AMTTCNT    CONTRA ACCOUNT                               
         XC    CHDKNULL,CHDKNULL                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,CHDKEY,DMDKEY                         
         CLC   CHDKEY(CHDKSPCS-CHDKEY),DMDKEY         SAME ACC/OFF/CTR?         
         BE    *+6                                                              
         DC    H'0'                DIE, IF NOT FOUND                            
         MVC   CHDKEY,DMDKEY       COPY NEW KEY                                 
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DMDKEYA,AIO2,DMWORK                    
*                                                                               
         L     R3,AIO2                                                          
         LA    R4,CHDRFST                                                       
         CLI   0(R4),CACELQ        HAS TO BE A CONTRA HEADER ELEM               
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OFFICE / CONTRA RECORD                                       *         
*---------------------------------------------------------------------*         
WRITEOC  NTR1                                                                   
         CLI   RECCHNGD,C'Y'       RECORD HAS BEEN CHANGED?                     
         BNE   XIT                 DON'T WRITE IT                               
         CLI   RCWRITE,C'N'                                                     
         BE    WOC100                                                           
         CLI   QOPT1,C'D'          DRAFT                                        
         BE    WOC100              DON'T WRITE TO FILE                          
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DMDKEYA,AIO2,DMWORK                   
         CLI   DMCB+8,0            DUMP ON ERROR                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WOC100   MVI   RECCHNGD,C'N'       RESET                                        
         CLI   QOPT7,C'X'                                                       
         BNE   XIT                                                              
         L     RF,AIO2                                                          
         XC    DMCB+12(4),DMCB+12                                               
         MVC   DMCB+14(2),CHDRLEN-CHDRECD(RF)                                   
         GOTO1 =V(PRNTBL),DMCB,=C'AOC REC',AIO2,C'DUMP',,=C'1D'                 
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
* UPDATE OFFICE LEVEL                                                 *         
*---------------------------------------------------------------------*         
         USING OFARECD,R3                                                       
         USING ABLELD,R4                                                        
UOFF200  NTR1                                                                   
UOFF215  LA    R3,DIRKEY                                                        
         MVC   DIRKEY,SVKEY                                                     
         MVC   OFAKOFF,LASTOFF                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,OFAKEY,DMDKEY                         
         CLC   OFAKEY(OFAKEND),DMDKEY      SAME ACCOUNT/OFFICE?                 
         BE    *+6                                                              
         DC    H'0'                DIE, IF NOT FOUND                            
         MVC   OFAKEY,DMDKEY       COPY NEW KEY                                 
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DMDKEYA,AIO2,DMWORK                    
*                                                                               
         L     R3,AIO2                                                          
         LA    R4,OFARFST                                                       
UOFF220  CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                SHOULD BE A BALANCE ELEMENT                  
         CLI   ABLEL,ABLELQ        BALANCE ELEMENT?                             
         BE    UOFF230                                                          
         ZIC   R1,ABLLN            NO, BUMP UNTIL WE FIND IT                    
         AR    R4,R1                                                            
         B     UOFF220                                                          
*                                                                               
UOFF230  SP    ABLFRWD,OFFDR       ADJUST BAL B/F                               
         AP    ABLFRWD,OFFCR                                                    
         AP    ABLDR,OFFDR         ADJUST DEBIT BALANCE                         
         AP    ABLCR,OFFCR         ADJUST CREDIT BALANCE                        
*                                                                               
         L     R3,AIO2                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    UOFF280                                                          
         CLI   QOPT1,C'D'          DRAFT                                        
         BE    UOFF280             DON'T WRITE TO FILE                          
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DMDKEYA,AIO2,DMWORK                   
         CLI   DMCB+8,0            DUMP ON ERROR                                
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UOFF280  CLI   QOPT7,C'X'                                                       
         BNE   XIT                                                              
         L     RF,AIO2                                                          
         XC    DMCB+12(4),DMCB+12                                               
         MVC   DMCB+14(2),CHDRLEN-CHDRECD(RF)                                   
         GOTO1 =V(PRNTBL),DMCB,=C'OFF REC',AIO2,C'DUMP',,=C'1D'                 
*                                                                               
UOFF290  B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              REQUEST LAST                                          *          
*--------------------------------------------------------------------*          
REQL     DS    0H                                                               
         CLC   DRREQ(12),=2PL8'0'                                               
         BE    XIT                 NO TRANSACTIONS FOR REQUEST                  
         GOTO1 ACREPORT                                                         
         AP    DRRUN,DRREQ         ROLL FWRD REQUEST TOTALS                     
         AP    CRRUN,CRREQ                                                      
         LA    R4,DRREQ                                                         
         BAS   RE,EDAMNT                                                        
         MVC   P+40(18),=C'TOTALS FOR REQUEST'                                  
         MVI   SPACING,3                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              RUN LAST                                              *          
*--------------------------------------------------------------------*          
RUNL     DS    0H                                                               
         CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         CLC   DRRUN(12),=2PL8'0'                                               
         BE    XIT                 NO TRANSACTIONS FOR RUN                      
         GOTO1 ACREPORT                                                         
         LA    R4,DRRUN                                                         
         BAS   RE,EDAMNT                                                        
         MVC   P+40(14),=C'TOTALS FOR RUN'                                      
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*              VARIOUS AIDS ETC                                      *          
*--------------------------------------------------------------------*          
                                                                                
GETEL    GETEL R3,DATADISP,ELCODE                                               
                                                                                
         USING ACNAMED,R4                                                       
NAMOUT   DS    0H                                                               
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),ACNMNAME                                                 
         BR    RE                                                               
                                                                                
                                                                                
EDAMNT   DS    0H                                                               
         ST    RE,SVRE                                                          
         CP    0(8,R4),=P'0'                                                    
         BE    EDAMNT2                                                          
         CURED (P8,0(R4)),(15,P+60),2,MINUS=YES                                 
         ZAP   0(8,R4),=P'0'                                                    
EDAMNT2  CP    8(8,R4),=P'0'                                                    
         BE    EDAMNT3                                                          
         CURED (P8,8(R4)),(15,P+78),2,MINUS=YES                                 
         ZAP   8(8,R4),=P'0'                                                    
EDAMNT3  L     RE,SVRE                                                          
         BR    RE                                                               
                                                                                
                                                                                
MYREPORT NTR1                                                                   
         MVC   HEAD5+10(1),QLEDGER                                              
         LA    R5,HEAD5+12                                                      
         L     R4,ADLDGNAM                                                      
         BAS   RE,NAMOUT                                                        
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              CONSTANTS                                              *         
*---------------------------------------------------------------------*         
UKTABLE  DC    C'VXFT'                                                          
         DC    X'FF'                                                            
USTABLE  DC    C'PQSTUVWXY'                                                     
         DC    X'FF'                                                            
*                                                                               
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCDIR   DC    CL8'ACCDIR '                                                     
ACCMST   DC    CL8'ACCMST '                                                     
PUTREC   DC    CL8'PUTREC '                                                     
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              EQUATES                                                *         
*---------------------------------------------------------------------*         
MAXNTRS  EQU   12000                                                            
*                                                                               
         DC    C'** IO AREA*'                                                   
IO       DS    2000C                                                            
*                                                                               
         DC    C'** IO 2   *'                                                   
IO2      DS    2000C                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              DSECT FOR PROGRAM                                                
*---------------------------------------------------------------------*         
                                                                                
ACUPD    DSECT                                                                  
DRTOT    DS    PL8                 ACCUMS                                       
CRTOT    DS    PL8                                                              
DRREQ    DS    PL8                                                              
CRREQ    DS    PL8                                                              
DRRUN    DS    PL8                                                              
CRRUN    DS    PL8                                                              
TODAY2   DS    CL2                 FROM DATECARD                                
TODAY2P  DS    CL2                 FROM PEEL ELEMENT (X'33')                    
ELCODE   DS    CL1                                                              
ACCTSW   DS    CL1                 FLAG FOR ACCOUNT PRINT                       
OFFICE2  DS    CL1                 2 BYTE OFFICE                                
CURROFF  DS    CL2                 CURRENT OFFICE                               
POSTMOS  DS    CL2                                                              
DIRKEY   DS    CL56                                                             
RECCHNGD DS    CL1                                                              
LASTOFF  DS    CL2                                                              
OFFDR    DS    PL8                                                              
OFFCR    DS    PL8                                                              
LASTCNT  DS    CL15                                                             
PAYEESW  DS    CL1                                                              
AIO2     DS    A                                                                
AAMTTAB  DS    A                   POINTER TO AMOUNT TABLE                      
AAMTTABX DS    A                   POINTER TO END OF AMOUNT TABLE               
TABPOINT DS    A                   POINTER TO AMOUNT TABLE                      
SVRE     DS    A                                                                
AMTNTRY  DS    CL(AMTTLNQ)         COPY OF CURRENT AMOUNT ENTRY                 
SVKEY    DS    CL56                                                             
DMDKEY   DS    CL56                                                             
         ORG   DMDKEY+(TRNKDA-TRNRECD)                                          
DMDKEYA  DS    0XL4                                                             
         ORG   DMDKEY                                                           
         DS    CL56                                                             
         EJECT                                                                  
AMTTABD  DSECT                                                                  
AMTTOFF  DS    CL2                 OFFICE                                       
AMTTCNT  DS    CL15                CONTRA                                       
AMTTMOS  DS    PL2                 MONTH OF SERVICE FOR X'45' BUCKETS           
AMTTDR   DS    PL8                 DEBIT                                        
AMTTCR   DS    PL8                 CREDIT                                       
AMTTLNQ  EQU   *-AMTTOFF                                                        
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENMODES                                                             
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098ACREPUP02 05/01/02'                                      
         END                                                                    
