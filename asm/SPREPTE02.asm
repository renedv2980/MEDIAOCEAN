*          DATA SET SPREPTE02  AT LEVEL 157 AS OF 04/01/03                      
*PHASE SPTE02A                                                                  
         TITLE 'SPREPTE02 - TEXACO INTERFACE'                                   
*                                                                               
*     CHANGE LOG                                                                
*                                                                               
*   BPLA    6/00     JWT VENDOR CODE ADDED                                      
*                                                                               
***********************************************************************         
*   QOPT1 -  D=DUMP OUTPUT RECORDS                                              
*                                                                               
***********************************************************************         
SPTE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPTE02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,CLTLAST                                                     
         BE    CLTL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*        RUNFRST                                                                
         SPACE 2                                                                
RUNF     DS    0H                                                               
*                                  RELOCATE ADDRESSES                           
         RELOC RELO                                                             
         LA    R0,(ACONSX-ACONS)/4      NO. OF ADDRS                            
         LA    R2,ACONS                                                         
         LA    R3,RCONS                                                         
RUNF2    DS    0H                                                               
         L     RF,0(R2)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R3)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,RUNF2                                                         
*                                                                               
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
         MVI   DASHES,C'-'                                                      
         MVC   DASHES+1(L'DASHES-1),DASHES                                      
*                                                                               
*                                  SET BUFFALO PARAMS                           
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFFC                                      
*                                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        REQFRST                                                                
         SPACE 2                                                                
REQF     DS    0H                                                               
         MVC   SVQOPT1,QOPT1                                                    
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 (RF),(R1),,(3,BQSTART)                                           
         GOTO1 (RF),(R1),,(5,PSTART)                                            
         GOTO1 (RF),(R1),QEND,(2,BQENDP)                                        
         GOTO1 (RF),(R1),,(3,BQEND)                                             
         GOTO1 (RF),(R1),,(5,PEND)                                              
*                                                                               
         ZAP   QTNET,=P'0'                                                      
         ZAP   QTGRS,=P'0'                                                      
         ZAP   QTDUE,=P'0'                                                      
         ZAP   ITGRS,=P'0'                                                      
         ZAP   ITNET,=P'0'                                                      
*                                                                               
         CLC   QEST,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   QEST,=C'NO '                                                     
*                                                                               
         CLI   FIRST,0             FIRST TIME TEST                              
         BNE   REQF20                                                           
         MVI   FIRST,1                                                          
*                                                                               
         ZAP   GTNET,=P'0'                                                      
         ZAP   GTGRS,=P'0'                                                      
         ZAP   GTDUE,=P'0'                                                      
         ZAP   GTINVS,=P'0'                                                     
         ZAP   GTLINS,=P'0'                                                     
*                                                                               
         LA    R5,OUTFILE                                                       
         OPEN  ((R5),OUTPUT)                                                    
*                                                                               
REQF20   DS    0H                                                               
         B     EXIT                                                             
         SPACE 3                                                                
*        CLTFRST                                                                
CLTF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDFRST                                                                
PRDF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        ESTFRST                                                                
ESTF     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        PRDLAST                                                                
PRDL     DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        CLT LAST                                                               
         SPACE 2                                                                
CLTL     DS    0H                                                               
         GOTO1 ARNBILL                                                          
         GOTO1 AROBILL                                                          
         GOTO1 AREPRT                                                           
         B     EXIT                                                             
         SPACE 3                                                                
*        REQ LAST                                                               
REQL     DS    0H                                                               
         GOTO1 APRNT                                                            
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   LDDAT(20),=C'** REQUEST TOTALS **'                               
         EDIT  (P8,QTNET),(15,LNET),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,QTGRS),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   COMMISSION TOTAL '                               
         ZAP   DUB,QTDUE                                                        
         SP    DUB,QTNET                                                        
         EDIT  (P8,DUB),(15,LGRS),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   BILL AMOUNT TOTAL'                               
         EDIT  (P8,QTDUE),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
         B     EXIT                                                             
         SPACE 3                                                                
*        RUN LAST                                                               
RUNL     DS    0H                                                               
         GOTO1 APRNT                                                            
         LA    R2,P                                                             
         USING LINED,R2                                                         
         MVC   LDDAT(16),=C'** RUN TOTALS **'                                   
         EDIT  (P8,GTNET),(15,LNET),2,COMMAS=YES,MINUS=YES                      
         EDIT  (P8,GTGRS),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   COMMISSION TOTAL '                               
         ZAP   DUB,GTDUE                                                        
         SP    DUB,GTNET                                                        
         EDIT  (P8,DUB),(15,LGRS),2,COMMAS=YES,MINUS=YES                        
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
         MVC   LDDAT(20),=C'   BILL AMOUNT TOTAL'                               
         EDIT  (P8,GTDUE),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         MVI   SPACING,1                                                        
         GOTO1 APRNT                                                            
*                                                                               
         MVC   P+2(14),=C'INVOICE COUNT='                                       
         EDIT  (P8,GTINVS),(7,P+21)                                             
         GOTO1 APRNT                                                            
         MVC   P+2(18),=C'DETAIL LINE COUNT='                                   
         EDIT  (P8,GTLINS),(7,P+21)                                             
         GOTO1 APRNT                                                            
*                                                                               
         CLI   ERROR,0                                                          
         BE    RUNL4                                                            
         MVC   P(45),=C'***** ERRORS - NO OUTPUT FILE GENERATED *****'          
         GOTO1 APRNT                                                            
         B     RUNLX                                                            
*                                                                               
RUNL4    DS    0H                                                               
RUNL8    DS    0H                                                               
         CLOSE (OUTFILE)                                                        
*                                                                               
         CLI   SVQOPT1,C'D'        DUMP OUTPUT RECS?                            
         BNE   RUNL10                                                           
         OPEN  (OUTFILR,INPUT)                                                  
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
RUNL9    DS    0H                                                               
         LA    R1,OUTFILR                                                       
         LA    R0,OUTREC                                                        
         GET   (R1),(R0)                                                        
         MVC   P(100),OUTREC                                                    
         MVC   P2(100),OUTREC+100                                               
         MVC   P3(100),OUTREC+200                                               
         MVI   SPACING,2                                                        
         GOTO1 APRNT                                                            
         B     RUNL9                                                            
*                                                                               
RUNL10   DS    0H                                                               
RUNLX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
OUTFEOD  DS    0H                                                               
         CLOSE (OUTFILR)                                                        
         B     RUNLX                                                            
         SPACE 3                                                                
         LTORG                                                                  
         SPACE 3                                                                
ACONS    DS    0F                                                               
         DC    A(PRNT)                                                          
         DC    A(RNBILL)                                                        
         DC    A(ROBILL)                                                        
         DC    A(REPRT)                                                         
         DC    A(STABUCKC)                                                      
         DC    A(BUFFALOC)                                                      
ACONSX   EQU   *                                                                
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=SPTEXOUT,DSORG=PS,RECFM=FB,LRECL=503,            X        
               BLKSIZE=5030,MACRF=PM                                            
OUTFILR  DCB   DDNAME=SPTEXOUT,DSORG=PS,RECFM=FB,LRECL=503,            X        
               BLKSIZE=5030,MACRF=GM,EODAD=OUTFEOD                              
         TITLE 'RNBILL - READ NEW BILL RECORDS'                                 
RNBILL   CSECT                                                                  
         NMOD1 0,RNBILL                                                         
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         MVC   KEY1,KEY                 SAVE KEY                                
         XC    KEY,KEY                                                          
RNB2     DS    0H                                                               
         MVC   KEY(2),=X'0E01'          RECORD TYPE                             
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         CLI   KPRD,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-PRD            
         MVC   KEY+5(1),KPRD                                                    
RNB2EST  DS    0H                                                               
         MVC   KEY+6(1),BEST                                                    
         XC    KEY+7(6),KEY+7                                                   
         CLI   BEST,0                                                           
         BE    RNB3                SKIP REST OF KEY NOW IF MULTI-EST            
RNB2MKT  DS    0H                                                               
         MVC   KEY+7(2),KMKT                                                    
         XC    KEY+9(4),KEY+9                                                   
         OC    KMKT,KMKT                                                        
         BZ    RNB3                SKIP STA NOW IF MULTI-MKT                    
RNB2STA  DS    0H                                                               
         MVC   KEY+9(3),KSTA                                                    
RNB3     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     RNB4B                                                            
RNB4     DS    0H                                                               
         GOTO1 SEQ                                                              
RNB4B    DS    0H                                                               
         CLC   KEY(5),KEYSAVE      AGM/CLT                                      
         BNE   RNB40                                                            
         CLI   KPRD,0                                                           
         BE    RNB5                                                             
         CLC   KPRD,KEY+5          ONE PROD MUST BE EQUAL                       
         BNE   RNB40                                                            
*                                                                               
RNB5     DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB7                                                             
*                                  ONE EST OR SERIES                            
         CLC   KEY+6(1),BEST                                                    
         BL    RNB2EST                                                          
         BE    RNB7                                                             
         CLI   BESTEND,0                                                        
         BE    RNB6D                                                            
         CLC   KEY+6(1),BESTEND                                                 
         BNH   RNB7                                                             
*                                                                               
RNB6D    DS    0H                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BNE   RNB40               DONE IF ONE PRD                              
         IC    RF,KEY+5            ELSE NEXT PROD                               
         LA    RF,1(RF)                                                         
         STC   RF,KEY+5                                                         
         B     RNB2EST                                                          
*                                                                               
RNB6H    DS    0H                  BUMP TO NEXT EST                             
         ZIC   RF,KEY+6                                                         
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         B     RNB2MKT                                                          
*                                                                               
RNB7     DS    0H                  MARKET                                       
         OC    KMKT,KMKT                                                        
         BZ    RNB8                                                             
         CLC   KEY+7(2),KMKT       ONE MKT                                      
         BE    RNB9                                                             
         BL    RNB2MKT                                                          
*                                  MKT HIGH                                     
*                                  IF MULT-EST BUMP TO NEXT EST                 
RNB7B    DS    0H                                                               
         CLI   BEST,0                                                           
         BE    RNB6H                                                            
         CLI   BESTEND,0                                                        
         BNE   RNB6H                                                            
         B     RNB6D               NEXT PRD (IF MULTI-PRD)                      
*                                                                               
RNB8     DS    0H                  MULTI-MKT                                    
RNB9     DS    0H                                                               
         OC    KSTA,KSTA           STATION                                      
         BZ    RNB10                                                            
         CLC   KSTA,KEY+9          ONE STA                                      
         BL    RNB2STA                                                          
         BH    RNB7B               NEXT EST -PRD (IF MULTI)                     
         SPACE 3                                                                
*                                  HAVE GOOD KEY                                
RNB10    DS    0H                                                               
         L     R7,ADSTABUC                                                      
         ST    R7,AREC                                                          
         USING STABUCK,R7                                                       
         GOTO1 GET                                                              
*                                                                               
         MVI   ELCODE,X'0E'                                                     
         LA    R2,STABELEM                                                      
         USING STABELEM,R2                                                      
*                                                                               
RNB11    DS    0H                                                               
         CLC   ELCODE,0(R2)                                                     
         BE    RNB13                                                            
*                                                                               
RNB12    DS    0H                                                               
         BAS   RE,RNBNXTEL                                                      
         BNE   RNB30                                                            
*                                                                               
RNB13    DS    0H                                                               
*                                  TEST IN REQ PERIOD                           
         CLC   STABBDT,BQSTARTP                                                 
         BL    RNB12                                                            
         CLC   STABBDT,BQENDP                                                   
         BH    RNB12                                                            
*                                                                               
RNB13T   DS    0H                                                               
         XC    X,X                                                              
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(3,WORK)                                 
         MVC   INVINV(1),WORK+1                                                 
         MVC   INVINV+1(2),STABINV                                              
         NI    INVINV+1,X'FF'-(STABINV_REVERSAL+STABINV_REVERSED)               
*                                       FIND PRD CODE                           
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDR(RF)                                              
*                                                                               
RNB14    DS    0H                                                               
         CLC   STABKPRD,3(RF)                                                   
         BE    RNB15                                                            
         LA    RF,4(RF)                                                         
         B     RNB14                                                            
*                                                                               
RNB15    DS    0H                                                               
         MVC   INVPRD,0(RF)                                                     
         MVC   INVEST,STABKEST                                                  
         MVC   INVPER,STABPER                                                   
         MVC   INVRDAT,STABBDT                                                  
         MVC   INVMKT(5),STABKMKT                                               
         ICM   RF,15,STABGRS                                                    
         CVD   RF,DUB                                                           
         ZAP   INVGRS,DUB                                                       
         ICM   RF,15,STABNET                                                    
         CVD   RF,DUB                                                           
         ZAP   INVNET,DUB                                                       
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
         B     RNB12                                                            
*                                                                               
RNB30    DS    0H                                                               
         B     RNB4                                                             
*                                                                               
RNB40    DS    0H                                                               
RNBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
RNBNXTEL DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    RNBNXTL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     RNBNXTEL                                                         
RNBNXTL2 DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'ROBILL  - READ OLD BILL RECORDS'                                
ROBILL   CSECT                                                                  
         NMOD1 0,ROBILL                                                         
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
ROB2     DS    0H                                                               
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         CLI   KPRD,0                                                           
         BE    ROB3                SKIP REST OF KEY IF MULTI-PRD                
         MVC   KEY+4(3),PRD                                                     
ROB2EST  DS    0H                                                               
         MVC   KEY+7(1),BEST                                                    
         XC    KEY+8(5),KEY+8                                                   
         CLI   BEST,0                                                           
         BE    ROB3                SKIP REST OF KEY IF MULTI-EST                
ROB2PER  DS    0H                                                               
         XC    KEY+8(5),KEY+8                                                   
*                                                                               
ROB3     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     ROB4B                                                            
ROB4     DS    0H                                                               
         GOTO1 SEQ                                                              
ROB4B    DS    0H                                                               
         CLC   KEY(4),KEYSAVE      AGM/CLT                                      
         BNE   ROB40                                                            
         CLI   KPRD,0                                                           
         BE    ROB5                                                             
         CLC   PRD,KEY+4           ONE PRD MUST BE EQUAL                        
         BNE   ROB40                                                            
ROB5     DS    0H                  MULTI-PROD SITUATION                         
         OC    KEY+8(5),KEY+8                                                   
         BZ    ROB4                BYPASS IF NOT A BILL                         
*                                                                               
*                                                                               
ROB6     DS    0H                                                               
         CLI   BEST,0                                                           
         BE    ROB7                                                             
*                                  ONE EST OR SERIES                            
         CLC   KEY+7(1),BEST                                                    
         BL    ROB2EST                                                          
         BE    ROB7                                                             
*                                                                               
         CLI   BESTEND,0                                                        
         BE    ROB6D                                                            
         CLC   KEY+7(1),BESTEND                                                 
         BNH   ROB7                                                             
*                                                                               
ROB6D    DS    0H                  EST NOT OK                                   
         CLI   KPRD,0                                                           
         BNE   ROB40               DONE IF ONE PROD                             
         IC    RF,KEY+6            BUMP TO NEXT PROD                            
         LA    RF,1(RF)                                                         
         STC   RF,KEY+6                                                         
         B     ROB2EST             SET KEY FROM EST DOWN                        
*                                                                               
ROB7     DS    0H                  BILL DATE                                    
         MVC   BYTE(1),KEY+10                                                   
         NI    BYTE,X'F0'                                                       
         PACK  BILDAT(1),BYTE(1)                                                
         ZIC   RF,BILDAT                                                        
         LA    RF,90(RF)                                                        
         CLI   BILDAT,3                                                         
         BH    *+8                                                              
         LA    RF,10(RF)                                                        
         STC   RF,BILDAT                                                        
*                                                                               
         MVC   BILDAT+1(1),KEY+10                                               
         NI    BILDAT+1,X'0F'                                                   
*                                                                               
         CLC   BILDAT(2),BQSTART                                                
         BL    ROB4                                                             
         CLC   BILDAT(2),BQEND                                                  
         BH    ROB4                                                             
*                                  HAVE GOOD KEY                                
*                                  PASS DATA TO SORT                            
ROB10    DS    0H                                                               
         GOTO1 GETBILL                                                          
         L     R7,ADBILL                                                        
         USING BILLREC,R7                                                       
*                                                                               
         CLC   BDATE,QSTART                                                     
         BL    ROB4                                                             
         CLC   BDATE,QEND                                                       
         BH    ROB4                                                             
         TM    BILSTAT,X'20'       SKIP AOR BILLS                               
         BNZ   ROB4                SKIP NET (USED FOR SOMETHING ELSE)           
*                                                                               
ROB10B   DS    0H                                                               
         XC    X,X                                                              
         LA    R4,X                                                             
         USING INVD,R4                                                          
*                                                                               
         MVC   INVINV(1),BILDAT+1                                               
         MVC   INVINV+1(2),BKEYINV                                              
*                                                                               
         MVC   INVPRD,BKEYPRD                                                   
         MVC   INVEST,BKEYEST                                                   
         XC    HALF,HALF                                                        
         CLI   BLMKT,C' '          ANY MARKET IN HEADER?                        
         BNH   ROB10F                                                           
         OC    BLMKT,=4C'0'                                                     
         PACK  DUB,BLMKT                                                        
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
ROB10F   DS    0H                                                               
         MVC   INVHMKT,HALF                                                     
*                                                                               
         ZAP   INVDUE,BACTP        AMOUNT DUE                                   
         ZAP   INVNET,BNETP        NET                                          
         ZAP   INVGRS,BGRSP        GROSS                                        
*                                                                               
         GOTO1 DATCON,DMCB,BDATE,(2,INVRDAT)                                    
         GOTO1 (RF),(R1),BQDATE,(2,INVINVD)                                     
         GOTO1 (RF),(R1),(3,BDUEDATE),(2,INVDUED)                               
*                                                                               
ROB11    DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
*                                                                               
         B     ROB4                                                             
*                                                                               
ROB40    DS    0H                                                               
ROBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    CSECT                                                                  
         NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         XC    SVHDR,SVHDR                                                      
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,P                                                             
         USING LINED,R2                                                         
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVD,R4                                                          
REP2     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFREC,0                           
         B     REP4B                                                            
REP4     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFREC,0                            
REP4B    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
         OC    INVPER,INVPER       TEST FOR HEADER                              
         BNZ   REP10                                                            
*                                  INVOICE HEADER                               
         OC    SVHDR,SVHDR         ANY OLD HDR TO FINISH?                       
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 APRNT                                                            
         BAS   RE,HDROUT           DO HEADER OUTPUT                             
         ZAP   ITGRS,=P'0'         CLEAR INV CHECKING TOTALS                    
         ZAP   ITNET,=P'0'                                                      
         ZIC   RF,INVINV                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINV(2),DUB                                                      
         MVI   LINV+2,C'-'                                                      
         MVC   HALF,INVINV+1                                                    
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LINV+3(4),DUB                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVINVD),(5,LIDAT)                                
         GOTO1 (RF),(R1),(2,INVDUED),(5,LDDAT)                                  
         GOTO1 (RF),(R1),(2,INVRDAT),(5,LRDAT)                                  
*                                                                               
         MVC   LPRD,INVPRD         PRD                                          
*                                                                               
         ZIC   RF,INVEST           EST                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LEST,DUB                                                         
*                                                                               
         MVC   SVHDR,INVD          SAVE HEADER                                  
         AP    GTINVS,=P'1'        BUMP INVOICE COUNT                           
         B     REP30                                                            
*                                                                               
REP10    DS    0H                  DETAIL LINE                                  
         BAS   RE,DETOUT           DO DETAIL OUTPUT                             
         GOTO1 DATCON,DMCB,(3,INVPER),(6,WORK)                                  
         MVC   LPER,WORK                                                        
*                                  MKT-STA                                      
         GOTO1 MSUNPK,DMCB,INVMKT,LMKT,LSTA                                     
*                                                                               
REP22    DS    0H                                                               
         EDIT  (P8,INVGRS),(15,LGRS),2,COMMAS=YES,MINUS=YES                     
         EDIT  (P8,INVNET),(15,LNET),2,COMMAS=YES,MINUS=YES                     
*                                                                               
         AP    QTGRS,INVGRS                                                     
         AP    GTGRS,INVGRS                                                     
         AP    ITGRS,INVGRS                                                     
         AP    QTNET,INVNET                                                     
         AP    GTNET,INVNET                                                     
         AP    ITNET,INVNET                                                     
         AP    GTLINS,=P'1'                                                     
*                                                                               
         GOTO1 APRNT                                                            
*                                                                               
REP30    DS    0H                                                               
         B     REP4                                                             
*                                                                               
REP50    DS    0H                                                               
         OC    SVHDR,SVHDR         ANY HEADER TO FINISH UP?                     
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
*                                                                               
REPX     DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
*        FINISH UP OLD HEADER                                                   
         SPACE 2                                                                
ENDINV   NTR1                                                                   
         MVC   LPER(15),=C'*INVOICE TOTAL*'                                     
         LA    R1,SVHDR+INVGRS-INVD                                             
         EDIT  (P8,0(R1)),(15,LGRS),2,COMMAS=YES,MINUS=YES                      
         LA    R1,SVHDR+INVNET-INVD                                             
         EDIT  (P8,0(R1)),(15,LNET),2,COMMAS=YES,MINUS=YES                      
*                                                                               
         CLC   ITGRS,SVHDR+INVGRS-INVD                                          
         BNE   EINV4                                                            
         CLC   ITNET,SVHDR+INVNET-INVD                                          
         BNE   EINV4                                                            
*                                                                               
         LA    R3,OUTREC                                                        
         USING TXDATA,R3                                                        
*                                                                               
         LA    RE,TXDATA           CLEAR RECORD                                 
         LA    RF,L'TXDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TXTT,=C'80'                                                      
         UNPK  TXTNET,ITNET                                                     
         TM    TXTNET+L'TXTNET-1,X'10'  IS IT NEGATIVE?                         
         BZ    *+8                                                              
         MVI   TXTNET,C'-'                                                      
         OI    TXTNET+L'TXTNET-1,X'F0'  NO OVERPUNCH                            
*                                                                               
         ZAP   DUB,SVHDR+INVDUE-INVD(8)                                         
         SP    DUB,ITNET           DUE-NET=COMM                                 
         UNPK  TXTCOM,DUB                                                       
         TM    TXTCOM+L'TXTCOM-1,X'10'  IS IT NEGATIVE?                         
         BZ    *+8                                                              
         MVI   TXTCOM,C'-'                                                      
         OI    TXTCOM+L'TXTCOM-1,X'F0'  NO OVERPUNCH                            
*                                                                               
         UNPK  TXTCD,=X'0F'              CASH DISC = 0                          
*                                                                               
         UNPK  TXTDUE,SVHDR+INVDUE-INVD(8)     AMOUNT DUE                       
         TM    TXTDUE+L'TXTDUE-1,X'10'  IS IT NEGATIVE?                         
         BZ    *+8                                                              
         MVI   TXTDUE,C'-'                                                      
         OI    TXTDUE+L'TXTDUE-1,X'F0'  NO OVERPUNCH                            
*                                                                               
         AP    QTDUE,SVHDR+INVDUE-INVD(8)   ADD TO TOTAL DUE AND COMM           
         AP    GTDUE,SVHDR+INVDUE-INVD(8)                                       
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         B     EINVX                                                            
         DROP  R3                                                               
*                                                                               
EINV4    DS    0H                                                               
         MVC   LMSG(18),=C'**OUT OF BALANCE**'                                  
         MVI   ERROR,C'Y'                                                       
*                                                                               
EINVX    DS    0H                                                               
         GOTO1 APRNT                                                            
         XIT1                                                                   
         EJECT                                                                  
*        HEADER OUTPUT                                                          
         SPACE 2                                                                
HDROUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING TXDATA,R3                                                        
*                                                                               
         LA    RE,TXDATA           CLEAR RECORD                                 
         LA    RF,L'TXDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TXDATA(13),=C'*****TEX01810'                                     
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
         MVC   TXHH,=C'10'         HEADER RECORD                                
*                                                                               
*                                                                               
         MVC   TXHVENCD(13),=C'0000500087994'   JWT VENDOR CODE                 
         CLC   QAGY,=C'JW'                                                      
         BE    HDR02                                                            
         CLC   QAGY,=C'H7'                      MINDSHARE VENDOR CODE           
         BE    HDR02                                                            
*                                                                               
         MVC   TXHVENCD(13),=C'1329938710001'   BATES VENDOR CODE               
         CLC   QAGY,=C'BS'                                                      
         BE    HDR02                                                            
         CLC   QAGY,=C'TH'                      ZENITH                          
         BE    HDR02                                                            
*                                                                               
         MVC   TXHVENCD(13),=C'1331980750001'   DFCN AND DWCN                   
         CLC   QAGY,=C'DF'                                                      
         BE    HDR02                                                            
         CLC   QAGY,=C'DW'                                                      
         BE    HDR02                                                            
*                                                                               
         MVC   TXHVENCD(13),=C'0000500000460'   BBDO VENDOR CODE                
         CLC   QAGY,=C'BD'                                                      
         BE    HDR02                                                            
         CLC   QAGY,=C'BN'                       AND BN?                        
         BE    HDR02                                                            
*                                                                               
         MVC   TXHVENCD(13),=C'0000500050808'   OMNYA VENDOR CODE               
         CLC   QAGY,=C'OM'                                                      
         BE    HDR02                                                            
*                                    ELSE MUST BE CME                           
*        OLD VENDOR CODES FOR CME                                               
*                                                                               
         MVC   TXHVENCD(13),=C'7412825880001'   HOUSTON OFFICE                  
         L     RF,ADCLT                                                         
         CLI   COFFICE-CLTHDR(RF),C'Z'          Z IS HOUSTON                    
         BE    *+10                             ELSE NEW YORK                   
         MVC   TXHVENCD(13),=C'4109856650002'   (SHOULD BE V)                   
*                                                                               
HDR02    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,INVINVD),(20,DUB)                                 
         MVC   TXHDATE+0(4),DUB+4    MMDD                                       
         MVC   TXHDATE+4(4),DUB+0    YEAR                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,INVDUED),(20,DUB)                                 
         MVC   TXHDUED+0(4),DUB+4    MMDD                                       
         MVC   TXHDUED+4(4),DUB+0    YEAR                                       
*                                                                               
         ZIC   RF,INVINV           INVOICE NUMBER                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHINUM(2),DUB                                                   
         MVC   HALF,INVINV+1                                                    
         LH    RF,HALF                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHINUM+2(4),DUB                                                 
*                                  VENDOR CONTROL # = CLTPRDEST                 
         MVC   TXHVENCN(3),CLT                                                  
         CLI   TXHVENCN+2,C' '      SPACE TO ZERO                               
         BH    *+8                                                              
         MVI   TXHVENCN+2,C'0'                                                  
         MVC   TXHVENCN+3(3),INVPRD                                             
         CLI   TXHVENCN+5,C' '      SPACE TO ZERO                               
         BH    *+8                                                              
         MVI   TXHVENCN+5,C'0'                                                  
         ZIC   R0,INVEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHVENCN+6(3),DUB                                                
*                                                                               
         MVC   TXHPRD,INVPRD+1    (LAST 2 POS OF PRD CODE)                      
         SR    R0,R0                                                            
         ICM   R0,3,INVHMKT       HEADER MARKET                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TXHMKT,DUB                                                       
*                                                                               
*                         ***NOTE  STILL NEED TO DO-                            
*                                    ORIGINAL INVOICE NUMBER                    
*                                                                               
*                                  GET EST DESC FIELDS                          
         XC    KEY,KEY             NOTE- WONT HAVE TO RESET SEQ                 
         LA    R5,KEY                                                           
         USING ESTHDR,R5                                                        
         L     RF,ADCLT                                                         
         MVC   EKEY,0(RF)                                                       
         MVC   EKEYPRD,INVPRD                                                   
         MVC   EKEYEST,INVEST                                                   
*                                                                               
         L     RF,ADEST                                                         
         CLC   KEY(13),0(RF)       TEST ALREADY HAVE ESTHDR                     
         BE    HDR06                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
HDR06    DS    0H                                                               
         L     R5,ADEST                                                         
         MVC   TXHIDESC(20),EDESC    ESTIMATE NAME                              
         OC    TXHIDESC(20),SPACES                                              
         OC    EUSER1,SPACES                                                    
         MVC   TXHBUDYR,EUSER1+0                                                
         MVC   TXHMBFL,EUSER1+4                                                 
         MVC   TXHVEH,EUSER1+8                                                  
         MVC   TXHATYP,EUSER1+11                                                
         MVC   TXHSVC,EUSER1+13                                                 
         MVC   TXHTERMS,EUSER1+15                                               
         MVC   TXHSUPES(3),EUSER1+5                                             
         MVC   TXHPAYR,EUSER1+18                                                
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
HDRO8    DS    0H                                                               
         DROP  R3                                                               
         XIT1                                                                   
*                                                                               
ESTERR   DS    0H                                                               
         MVI   ERROR,1                                                          
         MVC   LMSG(15),=C'**EST DESC ERR-'                                     
         MVC   LMSG+15(20),EDESC                                                
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*        DETAIL OUTPUT                                                          
         SPACE 2                                                                
DETOUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING TXDATA,R3                                                        
*                                                                               
         LA    RE,TXDATA           CLEAR RECORD                                 
         LA    RF,L'TXDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TXDD,=C'30'                                                      
*                                                                               
         UNPK  TXDNET,INVNET                                                    
         TM    TXDNET+L'TXDNET-1,X'10'  IS IT NEGATIVE?                         
         BZ    *+8                                                              
         MVI   TXDNET,C'-'                                                      
         OI    TXDNET+L'TXDNET-1,X'F0'  NO OVERPUNCH                            
*                                                                               
         UNPK  TXDCD,=X'0F'        CD=0                                         
         UNPK  TXDCOM,=X'0F'       COM=0 (FOR DETAILS)                          
*                                                                               
         GOTO1 MSUNPK,DMCB,INVMKT,WORK,WORK+4                                   
         MVC   TXDSUBNM(5),WORK+4  STAION CALL LETTERS                          
         CLI   QMED,C'R'           EXECPT FOR RADIO                             
         BE    *+8                                                              
         MVI   TXDSUBNM+4,C' '     CLEAR 'BAND'                                 
*                                                                               
         B     DETO7               SKIP MARKET CODE (MAY NEED IT LATER)         
         L     R5,ADMARKET                                                      
         USING MKTREC,R5                                                        
         CLC   WORK(4),MKTKMKT     TEST ALREADY HAVE MKT                        
         BNE   DETO4                                                            
         CLC   QMED,MKTKMED                                                     
         BE    DETO6                                                            
*                                                                               
DETO4    DS    0H                                                               
         LA    R5,KEY                                                           
         MVI   MKTREC,C'0'                                                      
         MVC   MKTREC+1(16),MKTREC                                              
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,WORK                                                     
         MVC   MKTKAGY,QAGY                                                     
         GOTO1 HIGHMKT                                                          
         CLC   KEY(15),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   TXDMARKT(9),=C'*UNKNOWN*'                                        
         B     DETO7                                                            
*                                                                               
DETO6    DS    0H                                                               
         L     R5,ADMARKET                                                      
         MVC   TXDMARKT(24),MKTNAME                                             
         OC    TXDMARKT,SPACES                                                  
*                                                                               
TXDMARKT DS    CL24                DUMMY TAG FOR MARKET                         
*                                                                               
DETO7    DS    0H                                                               
         DROP  R5                                                               
*                                                                               
         MVC   DUB(2),INVPER                                                    
         MVI   DUB+2,0                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(20,WORK)                                    
         MVC   TXDMOS+0(2),WORK+4       MM                                      
         MVC   TXDMOS+2(4),WORK+0       YEAR                                    
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
DETO8    DS    0H                                                               
         DROP  R3                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*       BILLING WORK AREA DSECT                                                 
BILWRKD  DSECT                                                                  
BILWRK   DS    0C                                                               
RCONS    DS    0F                                                               
APRNT    DS    A                                                                
ARNBILL  DS    A                                                                
AROBILL  DS    A                                                                
AREPRT   DS    A                                                                
ADSTABUC DS    A                                                                
ABUFFC   DS    A                                                                
*                                                                               
RELO     DS    A                                                                
*                                                                               
PEPPARS  DS    F                                                                
APEPTAB  DS    F                                                                
PEPTABN  DS    F                                                                
         DS    3F                                                               
*                                                                               
BRKTAB   DS    CL240               TABLE OF SORT/BREAK CONTROLS                 
SAVPRDC  DS    CL3                                                              
SAVPRD   DS    X                                                                
SAVPGR   DS    XL2                                                              
SAVPGRU  DS    CL4                                                              
SAVMKT   DS    XL2                                                              
SAVMGR   DS    XL2                                                              
SAVMGRU  DS    CL4                                                              
         DS    0F                                                               
X        DS    XL200                                                            
W        DS    CL132                                                            
SAVMODE  DS    X                                                                
MANRVNO  DS    H                                                                
MANMOS   DS    H                                                                
         DS    0F                                                               
         DS    0F                                                               
INVTOTS  DS    0XL8                                                             
INVTGRS  DS    F                                                                
INVTNET  DS    F                                                                
*                                                                               
INTC     DS    F                                                                
INTSTAT  DS    X                                                                
BUFFREC  DS    XL60                                                             
OLDKEY   DS    XL60                                                             
KPRD     DS    X                                                                
KMKT     DS    XL2                                                              
KSTA     DS    XL3                                                              
HOLDPRD  DS    XL1                                                              
HOLDPRD2 DS    XL1                                                              
SAVR1    DS    F                                                                
FFS      DS    XL6'FF'                                                          
DASHES   DS    CL35'-'                                                          
ELCODE   DS    X                                                                
RECSW    DS    X                                                                
ERR      DS    X                                                                
TOTSTAR  DS    CL2                                                              
BILDAT   DS    H                                                                
INVNO    DS    H                                                                
PINVNO   DS    CL10                                                             
SINVNO   DS    H                                                                
SPINVNO  DS    CL10                                                             
RSTART   DS    CL6                                                              
REND     DS    CL6                                                              
*                                                                               
PSTART   DS    CL8                                                              
PEND     DS    CL8                                                              
SVHDR    DS    XL(INVRL)                                                        
FIRST    DS    X                                                                
ERROR    DS    X                                                                
SVQOPT1  DS    C                                                                
*                                                                               
ITGRS    DS    PL8                                                              
ITNET    DS    PL8                                                              
*                                                                               
GTGRS    DS    PL8                                                              
GTNET    DS    PL8                                                              
GTDUE    DS    PL8                                                              
GTLINS   DS    PL8                                                              
GTINVS   DS    PL8                                                              
*                                                                               
QTGRS    DS    PL8                                                              
QTNET    DS    PL8                                                              
QTDUE    DS    PL8                                                              
*                                                                               
OUTREC   DS    XL(TXLRECL)                                                      
*                                                                               
LINED    DSECT                                                                  
         DS    CL1                                                              
LINV     DS    CL7                                                              
         DS    CL1                                                              
LRDAT    DS    CL8                                                              
         DS    CL1                                                              
LIDAT    DS    CL8                                                              
         DS    CL1                                                              
LDDAT    DS    CL8                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LPER     DS    CL6                                                              
         DS    CL1                                                              
LMKT     DS    CL4                                                              
         DS    CL1                                                              
LSTA     DS    CL6                                                              
         DS    CL2                                                              
LGRS     DS    CL15                                                             
         DS    CL1                                                              
LNET     DS    CL15                                                             
         DS    CL1                                                              
LMSG     DS    CL36                                                             
         DS    0C                                                               
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVD     DSECT                                                                  
INVINV   DS    XL3                                                              
INVRDAT  DS    XL2                                                              
INVPRD   DS    CL3                                                              
INVEST   DS    XL1                                                              
INVPER   DS    XL2                 NULL FOR HEADERS                             
INVMKT   DS    XL2                      ''                                      
INVSTA   DS    XL3                      ''                                      
         DS    XL1                 SPARE                                        
         ORG   INVMKT                                                           
INVHMKT  DS    XL2                 HEADERS ONLY - MKT FROM BILL HEADER          
INVINVD  DS    XL2                      ''                                      
INVDUED  DS    XL2                      ''                                      
         ORG                                                                    
INVKL    EQU   *-INVD                                                           
*                                                                               
INVGRS   DS    PL8                                                              
INVNET   DS    PL8                                                              
INVDUE   DS    PL8                                                              
INVRL    EQU   *-INVD                                                           
         SPACE 2                                                                
       ++INCLUDE SPGENSTAB                                                      
         SPACE 2                                                                
*                                                                               
STABUCKC CSECT                                                                  
         DS    2000C                                                            
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*                                  BUFFALO CSECT                                
         BUFF  LINES=9000,ROWS=1,COLUMNS=3,FLAVOR=PACKED,              X        
               KEYLIST=(17,A)                                                   
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
       ++INCLUDE DDTEXACOD                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
         ORG   QAREA+29                                                         
QINVDATE DS    CL6                                                              
QDUEDAYS DS    CL2                                                              
         ORG   QAREA+49                                                         
QMANUAL  DS    CL12                                                             
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'157SPREPTE02 04/01/03'                                      
         END                                                                    
