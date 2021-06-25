*          DATA SET SPREPC002S AT LEVEL 103 AS OF 08/29/00                      
*PHASE SPCO02A                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SPREPCO02 - COLGATE INTERFACE'                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*   QOPT1 -  D=DUMP OUTPUT RECORDS                                    *         
*                                                                     *         
***********************************************************************         
*                                                                               
SPCO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPCO02                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         LA    R9,1(RA)                                                         
         LA    R9,4095(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         LA    RC,SPACEND                                                       
         USING BILWRKD,RC                                                       
*                                                                               
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
*                                                                               
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        RUNFRST                                                      *         
***********************************************************************         
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
RUNFX    B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
***********************************************************************         
*        REQFRST                                                      *         
***********************************************************************         
*                                                                               
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
         CLC   QEST,=C'ALL'                                                     
         BNE   *+10                                                             
         MVC   QEST,=C'NO '                                                     
*                                                                               
         CLI   FIRST,0             FIRST TIME TEST                              
         BNE   REQF20                                                           
         MVI   FIRST,1                                                          
*                                                                               
         XC    GTDNET,GTDNET                                                    
         XC    GTHNET,GTHNET                                                    
         XC    TDCOUNT,TDCOUNT     TOTAL DETAIL RECORD COUNTER                  
         XC    THCOUNT,THCOUNT     TOTAL HEADER RECORD COUNTER                  
         XC    TRCOUNT,TRCOUNT     TOTAL DETAIL & HEADER REC COUNTER            
*                                                                               
         LA    R5,OUTFILE                                                       
         OPEN  ((R5),OUTPUT)                                                    
*                                                                               
REQF20   DS    0H                                                               
REQX     B     EXIT                                                             
         SPACE 3                                                                
*                                                                               
***********************************************************************         
*        CLTFRST                                                      *         
***********************************************************************         
*                                                                               
CLTF     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        PRDFRST                                                     *          
***********************************************************************         
PRDF     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        ESTFRST                                                      *         
***********************************************************************         
ESTF     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        PRDLAST                                                      *         
***********************************************************************         
PRDL     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        CLT LAST                                                    *          
***********************************************************************         
*                                                                               
CLTL     DS    0H                                                               
         GOTO1 ARNBILL                                                          
         GOTO1 AROBILL                                                          
         GOTO1 AREPRT                                                           
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        REQ LAST                                                     *         
***********************************************************************         
REQL     DS    0H                                                               
         B     EXIT                                                             
*                                                                               
***********************************************************************         
*        RUN LAST                                                               
***********************************************************************         
RUNL     DS    0H                                                               
*                                                                               
         ICM   R1,15,THCOUNT                                                    
         ICM   R0,15,TDCOUNT                                                    
         AR    R1,R0                                                            
         STCM  R1,15,FULL                                                       
         CLC   FULL,TRCOUNT                                                     
         BE    RUNL1                                                            
         MVC   P(43),=C'ERR: TOTAL NUMBER OF RECORDS DOES NOT MATCH'            
         GOTO1 APRNT                                                            
RUNL1    CLC   GTHNET,GTDNET                                                    
         BE    RUNL2                                                            
         MVC   P(44),=C'ERR: TOTAL HDR AMT DOES NOT EQUAL TO DET AMT'           
         GOTO1 APRNT                                                            
*                                                                               
RUNL2    LA    R8,OUTREC                                                        
         USING CGDATA,R8                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         LA    RE,CGDATA           CLEAR RECORD                                 
         LA    RF,L'CGDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   CGTREID,=20C'9'                                                  
         GOTO1 DATCON,DMCB,(5,WORK+8),(0,WORK)                                  
         MVC   CGTCRDT(4),WORK+2                                                
         MVC   CGTCRDT+4(2),=C'19'                                              
         MVC   CGTCRDT+6(2),WORK                                                
         CLC   =C'80',WORK                                                      
         BL    *+10                                                             
         MVC   CGTCRDT+4(2),=C'20'                                              
         TIME  DEC                                                              
         ST    R0,FULL                                                          
         GOTO1 =V(HEXOUT),DMCB,FULL,DUB,4,=C'TOG'                               
         MVC   CGTCRTM,DUB                                                      
*                                                                               
         EDIT  (B4,TRCOUNT),(10,CGTTRCT),FILL=0                                 
         EDIT  (B4,THCOUNT),(10,CGTHRCT),FILL=0                                 
         EDIT  (B4,TDCOUNT),(10,CGTDRCT),FILL=0                                 
         EDIT  (B4,GTHNET),(17,CGTGRAM+2),FILL=0,ALIGN=RIGHT                    
         EDIT  (B4,GTDNET),(17,CGTDIAM+2),FILL=0,ALIGN=RIGHT                    
         MVC   CGTGRAM(2),=C'00'                                                
         MVC   CGTDIAM(2),=C'00'                                                
         MVI   CGTRETY,C'9'                                                     
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
*                                                                               
         MVC   P(43),=C'SUMMARY RECORD - TOTAL OF HEADER AND DETAIL'            
         GOTO1 APRNT                                                            
         GOTO1 APRNT                                                            
*                                                                               
         MVC   P,SPACES                                                         
         LA    R2,P                                                             
         USING PLINED,R2                                                        
*                                                                               
         MVC   PTREID,CGTREID                                                   
         MVC   PTCRDT,CGTCRDT                                                   
         MVC   PTCRTM,CGTCRTM                                                   
         MVC   PTTRCT,CGTTRCT                                                   
         MVC   PTHRCT,CGTHRCT                                                   
         MVC   PTGRAM,CGTGRAM                                                   
         MVC   PTDRCT,CGTDRCT                                                   
         MVC   PTDIAM,CGTDIAM                                                   
         MVC   PTRETY,CGTRETY                                                   
*                                                                               
         GOTO1 APRNT                                                            
         DROP  R2,R8                                                            
*                                                                               
         CLI   ERROR,0                                                          
         BE    RUNL4                                                            
         MVC   P(45),=C'***** ERRORS - NO OUTPUT FILE GENERATED *****'          
         GOTO1 APRNT                                                            
         B     RUNLX                                                            
*                                                                               
RUNL4    DS    0H                                                               
*                                                                               
         CLOSE (OUTFILE)                                                        
*                                                                               
         CLI   SVQOPT1,C'D'        DUMP OUTPUT RECS?                            
         BNE   RUNLX                                                            
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
RUNLX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
OUTFEOD  DS    0H                                                               
         CLOSE (OUTFILR)                                                        
         B     RUNLX                                                            
         SPACE 3                                                                
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
ACONS    DS    0F                                                               
         DC    A(PRNT)                                                          
         DC    A(RNBILL)                                                        
         DC    A(ROBILL)                                                        
         DC    A(REPRT)                                                         
         DC    A(STABUCKC)                                                      
         DC    A(BUFFALOC)                                                      
ACONSX   EQU   *                                                                
         SPACE 2                                                                
OUTFILE  DCB   DDNAME=SPCOLOUT,DSORG=PS,RECFM=FB,LRECL=256,            X        
               BLKSIZE=2560,MACRF=PM                                            
OUTFILR  DCB   DDNAME=SPCOLOUT,DSORG=PS,RECFM=FB,LRECL=256,            X        
               BLKSIZE=2560,MACRF=GM,EODAD=OUTFEOD                              
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'RNBILL - READ NEW BILL RECORDS'                                 
RNBILL   CSECT                                                                  
         NMOD1 0,RNBILL                                                         
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         MVC   KEY1,KEY            SAVE KEY                                     
         XC    KEY,KEY                                                          
RNB2     DS    0H                                                               
         MVC   KEY(2),=X'0E01'     RECORD TYPE                                  
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
         USING INVDSECT,R4                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,STABBDT),(0,WORK)                                 
         MVC   INVINVNO(2),WORK+2                                               
         EDIT  (B2,STABINV),(4,INVINVNO+2),ZERO=NOBLANK,FILL=0                  
*                                                                               
         L     RF,ADCLT                 FIND PRD CODE                           
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
         MVC   INVAGMD,STABKAM                                                  
         MVC   INVCLT,STABKCLT                                                  
         MVC   INVMKT(5),STABKMKT                                               
         MVI   INVTYPE,C'2'        RECORD TYPE FOR DETAIL IS 2                  
         MVC   INVDNET,STABNET                                                  
         ICM   RF,15,STABNET                                                    
         CVD   RF,DUB                                                           
         ZAP   INVDIS,DUB          GROSS AMOUNT (NET)                           
         GOTO1 DATCON,DMCB,(3,STABPER),(0,INVMOS)                               
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
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'ROBILL  - READ OLD BILL RECORDS'                                
ROBILL   CSECT                                                                  
         NMOD1 0,ROBILL            HEADER RECORDS                               
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
         USING INVDSECT,R4                                                      
*                                                                               
         MVC   INVMONNO,BINVNO     GET FIRST 2 DIGITS FROM INV NUMB             
         MVC   INVINVNO,BINVNO     GET 6 DIGIT INVOICE NUMBER                   
*                                                                               
         PACK  DUB,INVINVNO(2)     CONVERT MONTH NUMBER TO MONTH                
         CVB   R5,DUB                                                           
         CH    R5,=H'12'                                                        
         BL    *+12                                                             
         SH    R5,=H'12'                                                        
         B     *-12                                                             
         STC   R5,BYTE                                                          
         LA    R5,MONCONTB                                                      
*                                                                               
ROB10BB  CLC   BYTE,0(R5)                                                       
         BE    ROB10CC                                                          
         CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CANNOT PASS END OF TABLE                     
         LA    R5,MONCONLQ(R5)                                                  
         B     ROB10BB                                                          
*                                                                               
ROB10CC  MVC   INVINVNO(2),1(R5)                                                
*                                                                               
         MVC   INVAGMD,BKEYAM                                                   
         MVC   INVCLT,BKEYCLT                                                   
         MVC   INVPRD,BKEYPRD                                                   
         MVC   INVEST,BKEYEST                                                   
         MVC   INVHNET,BNET                                                     
*                                                                               
         ICM   RF,15,BNET                                                       
         CVD   RF,DUB                                                           
         ZAP   INVGRS,DUB          GROSS AMOUNT (NET)                           
*                                                                               
         MVI   INVTYPE,C'1'        RECORD TYPE FOR HEADER IS 1                  
         MVC   INVINVDT,BQDATE                                                  
         GOTO1 DATCON,DMCB,(3,BDUEDATE),(0,INVDUEDT)                            
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFFC,X                                    
*                                                                               
         B     ROB4                                                             
*                                                                               
ROB40    DS    0H                                                               
ROBX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
MONCONTB DS    0X                                                               
         DC    XL1'00',CL2'04'     YNR START USING NEW INV NUMB ON APR          
MONCONLQ EQU   *-MONCONTB                                                       
         DC    XL1'01',CL2'05'                                                  
         DC    XL1'02',CL2'06'                                                  
         DC    XL1'03',CL2'07'                                                  
         DC    XL1'04',CL2'08'                                                  
         DC    XL1'05',CL2'09'                                                  
         DC    XL1'06',CL2'10'                                                  
         DC    XL1'07',CL2'11'                                                  
         DC    XL1'08',CL2'12'                                                  
         DC    XL1'09',CL2'01'                                                  
         DC    XL1'0A',CL2'02'                                                  
         DC    XL1'0B',CL2'03'                                                  
         DC    XL1'FF'                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'REPRT - CREATE TAPE RECS AND PRINT REPORT'                      
REPRT    CSECT                                                                  
         NMOD1 0,REPRT                                                          
         LA    RC,SPACEND                                                       
         SPACE 2                                                                
         XC    DSUBTOT,DSUBTOT     DETAIL SUBTOTAL                              
         XC    SVBUFF,SVBUFF                                                    
         MVI   FORCEHED,C'Y'                                                    
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         XC    BUFFREC,BUFFREC                                                  
         LA    R4,BUFFREC                                                       
         USING INVDSECT,R4                                                      
REP2     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFFC,BUFFREC,0                           
         B     REP4B                                                            
REP4     DS    0H                                                               
         GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFFC,BUFFREC,0                            
REP4B    DS    0H                                                               
         TM    DMCB+8,X'80'                                                     
         BNZ   REP50                                                            
*                                                                               
         CLI   INVTYPE,C'1'        TEST FOR HEADER                              
         BNE   REP10                                                            
*                                  INVOICE HEADER                               
         OC    SVBUFF,SVBUFF       ANYTHING IN SAVED BUFF?                      
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
******** CLI   SVBUFF+  ,C'1'      KEVIN                                        
******** BNE   *+                                                               
******** MVC   P(  ),                                                           
*                                                                               
         BAS   RE,HDROUT           DO HEADER OUTPUT                             
         LA    R8,OUTREC                                                        
         USING CGDATA,R8                                                        
*                                                                               
         MVC   P,SPACES                                                         
*                                                                               
         MVC   PHVENN,CGHVENN                                                   
         MVC   PHINUM,CGHINUM                                                   
         MVC   PHOPID,CGHOPID                                                   
         MVC   PHINCL,CGHINCL                                                   
         MVC   PHCOMM,CGHCOMM                                                   
         MVC   PHINDT,CGHINDT                                                   
         MVC   PHPYTM,CGHPYTM                                                   
         MVC   PHSDTY,CGHSDTY                                                   
         MVC   PHDUDT,CGHDUDT                                                   
         MVC   PHSIGN,CGHSIGN                                                   
         MVC   PHGRAM,CGHGRAM                                                   
         MVC   PHCUCD,CGHCUCD                                                   
         MVC   PHLGID,CGHLGID                                                   
         MVC   PHENTY,CGHENTY                                                   
         MVC   PHRETY,CGHRETY                                                   
         DROP  R8                                                               
*                                                                               
         MVC   SVBUFF,INVDSECT     SAVE HEADER                                  
         ICM   RE,15,THCOUNT       COUNT TOTAL NUMB OF HEADER RECS              
         LA    RE,1(RE)                                                         
         STCM  RE,15,THCOUNT                                                    
         ICM   RE,15,TRCOUNT       COUNT TOTAL NUMB OF RECS                     
         LA    RE,1(RE)                                                         
         STCM  RE,15,TRCOUNT                                                    
         GOTO1 APRNT               PRINT OUT HEADER RECORD                      
         B     REP30                                                            
*                                                                               
REP10    DS    0H                  DETAIL LINE                                  
         BAS   RE,DETOUT           DO DETAIL OUTPUT                             
         LA    R8,OUTREC                                                        
         USING CGDATA,R8                                                        
*                                                                               
         MVC   PDVENN,CGDVENN                                                   
         MVC   PDINUM,CGDINUM                                                   
         MVC   PDSBAR,CGDSBAR                                                   
         MVC   PDSEP1,CGDSEP1                                                   
         MVC   PDSGAC,CGDSGAC                                                   
         MVC   PDSEP2,CGDSEP2                                                   
         MVC   PDSCCT,CGDSCCT                                                   
         MVC   PDSCCD,CGDSCCD                                                   
         MVC   PDDESP,CGDDESP                                                   
         MVC   PDSIGN,CGDSIGN                                                   
         MVC   PDDISL,CGDDISL                                                   
         MVC   PDSRIN,CGDSRIN                                                   
         MVC   PDARDT,CGDARDT                                                   
**       MVC   PDSHOW,CGDSHOW                                                   
**       MVC   PDSTAT,CGDSTAT                                                   
**       MVC   PDLENG,CGDLENG                                                   
         MVC   PDRETY,CGDRETY                                                   
         DROP  R8                                                               
*                                                                               
         MVC   SVBUFF,INVDSECT                                                  
         ICM   RE,15,TDCOUNT       COUNT TOTAL NUMB OF DETAIL RECS              
         LA    RE,1(RE)                                                         
         STCM  RE,15,TDCOUNT                                                    
         ICM   RE,15,TRCOUNT       COUNT TOTAL NUMB OF RECS                     
         LA    RE,1(RE)                                                         
         STCM  RE,15,TRCOUNT                                                    
         GOTO1 APRNT                                                            
*                                                                               
REP30    DS    0H                                                               
         B     REP4                                                             
*                                                                               
REP50    DS    0H                                                               
         OC    SVBUFF,SVBUFF       ANY HEADER TO FINISH UP?                     
         BZ    *+8                                                              
         BAS   RE,ENDINV                                                        
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'RESET',ABUFFC                                    
*                                                                               
REPX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        FINISH UP OLD HEADER                                                   
*                                                                               
***********************************************************************         
*                                                                               
ENDINV   NTR1                                                                   
         ICM   R0,15,DSUBTOT                                                    
         ICM   R1,15,CURHAMT                                                    
         CR    R0,R1                                                            
         BNE   EINV4                                                            
         MVC   P,SPACES                                                         
         GOTO1 APRNT                                                            
         MVC   P(23),=C'*** DETAIL SUBTOTAL ***'                                
         EDIT  (B4,DSUBTOT),(13,P+30),MINUS=YES                                 
*                                                                               
         XC    DSUBTOT,DSUBTOT                                                  
         B     EINVX                                                            
*                                                                               
EINV4    DS    0H                                                               
         MVC   P(56),=C'ERR: HEADER AMOUNT AND TOTAL DETAIL AMOUNT ARE +        
               NOT EQUAL'                                                       
         MVC   P(18),=C'**OUT OF BALANCE**'                                     
         MVI   ERROR,C'Y'                                                       
*                                                                               
EINVX    DS    0H                                                               
         GOTO1 APRNT                                                            
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* *********************************************************************         
*                                                                               
*        HEADER OUTPUT                                                          
*                                                                               
HDROUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING CGDATA,R3                                                        
*                                                                               
         LA    RE,CGDATA           CLEAR RECORD                                 
         LA    RF,L'CGDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   MONNO,INVMONNO      MAKE COPY OF YNRO'S MONTH NUMBER             
*                                                                               
         MVI   CGHRETY,C'1'                                                     
         MVC   CGHVENN,=C'1252071'                                              
         MVI   CGHINUM,C'N'                                                     
         MVI   CGHINUM+1,C'-'                                                   
         MVI   CGHINUM+4,C'-'                                                   
         MVC   CGHINUM+2(2),INVMONNO                                            
         MVC   CGHINUM+5(4),INVINVNO+2                                          
         MVC   CGHOPID,=C'Y R'                                                  
         MVI   CGHOPID+1,X'50'                                                  
         MVI   CGHINCL,C'I'                                                     
         MVC   CGHINDT(4),INVINVDT+2                                            
         MVC   CGHINDT+4(2),=C'19'                                              
         MVC   CGHINDT+6(2),INVINVDT                                            
         CLC   =C'80',INVINVDT                                                  
         BL    *+10                                                             
         MVC   CGHINDT+4(2),=C'20'                                              
         MVC   CGHPYTM,=C'0025'                                                 
         MVC   CGHSDTY,=C'RN'                                                   
         MVC   CGHDUDT(4),INVDUEDT+2                                            
         MVC   CGHDUDT+4(2),=C'19'                                              
         MVC   CGHDUDT+6(2),INVDUEDT                                            
         CLC   =C'80',INVDUEDT                                                  
         BL    *+10                                                             
         MVC   CGHDUDT+4(2),=C'20'                                              
         MVI   CGHSIGN,C'+'                                                     
         MVI   CGHGRAM,C'0'                                                     
         EDIT  (B4,INVHNET),(17,CGHGRAM),FILL=0,ZERO=NOBLANK,          +        
               ALIGN=RIGHT                                                      
         MVC   CGHCUCD(3),=C'USD'                                               
         MVC   CGHLGID,=C'US01'                                                 
         MVC   CGHENTY(3),=C'101'                                               
*                                                                               
         ICM   R1,15,INVHNET       CK FOR NEGATIVE GROSS AMOUNT                 
         STCM  R1,15,CURHAMT       CURRENT HEADER AMOUNT                        
         CH    R1,=H'0'                                                         
         BNL   *+14                                                             
         MVI   CGHINCL,C'C'                                                     
         MVC   CGHSDTY,=C'KG'                                                   
*                                                                               
         ICM   RF,15,GTHNET                                                     
         AR    RF,R1                                                            
         STCM  RF,15,GTHNET        SUMMING UP HEADER NET AMOUNT                 
*                                                                               
*                                  GET EST DESC FIELDS                          
*                                                                               
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
         MVC   CGHCOMM,EDESC              ESTIMATE DESCRIPTION                  
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         DROP  R5                                                               
*                                                                               
         DROP  R3                                                               
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
*        DETAIL OUTPUT                                                          
*                                                                               
DETOUT   NTR1                                                                   
         LA    R3,OUTREC                                                        
         USING CGDATA,R3                                                        
*                                                                               
         LA    RE,CGDATA           CLEAR RECORD                                 
         LA    RF,L'CGDATA                                                      
         SR    R0,R0                                                            
         L     R1,=X'40000000'     PAD CHAR IS SPACE                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   CGDRETY,C'2'                                                     
         MVC   CGDVENN,=C'1252071'                                              
         MVI   CGDINUM,C'N'                                                     
         MVI   CGDINUM+1,C'-'                                                   
         MVI   CGDINUM+4,C'-'                                                   
         MVC   CGDINUM+2(2),MONNO                                               
         MVC   CGDINUM+5(4),INVINVNO+2                                          
         MVC   CGDSBAR,=C'101'                                                  
         MVI   CGDSEP1,C'-'                                                     
         MVI   CGDSEP2,C'-'                                                     
         MVC   CGDSCCD,=C'US01'    SAP COMP CODE HARD CODEDD                    
*                                                                               
         EDIT  (B1,INVEST),(3,CGDDESP),ALIGN=LEFT,FILL=0,ZERO=NOBLANK           
         MVI   CGDDESP+3,C'_'                                                   
         MVC   CGDDESP+4(2),INVMOS+2                                            
         MVI   CGDDESP+6,C'/'                                                   
         MVC   CGDDESP+7(2),=C'19'                                              
         MVC   CGDDESP+9(2),INVMOS                                              
         CLC   =C'80',INVMOS                                                    
         BL    *+10                                                             
         MVC   CGDDESP+7(2),=C'20' YEAR 20NN                                    
*                                                                               
*                                  GET PRD USER FIELDS                          
*                                                                               
         XC    KEY,KEY             NOTE- WONT HAVE TO RESET SEQ                 
         MVC   KEY+1(1),INVAGMD                                                 
         MVC   KEY+2(2),INVCLT                                                  
         MVC   KEY+4(3),INVPRD                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETPRD                                                           
*                                                                               
         L     R5,ADPRD                                                         
         USING PRDHDR,R5                                                        
         MVC   CGDSCCT,PUSER1      PRODUCT USER FIELD                           
         DROP  R5                                                               
*                                                                               
*                                  GET EST USER FIELDS                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),INVAGMD                                                 
         MVC   KEY+2(2),INVCLT                                                  
         MVC   KEY+4(3),INVPRD                                                  
         MVC   KEY+7(1),INVEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETEST                                                           
*                                                                               
         L     R5,ADEST                                                         
         USING ESTHDR,R5                                                        
         MVC   CGDSGAC,EUSER1      ESTIMATE USER FIELD                          
         CLC   CGDSGAC,=C'111052'  SPECIAL CASE...                              
         BNE   *+10                                                             
         MVC   CGDSCCT,=C'0000000'                                              
*                                                                               
         MVI   CGDSIGN,C'+'                                                     
         ICM   R0,15,INVDNET                                                    
         CH    R0,=H'0'                                                         
         BNL   *+8                                                              
         MVI   CGDSIGN,C'-'                                                     
*                                                                               
         ICM   R1,15,DSUBTOT                                                    
         AR    R1,R0                                                            
         STCM  R1,15,DSUBTOT       DETAIL REC SUB-TOTAL                         
         ICM   R1,15,GTDNET                                                     
         AR    R1,R0                                                            
         STCM  R1,15,GTDNET        SUMMING UP DETAIL NET AMOUNT                 
*                                                                               
         EDIT  (B4,INVDNET),(13,CGDDISL),FILL=0,ZERO=NOBLANK,          +        
               ALIGN=RIGHT                                                      
         MVC   CGDSRIN,=C'Y R'                                                  
         MVI   CGDSRIN+1,X'50'                                                  
         MVC   CGDARDT(8),=C'12345678'                                          
*                                                                               
         L     R1,=A(OUTFILE)                                                   
         LA    R0,OUTREC                                                        
         PUT   (1),(0)                                                          
         DROP  R5,R3                                                            
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
         TITLE 'PRNT - PRINT CONTROL MODULE'                                    
PRNT     CSECT                                                                  
         NMOD1 0,PRNT                                                           
         LA    RC,SPACEND                                                       
         GOTO1 REPORT                                                           
*                                                                               
PRNTX    DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
***********************************************************************         
*                                                                               
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
         DS    0F                                                               
X        DS    XL200                                                            
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
BUFFREC  DS    XL100                                                            
KPRD     DS    X                                                                
KMKT     DS    XL2                                                              
KSTA     DS    XL3                                                              
FFS      DS    XL6'FF'                                                          
DASHES   DS    CL35'-'                                                          
ELCODE   DS    X                                                                
ERR      DS    X                                                                
BILDAT   DS    H                                                                
MONNO    DS    XL2                                                              
*                                                                               
PSTART   DS    CL8                                                              
PEND     DS    CL8                                                              
SVBUFF   DS    XL(INVRECL)                                                      
FIRST    DS    X                                                                
ERROR    DS    X                                                                
SVQOPT1  DS    C                                                                
*                                                                               
CURHAMT  DS    XL4                 CURRENT HEADER AMOUNT                        
DSUBTOT  DS    XL4                 DETAIL SUBTOTAL                              
*                                                                               
GTHNET   DS    XL4                 GRAND TOTAL FOR HEADER NET AMOUNT            
GTDNET   DS    XL4                 GRAND TOTAL FOR DETAIL NET AMOUNT            
TDCOUNT  DS    XL4                 TOTAL DETAIL RECORD COUNTER                  
THCOUNT  DS    XL4                 TOTAL HEADER RECORD COUNTER                  
TRCOUNT  DS    XL4                 TOTAL DETAIL & HEADER REC COUNTER            
*                                                                               
ESTDESP  DS    CL12                ESTIMATE DESCRIPTION                         
ESTUSER1 DS    CL6                 ESTIMATE USER FIELD 1                        
PRDUSER1 DS    CL7                 PRODUCT USER FIELD 1                         
*                                                                               
OUTREC   DS    XL256               RECORD LENGTH IS 256                         
*                                                                               
PLINED   DSECT                     PRINT LINE DSECT                             
PLSTART  DS    0X                                                               
*                                                                               
PHVENN   DS    CL7                 HEAD RECORD                                  
         DS    CL1                                                              
PHINUM   DS    CL10                                                             
         DS    CL1                                                              
PHOPID   DS    CL3                                                              
         DS    CL1                                                              
PHINCL   DS    CL1                                                              
         DS    CL1                                                              
PHCOMM   DS    CL12                                                             
         DS    CL1                                                              
PHINDT   DS    CL8                                                              
         DS    CL1                                                              
PHPYTM   DS    CL4                                                              
         DS    CL1                                                              
PHSDTY   DS    CL2                                                              
         DS    CL1                                                              
PHDUDT   DS    CL8                                                              
         DS    CL1                                                              
PHSIGN   DS    CL1                                                              
PHGRAM   DS    CL18                                                             
         DS    CL1                                                              
PHCUCD   DS    CL4                                                              
         DS    CL1                                                              
PHLGID   DS    CL4                                                              
         DS    CL1                                                              
PHENTY   DS    CL4                                                              
         DS    CL1                                                              
PHRETY   DS    CL1                                                              
*                                                                               
         ORG   PLSTART             DETAIL RECORD                                
*                                                                               
PDVENN   DS    CL7                                                              
         DS    CL1                                                              
PDINUM   DS    CL10                                                             
         DS    CL1                                                              
PDSBAR   DS    CL3                                                              
PDSEP1   DS    CL1                                                              
PDSGAC   DS    CL6                                                              
PDSEP2   DS    CL1                                                              
PDSCCT   DS    CL7                                                              
         DS    CL1                                                              
PDSCCD   DS    CL4                                                              
         DS    CL1                                                              
PDDESP   DS    CL18                                                             
         DS    CL1                                                              
PDSIGN   DS    CL1                                                              
PDDISL   DS    CL13                                                             
         DS    CL1                                                              
PDSRIN   DS    CL3                                                              
         DS    CL1                                                              
*                                                                               
PDARDT   DS    CL8                                                              
         DS    CL1                                                              
PDSHOW   DS    CL27                                                             
         DS    CL1                                                              
PDSTAT   DS    CL4                                                              
         DS    CL1                                                              
PDLENG   DS    CL3                                                              
*                                                                               
         ORG   PDSHOW                                                           
*                                                                               
PDCSHO   DS    CL24                                                             
         DS    CL1                                                              
PDCSTA   DS    CL4                                                              
         DS    CL1                                                              
PDCLEN   DS    CL3                                                              
         DS    CL1                                                              
PDCNUN   DS    CL3                                                              
         DS    CL1                                                              
PDRETY   DS    CL1                                                              
*                                                                               
         ORG   PLSTART             SUMMARY RECORD                               
PTREID   DS    CL20                                                             
         DS    CL1                                                              
PTCRDT   DS    CL8                                                              
         DS    CL1                                                              
PTCRTM   DS    CL6                                                              
         DS    CL1                                                              
PTTRCT   DS    CL10                                                             
         DS    CL1                                                              
PTHRCT   DS    CL10                                                             
         DS    CL1                                                              
PTGRAM   DS    CL19                                                             
         DS    CL1                                                              
PTDRCT   DS    CL10                                                             
         DS    CL1                                                              
PTDIAM   DS    CL19                                                             
         DS    CL1                                                              
PTRETY   DS    CL1                                                              
*                                                                               
         DS    0C                                                               
         SPACE 3                                                                
*                                  DSECT FOR TABLE ENTRY                        
INVDSECT DSECT                                                                  
INVINVNO DS    CL6                 INVOICE NUMBER                               
INVAGMD  DS    XL1                                                              
INVCLT   DS    XL2                                                              
INVPRD   DS    XL3                                                              
INVEST   DS    XL1                                                              
INVMKT   DS    XL2                                                              
INVTYPE  DS    CL1                 RECORD TYPE                                  
INVINVDT DS    CL6                 INVOICE DATE                                 
INVDUEDT DS    CL6                 DUE DATE                                     
INVHNET  DS    XL4                 HEADER RECORD NET AMOUNT                     
INVDNET  DS    XL4                 DETAIL RECORD NET AMOUNT                     
INVMEDT  DS    CL1                 MEDIA TYPE                                   
INVMONNO DS    CL2                 MONTH NUMBER USED BY YNR                     
INVMOS   DS    CL6                 MONTH OF SERVICE (DETAIL REC ONLY)           
*NVARDT  DS    CL8                 AIR DATE FOR COLLAPSING DETAIL REC           
*NVSHOW  DS    CL16                PRG NAME FOR COLLAPSING DETAIL REC           
*NVSTAT  DS    CL4                 STATION FOR COLLAPSING DETAIL REC            
*NVLENG  DS    CL3                 LENGTH FOR COLLAPSING DETAIL REC             
INVKEYL  EQU   *-INVDSECT                                                       
*                                                                               
INVGRS   DS    PL8                 HEADER GROSS AMOUNT (NET)                    
INVDIS   DS    PL8                 DETAIL DISRIBUTION AMOUNT (NET)              
INVRECL  EQU   *-INVDSECT                                                       
*                                                                               
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
         BUFF  LINES=9000,ROWS=1,COLUMNS=2,FLAVOR=PACKED,              +        
               KEYLIST=(45,A)                                                   
*                                                                               
       ++INCLUDE DDBUFFALOD                                                     
*                                                                               
       ++INCLUDE DDCOLGATD         COLGATE RECORD DSECTS                        
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
**PAN#1  DC    CL21'103SPREPC002S08/29/00'                                      
         END                                                                    
