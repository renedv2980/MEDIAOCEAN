*          DATA SET PPREP9002  AT LEVEL 057 AS OF 05/01/02                      
*PHASE PP9002A,+0                                                               
         TITLE 'PP9002  PRINTPAK TRIAL BALANCE'                                 
PP9002   CSECT                                                                  
         NMOD1 0,PP9002,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         LA    R7,PP9002+4095                                                   
         LA    R7,1(R7)                                                         
         USING PP9002+4096,R7                                                   
         CLI   MODE,PROCBIL                                                     
         BNE   TB2                                                              
*                                                                               
         TM    KEY+25,X'C0'        NO CLOSED-OUT RECS                           
         BO    TBXIT                                                            
         TM    PBILLCTL,X'80'      TEST DELETED                                 
         BNZ   TBXIT                                                            
         CLC   PBILKMOS(2),MSSTART                                              
         BH    TBXIT                    NO - SUBSEQUENT                         
         CLC   PBILLDAT(4),QEND         DONE AFTER CURRENT MONTH                
         BH    TBXIT                    YES - IGNORE                            
         MVC   CLTACT(2),=C'YY'                                                 
         CLI   PAGYPROF+14,C'0'                                                 
         BNE   BNOCD            OMIT CASH DISCOUNT FROM REPORT                  
         ZAP   DGROSS,PBILLBIL                                                  
         ZAP   DNET,PBILLNET                                                    
         B     PRTBILL                                                          
*                                                                               
BNOCD    ZAP   DGROSS,PBILLGRS                                                  
         SP    DGROSS,PBILLBIL                                                  
         AP    PBILLNET,DGROSS                                                  
         ZAP   DGROSS,PBILLGRS                                                  
         ZAP   DNET,PBILLNET                                                    
PRTBILL  MVC   P+49(3),PBILKPRD                                                 
         CLC   PBILKEST(2),=2X'00'                                              
         BE    BILL1                                                            
         SR    R0,R0                                                            
         IC    R0,PBILKEST              ESTIMATE                                
         SLL   R0,8                                                             
         IC    R0,PBILKEST+1                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+53(3),DUB+6(2)                                                 
BILL1    BAS   R9,GETINV                                                        
         SR    R1,R1                                                            
         IC    R1,PBILKMOS+1            MONTH OF SERVICE                        
         BCTR  R1,R0                                                            
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   P+72(3),0(R1)                                                    
         MVI   P+75,C'/'                                                        
         SR    R1,R1                                                            
         IC    R1,PBILKMOS              YEAR OF SERVICE                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+76(2),DUB+6(2)                                                 
*                                                                               
         CLI   PBILLTYP,C'4'            SEE IF DETAIL BILL                      
         BE    DBILL                                                            
*                   ORIGINAL BILL                                               
         MVI   P+69,C'S'                                                        
         CLC   PBILLDAT(4),QEND         SEE IF DONE IN CURRENT MONTH            
         BNE   OBILL1                                                           
         EDIT  DGROSS,(14,P+18),2,COMMAS=YES,MINUS=YES                          
         EDIT  DNET,(14,P+102),2,COMMAS=YES,MINUS=YES                           
*                                                                               
         CLC   PBILKEST(2),=2X'00'      IF EST =0 DO NOT POST TO ACCUMS         
         BE    OBILL2                                                           
*                                                                               
         AP    TOCBILLG,DGROSS          POST TO CURRENT                         
         AP    TOCBILLN,DNET                                                    
         B     OBILL2                                                           
OBILL1   EDIT  DGROSS,(14,P+2),2,COMMAS=YES,MINUS=YES                           
         EDIT  DNET,(14,P+86),2,COMMAS=YES,MINUS=YES                            
*                                                                               
         CLC   PBILKEST(2),=2X'00'      IF EST =0 DO NOT POST TO ACCUMS         
         BE    OBILL2                                                           
*                                                                               
         AP    TOPBILLG,DGROSS          POST TO PRV                             
         AP    TOPBILLN,DNET                                                    
OBILL2   CLC   PBILLCAN(6),=6C'0'       SEE IF REVERSED                         
         BE    OBILLX                                                           
         CLC   PBILLCDT(4),QEND    SEE IF REVERSED AFTER CUR MONTH              
         BH    OBILLX              YES - CONSIDER UNREVERSED                    
         MVC   P+79(2),PBILLCAN                                                 
         MVI   P+81,C'-'                                                        
         MVC   P+82(4),PBILLCAN+2                                               
         EDIT  DGROSS,(14,P+34),2,COMMAS=YES,MINUS=YES                          
         EDIT  DNET,(14,P+118),2,COMMAS=YES,MINUS=YES                           
*                                                                               
         CLC   PBILKEST(2),=2X'00'      IF EST =0 DO NOT POST TO ACCUMS         
         BE    OBILLX                                                           
*                                                                               
         CLC   PBILLCDT(4),QEND     TEST CANCELED IN CUR MONTH                  
         BNE   OBILL3               NO                                          
         AP    TCREVG,DGROSS       POST TO CURRENT REVERSALS                    
         AP    TCREVN,DNET                                                      
         B     OBILLX                                                           
OBILL3   AP    TPREVG,DGROSS       POST TO PRV REVERSALS                        
         AP    TPREVN,DNET                                                      
OBILLX   MVI   RCSUBPRG,0                                                       
         BAS   R6,REPORTIT                                                      
         B     TBXIT                                                            
*                             DETAIL BILLS                                      
DBILL    MVI   P+69,C'D'                                                        
         CLC   PBILLDAT(4),QEND    SEE IF CURRENT MONTH                         
         BNE   DBILL1              NO                                           
         EDIT  DGROSS,(14,P+18),2,COMMAS=YES,MINUS=YES                          
         EDIT  DNET,(14,P+102),2,COMMAS=YES,MINUS=YES                           
*                                                                               
         CLC   PBILKEST(2),=2X'00'      IF EST =0 DO NOT POST TO ACCUMS         
         BE    DBILLX                                                           
*                                                                               
         AP    TDCBILLG,DGROSS           POST TO CURRENT                        
         AP    TDCBILLN,DNET                                                    
         B     DBILLX                                                           
DBILL1   EDIT  DGROSS,(14,P+2),2,COMMAS=YES,MINUS=YES                           
         EDIT  DNET,(14,P+86),2,COMMAS=YES,MINUS=YES                            
*                                                                               
         CLC   PBILKEST(2),=2X'00'      IF EST =0 DO NOT POST TO ACCUMS         
         BE    DBILLX                                                           
*                                                                               
         AP    TDPBILLG,DGROSS          POST TO PRV                             
         AP    TDPBILLN,DNET                                                    
DBILLX   MVI   RCSUBPRG,0                                                       
         BAS   R6,REPORTIT                                                      
         B     TBXIT                                                            
*                                                                               
*                                                                               
GETINV   MVC   P+57(2),PAGYPROF+2                                               
         MVI   P+59,C'-'                                                        
         SR    R0,R0                                                            
         IC    R0,PBILKBMN+1        MONTH                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+60(2),DUB+6(2)                                                 
         MVI   P+62,C'-'                                                        
         MVC   HALF,PBILKBNO                                                    
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+63(4),DUB+5(3)                                                 
         CLI   PAGYPROF+4,C'1'                                                  
         BNE   0(R9)                                                            
         MVC   WORKTB(2),P+57                                                   
         MVC   P+57(2),P+60                                                     
         MVC   P+60(2),WORKTB                                                   
         BR    R9                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
TB2      CLI   MODE,PROCBUY                                                     
         BNE   TB3                                                              
         TM    KEY+25,X'C0'        NO CLOSED-OUT RECS                           
         BO    TBXIT                                                            
         CLI   BUYSW,1                                                          
         BE    TB2A                                                             
         MVI   BUYSW,1                                                          
         CLI   LBILSW,1                                                         
         BE    TB2A                                                             
         ZAP   DGROSS,=P'0'        NO BILLS FOR THIS PRD SO                     
         ZAP   DNET,=P'0'          PRINT 0 TOTALS                               
         LA    R1,P+18                                                          
         BAS   R9,TBBEDT                                                        
         LA    R1,P+2                                                           
         BAS   R9,TBBEDT                                                        
         LA    R1,P+34                                                          
         BAS   R9,TBBEDT                                                        
         MVC   P+66-(L'BTITLE1/2)(L'BTITLE1),BTITLE1                            
         BAS   R9,TBBSIGNS                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         BAS   R6,REPORTIT                                                      
         LA    R1,P+34                                                          
         BAS   R9,TBBEDT                                                        
         MVC   P+66-(L'BTITLE2/2)(L'BTITLE2),BTITLE2                            
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT                                                      
         LA    R1,P+2                                                           
         BAS   R9,TBBEDT                                                        
         LA    R1,P+18                                                          
         BAS   R9,TBBEDT                                                        
         MVC   P+66-(L'BTITLE3/2)(L'BTITLE3),BTITLE3                            
         BAS   R6,REPORTIT                                                      
         MVI   FORCEHED,C'Y'                                                    
         B     TB2A                                                             
*                                                                               
TB2A     CLC   PPRDKPRD,PBUYKPRD          SEE IF POL                            
         BE    TB2D           NO                                                
         LA    R5,PBDELEM                                                       
         USING PPRELEM,R5                                                       
*                            BE SURE PRODUCT ELEMENT EXISTS FOR                 
*                            THIS PRODUCT                                       
         MVI   ELCOD,X'21'                                                      
TB2B     BAS   R9,NEXTEL                                                        
         BE    TB2C                                                             
         B     TBXIT            NO PRODUCT ELEM SO BYPASS THIS REC              
*                                                                               
TB2C     CLC   PPRCODE,PPRDKPRD                                                 
         BNE   TB2B                                                             
         B     TB2D          ELEM FOUND SO PROCESS                              
*                                                                               
*                                                                               
*                              PAID DATA                                        
*                                                                               
TB2D     MVI   ELCOD,X'25'                                                      
*                                                                               
*               ALL PAYMENTS MADE BEFORE OR DURING CURRENT                      
*                  MONTH ARE POSTED                                             
*                                                                               
         LA    R5,PBDELEM                                                       
GETP     BAS   R9,NEXTEL                                                        
         BNE   GETB                                                             
         USING PPDUMD03,R5                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    GETP                                                             
         MVC   FULL(4),PPGROSS                                                  
         L     R6,FULL                                                          
         MVC   FULL(4),PPAGYCOM                                                 
         L     R4,FULL                                                          
         MVC   FULL(4),PPCSHDSC                                                 
         L     R8,FULL                                                          
         CLI   PAGYPROF+14,C'0'                                                 
         BNE   PNOCD         OMIT CASH  DISCOUNT FROM REPORT                    
         SR    R6,R8                                                            
         ST    R6,FGROSS                                                        
         SR    R6,R4                                                            
         ST    R6,FNET                                                          
         B     PAIDPOST                                                         
*                                                                               
PNOCD    ST    R6,FGROSS                                                        
         SR    R6,R4                                                            
         ST    R6,FNET                                                          
PAIDPOST CLC   PPDDATE(3),CMSTART                                               
         BL    PRVPAY                                                           
         CLC   PPDDATE(3),CMEND                                                 
         BH    GETP                AFTER CURRENT MONTH                          
         LM    R3,R4,CDPAYG        POST TO CUR PAID                             
         A     R3,FGROSS                                                        
         A     R4,FNET                                                          
         STM   R3,R4,CDPAYG                                                     
         B     GETP2                                                            
*                                                                               
PRVPAY   LM    R3,R4,PDPAYG        POST TO PRV PAID                             
         A     R3,FGROSS                                                        
         A     R4,FNET                                                          
         STM   R3,R4,PDPAYG                                                     
GETP2    MVC   CLTACT(4),=4C'Y'                                                 
         MVC   TBPUB(6),PBUYKPUB                                                
         B     GETP                                                             
GETB     MVI   ELCOD,X'26'         BILLED DATA                                  
         CLC   PBDBDATE(2),MSEND          BILLABLE DATE VS  MOS                 
         BH    BUYEND              SKIP BILLED LOGIC                            
         LA    R5,PBDELEM                                                       
GETB2    BAS   R9,NEXTEL                                                        
         BNE   BUYEND                                                           
         USING PPDUMD02,R5                                                      
         OC    PBLDATE,PBLDATE                                                  
         BZ    GETB2                                                            
         CLC   PBPRD,PPRDKPRD                                                   
         BNE   GETB2                                                            
         MVC   FULL(4),PBGROSS                                                  
         L     R6,FULL                                                          
         MVC   FULL(4),PBAGYCOM                                                 
         L     R4,FULL                                                          
         MVC   FULL(4),PBCSHDSC                                                 
         L     R8,FULL                                                          
         CLI   PAGYPROF+14,C'0'       OMIT CASH DISCOUNT FROM REPORT            
         BNE   BILNOCD                                                          
         SR    R6,R8                                                            
         ST    R6,FGROSS                                                        
         SR    R6,R4                                                            
         ST    R6,FNET                                                          
         B     BILLPOST                                                         
*                                                                               
BILNOCD  ST    R6,FGROSS                                                        
         SR    R6,R4                                                            
         ST    R6,FNET                                                          
*                                                                               
BILLPOST CLC   PBLDATE(3),CMSTART                                               
         BL    PRVBILL                                                          
         CLC   PBLDATE(3),CMEND                                                 
         BH    GETB2                                                            
         LM    R3,R4,CDBILLG            POST TO CURRENT BILLING                 
         A     R3,FGROSS                                                        
         A     R4,FNET                                                          
         STM   R3,R4,CDBILLG                                                    
         B     GETB3                                                            
*                                                                               
PRVBILL  LM    R3,R4,PDBILLG                                                    
         A     R3,FGROSS                                                        
         A     R4,FNET                                                          
         STM   R3,R4,PDBILLG                                                    
GETB3    MVC   CLTACT(4),=4C'Y'                                                 
         MVC   TBPUB(6),PBUYKPUB                                                
         B     GETB2                                                            
BUYEND   CLI   BUYACT,C'Y'                                                      
         BNE   TBXIT                                                            
         CLC   PPRDKPRD,PBUYKPRD        SEE IF POL BUY                          
         BE    BUYEND1                                                          
*   FIND PRDELEM TO CALCULATE PRD SHARE                                         
*                                                                               
         XC    WORK(24),WORK                                                    
         IC    R3,PBDWTSUM                                                      
         N     R3,=F'127'                                                       
         BAS   R9,REMCOMP                                                       
         MVI   ELCOD,X'21'                                                      
         LA    R5,PBDELEM                                                       
BUYEND0  EQU   *                                                                
         USING PPRELEM,R5                                                       
         BAS   R9,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         IC    R4,PPRCOST                                                       
         N     R4,=F'127'                                                       
         BAS   R9,LEFTCOMP                                                      
         CLC   PPRCODE,PPRDKPRD                                                 
         BNE   BUYEND0                                                          
         BAS   R9,GETSHR                                                        
         B     BUYEND1                                                          
*                                                                               
*                                                                               
GETSHR   EQU   *                                                                
         LA    R8,PDPAYG                                                        
         LA    R6,4                                                             
GETSHR2  EQU   *                                                                
         L     RF,0(R8)            AMT                                          
         SR    R0,R0                                                            
         IC    R0,WORK(R6)                                                      
         SR    RE,RE                                                            
         LTR   RF,RF                                                            
         BZ    GETSHR6                                                          
         BP    *+10                                                             
         LCR   R0,R0                                                            
         L     RE,=F'-1'                                                        
         DR    RE,R3               / WTSUM (NO ROUND)                           
         MR    RE,R4               X THIS SHARE                                 
         AR    RF,R0               ADD SHARE OF PENNIES                         
         ST    RF,0(R8)                                                         
GETSHR6  EQU   *                                                                
         LA    R8,4(R8)                                                         
         BCT   R6,GETSHR2                                                       
         BR    R9                                                               
*                                                                               
*                                                                               
*                                  CALCULATE REMAINDER (AMT/WTSUM)              
*                                  STC REMAINDER IN WORK(R6) AND                
*                                  WORK+11(R6)                                  
REMCOMP  EQU   *                                                                
         LA    R8,PDPAYG                                                        
         LA    R6,4                                                             
REM2     EQU   *                                                                
         L     RF,0(R8)                                                         
         M     RE,=F'1'                                                         
         DR    RE,R3                                                            
         LPR   RE,RE                                                            
         STC   RE,WORK(R6)                                                      
         STC   RE,WORK+11(R6)                                                   
         LA    R8,4(R8)                                                         
         BCT   R6,REM2                                                          
         BR    R9                                                               
         SPACE 3                                                                
*                                  DETERMINE PART OF REMAINDER FOR              
*                                  THIS SHARE                                   
*                                                                               
LEFTCOMP EQU   *                                                                
         LA    R6,4                                                             
LEFT2    SR    R1,R1                                                            
         IC    R1,WORK+11(R6)      ORIG. REM                                    
         LTR   R1,R1                                                            
         BZ    LEFT4               NONE                                         
         SR    RF,RF                                                            
         IC    RF,WORK(R6)         LEFT OVER                                    
         LTR   RF,RF                                                            
         BZ    LEFT4               NONE LEFT                                    
         MR    R0,R4               X THIS SHARE                                 
         SLDL  R0,1                                                             
         DR    R0,R3               / WTSUM                                      
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,1                AT LEAST 1                                   
         LR    R0,RF                                                            
         SR    RF,R1                                                            
         BNM   *+8                                                              
         SR    RF,RF                                                            
         LR    R1,R0                                                            
         STC   RF,WORK(R6)         SAVE LEFT                                    
         CLC   PPRCODE,PPRDKPRD    UNLESS THIS IS OUR PRODUCT                   
         BNE   LEFT4                                                            
         STC   R1,WORK(R6)         THEN SAVE THIS SHARE                         
LEFT4    EQU   *                                                                
         BCT   R6,LEFT2                                                         
         BR    R9                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
NEXTEL   CLI   0(R5),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLC   ELCOD,0(R5)                                                      
         BCR   8,R9                                                             
         CLI   0(R5),0                                                          
         BNE   *-18                                                             
NEXTELX  LTR   R5,R5                                                            
         BR    R9                                                               
*                                                                               
*                                                                               
BUYEND1  LA    R1,PUBACC           ROLL TO PUB TOTALS                           
         LH    R2,NACCUMS                                                       
         LA    R3,LINACC                                                        
BEND1    L     R4,0(R1)                                                         
         A     R4,0(R3)                                                         
         ST    R4,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    R3,4(R3)                                                         
         BCT   R2,BEND1                                                         
*                                                                               
*                                 PRINT BUY                                     
*                                                                               
         GOTO1 DTCNV,DMCB,(1,PBUYKDAT),(3,P+54)                                 
         MVC   HALF,PBUYKEST                                                    
         LH    R2,HALF                                                          
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+68(3),DUB                                                      
         CLI   PBUYKLIN,X'02'                                                   
         BL    BEND2                                                            
         SR    R2,R2                                                            
         IC    R2,PBUYKLIN                                                      
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+63(2),DUB                                                      
         MVI   P+62,C'-'                                                        
BEND2    LA    R8,1                                                             
         BAS   R9,BUYFMT                                                        
         MVI   BUYACT,0                                                         
         XC    LINACC,LINACC                                                    
         MVI   RCSUBPRG,2                                                       
         BAS   R6,REPORTIT                                                      
         B     TBXIT                                                            
         EJECT                                                                  
TB3      CLI   MODE,LBUYPUB        LAST BUY FOR PUB                             
         BNE   TB4                                                              
         CLI   PUBACT,C'Y'                                                      
         BNE   TBXIT               NO ACTIVITY                                  
         IC    R2,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R2),TBPUB),(C'S',P+52),RR=RELO                    
         MVC   P+68(10),=C'PUB TOTALS'                                          
         LA    R0,3                                                             
         LA    R1,PRDACC            ROLL TO CLT AND AGY TOTALS                  
PUBEND   LH    R2,NACCUMS                                                       
         LA    R3,PUBACC                                                        
PUBEND1  L     R4,0(R3)                                                         
         CVD   R4,DUB                                                           
         AP    0(8,R1),DUB                                                      
         LA    R1,8(R1)                                                         
         LA    R3,4(R3)                                                         
         BCT   R2,PUBEND1                                                       
         BCT   R0,PUBEND                                                        
         LA    R8,2                                                             
         BAS   R9,BUYFMT                                                        
         MVI   PUBACT,0                                                         
         XC    PUBACC,PUBACC                                                    
         MVI   RCSUBPRG,2                                                       
         SR    R0,R0       ALWAYS PRINT PUB TOTALS                              
         IC    R0,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         BAS   R6,REPORTIT                                                      
*                                                                               
         LA    R9,PPFILED                                                       
         AH    R9,=Y(PUBREC-PPFILED)                                            
         USING PUBREC,R9                                                        
         OC    PUBNAME(40),SPACES                                               
         LA    R3,P+52                                                          
         MVC   0(20,R3),PUBNAME                                                 
         MVC   132(20,R3),PUBZNAME                                              
         CLI   QMEDIA,C'N'                                                      
         BNE   PBE6                                                             
*                                                                               
         LA    R3,20(R3)                                                        
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','                                                       
         MVC   3(16,R3),PUBCITY                                                 
         LA    R3,20(R3)                                                        
         CLI   0(R3),C' '                                                       
         BH    *+8                                                              
         BCT   R3,*-8                                                           
         MVI   1(R3),C','                                                       
         MVC   3(2,R3),PUBSTATE                                                 
         DROP  R9                                                               
*                                                                               
PBE6     DS    0H                                                               
         BAS   R6,REPORTIT                                                      
*                                                                               
         STC   R0,MAXLINES                                                      
         B     TBXIT                                                            
         EJECT                                                                  
*                                                                               
*                   EDIT SUBROUTINE FOR BUY/PUB TOTALS                          
*                                                                               
         DC    F'0'                                                             
BUYFMT   ST    R9,*-4                                                           
         BCTR  R8,0                                                             
         MH    R8,=H'96'           ACCUM DISPLACEMENT                           
         L     R0,PDBILLG(R8)                                                   
         LA    R1,P                                                             
         BAS   R9,BUYEDT                                                        
         L     R0,PDPAYG(R8)                                                    
         LA    R1,P+13                                                          
         BAS   R9,BUYEDT                                                        
         L     R0,CDBILLG(R8)                                                   
         LA    R1,P+26                                                          
         BAS   R9,BUYEDT                                                        
         L     R0,CDPAYG(R8)                                                    
         LA    R1,P+39                                                          
         BAS   R9,BUYEDT                                                        
         L     R0,PDBILLN(R8)                                                   
         LA    R1,P+78                                                          
         BAS   R9,BUYEDT                                                        
         L     R0,PDPAYN(R8)                                                    
         LA    R1,P+91                                                          
         BAS   R9,BUYEDT                                                        
         L     R0,CDBILLN(R8)                                                   
         LA    R1,P+104                                                         
         BAS   R9,BUYEDT                                                        
         L     R0,CDPAYN(R8)                                                    
         LA    R1,P+117                                                         
         BAS   R9,BUYEDT                                                        
         L     R9,BUYFMT-4                                                      
         BR    R9                                                               
*                                                                               
BUYEDT   LTR   R0,R0                                                            
         BCR   8,R9                                                             
         EDIT  (R0),(12,0(R1)),2,COMMAS=YES,MINUS=YES                           
         LTR   R0,R0                                                            
         BCR   12,R9                                                            
         LA    0,95               NO * FOR BUY LINES                            
         CR    R8,R0                                                            
         BCR   4,R9                                                             
         MVI   11(R1),C'*'                                                      
         BR    R9                                                               
         EJECT                                                                  
TB4      CLI   MODE,LBUYPRO        LAST BUY FOR PRODUCT                         
         BNE   TB5                                                              
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDEND                                                           
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'5'                                                         
         STC   R0,WORKTB                                                        
         CLC   WORKTB(1),MAXLINES                                               
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT         SKIP 2 LINES                                 
         MVC   P+53(22),=C'*** PRODUCT TOTALS ***'                              
         LA    R8,1                                                             
         BAS   R9,TOTFMT                                                        
         LA    R8,1                                                             
         BAS   R9,TESTBILL                                                      
*                                                                               
PRDEND   MVI   PRDACT,0                                                         
         MVI   LBILSW,0                                                         
         MVI   BUYSW,0                                                          
         LA    R1,PRDACC                                                        
         LH    R0,NACCUMS                                                       
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         MVI   FORCEHED,C'Y'                                                    
         B     TBXIT                                                            
         EJECT                                                                  
TB5      CLI   MODE,LBUYCLT        LAST BUY FOR CLIENT                          
         BNE   TB6                                                              
         MVI   SUMSW,0           SO LINES WILL PRINT                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   P,C' '         BE SURE P IS CLEARED                              
         MVC   P+1(131),P                                                       
         MVC   P+53(21),=C'*** CLIENT TOTALS ***'                               
         LA    R8,2                                                             
         BAS   R9,TOTFMT                                                        
         LA    R8,2                                                             
         BAS   R9,TESTBILL                                                      
         LA    R8,2                                                             
         BAS   R9,TBSUM            PRINT CLIENT TRIAL BALANCE                   
         MVI   CLTACT,0                                                         
         MVI   TBERROR,0                                                        
         LA    R1,CLTACC           CLEAR ACCUMS                                 
         LH    R0,NACCUMS                                                       
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'Y'          SEE IF DOING SUMMARY ONLY                    
         BNE   *+8                                                              
         MVI   SUMSW,1             YES - RESET SUMSW FOR NO PRINTING            
         B     TBXIT                                                            
         EJECT                                                                  
TB6      CLI   MODE,LBUYREQ                                                     
         BNE   TB7                                                              
         CLI   QCLIENT,C'*'                                                     
         BE    TB6A                                                             
         CLC   QCLIENT(3),=C'ALL'                                               
         BNE   TB6C                SKIP AGENCY TOTALS                           
TB6A     MVI   FORCEHED,C'Y'                                                    
         MVI   SUMSW,0            SO LINES WILL PRINT                           
         MVC   P+53(21),=C'*** AGENCY TOTALS ***'                               
         LA    R8,3                                                             
         BAS   R9,TOTFMT                                                        
         LA    R8,3                                                             
         BAS   R9,TBSUM                                                         
         CLI   TBERROR1,1         SEE IF ERROR IN TRIAL BALANCE                 
         BNE   TB6C                                                             
         BAS   R6,REPORTIT                                                      
         MVC   P+66-(L'BILLMSG/2)(L'BILLMSG),BILLMSG                            
         BAS   R6,REPORTIT                                                      
TB6C     LA    R1,AGYACC                                                        
         LH    R0,NACCUMS                                                       
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         CLI   QOPT1,C'Y'                                                       
         BNE   *+8                                                              
         MVI   SUMSW,1                                                          
         B     TBXIT                                                            
         EJECT                                                                  
*                   EDIT SUBROUTINE FOR PRD/CLT/AGY TOTALS                      
*                                                                               
         DC    F'0'                                                             
TOTFMT   ST    R9,*-4                                                           
         BCTR  R8,R0               R8 CONTAIN ACCUM NUMBER                      
         MH    R8,=H'192'                                                       
         LA    R2,TPDBILLG(R8)                                                  
         LA    R1,P                                                             
         BAS   R9,TOTEDIT                                                       
         LA    R2,TCDBILLG(R8)                                                  
         LA    R1,P+25                                                          
         BAS   R9,TOTEDIT                                                       
         LA    R2,TPDBILLN(8)                                                   
         LA    R1,P+78                                                          
         BAS   R9,TOTEDIT                                                       
         LA    R2,TCDBILLN(8)                                                   
         LA    R1,P+103                                                         
         BAS   R9,TOTEDIT                                                       
         LA    R2,TPDPAYG(R8)                                                   
         LA    R1,PSECOND+12                                                    
         BAS   R9,TOTEDIT                                                       
         LA    R2,TCDPAYG(R8)                                                   
         LA    R1,PSECOND+38                                                    
         BAS   R9,TOTEDIT                                                       
         LA    R2,TPDPAYN(R8)                                                   
         LA    R1,PSECOND+90                                                    
         BAS   R9,TOTEDIT                                                       
         LA    R2,TCDPAYN(R8)                                                   
         LA    R1,PSECOND+116                                                   
         BAS   R9,TOTEDIT                                                       
         MVI   RCSUBPRG,2                                                       
         CLI   MODE,LBUYPRO                                                     
         BE    TOTFMT1                                                          
         MVI   RCSUBPRG,3                                                       
         CLI   MODE,LBUYCLT                                                     
         BE    TOTFMT1                                                          
         MVI   TBERROR,0                                                        
         MVI   RCSUBPRG,4                                                       
         CLI   MODE,LBUYREQ                                                     
         BE    TOTFMT1                                                          
         DC    H'0'                                                             
TOTFMT1  BAS   R6,REPORTIT                                                      
         L     R9,TOTFMT-4                                                      
         BR    R9                                                               
*                                                                               
*                                                                               
TOTEDIT  EDIT  (P8,0(R2)),(14,0(R1)),2,COMMAS=YES,MINUS=YES                     
         CP    0(8,R2),=P'0'                                                    
         BCR   4,R9                                                             
         MVI   13(R1),C'*'                                                      
         BR    R9                                                               
*                             VERIFY DETAIL BILL = FILE                         
*                             FOR  PRD/CLT TOTALS                               
TESTBILL BCTR  R8,R0                                                            
         MH    R8,=H'192'                                                       
         LA    R2,TPDBILLG(R8)                                                  
         CP    0(8,R2),=P'0'                                                    
         BNE   TESTBIL4                                                         
         LA    R2,TCDBILLG(R8)                                                  
         CP    0(8,R2),=P'0'                                                    
         BCR   8,R9                NO TESTS - NO DETAIL BILLING                 
*                                                                               
TESTBIL4 EQU   *                                                                
         LA    R2,TDPBILLG(R8)          PRV DETAIL BILLS GROSS                  
         ZAP   DGROSS,0(8,R2)                                                   
         ZAP   DNET,8(8,R2)                                                     
         LA    R2,DGROSS                                                        
         LA    R3,TPREVG(R8)            PRV REVERSALS GROSS                     
         AP    0(8,R2),0(8,R3)          GROSS                                   
         AP    8(8,R2),8(8,R3)          NET                                     
         LA    R3,TPDBILLG(8)                                                   
         CP    0(8,R2),0(8,R3)                                                  
         BE    *+8                                                              
         MVI   TBERROR,1                                                        
         CP    8(8,R2),8(8,R3)                                                  
         BE    *+8                                                              
         MVI   TBERROR,1                                                        
         LA    R2,TDCBILLG(R8)                                                  
         ZAP   DGROSS,0(8,R2)                                                   
         ZAP   DNET,8(8,R2)                                                     
         LA    R2,DGROSS                                                        
         LA    R3,TCREVG(R8)                                                    
         AP    0(8,R2),0(8,R3)                                                  
         AP    8(8,R2),8(8,R3)                                                  
         LA    R3,TCDBILLG(R8)                                                  
         CP    0(8,R2),0(8,R3)                                                  
         BE    *+8                                                              
         MVI   TBERROR,1                                                        
         CP    8(8,R2),8(8,R3)                                                  
         BE    *+8                                                              
         MVI   TBERROR,1                                                        
         CLI   TBERROR,0                                                        
         BER   R9                                                               
*                                                                               
TBILLERR EQU   *                                                                
         BAS   R6,REPORTIT                                                      
         MVC   P+66-(L'BILLMSG/2)(L'BILLMSG),BILLMSG                            
         MVI   RCSUBPRG,2                                                       
         BAS   R6,REPORTIT                                                      
         OI    TBERROR1,1          SET ERROR FOR AGENCY                         
         BR    R9                                                               
*                                                                               
BILLMSG  DC    C'** ERROR ** DETAIL BILLS + REVERSALS DO NOT EQUAL SUM X        
               OF DETAIL BILLED ITEMS ON FILE'                                  
         EJECT                                                                  
*                        TRIAL BALANCE SUMMARY                                  
         DC    F'0'                                                             
TBSUM    ST    R9,*-4                                                           
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT                                                      
         MVC   P(132),TITLES1                                                   
         CLI   PAGYPROF+14,C'0'    OMIT CASH DISCOUNT FROM REPORT               
         BE    *+16                                                             
         MVC   P+14(22),=C'---  ** GROSS **  ----'                              
         MVC   P+90(16),=C'-  ** NET **  --'                                    
         MVC   PSECOND(132),TITLES2                                             
         BAS   R6,REPORTIT                                                      
         MVC   P(132),TITLES3                                                   
         MVC   PSECOND(132),=132C'-'                                            
         BAS   R6,REPORTIT                                                      
         BCTR  R8,R0                                                            
         MH    R8,=H'192'                                                       
*                            TOTAL PRV O/B - PRV REV + PRV D/B                  
*                                                                               
         LA    R2,TOPBILLG(R8)                                                  
         LA    R3,TPREVG(R8)                                                    
         SP    0(8,R2),0(8,R3)                                                  
         SP    8(8,R2),8(8,R3)                                                  
         LA    R3,TPDBILLG(R8)                                                  
         AP    0(8,R2),0(8,R3)                                                  
         AP    8(8,R2),8(8,R3)                                                  
         LA    R1,P                                                             
         BAS   R9,TBSEDT                                                        
         LA    R2,TOCBILLG(R8)                                                  
         LA    R3,TCREVG(R8)                                                    
         SP    0(8,R2),0(8,R3)                                                  
         SP    8(8,R2),8(8,R3)                                                  
         LA    R3,TCDBILLG(R8)                                                  
         AP    0(8,R2),0(8,R3)                                                  
         AP    8(8,R2),8(8,R3)                                                  
         LA    R1,P+24                                                          
         BAS   R9,TBSEDT                                                        
         LA    R2,TCREVG(R8)            CUR REVERSALS                           
         LA    R1,P+48                                                          
         BAS   R9,TBSEDT                                                        
         LA    R2,TPDPAYG(R8)           PRV  PAY GROSS                          
         LA    R1,PSECOND+12                                                    
         BAS   R9,TBSEDT                                                        
         LA    R2,TCDPAYG(R8)           CUR  PAY GROSS                          
         LA    R1,PSECOND+36                                                    
         BAS   R9,TBSEDT                                                        
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT                                                      
*                                                                               
         LA    R2,TOPBILLG(R8)                                                  
         LA    R3,TPDPAYG(R8)                                                   
         SP    0(8,R2),0(8,R3)                                                  
         SP    8(8,R2),8(8,R3)                                                  
         LA    R1,P+12                                                          
         BAS   R9,TBSEDT                                                        
         CP    0(8,R2),=P'0'                                                    
         BC    4,*+12                                                           
         MVI   P+26,C'*'                                                        
         MVI   P+93,C'*'                                                        
         LA    R2,TOCBILLG(R8)                                                  
         LA    R3,TCDPAYG(R8)                                                   
         SP    0(8,R2),0(8,R3)                                                  
         SP    8(8,R2),8(8,R3)                                                  
         LA    R1,P+36                                                          
         BAS   R9,TBSEDT                                                        
         CP    0(8,R2),=P'0'                                                    
         BC    4,*+12                                                           
         MVI   P+50,C'*'                                                        
         MVI   P+117,C'*'                                                       
         MVC   P+55(21),=C'BILLINGS - CLEARANCES'                               
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT         SKIP A LINE                                  
*                                                                               
         LA    R2,TOPBILLG(R8)                                                  
         LA    R3,TOCBILLG(R8)                                                  
         AP    0(8,R2),0(8,R3)                                                  
         AP    8(8,R2),8(8,R3)                                                  
         LA    R1,P+24                                                          
         BAS   R9,TBSEDT                                                        
         CP    0(8,R2),=P'0'                                                    
         BC    4,*+10                                                           
         MVC   P+38(2),=C'**'                                                   
         CP    8(8,R2),=P'0'                                                    
         BC    4,*+10                                                           
         MVC   P+105(2),=C'**'                                                  
         MVC   P+55(21),=C'CLOSING TRIAL BALANCE'                               
         BAS   R6,REPORTIT                                                      
         L     R9,TBSUM-4                                                       
         BR    R9                                                               
*                                                                               
TITLES1  DC    C'--------------  * GROSS LESS C/D *  ------------- TRIAX        
               L BALANCE SUMMARY ------------------  * NET/NET *  -----X        
               ----------------------'                                          
TITLES2  DC    CL132'         OPENING                 CURRENT          X        
                REVERSED                 OPENING                  CURREX        
               NT           REVERSED'                                           
TITLES3  DC    CL132'   BILLINGS   CLEARANCES   BILLINGS   CLEARANCES  X        
                BILLINGS           BILLINGS   CLEARANCES   BILLINGS   CX        
               LEARANCES    BILLINGS'                                           
*                                                                               
TBSEDT   EDIT  (P8,0(R2)),(15,0(R1)),2,COMMAS=YES,MINUS=YES                     
TBSEDT1  EDIT  (P8,8(R2)),(15,67(R1)),2,COMMAS=YES,MINUS=YES                    
         BR    R9                                                               
         EJECT                                                                  
TB7      CLI   MODE,LBILPRO        LAST BILL FOR PROD                           
         BNE   TB8                                                              
         MVI   LBILSW,1                                                         
         MVI   RCSUBPRG,0                                                       
         BAS   R6,REPORTIT                                                      
         ZAP   DGROSS,TDCBILLG                                                  
         ZAP   DNET,TDCBILLN                                                    
         AP    DGROSS,TCREVG                                                    
         AP    DNET,TCREVN                                                      
         LA    R1,P+18                                                          
         BAS   R9,TBBEDT                                                        
         ZAP   WORKTB(8),DGROSS                                                 
         ZAP   WORKTB+8(8),DNET                                                 
         ZAP   DGROSS,TDPBILLG                                                  
         ZAP   DNET,TDPBILLN                                                    
         AP    DGROSS,TPREVG                                                    
         AP    DNET,TPREVN                                                      
         LA    R1,P+2                                                           
         BAS   R9,TBBEDT                                                        
         AP    DGROSS,WORKTB(8)                                                 
         AP    DNET,WORKTB+8(8)                                                 
         LA    R1,P+34                                                          
         BAS   R9,TBBEDT                                                        
         CP    DGROSS,=P'0'                                                     
         BC    4,*+12                                                           
         MVI   P+47,C'*'                                                        
         MVI   P+131,C'*'                                                       
         MVC   P+66-(L'BTITLE1/2)(L'BTITLE1),BTITLE1                            
         BAS   R9,TBBSIGNS                                                      
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'5'                                                         
         STC   R0,WORKTB                                                        
         CLC   WORKTB(1),MAXLINES                                               
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
         BAS   R6,REPORTIT                                                      
*                        UNREVERSED ORG BILLS                                   
*                                                                               
         ZAP   DGROSS,TOPBILLG                                                  
         ZAP   DNET,TOPBILLN                                                    
         SP    DGROSS,TPREVG                                                    
         SP    DNET,TPREVN                                                      
         AP    DGROSS,TOCBILLG                                                  
         AP    DNET,TOCBILLN                                                    
         SP    DGROSS,TCREVG                                                    
         SP    DNET,TCREVN                                                      
         LA    R1,P+34                                                          
         BAS   R9,TBBEDT                                                        
         CP    DGROSS,=P'0'                                                     
         BC    4,*+12                                                           
         MVI   P+47,C'*'                                                        
         MVI   P+131,C'*'                                                       
         MVC   P+66-(L'BTITLE2/2)(L'BTITLE2),BTITLE2                            
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT         SKIP A LINE                                  
*                             TOTAL BILLS                                       
*                                                                               
         ZAP   DGROSS,TOPBILLG                                                  
         ZAP   DNET,TOPBILLN                                                    
         AP    DGROSS,TDPBILLG                                                  
         AP    DNET,TDPBILLN                                                    
         LA    R1,P+2                                                           
         BAS   R9,TBBEDT                                                        
         ZAP   DGROSS,TOCBILLG                                                  
         ZAP   DNET,TOCBILLN                                                    
         AP    DGROSS,TDCBILLG                                                  
         AP    DNET,TDCBILLN                                                    
         LA    R1,P+18                                                          
         BAS   R9,TBBEDT                                                        
         MVC   P+66-(L'BTITLE3/2)(L'BTITLE3),BTITLE3                            
         BAS   R6,REPORTIT                                                      
*                                  ROLL TO CLT/AGY TOTALS                       
         LA    R0,2                                                             
         LA    R1,CLTACC                                                        
TBBX1    LH    R2,NACCUMS                                                       
         LA    R3,PRDACC                                                        
TBBX2    AP    0(8,R1),0(8,R3)                                                  
         LA    R1,8(R1)                                                         
         LA    R3,8(R3)                                                         
         BCT   R2,TBBX2                                                         
         BCT   R0,TBBX1                                                         
         MVI   FORCEHED,C'Y'                                                    
         B     TBXIT                                                            
*                                                                               
BUYSW    DC    X'00'                                                            
LBILSW   DC    X'00'                                                            
BTITLE1  DC    C'DETAIL BILLS + REVERSALS'                                      
BTITLE2  DC    C'UNREVERSED ORIGINAL BILLS'                                     
BTITLE3  DC    C'  *** TOTAL BILLS ***'                                         
TBBSIGNS MVI   P+17,C'+'                                                        
         MVI   P+33,C'='                                                        
         MVI   P+101,C'+'                                                       
         MVI   P+117,C'='                                                       
         BR    R9                                                               
*                                                                               
TBBEDT   EDIT  DGROSS,(14,0(R1)),2,COMMAS=YES,MINUS=YES                         
         EDIT  DNET,(14,84(R1)),2,COMMAS=YES,MINUS=YES                          
         BR    R9                                                               
*                                                                               
TB8      CLI   MODE,LBILEST                                                     
         BNE   TBX                                                              
         CLC   PBILKPRD(3),PPRDKPRD                                             
         BNE   TBXIT                                                            
         CLI   PRDACT,C'Y'                                                      
         BNE   TBXIT                                                            
         CLC   PBILKEST,=2X'00'                                                 
         BNE   TBXIT                                                            
         MVI   RCSUBPRG,0                                                       
         BAS   R6,REPORTIT                                                      
         MVC   P+30(71),=C'***** THE ABOVE ''PRODUCT TOTAL'' BILLS ARE X        
               NOT REFLECTED IN TOTALS *****'                                   
         BAS   R6,REPORTIT                                                      
         BAS   R6,REPORTIT                                                      
         B     TBXIT                                                            
         EJECT                                                                  
*                                                                               
TBX      CLI   MODE,FBUYREQ                                                     
         BNE   TBXIT                                                            
*                                                                               
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         OI    DMOUTBTS,X'FD'                                                   
*                                                                               
         MVI   SUMSW,0                                                          
         CLI   QOPT1,C'Y'         SUMMARY ONLY SO DON'T PRINT LINES             
         BNE   *+8                                                              
         MVI   SUMSW,1                                                          
         MVC   NACCUMS(2),=H'24'                                                
         MVI   TBERROR,X'00'                                                    
         MVI   TBERROR1,0                ERROR FOR AGENCY                       
         MVI   BUYSW,0                                                          
         MVI   LBILSW,0                                                         
         MVI   FORCEHED,C'Y'                                                    
         XC    CLTACT(4),CLTACT                                                 
         GOTO1 DTCNV,DMCB,(0,QSTART),(1,MSSTART)                                
         MVC   MSEND(3),MSSTART                                                 
         MVI   MSEND+2,31                                                       
         GOTO1 DTCNV,DMCB,(0,QEND),(1,CMSTART)                                  
         MVC   CMEND(3),CMSTART                                                 
         MVI   CMEND+2,31                                                       
         MVI   CMSTART+2,1                                                      
         MVI   MSSTART+2,1                                                      
         MVI   LBILSW,0                                                         
*                             CLEAR BUFFERS                                     
         XC    LINACC,LINACC                                                    
         XC    PUBACC,PUBACC                                                    
         LH    R0,NACCUMS                                                       
         MH    R0,=H'3'                                                         
         LA    R1,PRDACC           CLEAR PRD/CLT/AGY ACCUMS                     
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         B     TBXIT                                                            
*                                                                               
TBXIT    XMOD1 1                                                                
*                                                                               
*                                                                               
REPORTIT CLI   SUMSW,1        SEE IF LINE SHOULD PRINT                          
         BE    0(R6)        NO - RETURN VIA R6                                  
         CLI   QCLIENT,C'*'                                                     
         BNE   RPT10               NO CLT FILTERS                               
         CLI   QCLIENT+1,C'-'         ALL BUT                                   
         BE    RPT5                                                             
         MVC   HEAD1+80(14),=C'** OFFICE 9 **'                                  
         MVC   HEAD1+90(1),QCLIENT+1                                            
         CLI   QCLIENT+2,C' '      CHK FOR RANGE                                
         BE    RPT10               NO                                           
         MVC   HEAD1+80(15),=C'* OFFICES 1-9 *'                                 
         MVC   HEAD1+90(1),QCLIENT+1                                            
         MVC   HEAD1+92(1),QCLIENT+2                                            
         B     RPT10                                                            
*                                                                               
RPT5     MVC   HEAD1+80(15),=C'* NOT OFFICE 9 *'                                
         MVC   HEAD1+93(1),QCLIENT+2                                            
*                                                                               
RPT10    DS    0H                                                               
         GOTO1 DTCNV,DMCB,(0,QSTART),(5,HEAD3+76)                               
         GOTO1 DTCNV,DMCB,(0,QEND),(5,HEAD5+73)                                 
         MVC   SAVEPRG,RCSUBPRG    SAVE SPROG                                   
         ST    R0,SAVER0                                                        
         CLI   PAGYPROF+14,C'0'      OMIT CASH DISCOUNT FROM REPORT             
         BE    RPTX                                                             
         SR    R0,R0                                                            
         IC    R0,RCSUBPRG                                                      
         AH    R0,=H'5'                                                         
         STC   R0,RCSUBPRG                                                      
RPTX     GOTO1 REPORT                                                           
         MVC   RCSUBPRG,SAVEPRG     RESTORE SPROG                               
         L     R0,SAVER0                                                        
         BR    R6                                                               
*                                                                               
*                                                                               
PROCBIL  EQU   59                                                               
LBILPRO  EQU   153                                                              
LBUYCLT  EQU   191                                                              
LBUYPUB  EQU   197                                                              
LBUYPRO  EQU   193                                                              
LBUYREQ  EQU   190                                                              
PROCBUY  EQU   99                                                               
FBUYREQ  EQU   90                                                               
FBUYPRO  EQU   93                                                               
LBILEST  EQU   154                                                              
SAVER0   DS    F                                                                
SAVEPRG  DS    CL1                                                              
CLTACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
BUYACT   DS    CL1                                                              
ELCOD    DS    CL1                                                              
TBERROR  DS    CL1                                                              
TBERROR1 DS    CL1                                                              
SUMSW    DS    CL1         FOR REPORT 0=PRINT LINE, 1= DON'T                    
CMSTART  DS    CL3            QEND      START DATE OF CURRENT MONTH             
CMEND    DS    CL3                      END                                     
MSSTART  DS    CL3            QSTART    START DATE OF LAST REPORT MONTH         
MSEND    DS    CL3                      END                                     
NACCUMS  DS    H                                                                
TBPUB    DS    CL6                                                              
WORKTB   DS    CL35                                                             
         DS    0D                                                               
DGROSS   DS    PL8                                                              
DNET     DS    PL8                                                              
FGROSS   DS    F                                                                
FNET     DS    F                                                                
LINACC   DS    CL96                                                             
PUBACC   DS    CL96                                                             
*                                                                               
         ORG   LINACC                                                           
*                                  DETAIL ACTIVITY BUCKETS                      
PDBILLG  DS    F         PRV BILL GROSS LESS C/D                                
PDBILLN  DS    F         PRV BILL NET/NET                                       
CDBILLG  DS    F         CUR BILL GROSS LESS C/D                                
CDBILLN  DS    F         CUR BILL NET/NET                                       
PDPAYG   DS    F         PRV PAY GROSS LESS C/D                                 
PDPAYN   DS    F         PRV PAY NET/NET                                        
CDPAYG   DS    F         CUR PAY GROSS LESS C/D                                 
CDPAYN   DS    F         CUR PAY NET/NET                                        
*                                                                               
         EJECT                                                                  
         ORG                                                                    
PRDACC   DS    CL192                                                            
CLTACC   DS    CL192                                                            
AGYACC   DS    CL192                                                            
*                                                                               
*                                                                               
         ORG   PRDACC                                                           
TPDBILLG DS    D         PRV BILL GROSS LESS C/D   - FILE                       
TPDBILLN DS    D         PRV BILL NET/NET          - FILE                       
TCDBILLG DS    D         CUR BILL GROSS LESS  C/D  - FILE                       
TCDBILLN DS    D         CUR BILL NET/NET          - FILE                       
TPDPAYG  DS    D         PRV PAY GROSS LESS C/D    - FILE                       
TPDPAYN  DS    D         PRV PAY NET/NET           - FILE                       
TCDPAYG  DS    D         CUR PAY GROSS LESS C/D    - FILE                       
TCDPAYN  DS    D         CUR PAY NET/NET           - FILE                       
*                                                                               
TOPBILLG DS    D         PRV ORG BILLS GROSS LESS C/D                           
TOPBILLN DS    D         PRV ORG BILLS NET/NET                                  
TOCBILLG DS    D         CUR ORG BILLS GROSS LESS C/D                           
TOCBILLN DS    D         CUR ORG BILLS NET/NET                                  
TPREVG   DS    D         PRV REV GROSS LESS C/D                                 
TPREVN   DS    D         PRV REV NET/NET                                        
TCREVG   DS    D         CUR REV GROSS LESS C/D                                 
TCREVN   DS    D         CUR REV NET/NET                                        
TDPBILLG DS    D         PRV DETAIL BILLS GROSS LESS C/D                        
TDPBILLN DS    D         PRV DETAIL BILLS NET/NET                               
TDCBILLG DS    D         CUR DETAIL BILLS GROSS LESS C/D                        
TDCBILLN DS    D         CUR DETAIL BILLS NET/NET                               
         ORG                                                                    
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPWORKD                                                        
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057PPREP9002 05/01/02'                                      
         END                                                                    
