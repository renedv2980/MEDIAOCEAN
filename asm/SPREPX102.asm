*          DATA SET SPREPX102  AT LEVEL 223 AS OF 05/01/02                      
*PHASE SPX102A,+0                                                               
*INCLUDE DTCNV                                                                  
*INCLUDE BRDMON                                                                 
*INCLUDE GETDAY                                                                 
         TITLE 'SPX102 - COKE BRDCAST PLANNING BDGT RPT'                        
         PRINT NOGEN                                                            
*                                                                               
*        REQUEST OPTIONS                                                        
*                                                                               
*        QOPT1      Y=SUMMARIES ONLY                                            
*        QOPT2      Y=PRODUCE EXTRACT TAPE                                      
*        QOPT3      P=ONLY PURCHASED COLUMN, S=PUR ONLY WITH STA DETAIL         
         SPACE 2                                                                
SPX102   CSECT                                                                  
         NMOD1 0,SPX102,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPX1WRKD,R9                                                      
         LA    R8,SPX102+4095                                                   
         LA    R8,1(R8)                                                         
         USING SPX102+4096,R8      ** NOTE USE OF R8 AS SECOND BASE **          
         CLI   MODE,REQFRST                                                     
         BE    INITIAL                                                          
         CLI   MODE,CLTFRST                                                     
         BE    SETCLT                                                           
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,PROCBUY                                                     
         BE    PROCB                                                            
         CLI   MODE,PROCGOAL                                                    
         BE    PROCG                                                            
         CLI   MODE,CLTLAST                                                     
         BE    TBCLTL                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
INITIAL  DS    0H                                                               
         STM   R7,RC,HDHKR7                                                     
         LA    R0,TBHDHK                                                        
         ST    R0,HEADHOOK                                                      
         OC    PROGPROF,PROGPROF   CK FOR PROFILE                               
         BZ    INIT10                                                           
         LA    R2,QOPT1                                                         
         LA    R3,PROGPROF                                                      
         LA    R4,5                                                             
INIT5    CLI   0(R2),C' '          SEE IF OPTION REQUESTED                      
         BNE   *+10                YES                                          
         MVC   0(1,R2),0(R3)       GET VALUE FROM PROFILE                       
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,INIT5                                                         
*                                                                               
INIT10   DS    0H                                                               
         CLC   QEST(3),=C'ALL'                                                  
         BNE   *+10                                                             
         MVC   QEST(3),=C'NO '     CHANGE EST=ALL TO NO                         
*                          SINCE THE REPORT COMBINES ESTS ANYWAY                
         MVI   CLTACT,0                                                         
         MVC   PAGE,=H'1'                                                       
         LA    R0,BUFREC                                                        
         ST    R0,BUFFIO                                                        
         L     R0,=A(BUFFALOC)                                                  
         A     R0,RELO                                                          
         ST    R0,BUFFBUFF                                                      
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF                                    
         L     RF,=A(BTOTALS)                                                   
         A     RF,RELO                                                          
         ST    RF,VBTOTS                                                        
         L     RF,=V(DTCNV)                                                     
         A     RF,RELO                                                          
         ST    RF,DTCNV                                                         
*                                                                               
*                                                                               
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDNUMPE,=F'1'                                                   
         MVC   MEDNUMMO,=F'13'                                                  
         MVC   MEDLCHNK,=F'200'                                                 
         MVI   MEDEXTDM,4                                                       
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQMGOPT,C'Y'        INCLUDE MGOOD $'S IN MISSED MTH              
*                                  CONTROLLED BY CLT PROFILE                    
         MVI   RQEQUIV,C'Y'                                                     
         MVI   MEDEXTAC,C'Y'                                                    
         DROP  RE                                                               
         MVI   MYMKTSW,0                                                        
         MVI   MYSTASW,0                                                        
         CLI   QMGR,C' '           SEE IF DOING MKTGROUPS                       
         BE    INIT15              NO                                           
         MVI   MYMKTSW,X'F2'                                                    
         CLC   QSTA(3),=C'ALL'     SEE IF DOING ALL MKTGRPS                     
         BE    INIT20                                                           
         MVI   MYMKTSW,X'F1'       SET FOR ONE MKTGROUP                         
         B     INIT20                                                           
*                                                                               
INIT15   MVI   MYMKTSW,2                                                        
         CLC   QMKT(3),=C'ALL'     SEE IF DOING ALL MKTS                        
         BE    INIT20              YES                                          
         MVI   MYMKTSW,1           SET FOR ONE MARKET                           
         CLC   QSTA(3),=C'ALL'                                                  
         BE    INIT20                                                           
         CLC   QSTA(3),=C'   '                                                  
         BE    INIT20                                                           
         MVI   MYSTASW,1           SET FOR ONE STATION                          
INIT20   EQU   *                                                                
*                                  MYSTASW - 01=ONE STATION                     
*                                  MYMKTSW - 01=ONE MKT,2=ALL MKTS              
*                                  F1=ONE MKTGROUP,F2=ALL MKTGROUPS             
         B     EXIT                                                             
         EJECT                                                                  
ESTF     DS    0H                  ESTFIRST                                     
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         CLI   QSTART,C' '         ES DATES                                     
         BNE   ESTF10                                                           
         MVC   QSTART(12),ESTART                                                
*                                                                               
ESTF10   GOTO1 DATCON,DMCB,(0,QSTART),(1,BQSTART)                               
         GOTO1 DATCON,DMCB,(0,QSTART),(2,BQSTARTP)                              
         GOTO1 DATCON,DMCB,(0,QEND),(1,BQEND)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(2,BQENDP)                                  
         GOTO1 MEDDATE,DMCB,(RC)                                                
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
SETCLT   DS    0H                  CLIENT FIRST                                 
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'01',BUFFBUFF),(X'80',1)                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'02',BUFFBUFF),(X'80',1)                
         GOTO1 BUFFALO,DMCB,=C'CLEAR',(X'03',BUFFBUFF),(X'80',1)                
         GOTO1 MEDPRDRD,DMCB,(RC)                                               
         MVC   DUB(3),QAGY                                                      
         MVC   DUB+3(2),BCLT                                                    
         GOTO1 EQVRD,DMCB,DUB,DPEQTAB,ADBUY,DATAMGR                             
         L     RE,ADBUY                                                         
         MVC   EQTAB,0(RE)                                                      
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RQRDPOL,C'Y'                                                     
         CLC   QPRD,=C'POL'                                                     
         BE    *+8                                                              
         MVI   RQRDPOL,C'N'                                                     
*                                  BUILD TABLE OF POL ESTS                      
         XC    ESTTAB(256),ESTTAB                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),=C'POL'                                                 
         GOTO1 HIGH                                                             
SETP5    GOTO1 SEQ                                                              
SETP7    CLC   KEYSAVE(7),KEY      CHK AGY/MED/CLT/POL                          
         BNE   SETP20                                                           
         CLI   KEY+8,0                                                          
         BNE   SETP5               NOT AN EST SKIP                              
         GOTO1 GETEST                                                           
SETEST   DS    0H                  EST FIRST - SEE WHICH KIND OF EST            
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         ZIC   R5,EKEY+7           EST NUMBER                                   
         LA    R5,ESTTAB-1(R5)                                                  
         CLI   EPROF,C'U'                                                       
         BNE   *+8                                                              
         OI    0(R5),X'80'         NON-BILLABLE EST                             
         MVI   WORK,0                                                           
         CLI   EBILLBAS,0          CORPORATE EST NO FORMULA                     
         BE    SETEX                                                            
         CLI   EBILLBAS,X'51'      CHK FOR NET/NET WITH COMMISSION              
         BE    SETE5               LEAVE ESTBFSW AS 0                           
         CLI   EBILLBAS,X'11'      CHK FOR NET/NET                              
         BNE   SETEX               LEAVE ESTBFSW AS 0                           
SETE5    MVI   WORK,1                                                           
         CLC   EBILLCOM,=F'150000' NET + 15 PCT NET                             
         BE    SETEX                                                            
         MVI   WORK,2                                                           
         CLC   EBILLCOM,=F'176500' NET + 17.65 PCT OF NET                       
         BE    SETEX                                                            
         MVI   WORK,4                                                           
         CLC   EBILLCOM,=F'030000' NET + 3.0 PCT OF NET (MTTO)                  
         BE    SETEX                                                            
         MVI   WORK,3                                                           
         CLC   EBILLCOM,=F'111000' NET + 11.10 PCT OF NET (MTTO)                
         BE    SETEX                                                            
         CLI   EBILLBAS,X'51'      CHK FOR NET/NET WITH COMMISSION              
         BNE   SETE10              LEAVE ESTBFSW AS 0                           
         CLC   EBILLCOM,=F'-889000'                                             
*                                                                               
*        COMMISSION ONLY CNET -88.9000 OF NET                                   
*        TREAT SAME AS NET +11.1000 OF NET                                      
*                                                                               
         BE    SETEX                                                            
SETE10   MVI   WORK,0           RESET TO 0                                      
*                                                                               
SETEX    OC    0(1,R5),WORK                                                     
         B     SETP5                                                            
*                                                                               
SETP20   MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESTORE SEQ READ                             
TBCLTFX  B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
PROCB    DS    0H                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   KEY+4(2),BUYREC+4   PASSIVE SPILL POINTER                        
         BNE   EXIT                BYPASS                                       
         MVI   CLTACT,C'Y'                                                      
         ZIC   R5,BUYREC+9                                                      
         LA    R5,ESTTAB-1(R5)                                                  
         MVI   ESTBFSW,0                                                        
         MVI   ESTNBSW,X'FF'       SO I'LL GET MTH TOTAL AFTER                  
*                                  NON-BILLABLE                                 
         TM    0(R5),X'80'         CHK FOR NON-BILLABLE EST                     
         BZ    *+8                                                              
         MVI   ESTNBSW,1                                                        
         OC    ESTBFSW,0(R5)                                                    
         NI    ESTBFSW,X'7F'      SET OF X'80' BIT                              
         XC    BUFREC,BUFREC       CLEAR AND BUILD BUFF KEY                     
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFMGID(5),MGR1                                                  
         MVC   BUFGNAME,MGR1NM                                                  
         MVC   BUFMKT,MKT                                                       
         MVC   BUFSTA,STA          STATION                                      
         CLI   QOPT3,C'S'          SEE IF DOING STATION TOTALS                  
         BE    *+10                                                             
         MVC   BUFSTA,=5X'FF'                                                   
         MVC   BUFNBMTH,ESTNBSW                                                 
         MVC   BUFMNAME,MKTNM      USE MKTNAME FOR POL REQS                     
         LA    R5,6                                                             
         LA    R4,BUFPGR                                                        
PROCB5   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R5,PROCB5                                                        
*                                                                               
         MVC   MYPRD,BPRD                                                       
         MVC   MYBDSEC,BDSEC                                                    
         CLC   QPRD,=C'POL'                                                     
         BNE   PROCB8              ONE PRODUCT                                  
         XC    MPDLIST,MPDLIST                                                  
         GOTO1 MEDPSL,DMCB,(RC),MPDLIST                                         
         LA    R5,MPDLIST                                                       
         ST    R5,ANXTPRD                                                       
PROCB6   L     R5,ANXTPRD                                                       
         CLI   0(R5),0             END OF PRDS                                  
         BE    PROCBXX                                                          
         MVC   MYPRD,0(R5)                                                      
         MVC   MYBDSEC,1(R5)                                                    
         LA    R5,2(R5)                                                         
         ST    R5,ANXTPRD                                                       
PROCB8   DS    0H                                                               
         MVC   BUFPRD,=3X'FF'                                                   
**NEW**                                                                         
****     CLC   QPRD,=C'POL'                                                     
****     BNE   PROCB9              ONE PRD - MKT TOTALS ONLY                    
**NEW**                                                                         
*                                  GET PRD FROM PRDBUFF                         
         ZIC   RE,MYPRD                                                         
         CLI   MYPRD,X'FF'                                                      
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         AR    RE,RF                                                            
**NEW**                                                                         
         CLC   QPRD,=C'POL'                                                     
         BE    PROCB8D                                                          
         CLC   QPRD,=C'ALL'        SEE IF DOING EACH PRD SEPERATELY             
         BNE   PROCB9                                                           
         MVC   BUFMPRD,1(RE)                                                    
         B     PROCB8F             STILL STORE NAME                             
PROCB8D  MVC   BUFPRD,1(RE)                                                     
PROCB8F  MVC   BUFPNAME,4(RE)                                                   
**NEW**                                                                         
PROCB9   MVC   SVBUFKEY,BUFKEY                                                  
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,MYPRD                                                   
         MVC   MEDSPTLN,MYBDSEC                                                 
         GOTO1 MEDGETBY,DMCB,(RC),0                                             
         LA    R3,13               FOR BCT                                      
         L     RE,MEDBUFF                                                       
         LA    R5,MEDMON01         FIRST MONTH                                  
PROCB10  L     R2,4(R5)                                                         
         USING MEDDATA,R2                                                       
         L     R0,MEDBYGRS                                                      
         CVD   R0,DUB                                                           
         L     R0,MEDBYTAX         SUBTRACT TAX                                 
         CVD   R0,DOUBLE                                                        
         SP    DUB,DOUBLE          SUBTRACT TAX BEFORE FACTORING                
         CLI   ESTBFSW,0           CHK FOR BILL FORMULA                         
         BE    PROCB12                                                          
         ZAP   DOUBLE,=P'11500'    15.00 PCT                                    
         CLI   ESTBFSW,1                                                        
         BE    PROCB11                                                          
         ZAP   DOUBLE,=P'11765'    17.65 PCT                                    
         CLI   ESTBFSW,2                                                        
         BE    PROCB11                                                          
         ZAP   DOUBLE,=P'10300'    3.0  PCT FOR MTTO                            
         CLI   ESTBFSW,4                                                        
         BE    PROCB11                                                          
         ZAP   DOUBLE,=P'11110'    11.1 PCT FOR MTTO                            
         CLI   ESTBFSW,3                                                        
         BE    PROCB11                                                          
         B     PROCB12                                                          
*                                                                               
PROCB11  MP    DUB,DOUBLE+5(3)                                                  
         DP    DUB,=P'10000'                                                    
         ZAP   DOUBLE,DUB(5)                                                    
         ZAP   DUB,DOUBLE                                                       
PROCB12  ZAP   BUFOGRS,DUB                                                      
         L     R0,MEDBYNET                                                      
         CVD   R0,DUB                                                           
         ZAP   BUFONET,DUB                                                      
         L     R0,MEDBYTAX                                                      
         CVD   R0,DUB                                                           
         SP    BUFONET,DUB         SUBTRACT TAX FROM NET                        
         ZAP   BUFOTAX,DUB                                                      
         GOTO1 PUTBUFF                                                          
         LA    R5,12(R5)          NEXT MONTH                                    
         CLC   0(2,R5),=X'0000'    END OF DATA                                  
         BE    PROCB14                                                          
         BCT   R3,PROCB10                                                       
PROCB14  BE    PROCBX                                                           
*                                                                               
PROCBX   CLC   QPRD,=C'POL'                                                     
         BE    PROCB6              GO DO NEXT PRD IN MPDLIST                    
PROCBXX  B     EXIT                                                             
         EJECT                                                                  
PROCG    DS    0H                  PROCESS GOALS                                
         CLI   QOPT3,C'P'          SEE IF DOING PURCHASED ONLY                  
         BE    PROCGX              YES SKIP GOALS                               
         CLI   QOPT3,C'S'          SEE IF DOING PURCHASED ONLY + STA            
         BE    PROCGX              YES SKIP GOALS                               
         MVI   CLTACT,C'Y'                                                      
         L     R6,ADGOAL                                                        
         USING GOALRECD,R6                                                      
         MVC   MYPRD,GOALREC+4                                                  
         ZIC   R5,GOALREC+7                                                     
         LA    R5,ESTTAB-1(R5)                                                  
         MVI   ESTBFSW,0                                                        
         MVI   ESTNBSW,X'FF'       SO I'LL GET MTH TOTAL AFTER                  
*                                  NON-BILLABLE                                 
         TM    0(R5),X'80'         CHK FOR NON-BILLABLE EST                     
         BZ    *+8                                                              
         MVI   ESTNBSW,1                                                        
         OC    ESTBFSW,0(R5)                                                    
         NI    ESTBFSW,X'7F'      SET OF X'80' BIT                              
         XC    BUFREC,BUFREC       CLEAR AND BUILD BUFF KEY                     
         MVI   BUFTYP,X'01'                                                     
         MVC   BUFMGID(5),MGR1                                                  
         MVC   BUFGNAME,MGR1NM                                                  
         MVC   BUFMKT,MKT                                                       
         MVC   BUFSTA,=5X'FF'      SINCE GOALS ARE NOT BY STATION               
         MVC   BUFNBMTH,ESTNBSW                                                 
         MVC   BUFMNAME,MKTNM      USE MKTNAME FOR POL REQS                     
         MVC   BUFPRD,=3X'FF'                                                   
**NEW**                                                                         
*****    CLC   QPRD,=C'POL'                                                     
*****    BNE   PROCG9              ONE PRD - MKT TOTALS ONLY                    
**NEW**                                                                         
*                                  GET PRD FROM PRDBUFF                         
         ZIC   RE,MYPRD                                                         
         CLI   MYPRD,X'FF'                                                      
         BNE   *+8                                                              
         LA    RE,220                                                           
         BCTR  RE,0                                                             
         MH    RE,PRDBUFLN                                                      
         L     RF,PRDBUFF                                                       
         AR    RE,RF                                                            
**NEW**                                                                         
         CLC   QPRD,=C'POL'                                                     
         BE    PROCG8D                                                          
         CLC   QPRD,=C'ALL'        SEE IF DOING EACH PRD SEPERATELY             
         BNE   PROCG9                                                           
         MVC   BUFMPRD,1(RE)                                                    
         B     PROCG8F             STILL STORE NAME                             
PROCG8D  MVC   BUFPRD,1(RE)                                                     
PROCG8F  MVC   BUFPNAME,4(RE)                                                   
**NEW**                                                                         
PROCG9   MVC   SVBUFKEY,BUFKEY                                                  
         LA    R5,6                                                             
         LA    R4,BUFPGR                                                        
PROCG5   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R5,PROCG5                                                        
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         MVC   MEDBRAND,MYPRD                                                   
         GOTO1 MEDGETGL,DMCB,(RC)                                               
         LA    R3,13               FOR BCT                                      
         L     RE,MEDBUFF          RESET RE TO MEDBUFF                          
         LA    R5,MEDMON01         FIRST MONTH                                  
PROCG10  L     R2,4(R5)                                                         
         USING MEDDATA,R2                                                       
         L     R0,MEDGLD           GOAL DOLLARS                                 
         CVD   R0,DUB                                                           
         CLI   ESTBFSW,0           SEE IF GOAL DOLLARS ARE GROSS                
         BNE   PROCG20                                                          
         ZAP   BUFPGR,DUB                                                       
         MP    DUB,=P'85'                                                       
         DP    DUB,=P'100'                                                      
         ZAP   BUFPNET,DUB(6)      FACTORED DOWN                                
         B     PROCG30             NOW GO CALC TAX                              
*                                                                               
PROCG20  DS    0H                  GOAL DOLLARS ARE NET                         
*                                  MUST FACTOR THEM UP                          
         ZAP   BUFPNET,DUB                                                      
         ZAP   DOUBLE,=P'11500'    15 PCT                                       
         CLI   ESTBFSW,1                                                        
         BE    PROCG22                                                          
         ZAP   DOUBLE,=P'11765'    17.65 PCT                                    
         CLI   ESTBFSW,2                                                        
         BE    PROCG22                                                          
         ZAP   DOUBLE,=P'11100'    11.1  PCT                                    
         CLI   ESTBFSW,3                                                        
         BE    PROCG22                                                          
         ZAP   DOUBLE,=P'10300'    3.00  PCT                                    
         CLI   ESTBFSW,4                                                        
         BE    PROCG22                                                          
         DC    H'0'                INVALID ESTBFSW MUST BE 0-4                  
*                                                                               
PROCG22  MP    DUB,DOUBLE+5(3)                                                  
         DP    DUB,=P'10000'                                                    
         ZAP   BUFPGR,DUB(5)       FACTORED UP NET                              
*                                                                               
PROCG30  EQU   *                   TAX CALC                                     
         BAS   RE,GETTAX                                                        
         GOTO1 PUTBUFF                                                          
         LA    R5,12(R5)           NEXT MONTH                                   
         CLC   0(2,R5),=X'0000'    END OF DATA                                  
         BE    PROCGX                                                           
         BCT   R3,PROCG10                                                       
*                                                                               
PROCGX   B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
PUTBUFF  NTR1                                                                   
*                                  DON'T POST NON-BILLABLE IF $                 
         CLI   BUFNBMTH,X'FF'                                                   
         BE    PUTB10                                                           
         MVI   WORK,0                                                           
         LA    R6,6                                                             
         LA    R4,BUFPGR                                                        
PUTB5    CP    0(8,R4),=P'0'                                                    
         BE    *+8                                                              
         MVI   WORK,1                                                           
         LA    R4,8(R4)                                                         
         BCT   R6,PUTB5                                                         
         CLI   WORK,1                                                           
         BNE   PUTB20                                                           
PUTB10   MVC   BUFKEY,SVBUFKEY                                                  
         MVC   BUFMTH,0(R5)       START DATE OF BCAST MTH                       
         BAS   RE,PUTSET                                                        
         MVC   BUFKEY,SVBUFKEY                                                  
         MVC   BUFMTH,=X'FFFF'      DO MTHS TOTAL                               
         BAS   RE,PUTSET                                                        
         MVC   BUFKEY,SVBUFKEY                                                  
         BAS   RE,PUTTOTS                                                       
         MVC   BUFKEY,SVBUFKEY                                                  
         CLI   BUFNBMTH,1          SEE IF I DID NON-BILLABLE                    
         BNE   PUTBX               NO THEN I'M DONE                             
PUTB20   MVC   BUFKEY,SVBUFKEY                                                  
         MVC   BUFMTH,0(R5)                                                     
         MVI   BUFNBMTH,X'FF'                                                   
         BAS   RE,PUTSET                                                        
         MVC   BUFKEY,SVBUFKEY                                                  
         MVC   BUFMTH,=X'FFFF'                                                  
         MVI   BUFNBMTH,X'FF'                                                   
         BAS   RE,PUTSET                                                        
         MVC   BUFKEY,SVBUFKEY                                                  
         MVI   BUFNBMTH,X'FF'                                                   
         BAS   RE,PUTTOTS                                                       
         MVC   BUFKEY,SVBUFKEY     RESTORE ORIGIONAL BUFKEY                     
*                                                                               
PUTBX    XIT1                                                                   
         SPACE 2                                                                
PUTSET   NTR1                                                                   
         CLI   QOPT1,C'Y'          SEE IF DOING SUMMARIES ONLY                  
         BNE   PUTS5                                                            
         MVC   BUFMKT,=4X'FF'                                                   
         MVC   BUFSTA,=5X'FF'                                                   
PUTS5    GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         CLI   MYSTASW,1           SEE IF DOING ONE STA                         
         BE    PUTS14              YES - SKIP MKT AND HIGHER TOTALS             
         CLC   BUFSTA,=5X'FF'      SEE IF I'VE DONE MKT TOTAL                   
         BE    PUTS10                                                           
         MVC   BUFSTA,=5X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
PUTS10   CLI   MYMKTSW,1           SEE IF DOING ONE MKT                         
         BE    PUTS14              YES SKIP ALLMKT AND ALLMKTG TOTALS           
         CLC   BUFMKT,=4X'FF'      SEE IF I'VE DONE ALL MKTS                    
         BE    PUTS12                                                           
         MVC   BUFMKT,=4X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
PUTS12   CLI   MYMKTSW,X'F2'       SEE IF DOING ALL MKTGROUPS                   
         BNE   PUTS14              NO - SKIP ALL MKTGROUPS                      
         MVC   BUFMGID(5),=5X'FF'                                               
         MVC   WORK(24),BUFGNAME                                                
         MVC   BUFGNAME,=CL24'ALL MARKET GROUPS'                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFGNAME,WORK                                                    
*                                                                               
PUTS14   CLI   BUFPRD,X'FF'        SEE IF I'VE DOING ONE PRD                    
         BE    PUTSX               YES- DONE                                    
*                                                                               
PUTS15   MVC   BUFKEY(18),SVBUFKEY RESET MKT AND MGRP AND STA                   
         MVC   BUFPRD,=3X'FF'      FOR STA TOTAL                                
         CLI   QOPT1,C'Y'          SEE IF DOING SUMMARIES ONLY                  
         BNE   PUTS20                                                           
         MVC   BUFMKT,=4X'FF'                                                   
         MVC   BUFSTA,=5X'FF'                                                   
PUTS20   GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         CLI   MYSTASW,1           SEE IF DOING ONE STA                         
         BE    PUTSX               YES - DONE                                   
         CLC   BUFSTA,=5X'FF'      SEE IF I'VE DONE MKT TOTAL                   
         BE    PUTS25                                                           
PUTS22   MVC   BUFSTA,=5X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
PUTS25   CLI   MYMKTSW,1           SEE IF DOING ONE MKT                         
         BE    PUTSX               YES DONE                                     
         CLC   BUFMKT,=4X'FF'      SEE IF I'VE DONE ALL MKTS                    
         BE    PUTS30                                                           
         MVC   BUFMKT,=4X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
PUTS30   CLI   MYMKTSW,X'F2'       SEE IF DOING ALL MKTGROUPS                   
         BNE   PUTSX               NO DONE                                      
         MVC   BUFMGID(5),=5X'FF'                                               
         MVC   WORK(24),BUFGNAME                                                
         MVC   BUFGNAME,=CL24'ALL MARKET GROUPS'                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFGNAME,WORK                                                    
*                                                                               
PUTSX    XIT1                                                                   
         EJECT                                                                  
PUTTOTS  NTR1                      MKT AND MKTGRP RECAPS                        
         CLI   MYMKTSW,1           SEE IF DOING ONE MKT                         
         BE    PUTTOTX             YES NO RECAPS                                
         MVI   BUFTYP,X'02'        MARKET RERCAP                                
         XC    BUFMTH,BUFMTH                                                    
         XC    BUFPRD,BUFPRD                                                    
         XC    BUFSTA,BUFSTA                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFMKT,=4X'FF'                                                   
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         XC    BUFMKT,BUFMKT                                                    
         CLI   MYMKTSW,X'F2'       SEE IF DOING ALL MKTGROUPS                   
         BNE   PUTTOTX             NO - THEN DONE                               
         MVI   BUFTYP,X'03'        MKTGRP RECAP                                 
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFMGID(5),=5X'FF'                                               
         MVC   WORK(24),BUFGNAME                                                
         MVC   BUFGNAME,=CL24'ALL MARKET GROUPS'                                
         GOTO1 BUFFALO,DMCB,=C'PUT',BUFFBUFF,BUFREC                             
         MVC   BUFGNAME,WORK                                                    
PUTTOTX  XIT1                                                                   
         EJECT                                                                  
NEXTEL   DS    0H                                                               
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL                                                           
*                                                                               
NEXTEL2  DS    0H                                                               
         LTR   R2,R2                                                            
         BR    RE                  SET CC NOT EQ                                
         EJECT                                                                  
GETTAX   NTR1                                                                   
         LA    R5,TAXTBL                                                        
GETT5    CLI   0(R5),X'FF'         END OF TABLE                                 
         BE    GETTX                                                            
         CLC   MKT,0(R5)                                                        
         BL    GETTX               NOT FOUND                                    
         BE    GETT10              FOUND                                        
         LA    R5,6(R5)                                                         
         B     GETT5                                                            
*                                                                               
GETT10   ZAP   DUB,BUFPNET                                                      
         MP    DUB,4(2,R5)                                                      
         DP    DUB,=P'1000'                                                     
         ZAP   BUFPTAX,DUB(5)                                                   
GETTX    XIT1                                                                   
*  HEADHOOK                                                                     
*                                                                               
         CNOP  0,4                                                              
         USING *,RF                                                             
TBHDHK   NTR1                                                                   
         LM    R7,RC,HDHKR7                                                     
         B     HDHK2                                                            
*                                                                               
HDHKR7   DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    DS    0H                                                               
         CLI   QCLT,C'*'           CHK FOR OFFICE FILTERS                       
         BNE   HDHK6                                                            
         CLI   QCLT+1,C'-'         ALL BUT                                      
         BE    HDHK4                                                            
         MVC   HEAD1+82(14),=C'** OFFICE 9 **'                                  
         MVC   HEAD1+92(1),QCLT+1                                               
         CLI   QCLT+2,C' '                                                      
         BE    HDHK6               NO RANGE                                     
         MVC   HEAD1+82(15),=C'* OFFICES 1-9 *'                                 
         MVC   HEAD1+92(1),QCLT+1                                               
         MVC   HEAD1+94(1),QCLT+2                                               
         B     HDHK6                                                            
*                                                                               
HDHK4    MVC   HEAD1+82(16),=C'* NOT OFFICE 9 *'                                
         MVC   HEAD1+95(1),QCLT+2                                               
*                                                                               
HDHK6    DS    0H                                                               
         IF    TOTTYP,NE,X'01',HDHK8                                            
         CLI   BUFMGID,X'FF'       SEE IF DOING CORP TOTALS                     
         BNE   HDHK7                                                            
         MVC   HEAD7(24),BUFGNAME                                               
         B     HDHK8                                                            
HDHK7    MVC   HEAD7(5),BUFMGID                                                 
         MVC   HEAD7+6(24),BUFGNAME                                             
*                                                                               
HDHK8    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
TBCLTL   DS    0H                                                               
         CLI   CLTACT,0                                                         
         BE    CLTEX                                                            
         MVI   MODE,PRDLAST        SO PRODUCT WILL PRINT IN HEADS               
         MVI   FORCEHED,C'Y'                                                    
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'01'                                                     
         GOTO1 VBTOTS,DMCB,(RC)                                                 
         CLI   MYMKTSW,1           SEE IF DOING ONE MKT                         
         BE    CLTE50              YES - DONE                                   
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'02'             MARKET RECAP                            
         GOTO1 VBTOTS,DMCB,(RC)                                                 
         CLI   MYMKTSW,X'F2'       SEE IF DOING ALL MKTGROUPS                   
         BNE   CLTE50              NO - DONE                                    
         XC    BUFREC,BUFREC                                                    
         MVI   TOTTYP,X'03'             MKT GROUP RECAP                         
         GOTO1 VBTOTS,DMCB,(RC)                                                 
CLTE50   DS    0H                                                               
         MVI   CLTACT,0                                                         
         MVI   MODE,CLTLAST        RESET MODE                                   
CLTEX    XIT1                                                                   
         SPACE 2                                                                
PRINTITA ST    RE,SAVERE                                                        
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT3,C' '          SEE IF DOING PURCHASED ONLY                  
         BE    *+8                  OR PURCHASED ONLY WITH STA DETAIL           
         MVI   RCSUBPRG,4                                                       
         GOTO1 REPORT                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
PATCH    DC    30X'00'                                                          
         LTORG                                                                  
TAXTBL   DS    0H                                                               
         DC    C'0102',PL2'40'     TAX TO ONE DECIMAL                           
         DC    C'0159',PL2'20'                                                  
         DC    C'0163',PL2'20'                                                  
         DC    C'0164',PL2'20'                                                  
         DC    C'0165',PL2'20'                                                  
         DC    C'0166',PL2'20'                                                  
         DC    C'0167',PL2'20'                                                  
         DC    C'0172',PL2'20'                                                  
         DC    C'0173',PL2'20'                                                  
         DC    C'0175',PL2'20'                                                  
         DC    C'0176',PL2'20'                                                  
         DC    C'0202',PL2'20'                                                  
         DC    C'0215',PL2'20'                                                  
         DC    C'0221',PL2'20'                                                  
         DC    C'0223',PL2'20'                                                  
         DC    C'0224',PL2'20'                                                  
         DC    C'0235',PL2'20'                                                  
         DC    C'0236',PL2'20'                                                  
         DC    C'0237',PL2'20'                                                  
         DC    C'0238',PL2'20'                                                  
         DC    C'0244',PL2'20'                                                  
         DC    C'0246',PL2'20'                                                  
         DC    C'0265',PL2'20'                                                  
         DC    C'0272',PL2'20'                                                  
         DC    X'FFFF'                                                          
         EJECT                                                                  
BTOTALS  CSECT                                                                  
         NMOD1 0,BTOTALS,RR=R9                                                  
*                                                                               
         ST    R9,RELOB                                                         
         B     *+8                                                              
RELOB    DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         LA    RA,2048(RC)                                                      
         LA    RA,2048(RA)                                                      
         USING SPWORKD,RC,RA                                                    
         LA    R9,SPACEND                                                       
         USING SPX1WRKD,R9                                                      
         LA    R8,BTOTALS+4095                                                  
         LA    R8,1(R8)                                                         
         USING BTOTALS+4096,R8      ** NOTE USE OF R8 AS SECOND BASE **         
         MVC   BUFTYP,TOTTYP                                                    
         XC    LASTMPRD,LASTMPRD                                                
         XC    LASTSTA,LASTSTA                                                  
         XC    LASTMKT,LASTMKT                                                  
         XC    LASTPRD,LASTPRD                                                  
         XC    LASTMGR,LASTMGR                                                  
         MVI   TOTPSW,0                                                         
         ZAP   PRDSCNT,=P'0'       USED TO COUNT PRDS PER STA                   
         ZAP   STACNT,=P'0'                                                     
         GOTO1 BUFFALO,DMCB,=C'HIGH',BUFFBUFF,BUFREC,0                          
         B     BTOT10                                                           
BTOT5    GOTO1 BUFFALO,DMCB,=C'SEQ',(TOTTYP,BUFFBUFF),BUFREC,0                  
*                                                                               
BTOT10   CLI   DMCB+8,X'80'        END                                          
         BE    BTOTX                                                            
         MVI   TOTSW,0                                                          
**NEW**                                                                         
         OC    BUFMPRD,BUFMPRD                                                  
         BZ    BTOT10C                                                          
         CLC   BUFMPRD,LASTMPRD                                                 
         BE    BTOT12                                                           
         MVI   FORCEHED,C'Y'           NEW  PAGE PER PRODUCT                    
         MVC   PRD,BUFMPRD                                                      
         MVC   PRDNM,BUFPNAME                                                   
         XC    LASTSTA,LASTSTA                                                  
         XC    LASTMKT,LASTMKT                                                  
         XC    LASTPRD,LASTPRD                                                  
         XC    LASTMGR,LASTMGR                                                  
BTOT10C  OC    LASTMGR(17),LASTMGR      SEE IF FIRST TIME                       
         BNZ   BTOT12                                                           
*                                       OR FIRST FOR PRD                        
         MVI   FORCEHED,C'Y'           NEW  PAGE PER PRODUCT                    
         CLI   TOTTYP,X'02'                                                     
         BNE   BTOT11                                                           
         MVC   P(12),=C'MARKET RECAP'                                           
         MVI   SPACING,2                                                        
         BAS   RE,PRINTIT                                                       
         B     BTOT12                                                           
BTOT11   CLI   TOTTYP,X'03'                                                     
         BNE   BTOT12                                                           
         MVI   SPACING,2                                                        
         MVC   P(18),=C'MARKET GROUP RECAP'                                     
         BAS   RE,PRINTIT                                                       
**NEW**                                                                         
*                                                                               
BTOT12   CLC   BUFMGID(5),LASTMGR      CHK FOR CHG OF MKT GROUP                 
         BE    BTOT13                                                           
         XC    LASTMKT,LASTMKT                                                  
         CLI   TOTTYP,X'02'        SEE IF DOING A RECAP                         
         BNL   BTOT12A             NO                                           
         MVI   FORCEHED,C'Y'                                                    
         B     BTOT12D                                                          
BTOT12A  CLI   BUFMGID,X'FF'                                                    
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
BTOT12C  MVC   P+6(20),BUFGNAME                                                 
         MVC   P(5),BUFMGID                                                     
         CLI   BUFMGID,X'FF'                                                    
         BNE   BTOT12D                                                          
         MVC   P+6(20),=CL20'REPORT TOTALS*'                                    
         MVC   P(5),SPACES         CLEAR X'FF'S                                 
BTOT12D  CLI   TOTTYP,X'03'        SEE IF DOING MKTGRP RECAP                    
         BE    BTOT20              DON'T PRINT NOW                              
         LA    R4,P2                                                            
         LA    R5,5                                                             
         LA    R6,P                                                             
BTOT12E  CLI   0(R6),C' '          UNDERLINE MKTGROUP                           
         BNH   BTOT12F                                                          
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         LA    R6,1(R6)                                                         
         BCT   R5,BTOT12E                                                       
BTOT12F  BAS   RE,PRINTIT                                                       
*                                                                               
BTOT13   CLC   BUFMKT,LASTMKT                                                   
         BE    BTOT15                                                           
         XC    LASTSTA,LASTSTA                                                  
         ZAP   STACNT,=P'0'        CLEAR NUMBER OF STATIONS                     
         CLI   TOTTYP,X'02'        SEE IF DOING A RECAP                         
         BL    BTOT14                                                           
         MVC   P+6(6),=C'TOTAL*'                                                
         MVI   SPACING,2                                                        
         CLI   BUFMKT,X'FF'                                                     
         BE    BTOT20                                                           
         MVI   SPACING,1                                                        
         MVC   P(4),BUFMKT                                                      
         MVC   P+5(20),BUFMNAME                                                 
         B     BTOT20              DON'T PRINT ANYTHING NOW                     
BTOT14   CLI   LASTMKT,0           CHK FOR FIRST MKT                            
         BE    BTOT14B                                                          
         CLI   QOPT3,C'S'             SEE IF SHOWING STATIONS                   
         BE    BTOT14A                YES SKIP TO NEW PAGE                      
         CLC   QPRD,=C'POL'        DON'T SKIP IF DOING ONE PRD                  
         BNE   BTOT14B                                                          
BTOT14A  MVI   FORCEHED,C'Y'       SKIP TO NEW PAGE                             
BTOT14B  MVC   P(4),BUFMKT                                                      
         MVC   P+6(20),BUFMNAME                                                 
         CLI   BUFMKT,X'FF'                                                     
         BNE   BTOT14E                                                          
         MVC   P,SPACES                                                         
         B     BTOT15                                                           
BTOT14E  MVC   P2(4),=4C'-'                                                     
         MVI   ALLOWLIN,6          NEED AT LEAST 5 LINES                        
         BAS   RE,PRINTIT                                                       
BTOT15   CLC   BUFSTA,LASTSTA                                                   
         BE    BTOT18                                                           
         MVI   TOTPSW,0                                                         
         AP    STACNT,=P'1'        BUMB STATION COUNTER                         
         XC    LASTPRD,LASTPRD                                                  
         ZAP   PRDSCNT,=P'0'   RESET PRD COUNTER                                
         MVI   SPACING,2                                                        
         MVC   P(4),BUFSTA         LEAVE OFF MEDIA                              
         CLI   BUFSTA,X'FF'                                                     
         BNE   BTOT15C                                                          
         MVC   P,SPACES                                                         
         B     BTOT18                                                           
BTOT15C  MVI   ALLOWLIN,5          NEED AT LEAST 5 LINES                        
         BAS   RE,PRINTIT                                                       
BTOT18   CLC   BUFPRD,LASTPRD                                                   
         BE    BTOT20                                                           
**NEW**                                                                         
         CLC   QPRD,=C'ALL'                                                     
         BE    BTOT20                                                           
**NEW**                                                                         
         MVI   SPACING,2                                                        
         MVC   P(3),BUFPRD                                                      
         MVC   P+6(20),BUFPNAME                                                 
         AP    PRDSCNT,=P'1'                                                    
         CLI   BUFPRD,X'FF'                                                     
         BE    BTOT18C                                                          
         MVI   ALLOWLIN,4                                                       
         CLI   BUFSTA,X'FF'       SEE IF DOING MKT TOTALS                       
         BNE   BTOT18L             NO                                           
         CLI   QOPT3,C'S'          SEE IF SHOWING STATIONS                      
         BNE   BTOT18L             NO                                           
         CLI   TOTPSW,1            SEE IF I'VE PRINTED TOTAL MSG                
         BE    BTOT18L             YES - DON'T DO IT AGAIN                      
         MVC   P(26),=CL26'MARKET TOTALS'                                       
         CLI   BUFMKT,X'FF'                                                     
         BE    BTOT18B3            GO CHK NUMBER OF PRDS                        
         CP    STACNT,=P'2'    SEE IF THERE WAS MORE THAN ONE STA               
         BNH   BTOT18I             NO - SKIP THESE TOTALS                       
         B     BTOT18BX                                                         
*                                                                               
BTOT18B3 CLI   BUFMGID,X'FF'       SEE IF DOING REPORT TOTALS                   
         BNE   BTOT18B5            YES SKIP                                     
         MVC   P(26),=CL26'REPORT TOTALS'                                       
         B     BTOT18BX                                                         
BTOT18B5 MVC   P(26),=CL26'MARKET GROUP TOTALS'                                 
         TM    MYMKTSW,X'F0'       SEE IF DOING MKTGROUPS                       
         BO    BTOT18BX            YES                                          
         MVC   P(26),=CL26'ALL MARKETS'                                         
         CLI   MYMKTSW,1           SEE IF ONLY REPORTING ONE MKT                
         BNE   BTOT18BX            NO                                           
         MVC   P(26),=CL26'MARKET TOTALS'                                       
         B     BTOT18BX                                                         
*                                                                               
BTOT18BX MVI   ALLOWLIN,5                                                       
         BAS   RE,PRINTIT                                                       
         MVI   TOTPSW,1            SET TOTAL MSG PRINTED                        
         MVI   SPACING,2                                                        
         MVC   P(3),BUFPRD                                                      
         MVC   P+6(20),BUFPNAME                                                 
         B     BTOT18L                                                          
*                                                                               
BTOT18C  MVC   P(26),=CL26'STATION TOTALS'                                      
         CLI   BUFSTA,X'FF'                                                     
         BNE   BTOT18G             GO CHK NUMBER OF PRDS                        
         MVC   P(26),=CL26'MARKET TOTALS'                                       
         CLI   BUFMKT,X'FF'                                                     
         BNE   BTOT18F             GO CHK NUMBER OF PRDS                        
         CLI   BUFMGID,X'FF'       SEE IF DOING REPORT TOTALS                   
         BNE   BTOT18D             YES SKIP                                     
         MVC   P(26),=CL26'REPORT TOTALS'                                       
         B     BTOT18H                                                          
BTOT18D  MVC   P(26),=CL26'MARKET GROUP TOTALS'                                 
         TM    MYMKTSW,X'F0'       SEE IF DOING MKTGROUPS                       
         BO    BTOT18H             YES                                          
         MVC   P(26),=CL26'ALL MARKETS'                                         
         CLI   MYMKTSW,1           SEE IF ONLY REPORTING ONE MKT                
         BNE   BTOT18H             NO                                           
         MVC   P(26),=CL26'MARKET TOTALS'                                       
         B     BTOT18H                                                          
*                                                                               
BTOT18F  DS    0H                  YOU GET HERE FOR MKT TOTALS                  
         CLC   QPRD,=C'POL'        IF DOING ALL PRDS ONLY CHK PRD               
         BE    BTOT18H                                                          
         CLI   QOPT3,C'S'          SEE IF SHOWING STATIONS                      
         BNE   BTOT18L             NO                                           
         CP    STACNT,=P'2'        MUST DO MKT TOTALS IF                        
         BNH   BTOT18I             MULTIPLE STAS  FOUND                         
         B     BTOT18L             MORE THAN ONE STA                            
*                                                                               
BTOT18G  DS    0H                  HERE FOR STA TOTALS                          
         CLC   QPRD,=C'POL'      SEE IF DOING ALL PRDS                          
         BE    BTOT18H                                                          
         MVC   P,SPACES            ONE PRD - DON'T PRINT TOTAL MSG              
         B     BTOT20                                                           
*                                                                               
BTOT18H  DS    0H                  YOU GET HERE FOR OTHER TOTALS                
         CLC   QPRD,=C'POL'        IF DOING ALL PRDS                            
         BNE   BTOT18L             NO  - SHOW TOTALS                            
*                                  FOR ALL PRDS CHG TITLE                       
         MVC   P(26),=CL26'ALL PRODUCTS'                                        
         CP    PRDSCNT,=P'2'       SEE IF I HAD MORE THAN ONE PRD               
         BNH   BTOT18I                                                          
         MVI   ALLOWLIN,4                                                       
         B     BTOT18L                                                          
*                                  REGULAR + X'FF'                              
BTOT18I  MVC   P,SPACES                                                         
         XC    LASTPRD,LASTPRD     SO THE NEXT MTH WILL ALSO BE                 
         ZAP   PRDSCNT,=P'0'       SKIPPED                                      
         B     BTOT56              NO SKIP THIS BUFFER                          
*                                                                               
BTOT18L  BAS   RE,PRINTIT                                                       
BTOT20   CLI   BUFNBMTH,1          CHK FOR NON-BILLABLE MTH                     
         BNE   BTOT30                                                           
         LA    R4,P3                                                            
         CP    BUFOTAX,=P'0'       SEE IF I HAVE TAX                            
         BNE   BTOT21                                                           
         CP    BUFPTAX,=P'0'       SEE IF I HAVE TAX                            
         BNE   BTOT21                                                           
         B     BTOT21B                                                          
BTOT21   MVC   132+17(5,R4),=C'(TAX)'                                           
*                                                                               
BTOT21B  MVC   8(14,R4),=C'(NON-BILLABLE)'                                      
         CLI   QOPT3,C' '          SEE IF DOING PURCHASED ONLY                  
         BE    BTOT22              OR PURCHASED ONLY WITH STA DETAIL            
         EDIT  BUFOGRS,(10,25(R4)),2,FLOAT=-,BRACKET=YES                        
         EDIT  BUFONET,(10,49(R4)),2,FLOAT=-,BRACKET=YES                        
         CP    BUFOTAX,=P'0'                                                    
         BE    BTOT21C                                                          
*  132+49=181                                                                   
         EDIT  BUFOTAX,(10,181(R4)),2,FLOAT=-,BRACKET=YES                       
BTOT21C  ZAP   OCOMM,BUFOGRS                                                    
         SP    OCOMM,BUFONET                                                    
         EDIT  OCOMM,(10,37(R4)),2,FLOAT=-,BRACKET=YES                          
         B     BTOT55                                                           
*                                                                               
BTOT22   EDIT  BUFPGR,(10,25(R4)),2,FLOAT=-,BRACKET=YES                         
         EDIT  BUFPNET,(10,49(R4)),2,FLOAT=-,BRACKET=YES                        
         CP    BUFPTAX,=P'0'                                                    
         BE    BTOT23                                                           
*  132+49=181                                                                   
         EDIT  BUFPTAX,(10,181(R4)),2,FLOAT=-,BRACKET=YES                       
BTOT23   EDIT  BUFOGRS,(10,61(R4)),2,FLOAT=-,BRACKET=YES                        
         EDIT  BUFONET,(10,85(R4)),2,FLOAT=-,BRACKET=YES                        
         CP    BUFOTAX,=P'0'                                                    
         BE    BTOT24                                                           
*  132+85=217                                                                   
         EDIT  BUFOTAX,(10,217(R4)),2,FLOAT=-,BRACKET=YES                       
BTOT24   ZAP   PCOMM,BUFPGR                                                     
         SP    PCOMM,BUFPNET                                                    
         EDIT  PCOMM,(10,37(R4)),2,FLOAT=-,BRACKET=YES                          
         ZAP   OCOMM,BUFOGRS                                                    
         SP    OCOMM,BUFONET                                                    
         EDIT  OCOMM,(10,73(R4)),2,FLOAT=-,BRACKET=YES                          
         SP    BUFPGR,BUFOGRS               GET DIFFERENCES                     
         SP    BUFPNET,BUFONET                                                  
         SP    BUFPTAX,BUFOTAX                                                  
         SP    PCOMM,OCOMM                                                      
         EDIT  BUFPGR,(10,97(R4)),2,FLOAT=-,BRACKET=YES                         
         EDIT  BUFPNET,(10,121(R4)),2,FLOAT=-,BRACKET=YES                       
*              SHOW DIFFERECCE IN TAX IF EITHER WAS NOT 0                       
         CP    BUFOTAX,=P'0'                                                    
         BNE   BTOT25                                                           
         CP    BUFPTAX,=P'0'                                                    
         BE    BTOT26                                                           
*  132+121=253                                                                  
BTOT25   EDIT  BUFPTAX,(10,253(R4)),2,FLOAT=-,BRACKET=YES                       
BTOT26   EDIT  PCOMM,(10,109(R4)),2,FLOAT=-,BRACKET=YES                         
         B     BTOT55                                                           
*                                                                               
BTOT30   CLI   BUFTYP,X'02'        SEE IF DOING A RECAP                         
         BL    BTOT33              NO                                           
         MVI   TOTSW,1                                                          
         CLI   BUFMGID,X'FF'        SEE IF DOING A TOTAL LINE                   
         BE    BTOT35                                                           
         CLI   BUFTYP,X'03'        SEE IF DOING MKTGRP RECAP                    
         BE    BTOT32              YES                                          
         CLI   BUFMKT,X'FF'                                                     
         BE    BTOT35                                                           
BTOT32   MVI   TOTSW,0                                                          
         B     BTOT35                                                           
*                                                                               
BTOT33   MVC   P+4(6),=C'TOTAL*'                                                
         MVI   TOTSW,1                                                          
         MVI   SPACING,2                                                        
         CLC   BUFMTH,=2X'FF'          TOTAL LINE                               
         BE    BTOT35                                                           
         MVI   TOTSW,0                                                          
         MVI   SPACING,1                                                        
         GOTO1 =V(BRDMON),DMCB,(X'FF',BUFMTH),WORK,RR=RELOB                     
         GOTO1 DTCNV,DMCB,(2,WORK),(5,P+4)                                      
BTOT35   DS    0H                                                               
         CLI   QOPT3,C' '          SEE IF DOING PURCHASED ONLY                  
         BE    BTOT37              OR PURCHASED ONLY WITH STA DETAIL            
         EDIT  BUFOGRS,(10,P+25),2,FLOAT=-                                      
         EDIT  BUFONET,(10,P+49),2,FLOAT=-                                      
         CP    BUFOTAX,=P'0'                                                    
         BE    BTOT35B                                                          
         MVC   P2+8(3),=C'TAX'                                                  
         EDIT  BUFOTAX,(10,P2+49),2,FLOAT=-                                     
BTOT35B  ZAP   OCOMM,BUFOGRS                                                    
         SP    OCOMM,BUFONET                                                    
         EDIT  OCOMM,(10,P+37),2,FLOAT=-     ORDERED COMM                       
         CLI   TOTSW,0                                                          
         BE    BTOT50                                                           
         MVI   P+35,C'*'                                                        
         MVI   P+47,C'*'                                                        
         MVI   P+59,C'*'                                                        
         OC    P+32(3),=C'.00'                                                  
         OC    P+44(3),=C'.00'                                                  
         OC    P+56(3),=C'.00'                                                  
         CP    BUFOTAX,=P'0'                                                    
         BE    BTOT50                                                           
         MVC   P2+8(4),=C'TAX*'                                                 
         MVI   P2+59,C'*'                                                       
         B     BTOT50                                                           
*                                                                               
BTOT37   EDIT  BUFPGR,(10,P+25),2,FLOAT=-                                       
         EDIT  BUFPNET,(10,P+49),2,FLOAT=-                                      
         CP    BUFPTAX,=P'0'                                                    
         BE    BTOT37B                                                          
         MVC   P2+8(3),=C'TAX'                                                  
         EDIT  BUFPTAX,(10,P2+49),2,FLOAT=-                                     
BTOT37B  EDIT  BUFOGRS,(10,P+61),2,FLOAT=-                                      
         EDIT  BUFONET,(10,P+85),2,FLOAT=-                                      
         CP    BUFOTAX,=P'0'                                                    
         BE    BTOT37C                                                          
         MVC   P2+8(3),=C'TAX'                                                  
         EDIT  BUFOTAX,(10,P2+85),2,FLOAT=-                                     
BTOT37C  ZAP   PCOMM,BUFPGR                                                     
         SP    PCOMM,BUFPNET                                                    
         EDIT  PCOMM,(10,P+37),2,FLOAT=-     PLANNED COMM                       
         ZAP   OCOMM,BUFOGRS                                                    
         SP    OCOMM,BUFONET                                                    
         EDIT  OCOMM,(10,P+73),2,FLOAT=-     ORDERED COMM                       
         SP    BUFPGR,BUFOGRS               GET DIFFERENCES                     
         SP    BUFPNET,BUFONET                                                  
         SP    BUFPTAX,BUFOTAX                                                  
         SP    PCOMM,OCOMM                                                      
         EDIT  BUFPGR,(10,P+97),2,FLOAT=-                                       
         EDIT  BUFPNET,(10,P+121),2,FLOAT=-                                     
*              SHOW DIFFERENCE IN TAX IF EITHER WAS NON ZERO                    
         CP    BUFPTAX,=P'0'                                                    
         BNE   BTOT37E                                                          
         CP    BUFOTAX,=P'0'                                                    
         BE    BTOT37F                                                          
BTOT37E  MVC   P2+8(3),=C'TAX'                                                  
         EDIT  BUFPTAX,(10,P2+121),2,FLOAT=-                                    
BTOT37F  EDIT  PCOMM,(10,P+109),2,FLOAT=-     DIFF COMM                         
         CLI   TOTSW,0                                                          
         BE    BTOT50                                                           
         MVI   P+35,C'*'                                                        
         MVI   P+47,C'*'                                                        
         MVI   P+59,C'*'                                                        
         MVI   P+71,C'*'                                                        
         MVI   P+83,C'*'                                                        
         MVI   P+95,C'*'                                                        
         MVI   P+107,C'*'                                                       
         MVI   P+119,C'*'                                                       
         MVI   P+131,C'*'                                                       
         OC    P+32(3),=C'.00'                                                  
         OC    P+44(3),=C'.00'                                                  
         OC    P+56(3),=C'.00'                                                  
         OC    P+68(3),=C'.00'                                                  
         OC    P+80(3),=C'.00'                                                  
         OC    P+92(3),=C'.00'                                                  
         OC    P+104(3),=C'.00'                                                 
         OC    P+116(3),=C'.00'                                                 
         OC    P+128(3),=C'.00'                                                 
         CP    BUFOTAX,=P'0'       CHK FOR TAX                                  
         BNE   BTOT40                                                           
         CP    BUFPTAX,=P'0'                                                    
         BE    BTOT50                                                           
BTOT40   MVI   P2+59,C'*'                                                       
         MVI   P2+95,C'*'                                                       
         MVI   P2+131,C'*'                                                      
         OC    P2+56(3),=C'.00'                                                 
         OC    P2+92(3),=C'.00'                                                 
         OC    P2+128(3),=C'.00'                                                
         MVC   P2+8(4),=C'TAX*'                                                 
*                                                                               
BTOT50   CLC   P2+8(3),=C'TAX'                                                  
         BE    BTOT51                                                           
         MVC   P2,P3               MOVE NON-BILLABLE LINE UP                    
         MVC   P3,P4                                                            
         MVC   P4,SPACES                                                        
BTOT51   BAS   RE,PRINTIT                                                       
BTOT55   MVC   LASTPRD,BUFPRD                                                   
BTOT56   MVC   LASTMKT,BUFMKT                                                   
         MVC   LASTSTA,BUFSTA                                                   
         MVC   LASTMGR,BUFMGID                                                  
         MVC   LASTMPRD,BUFMPRD                                                 
         B     BTOT5                                                            
*                                                                               
BTOTX    XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
ROUND    NTR1                      ROUTINE TO ROUND BUFFALO ACCUMS              
         LA    R2,6                FOR BCT                                      
         LA    R3,BUFPGR                                                        
RND5     ZAP   DUB,0(8,R3)                                                      
         CP    DUB,=P'0'                                                        
         BL    RND10                                                            
         BE    RND20               0 LEAVE ALONE                                
         AP    DUB,=P'50'                                                       
         B     RND15                                                            
RND10    SP    DUB,=P'50'          NEGATIVE                                     
RND15    DP    DUB,=P'100'                                                      
         ZAP   0(8,R3),DUB(6)                                                   
RND20    LA    R3,8(R3)                                                         
         BCT   R2,RND5                                                          
         XIT1                                                                   
         SPACE 2                                                                
PRINTIT  ST    RE,SAVERE                                                        
         MVI   RCSUBPRG,2                                                       
         CLI   QOPT3,C' '          SEE IF DOING PURCHASED ONLY                  
         BE    *+8                 OR PURCHASED ONLY WITH STA DETAIL            
         MVI   RCSUBPRG,4                                                       
         GOTO1 REPORT                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
BPATCH   DC    30X'00'                                                          
         LTORG                                                                  
         EJECT                                                                  
SPX1WRKD DSECT                                                                  
ELCODE   DS    CL1                                                              
SAVERE   DS    F                                                                
SAVEKEY  DS    CL13                                                             
**NEW**                                                                         
LASTMPRD DS    CL3                                                              
**NEW**                                                                         
LASTMGR  DS    CL5                 MKTGRP ID + NUMBER                           
LASTMKT  DS    CL4                                                              
LASTSTA  DS    CL5                                                              
LASTPRD  DS    CL3                                                              
PRDSCNT  DS    PL2                                                              
STACNT   DS    PL2                                                              
TOTTYP   DS    CL1                                                              
CLTACT   DS    CL1                                                              
MYPRD    DS    CL1                                                              
MYBDSEC  DS    CL1                                                              
TOTSW    DS    CL1                 X'01' IF DOING TOTAL LINE                    
TOTPSW   DS    CL1                 X'01' IF I'VE PRINTED TOTAL MSG              
MYMKTSW  DS    CL1                                                              
MYSTASW  DS    CL1                                                              
X        DS    F                                                                
SORTER   DS    A                                                                
DTCNV    DS    A                                                                
ANXTPRD  DS    A                                                                
VBTOTS   DS    A                                                                
VCLTLAST DS    A                                                                
*                                                                               
*                                                                               
PCOMM    DS    PL8                                                              
OCOMM    DS    PL8                                                              
ESTBFSW  DS    CL1                 1=N+15N 2=N+17.65N 3=N+11.1N                 
*                                  4=N+3N,0=NO OR OTHER FORMULA                 
ESTNBSW  DS    CL1                 X'01'=NON BILLABLE EST                       
**NEW**                                                                         
SVBUFKEY DS    CL28                                                             
**NEW**                                                                         
SVM1GRS  DS    D                   USED TO SAVE MTH GRS + NET                   
SVM1NET  DS    D                   FIRST MTH                                    
         DS    24D                 12 OTHER MTHS                                
*                                  BUFFALO RECORD                               
         DS    0D                                                               
**NEW**                                                                         
BUFREC   DS    0CL148                                                           
BUFKEY   DS    0CL28                                                            
**NEW**                                                                         
BUFTYP   DS    CL1                                                              
**NEW**                                                                         
BUFMPRD  DS    CL3                 USED FOR QPRD='ALL' REQS                     
**NEW**                                                                         
BUFMGID  DS    CL1                 MKT GRP SCHEME                               
BUFMG    DS    CL4                 MKT GRP                                      
BUFMKT   DS    CL4                 MKT - 4X'FF' FOR GRP TOTALS                  
BUFSTA   DS    CL5                 STATION 5X'FF' FOR MKT TOTALS                
BUFPGID  DS    CL1                 PRD GRP ID    T                              
BUFPG    DS    CL2                 PRD GRP                                      
BUFPRD   DS    CL3                 PRODUCT                                      
BUFMTH   DS    CL2                 MONTH YM                                     
BUFNBMTH DS    CL1                 X'01' FOR NON BILLABLE                       
         DS    CL1                 KEY SPARE                                    
BUFGNAME DS    CL24                GROUP NAME                                   
BUFMNAME DS    CL24                MARKET NAME                                  
BUFPNAME DS    CL24                PRODUCT NAME                                 
BUFPGR   DS    PL8                 GOAL GROSS                                   
BUFPNET  DS    PL8                 GOAL NET                                     
BUFPTAX  DS    PL8                 GOAL TAX                                     
BUFOGRS  DS    PL8                 ORDERED GROSS                                
BUFONET  DS    PL8                 ORDERED NET                                  
BUFOTAX  DS    PL8                 ORDERED TAX                                  
*                                                                               
ESTTAB   DS    CL256               EST TABLE X'80'= NON-BILLABLE                
*                                                                               
MPDLIST  DS    CL250                                                            
*                                                                               
*                                  X'01'=N+15N,X'02'=N+17.76N                   
         DS    0F                                                               
         SPACE 2                                                                
         BUFF  LINES=1000,ROWS=1,COLUMNS=6,FLAVOR=PACKED,              X        
               KEYLIST=(28,A),COMMENT=72                                        
         EJECT                                                                  
*BUYREC                                                                         
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*GOALREC                                                                        
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
*CLTHDR                                                                         
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
*PRDHDR                                                                         
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
*ESTHDR                                                                         
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE DDBUFFALOD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'223SPREPX102 05/01/02'                                      
         END                                                                    
