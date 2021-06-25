*          DATA SET ACREP1402  AT LEVEL 046 AS OF 05/01/02                      
*PHASE AC1402A,+0                                                               
*INCLUDE EDITOR                                                                 
         TITLE 'AC1402 - MODULE FOR ESTIMATE PRINTING'                          
AC1402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC1402                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING AC1402+4096,R9                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC1402D,RC                                                       
         SPACE 1                                                                
*              PROFILES                                                         
*        1     IGNORE JOBS WITH NO ESTIMATE                   Y,N               
*        2     MATCH JOB/MEDIA COMMENT                        Y,N               
*        3     SUPPRESS COMMISSION RATE                       Y,N               
*        4     PRINT REQUEST DETAILS                          Y,N               
*        5     SHOW PRESENT ESTIMATE AS ZERO                  Y,N               
*        6     SHOW PREVIOUS/CURRENT/DIFFERENCE               Y,N               
*        7     CHANGE REVISED EST TO UNAPP                    Y,N               
*        8     SUPPRESS PRESENT IF = ORIGINAL                 Y,N               
*        9     SHOW ORIGINAL AND CURRENT                      Y,N               
*       10     ROUND ESTIMATE TO NEAREST DOLLAR               Y,N,P             
*              P=ROUND, BUT SHOW PENNIES AS .00                                 
*       11     SINGLE SPACE REPORT                            Y,N               
         EJECT                                                                  
*              ROUTINE TO LOOK FOR ESTIMATES AND POST                           
         SPACE 3                                                                
         CLI   MODE,REQFRST                                                     
         BNE   EM1                                                              
         RELOC RELO                                                             
*                                                                               
         MVC   MYSTART,=3X'00'                                                  
         MVC   MYEND,=3X'FF'                                                    
         CLC   QSTART,SPACES                                                    
         BE    C4                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(1,MYSTART)                               
C4       CLC   QEND,SPACES                                                      
         BE    C5                                                               
         GOTO1 DATCON,DMCB,(0,QEND),(1,MYEND)                                   
C5       EQU   *                                                                
         CLI   QPROG+1,C'5'        REQUEST AC15                                 
         BE    YEREQSUM            YES                                          
*   FOR AC14 TURNAROUNDS PRINT REQ SUM FOR FIRST ONE ONLY.                      
         CLC   QUESTOR,=C'*TURNAROUND*'                                         
         BNE   YEREQSUM            NO, CHECK FOR REQ SUM DESIRE.                
TSW      NOP   NOREQSUM            NOP  UNTIL FIRST TURNAROUND.                 
         OI    TSW+1,X'F0'         TURN ON SWT FOR REST OF RUN.                 
YEREQSUM CLI   PROGPROF+3,C'Y'                                                  
         BNE   NOREQSUM                                                         
         MVI   RCREQREP,C'Y'                                                    
         MVI   SKIPSPEC,C'R'                                                    
         GOTO1 ACREPORT                                                         
NOREQSUM MVI   RCREQREP,C'N'                                                    
         MVI   MAXLINES,54                                                      
         MVI   RCSUBPRG,0          DROP RIGHTMOST COLUMN HEADINGS               
         CLI   QOPT1,C'S'                                                       
         BE    XIT                                                              
         MVI   RCSUBPRG,1                                                       
         CLI   QOPT1,C'D'                                                       
         BE    XIT                                                              
         MVI   RCSUBPRG,2                                                       
         B     XIT                                                              
         EJECT                                                                  
EM1      CLI   MODE,PROCACC                                                     
         BNE   EM10                                                             
         SPACE 1                                                                
         MVI   FCRDTRNS,C'N'                                                    
         SPACE 1                                                                
         MVI   ELCODE,ACJBELQ      GET JOB ELEMENT                              
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACJOBD,R2                                                        
         TM    ACJBSTAT,ACJBNEWQ   TEST IF JOB ON NEW ESTIMATES                 
         BO    XIT                 YES-EXIT RIGHT NOW                           
         DROP  R2                                                               
         SPACE 1                                                                
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLI   QOPT2,C' '                                                       
         BE    EM1A                                                             
         CLI   QOPT2,C'S'          CLOSED JOBS                                  
         BE    *+16                                                             
         TM    ACSTSTAT,X'40'      CLOSED ONLY                                  
         BZ    XIT                                                              
         B     EM1A                                                             
         TM    ACSTSTAT,X'40'      SUPPRESS CLOSED                              
         BO    XIT                                                              
         SPACE 1                                                                
EM1A     DS    0H                                                               
         BAS   RE,OCDATCK          CHECK DATE FOR SELECTION.                    
         BNE   XIT                 TAKE NOT IF OUT OF RANGE WHEN                
*                                  DATES ARE PRESENT.                           
         BAS   RE,BLDSOFT          BUILD USER HEADLINE FIELDS                   
         CLI   QOPT3,C' '                                                       
         BE    EM1C                                                             
         CLI   QOPT3,C'S'          LOCKED JOBS                                  
         BE    *+16                                                             
         TM    ACSTSTAT,X'20'      LOCKED ONLY                                  
         BZ    XIT                                                              
         B     EM1C                                                             
         TM    ACSTSTAT,X'20'      SUPPRESS LOCKED                              
         BO    XIT                                                              
EM1C     DS    0H                                                               
         MVI   FCRDTRNS,C'Y'                                                    
         MVI   ANYEST,C'N'                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   JOBCDSW,C'N'                                                     
         MVC   PAGE,=H'1'                                                       
         BAS   RE,FILLUP                                                        
         GOTO1 PROLLER,DMCB,0,ACCUMS,505,5                                      
         MVC   WKCDLST,SPACES                                                   
         ZAP   DOUBLE,=P'0'                                                     
         ZAP   MYDUB,=P'0'                                                      
         MVI   ORGEQCUR,C'Y'       ASSUME ORIGINAL = CURRENT                    
         MVI   PRV,C'N'            ASSUME NO PREVIOUS                           
         CLI   PROGPROF+8,C'P'                                                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,5          ORIGINAL/PREVIOUS/CURRENT                    
         L     R4,ADACC                                                         
         AH    R4,DATADISP                                                      
         SPACE 1                                                                
EM1D     CLI   0(R4),0                                                          
         BE    EM1X                                                             
         CLI   0(R4),X'35'                                                      
         BNE   EM1N                                                             
         USING ACESTD,R4                                                        
         CLI   ACESTLEN,22                                                      
         BL    EM1E                                                             
         CP    ACESTPRV,=P'0'                                                   
         BE    EM1E                IF ANY PREVIOUS NOT ZERO                     
         MVI   PRV,C'Y'            SHOW PREVIOUS / CURRENT                      
EM1E     CP    ACESTORG,ACESTCUR                                                
         BE    EM1N                                                             
         MVI   ORGEQCUR,C'N'       ORIGINAL NOT EQUAL CURRENT                   
EM1N     ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     EM1D                                                             
         SPACE 1                                                                
EM1X     CLI   RCSUBPRG,5          ORIGINAL/PREVIOUS/CURRENT                    
         BE    EM1Z3                                                            
EM1X2    CLI   PROGPROF+8,C'Y'                                                  
         BE    EM1Z                ALWAYS PRINT ORIGINAL AND CURRENT            
         CLI   PROGPROF+5,C'Y'                                                  
         BE    *+8                 USING REVISION METHOD                        
EM1Z     MVI   PRV,C'N'            DON'T USE REVISION METHOD                    
         CLI   PROGPROF+7,C'Y'     SUPPRESS PRESENT IF ORIGINAL = CURR          
         BE    *+8                                                              
         MVI   ORGEQCUR,C'N'       ASSUME NOT EQUAL                             
EM1Z3    L     R4,ADACC                                                         
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
EM2      CLI   0(R4),0                                                          
         BE    EM5                                                              
         CLI   0(R4),X'32'                                                      
         BE    EM3                                                              
         CLI   0(R4),X'35'                                                      
         BNE   EM4                                                              
         ZAP   PRVBUD,=P'0'                                                     
         USING ACESTD,R4                                                        
         MVC   DUB,ACESTWRK                                                     
         CLI   ACESTLEN,22                                                      
         BL    EM2A                                                             
         ZAP   PRVBUD,ACESTPRV                                                  
         CLI   RCSUBPRG,5          ORIGIANL/PREVIOUS/CURRENT                    
         BE    EM2A                                                             
         CLI   PRV,C'N'                                                         
         BE    EM2A                NOT A REVISION                               
         ZAP   ACESTORG,ACESTPRV                                                
EM2A     ZAP   ORGBUD,ACESTORG                                                  
         ZAP   CURBUD,ACESTCUR                                                  
         SPACE 1                                                                
         MVI   ANYEST,C'Y'                                                      
         AP    MYDUB,CURBUD        TOTAL PRESENT ESTIMATE                       
         BAS   RE,GETWCNO                                                       
         LA    R3,1                                                             
         ZAP   DUB,ORGBUD                                                       
         BAS   RE,ADDEM                                                         
         CLI   RCSUBPRG,5                                                       
         BE    EM2C                                                             
         ZAP   DUB,CURBUD                                                       
         LA    R3,2                                                             
         BAS   RE,ADDEM                                                         
         ZAP   DUB,CURBUD          RESET DUB                                    
         LA    R3,3                                                             
         BAS   RE,ADDEM                                                         
         B     EM4                                                              
         SPACE 1                                                                
EM2C     ZAP   DUB,PRVBUD          ORIGIANL/PREVIOUS/CURRENT                    
         LA    R3,2                                                             
         BAS   RE,ADDEM                                                         
         ZAP   DUB,CURBUD                                                       
         LA    R3,3                                                             
         BAS   RE,ADDEM                                                         
         B     EM4                                                              
         SPACE 2                                                                
         USING ACBALD,R4                                                        
EM3      ZAP   DOUBLE,ACBLDR       TOTAL CHARGES                                
         SPACE 2                                                                
EM4      SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     EM2                                                              
         SPACE 2                                                                
EM5      DS    0H                                                               
         CLI   RCSUBPRG,5                                                       
         BE    XIT                                                              
         SPACE 1                                                                
         CLI   PRV,C'N'                                                         
         BE    EM5A                                                             
         MVI   RCSUBPRG,3                                                       
         MVI   QOPT1,C'D'                                                       
         B     XIT                                                              
EM5A     CLI   ORGEQCUR,C'Y'       IF ORIGINAL EQUAL PRESENT                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,4          SUPPRESS PRESENT                             
         CLI   QOPT5,C'Y'          WANT ONLY JOBS OVER ESTIMATE                 
         BNE   XIT                                                              
         CP    DOUBLE,MYDUB                                                     
         BH    XIT                                                              
         MVI   FCRDTRNS,C'N'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO POST TRANSACTIONS                                     
         SPACE 3                                                                
EM10     CLI   MODE,PROCTRNS                                                    
         BNE   EM20                                                             
         CLI   RCSUBPRG,5                                                       
         BE    XIT                                                              
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
         CLC   TRNSANAL,=C'99'     IGNORE BILLS                                 
         BE    XIT                                                              
         ZAP   DUB,TRNSAMNT                                                     
         ZAP   CDAMNT,=P'0'                                                     
         MVI   COMLEVEL,C'D'                                                    
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         MVC   GOSELWC,TRNSANAL                                                 
         SPACE 1                                                                
         MVI   CDSW,C'N'                                                        
         LR    R5,R4                                                            
         SR    R3,R3                                                            
EM10A    CLI   0(R5),0                                                          
         BE    EM10C                                                            
         CLI   0(R5),X'50'         DEAL WITH CASH DISCOUNT                      
         BE    EM10B                                                            
         IC    R3,1(R5)                                                         
         AR    R5,R3                                                            
         B     EM10A                                                            
         SPACE 1                                                                
         USING TRCASHD,R5                                                       
EM10B    DS    0H                                                               
         CLI   TRCSTYPE,C'D'                                                    
         BNE   EM10C                                                            
         MVI   CDSW,C'Y'                                                        
         MVI   JOBCDSW,C'Y'                                                     
         AP    DUB,TRCSAMNT                                                     
         ZAP   CDAMNT,TRCSAMNT                                                  
         CLI   PROGPROF+9,C'Y'                                                  
         BE    EMC10B01                                                         
         CLI   PROGPROF+9,C'P'                                                  
         BE    EMC10B01                                                         
         B     EMC10B02                                                         
EMC10B01 SRP   CDAMNT,64-2,5        ROUND OUT PENNIES                           
EMC10B02 GOTO1 PROLLER,DMCB,3,ACCUMS,CDAMNT,502,3                               
         MP    CDAMNT,=P'-1'                                                    
         MVI   DMCB+19,4                                                        
         BASR  RE,RF                                                            
         ZAP   CDAMNT,TRCSAMNT                                                  
EM10C    DS    0H                                                               
         TM    TRNSSTAT,X'80'                                                   
         BO    EM12                                                             
         MP    DUB,=P'-1'                                                       
         SPACE 2                                                                
EM12     MVC   DUB(2),TRNSANAL                                                  
         BAS   RE,GETWCNO                                                       
         LA    R3,4                                                             
         XC    DUB(2),DUB                                                       
         ZAP   WORK(6),DUB                                                      
         BAS   RE,ADDEM                                                         
         ZAP   DUB,=P'0'                                                        
         SP    DUB,WORK(6)                                                      
         LA    R3,3                                                             
         BAS   RE,ADDEM                                                         
         TM    TRNSSTAT,X'01'                                                   
         BO    XIT                                                              
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         MVC   GOAKEY,ACMALTN                                                   
         BAS   RE,ADDCOMM                                                       
         CLI   CDSW,C'Y'                                                        
         BNE   XIT                                                              
         BAS   RE,ADDWKLST         ADD TO LIST OF CD WORK CODES                 
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              CONTROL PRINTING OF ESTIMATE                                     
         SPACE 3                                                                
EM20     CLI   MODE,ACCLAST                                                     
         BNE   XIT                                                              
         CLI   ANYEST,C'N'                                                      
         BNE   EM21                                                             
         CLI   PROGPROF,C'Y'       OPTION TO SKIP ZERO ESTIMATES                
         BE    XIT                                                              
         BAS   RE,PRINTEM                                                       
         B     EM30                                                             
         SPACE 1                                                                
EM21     DS    0H                                                               
         MVI   MYSPACE,2                                                        
         CLI   PROGPROF+10,C'Y'      SINGLE SPACING OPTION                      
         BNE   EM21A                                                            
         MVI   MYSPACE,1                                                        
EM21A    L     R4,ADLEDGER                                                      
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
EM22     CLI   0(R4),0                                                          
         BE    EM27                                                             
         CLI   0(R4),X'12'                                                      
         BNE   EM26                                                             
         USING ACANALD,R4                                                       
         MVC   DUB,ACANCODE                                                     
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         MVC   GOSELWC,ACANCODE                                                 
         MVI   COMLEVEL,C'S'                                                    
         BAS   RE,GETWCNO                                                       
         CH    R2,=H'500'          AM I UP TO THE OTHERS BUCKET                 
         BE    EM27                YES, NO MORE W/C DETAIL                      
*                                                                               
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         MVC   GOAKEY,ADACC                                                     
         BAS   RE,ADDCOMM                                                       
         BAS   RE,FORMAT                                                        
         CLC   P+30(80),SPACES                                                  
         BE    EM26                                                             
         SPACE 2                                                                
EM24     MVC   P+1(2),ACANCODE                                                  
         MVC   P+4(15),ACANDESC                                                 
         CLI   PROGPROF+2,C'Y'     DON'T PRINT COMMISSION RATE                  
         BE    EM25                                                             
         MVC   P+28(4),=C'ZERO'                                                 
         LTR   R6,R6                                                            
         BZ    EM25                                                             
         EDIT  (R6),(7,P+26),4,DROP=1                                           
EM24B    MVI   CDSW,C'N'                                                        
         BAS   RE,LKWKLST                                                       
         CLI   CDSW,C'N'                                                        
         BE    EM25                                                             
         CLI   QOPT1,C' '                                                       
         BNE   EM25                                                             
         MVC   P+33(4),=C'*CD*'                                                 
         SPACE 2                                                                
EM25     DS    0H                                                               
         BAS   RE,PRINTEM                                                       
         SPACE 2                                                                
EM26     SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     EM22                                                             
         SPACE 2                                                                
EM27     LA    R2,500              LOOK FOR OTHERS                              
         MVC   P,SPACES                                                         
         BAS   RE,FORMAT                                                        
         CLC   P+30(80),SPACES                                                  
         BE    EM28                                                             
         MVC   P+4(6),=CL15'OTHERS'                                             
         BAS   RE,PRINTEM                                                       
         SPACE 2                                                                
EM28     MVC   P,SPACES                                                         
         BAS   RE,PRINTEM                                                       
         MVI   P+1,C'-'                                                         
         MVC   P+2(103),P+1                                                     
         CLI   QOPT1,C'S'                                                       
         BNE   *+10                                                             
         MVC   P+70(40),SPACES     DONT UNDERLINE ALL THE WAY ACROSS            
         BAS   RE,PRINTEM                                                       
         LA    R2,501                                                           
         BAS   RE,FORMAT                                                        
         MVC   P+4(12),=C'TOTALS (NET)'                                         
         CLI   JOBCDSW,C'N'                                                     
         BE    EM28A                                                            
         CLI   QOPT1,C' '                                                       
         BNE   EM28A                                                            
         MVC   P+11(8),=C'(NET+CD)'                                             
EM28A    DS    0H                                                               
         BAS   RE,PRINTEM                                                       
         BAS   RE,CRNCHADD         CRUNCH PENNIES AND ADD TO TOTAL              
         CLI   JOBCDSW,C'N'                                                     
         BE    EM29                                                             
         CLI   QOPT1,C' '                                                       
         BNE   EM29                                                             
         LA    R2,502                                                           
         BAS   RE,FORMAT                                                        
         MVC   P+4(13),=C'CASH DISCOUNT'                                        
         BAS   RE,PRINTEM                                                       
         LA    R2,503                                                           
         BAS   RE,FORMAT                                                        
         MVC   P+4(15),=C'TOTALS (NET-CD)'                                      
         BAS   RE,PRINTEM                                                       
EM29     LA    R2,504                                                           
         BAS   RE,FORMAT                                                        
         CLC   P+36(50),SPACES                                                  
         BE    EM29A                                                            
         MVC   P+4(11),=C'COMMISSIONS'                                          
         BAS   RE,PRINTEM                                                       
         LA    R2,505                                                           
         BAS   RE,FORMAT                                                        
         MVC   P+4(14),=C'TOTALS (GROSS)'                                       
         BAS   RE,PRINTEM                                                       
EM29A    MVI   P+1,C'-'                                                         
         MVC   P+2(103),P+1                                                     
         CLI   QOPT1,C'S'                                                       
         BNE   *+10                                                             
         MVC   P+70(40),SPACES                                                  
         BAS   RE,PRINTEM                                                       
         SPACE 1                                                                
EM30     CLI   QOPT4,C'Y'          OPTION TO IGNORE COMMENTS                    
         BE    XIT                                                              
         MVI   COMMBYTE,2          AFTER                                        
         L     R2,ADACC                                                         
         BAS   RE,COMMPRT                                                       
         L     R2,ADHEIRB                                                       
         BAS   RE,COMMPRT                                                       
         L     R2,ADHEIRA                                                       
         BAS   RE,COMMPRT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD INTO ACCUMULATORS                                 
         SPACE 3                                                                
ADDEM    NTR1                                                                   
         CLI   PROGPROF+9,C'Y'                                                  
         BE    ADDEM01                                                          
         CLI   PROGPROF+9,C'P'                                                  
         BE    ADDEM01                                                          
         B     ADDEM02                                                          
ADDEM01  SRP   DUB+2(6),64-2,5        ROUND OUT PENNIES                         
ADDEM02  GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,(R2),(R3)                            
         MVC   DMCB+12(4),=F'501'                                               
         BASR  RE,RF                                                            
         MVC   DMCB+12(4),=F'503'                                               
         BASR  RE,RF                                                            
         MVC   DMCB+12(4),=F'505'                                               
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO WORK OUT COMMISSIONS                                  
         SPACE 3                                                                
ADDCOMM  NTR1                                                                   
         L     R7,ADGOBLOC                                                      
         USING GOBLOCKD,R7                                                      
         GOTO1 GETOPT,DMCB,(R7)                                                 
         XC    GOSELWC,GOSELWC                                                  
         XC    GOAKEY,GOAKEY                                                    
         ZAP   DUB,GOAGYCOM                                                     
         CVB   R6,DUB                                                           
         LTR   R6,R6                                                            
         BZ    AC040                                                            
         CLI   COMLEVEL,C'D'                                                    
         BE    AC060                                                            
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R2)                                       
         L     R2,DMCB                                                          
         GOTO1 (RF),(R1),1,,504                                                 
         L     R3,DMCB                                                          
         LA    R5,2                                                             
         CLI   RCSUBPRG,5                                                       
         BNE   AC020                                                            
         LA    R5,3                                                             
         SPACE 2                                                                
AC020    CVD   R6,DUB                                                           
         ZAP   PL13,DUB                                                         
         MP    PL13,0(6,R2)                                                     
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   DUB,PL13                                                         
         AP    0(6,R3),DUB         ADD TO LINE 504                              
         LA    R2,6(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R5,AC020                                                         
         AP    0(6,R3),DUB         (ADD ESTIMATE INTO COLUMN 3 AS WELL)         
         SPACE 2                                                                
AC040    XIT1  REGS=(R6)                                                        
         SPACE 2                                                                
AC060    L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         ZAP   PL13,TRNSAMNT                                                    
         AP    PL13,CDAMNT                                                      
         TM    TRNSSTAT,X'80'                                                   
         BO    *+10                                                             
         MP    PL13,=P'-1'                                                      
         CVD   R6,DUB                                                           
         MP    PL13,DUB                                                         
         SRP   PL13,64-6,5         RATE IS FOUR DECIMAL PLACES                  
         ZAP   DUB,PL13                                                         
         SPACE 1                                                                
         CLI   PROGPROF+9,C'Y'     ROUNDING REQUIRED ?                          
         BE    ACC0601                                                          
         CLI   PROGPROF+9,C'P'                                                  
         BE    ACC0601                                                          
         B     ACC0602             NO                                           
ACC0601  SRP   DUB+2(6),64-2,5     YES                                          
ACC0602  GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,504,4                                
         CVB   R1,DUB                                                           
         LCR   R1,R1                                                            
         CVD   R1,DUB              SUBTRACT FROM 504,3 (OVER/UNDER)             
         GOTO1 PROLLER,DMCB,3,ACCUMS,DUB+2,504,3                                
         B     AC040                                                            
         SPACE 2                                                                
COMLEVEL DS    CL1                                                              
         DS    CL1                                                              
         EJECT                                                                  
*              ROUTINE TO CRUNCH PENNIES AND ADD COMMISSIONS TO TOTAL           
         SPACE 3                                                                
CRNCHADD NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,504                                        
         L     R3,DMCB                                                          
         GOTO1 (RF),(R1),1,,505                                                 
         L     R2,DMCB                                                          
         LA    R4,5                                                             
         SPACE 2                                                                
CA2      ZAP   DUB,0(6,R3)                                                      
         AP    0(6,R2),DUB                                                      
         LA    R2,6(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R4,CA2                                                           
         SPACE 1                                                                
*                                  ADD 502(-CD) TO 503 & 505                    
         GOTO1 (RF),(R1),4,,502,503                                             
         MVC   DMCB+12(4),=F'505'                                               
         BASR  RE,RF                                                            
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO FIND WORK-CODE NUMBER FOR POSTING                     
         SPACE 3                                                                
GETWCNO  NTR1                                                                   
         LA    R2,1                                                             
         L     R4,ADLEDGER                                                      
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
GW2      CLI   0(R4),0                                                          
         BE    GW6                                                              
         CLI   0(R4),X'12'                                                      
         BNE   GW4                                                              
         USING ACANALD,R4                                                       
         CLC   ACANCODE,DUB                                                     
         BE    GW8                                                              
         LA    R2,1(R2)                                                         
         CH    R2,=H'500'                                                       
         BE    GW8                                                              
         SPACE 2                                                                
GW4      SR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     GW2                                                              
         SPACE 2                                                                
GW6      LA    R2,500                                                           
         SPACE 2                                                                
GW8      XIT1  REGS=(R2)                                                        
         EJECT                                                                  
*              ROUTINE TO FILL DSECT WITH PRINTING VALUES                       
         SPACE 3                                                                
FILLUP   NTR1                                                                   
         MVC   CLIA(50),SPACES     CLEAR FIRST                                  
         MVC   PRDA(50),SPACES                                                  
         MVC   MEDA(50),SPACES                                                  
         MVC   JOBA(50),SPACES                                                  
         MVC   INF1(50),SPACES                                                  
         MVC   INF2(50),SPACES                                                  
         MVC   INF3(50),SPACES                                                  
         MVC   INF4(50),SPACES                                                  
         MVC   BLLA(25),SPACES                                                  
         MVC   CLSA(08),SPACES                                                  
         MVC   ADDA(26),SPACES                                                  
         MVC   ADDB(26),SPACES                                                  
         MVC   ADDC(26),SPACES                                                  
         MVC   ADDD(26),SPACES                                                  
         MVC   REVHEAD,SPACES                                                   
         SPACE 2                                                                
         L     R2,ADHEIRA          HANDLE NAMES AND CODES                       
         LA    R3,3                                                             
         L     R5,ADLVANAM                                                      
         LA    R6,CLIA                                                          
         BAS   RE,GETNAME                                                       
         L     R2,ADHEIRB                                                       
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         SR    R7,R7                                                            
         IC    R7,ACHRLEVA                                                      
         AR    R3,R7                                                            
         L     R5,ADLVBNAM                                                      
         LA    R6,PRDA                                                          
         BAS   RE,GETNAME                                                       
         L     R2,ADHEIRC                                                       
         IC    R7,ACHRLEVB                                                      
         LA    R3,3(R7)                                                         
         L     R5,ADACCNAM                                                      
         LA    R6,JOBA                                                          
         BAS   RE,GETNAME                                                       
         MVC   FCRULMED,JOBA                                                    
         B     FILL2                                                            
         SPACE 2                                                                
GETNAME  NTR1                                                                   
         MVC   DUB,15(R2)                                                       
         MVC   15(8,R2),SPACES                                                  
         AR    R3,R2                                                            
         MVC   0(6,R6),0(R3)                                                    
         MVC   15(8,R2),DUB                                                     
         LR    R4,R5                                                            
         USING ACNAMED,R4                                                       
         SR    R5,R5                                                            
         IC    R5,ACNMLEN                                                       
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     XIT                                                              
         MVC   7(0,R6),ACNMNAME                                                 
         EJECT                                                                  
*              ROUTINE TO HANDLE MEDIA AND CLOSING DATE                         
         SPACE 3                                                                
FILL2    MVC   MEDA(1),FCRULMED                                                 
         L     R4,ADCOMP                                                        
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
FILL4    CLI   0(R4),0                                                          
         BE    FILL8                                                            
         CLI   0(R4),X'11'                                                      
         BNE   FILL6                                                            
         USING ACMEDIAD,R4                                                      
         CLC   ACMDCODE,FCRULMED                                                
         BNE   FILL6                                                            
         MVC   MEDA+7(12),ACMDDESC+3                                            
         B     FILL10                                                           
         SPACE 2                                                                
FILL6    SR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     FILL4                                                            
         SPACE 2                                                                
FILL8    MVC   MEDA+7(15),=CL15'OTHERS'                                         
         SPACE 2                                                                
FILL10   L     R4,ADACC                                                         
         AH    R4,DATADISP                                                      
         SPACE 2                                                                
FILL12   CLI   0(R4),0                                                          
         BE    FILL16                                                           
         CLI   0(R4),X'26'                                                      
         BNE   FILL14                                                           
         USING ACJOBD,R4                                                        
         GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,CLSA)                                
         CLI   ACJBLEN,16                                                       
         BL    FILL16                                                           
         OC    ACJBREV,ACJBREV                                                  
         BZ    FILL16                                                           
         MVC   REVHEAD(8),=C'REVISION'                                          
         LA    R3,REVHEAD+9                                                     
         EDIT  (B2,ACJBREV),(4,0(R3)),ALIGN=LEFT                                
         OC    ACJBREVD,ACJBREVD                                                
         BZ    FILL16                                                           
         MVC   REVHEAD+15(5),=C'DATED'                                          
         GOTO1 DATCON,DMCB,(1,ACJBREVD),(8,REVHEAD+21)                          
         B     FILL16                                                           
         SPACE 2                                                                
FILL14   SR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     FILL12                                                           
         EJECT                                                                  
*              ROUTINE TO HANDLE PROFILES ETC                                   
         SPACE 3                                                                
FILL16   L     R4,ADPROFIL                                                      
         LTR   R4,R4                                                            
         BZ    FILL18                                                           
         BAS   RE,INFO                                                          
        SPACE 2                                                                 
FILL18   L     R4,ADPROFIL         COMPOSITE PROFILE FOR ALL OTHER INFO         
         BAS   RE,BILLING                                                       
         SPACE 2                                                                
FILL22   L     R4,ADLVCADD                                                      
         LTR   R4,R4                                                            
         BNZ   FILL24                                                           
         L     R4,ADLVBADD                                                      
         LTR   R4,R4                                                            
         BNZ   FILL24                                                           
         L     R4,ADLVAADD                                                      
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         SPACE 2                                                                
FILL24   BAS   RE,ADDRESS                                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE BILLING TYPES                                  
         SPACE 3                                                                
BILLING  NTR1                                                                   
         USING ACPROFD,R4                                                       
         MVC   BLLA(5),=C'TOTAL'                                                
         CLI   ACPRBILL,C'T'                                                    
         BE    XIT                                                              
         MVC   BLLA(8),=C'ONE-LINE'                                             
         CLI   ACPRBILL,C'1'                                                    
         BE    XIT                                                              
         MVC   BLLA(10),SPACES     DON'T PRINT 'UNBILLABLE'                     
         CLI   ACPRBILL,C'U'                                                    
         BE    XIT                                                              
         MVC   BLLA(6),=C'CLIENT'                                               
         CLI   ACPRBILL,C'C'                                                    
         BE    XIT                                                              
         MVC   BLLA(11),=C'PROGRESSIVE'                                         
         CLI   ACPRBILL,C'P'                                                    
         BE    XIT                                                              
         EDIT  (4,ACPRBLAM),(11,BLLA),2,ALIGN=LEFT                              
         LA    R3,BLLA+11                                                       
         CLC   BLLA(11),SPACES                                                  
         BE    XIT                                                              
         SPACE 2                                                                
BILLING2 CLI   0(R3),C' '                                                       
         BNE   *+8                                                              
         BCT   R3,BILLING2                                                      
         LA    R3,2(R3)                                                         
         MVC   0(3,R3),=C'FEE'                                                  
         CLI   ACPRBILL,C'F'                                                    
         BE    XIT                                                              
         MVC   0(7,R3),=C'SPECIAL'                                              
         CLI   ACPRBILL,C'S'                                                    
         BE    XIT                                                              
         SH    R3,=H'4'                                                         
         MVC   0(13,R3),=C' PCT ESTIMATE'                                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DIG OUT INFORMATION                                   
         SPACE 3                                                                
INFO     NTR1                                                                   
         USING ACPROFD,R4                                                       
         MVC   INF1(50),ACPRBLPR                                                
         CLI   ACPRLEN,105                                                      
         BE    XIT                                                              
         MVC   INF2(50),ACPRNARR                                                
         CLI   ACPRLEN,155                                                      
         BE    XIT                                                              
         MVC   INF3(50),ACPRNARR+50                                             
         CLI   ACPRLEN,205                                                      
         BE    XIT                                                              
         MVC   INF4(50),ACPRNARR+100                                            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO HANDLE ADDRESSES                                      
         SPACE 3                                                                
ADDRESS  NTR1                                                                   
         USING ACADDD,R4                                                        
         SR    R5,R5                                                            
         IC    R5,ACADLEN                                                       
         SH    R5,=H'4'                                                         
         EX    R5,*+8                                                           
         B     XIT                                                              
         SPACE 2                                                                
         MVC   ADDA(0),ACADADD                                                  
         EJECT                                                                  
*              ROUTINE TO FORMAT A LINE OF PRINT                                
         SPACE 3                                                                
FORMAT   NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,ACCUMS,(R2)                                       
         L     R2,DMCB                                                          
         CLI   RCSUBPRG,5                                                       
         BNE   FM05                                                             
         ZAP   CHANGE,12(6,R2)     CURRENT                                      
         SP    CHANGE,6(6,R2)      LESS PREVIOUS                                
         LA    R3,P+36             ORIGINAL                                     
         BAS   R4,FM2                                                           
         LA    R3,P+52             PREVIOUS                                     
         BAS   R4,FM2                                                           
         LA    R3,P+68             CURRENT                                      
         BAS   R4,FM2                                                           
         LA    R2,CHANGE                                                        
         LA    R3,P+84             NET CHANGE                                   
         BAS   R4,FM2                                                           
         B     XIT                                                              
         SPACE 1                                                                
FM05     CLI   QOPT1,C'D'                                                       
         BNE   FM06                                                             
         ZAP   DOUBLE,6(6,R2)      PRESENT                                      
         SP    DOUBLE,0(6,R2)      LESS ORIGINAL                                
FM06     DS    0H                                                               
         LA    R3,P+36                                                          
         BAS   R4,FM2                                                           
         CLI   RCSUBPRG,4                                                       
         BE    XIT                 ONLY ORIGINAL                                
         LA    R3,P+52              PRESENT ESTIMATE                            
         CLI   PROGPROF+4,C'Y'     PRINT ZERO PRESENT ESTIMATE                  
         BNE   *+12                                                             
         BAS   R4,FM3                                                           
         B     *+8                                                              
         BAS   R4,FM2                                                           
         LA    R3,P+82              PRES EST LESS ACTUAL                        
         BAS   R4,FM2                                                           
         LA    R3,P+69              ACTUAL                                      
         BAS   R4,FM2                                                           
         SH    R2,=H'24'                                                        
         ZAP   DUB,6(6,R2)                                                      
         ZAP   PKFLD,18(6,R2)                                                   
         MVC   0(30,R2),=5PL6'0'                                                
         CP    PKFLD,=P'0'                                                      
         BE    FM1                                                              
         CP    DUB,=P'0'                                                        
         BE    FM1                                                              
         MP    PKFLD,=P'10000'                                                  
         DP    PKFLD,DUB+2(6)                                                   
         EDIT  (P7,PKFLD),(8,P+97),2                                            
         MVI   P+104,C' '          DROP LAST PLACE                              
         SPACE 1                                                                
FM1      DS    0H                                                               
         CLI   QOPT1,C' '                                                       
         BE    XIT                                                              
         MVC   P+71(39),SPACES                                                  
         CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         LA    R2,DOUBLE+2                                                      
         LA    R3,P+69                                                          
         BAS   R4,FM2                                                           
         B     XIT                                                              
         SPACE 2                                                                
FM2      CP    0(6,R2),=P'0'       DON'T PRINT IF ZERO                          
         BE    FM4                                                              
         B     FM3B                                                             
         SPACE 1                                                                
FM3      LR    RF,R2               SPECIAL TEST FOR PRESENT ESTIMATE            
         SH    RF,=H'6'            RF=A(ORIG. EST.)                             
         CLC   0(24,RF),=4PL6'0'   PRINT IF ANY ESTIMATE OR CHARGE              
         BE    FM4                                                              
*M3B     EDIT  (P6,0(R2)),(15,0(R3)),2,MINUS=YES                                
         SPACE 2                                                                
         USING EDITORD,R5                                                       
FM3B     EQU   *                                                                
         CLI   PROGPROF+9,C'P'       PRINT ROUNDED AMOUNT W/.00'S               
         BNE   FM3B01                                                           
         SRP   0(6,R2),2,0           SHIFT BACK                                 
FM3B01   XC    EDITPARM,EDITPARM    SPACE TO BUILD PARMS                        
         LA    R5,EDITPARM                                                      
         ST    R2,EBAIN             ADDRESS OF PACKED FIELD(INPUT)              
         MVI   EBLIN,6              LENGTH                                      
         MVI   EBTIN,C'P'           'P'ACKED                                    
         SPACE 1                                                                
         MVI   EBOPT,X'40'          MINUS=YES                                   
         ST    R3,EBAOUT            ADDRESS OF OUTPUT FIELD                     
         MVI   EBLOUT,15            LENGTH                                      
         MVI   EBDECS,2             DEFAULT NUMBER OF DECIMALS                  
         CLI   PROGPROF+9,C'Y'      DONT PRINT PENNIES                          
         BNE   FM3B02               NO, SKIP                                    
         MVI   EBDECS,0                                                         
FM3B02   EQU   *                                                                
         GOTO1 =V(EDITOR),DMCB,(R5)                                             
         DROP  R5                                                               
         SPACE 2                                                                
FM4      LA    R2,6(R2)                                                         
         BR    R4                                                               
         EJECT                                                                  
*              ADD TO A LIST OF WORK-CODES FOR WHICH CASH DISCOUNT              
*              HAS BEEN TAKEN                                                   
         SPACE 2                                                                
ADDWKLST NTR1                                                                   
         USING TRANSD,R4                                                        
         LA    R5,WKCDLST                                                       
         LA    R6,50                                                            
ADDW2    CLC   TRNSANAL,0(R5)      GOT IT ALREADY?                              
         BE    XIT                                                              
         CLC   0(2,R5),SPACES                                                   
         BNE   ADDW4                                                            
         MVC   0(2,R5),TRNSANAL                                                 
         B     XIT                                                              
         SPACE 1                                                                
ADDW4    LA    R5,2(R5)                                                         
         BCT   R6,ADDW2                                                         
         B     XIT                                                              
         SPACE 3                                                                
*              FIND IF A WORK-CODE IS IN THE LIST                               
         SPACE 2                                                                
LKWKLST  NTR1                                                                   
         USING ACANALD,R4                                                       
         LA    R5,WKCDLST                                                       
         LA    R6,50                                                            
LK2      CLC   ACANCODE,0(R5)                                                   
         BE    LK4                                                              
         CLC   0(2,R5),SPACES                                                   
         BE    XIT                                                              
         LA    R5,2(R5)                                                         
         BCT   R6,LK2                                                           
         B     XIT                                                              
         SPACE 1                                                                
LK4      MVI   CDSW,C'Y'                                                        
         B     XIT                                                              
         EJECT                                                                  
*              PRINTING OF COMMENTS                                             
         SPACE 2                                                                
COMMPRT  NTR1                                                                   
         MVI   MYSPACE,1                                                        
         MVI   COMMSW,C'N'                                                      
         SR    R3,R3                                                            
         L     RF,ADACC            SAVE JOB KEY                                 
         MVC   SAVEKEY,0(RF)                                                    
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
CM8      IC    R3,ACOMLEN                                                       
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),ACOMMENT                                                  
         BAS   RE,PRINTEM                                                       
         B     CM4                                                              
         SPACE 3                                                                
CM10     TM    ACOMTYPE,X'40'      ESTIMATE                                     
         BZ    CM4                                                              
         CLI   COMMBYTE,1          BEFORE                                       
         BE    CM12                                                             
         TM    ACOMTYPE,X'04'                                                   
         BZ    CM4                                                              
         B     CM14                                                             
CM12     TM    ACOMTYPE,X'08'      BEFORE                                       
         BZ    CM4                                                              
CM14     DS    0H                                                               
         CLI   PROGPROF+1,C'Y'     PROFILE OPTION FOR 1-BYTE                    
         BNE   CM14E               MEDIA/COMMENT MATCH                          
         LA    R1,ACOMMENT                                                      
CM14A    CLI   0(R1),C' '          FIND FIRST SIGNIFICANT                       
         BNE   CM14C               BYTE OF COMMENT CODE                         
         LA    R1,1(R1)                                                         
         B     CM14A                                                            
CM14C    CLC   MEDA(1),0(R1)                                                    
         BNE   CM4                                                              
CM14E    DS    0H                                                               
         XC    COMMKEY,COMMKEY     READ A STANDARD COMMENT RECORD               
         MVI   COMMKEY,X'0C'                                                    
         MVC   COMMKEY+1(1),QCOMPANY                                            
         MVC   COMMKEY+2(6),ACOMMENT                                            
         MVI   COMMSW,C'Y'                                                      
         L     R4,=A(COMMBUFF)                                                  
         A     R4,RELO                                                          
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
         BNH   CM22                                                             
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,PRINTEM                                                       
CM22     SR    R3,R3                                                            
         CLI   0(R4),0                                                          
         BE    CM4                                                              
         CLI   0(R4),X'3E'                                                      
         BE    CM26                                                             
CM24     IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     CM22                                                             
         SPACE 1                                                                
         DROP  R2                                                               
         USING ACOMMD,R4                                                        
CM26     IC    R3,ACOMLEN                                                       
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),ACOMMENT                                                  
         BAS   RE,PRINTEM                                                       
         B     CM24                GET NEXT ELEMENT                             
         SPACE 2                                                                
CMXT     CLI   COMMSW,C'N'         DID WE ACTUALLY READ 'ACCOUNT'               
         BE    CMXT2                                                            
         L     R4,=A(COMMBUFF)                                                  
         A     R4,RELO                                                          
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEKEY,(R4)                     
CMXT2    MVI   MYSPACE,2           RESET SPACEING                               
         CLI   PROGPROF+10,C'Y'      SINGLE SPACING OPTION                      
         BNE   CMXT2A                                                           
         MVI   MYSPACE,1                                                        
CMXT2A   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT REPORT                                          
         SPACE 3                                                                
PRINTEM  NTR1                                                                   
         USING ACPROFSD,RE                                                      
         L     RE,APROFILE                                                      
         CLI   ACPPFC12,C'Y'       PRINT ORIGIN ? (COMPANY LEVEL)               
         BNE   PRT0                NO                                           
*                                                                               
         USING RUNXTRAD,RF                                                      
         L     RF,VEXTRAS                                                       
         MVC   UPPER1+33(33),ORIGINAM                                           
         MVC   UPPER2+33(33),ORIGINAD                                           
*                                                                               
PRT0     MVC   HEAD1+9(43),CLIA                                                 
         MVC   HEAD2+9(43),PRDA                                                 
         MVC   HEAD3+9(43),MEDA                                                 
         MVC   HEAD4+9(43),JOBA                                                 
         MVC   HEAD5+01(50),INF1                                                
         MVC   HEAD6+01(50),INF2                                                
         MVC   HEAD7+01(50),INF3                                                
         MVC   HEAD8+01(50),INF4                                                
         MVC   HEAD3+52(12),=C'BILLING TYPE'                                    
         MVC   HEAD3+66(19),BLLA                                                
         MVC   HEAD4+52(L'REVHEAD),REVHEAD                                      
         LA    R5,HEAD4                                                         
         CLC   52(34,R5),SPACES     IF NO REVISION START ADDRESS                
         BE    *+8                  AT HEAD4                                    
         LA    R5,HEAD5             ELSE START AT HEAD5                         
         CLC   ADDA(26*4),SPACES                                                
         BE    PRT1                 NO ADDRESS                                  
         MVC   52(7,R5),=C'BILLING'                                             
         MVC   52+132(7,R5),=C'ADDRESS'                                         
         MVC   61(26,R5),ADDA                                                   
         LA    R5,132(R5)                                                       
         MVC   61(26,R5),ADDB                                                   
         LA    R5,132(R5)                                                       
         MVC   61(26,R5),ADDC                                                   
         LA    R5,132(R5)                                                       
         MVC   61(26,R5),ADDD                                                   
PRT1     CLI   PROGPROF+2,C'Y'     DON'T PRINT COMMISSION RATE                  
         BE    *+16                                                             
         MVC   HEAD11+24(10),=C'COMMISSION'                                     
         MVC   HEAD12+27(4),=C'RATE'                                            
         CLI   ANYEST,C'Y'                                                      
         MVC   SPACING,MYSPACE                                                  
         BE    *+8                                                              
         MVI   SPACING,1                                                        
         CLC   P+4(11),=C'COMMISSIONS'                                          
         BE    PRT2                                                             
         CLC   P+4(14),=C'TOTALS (GROSS)'                                       
         BE    PRT2                                                             
         CLC   P+4(15),=C'TOTALS (NET-CD)'                                      
         BE    PRT2                                                             
         CLC   P+4(13),=C'CASH DISCOUNT'                                        
         BNE   PRT4                                                             
PRT2     DC    0H'0'                                                            
         B     PRT4                                                             
         MVC   P+87(22),SPACES                                                  
         SPACE 1                                                                
PRT4     MVI   ELCODE,X'27'        LOOK FOR E/A NUMBER                          
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   PRT9                                                             
         LA    R3,6                                                             
         LA    R4,HEAD5                                                         
PRT5     CLC   0(132,R4),SPACES    FIRST BLANK LINE IN HEADS                    
         BE    PRT6                                                             
         LA    R4,132(R4)                                                       
         BCT   R3,PRT5                                                          
         B     PRT9                NO ROOM - THEY CAN TYPE IT IN                
         USING ACABILLD,R2                                                      
PRT6     CLC   ACABEANO(L'ACABEANO),SPACES                                      
         BE    PRT6A                                                            
         MVC   16(3,R4),=C'EA='                                                 
         MVC   20(L'ACABEANO,R4),ACABEANO                                       
PRT6A    MVC   52(L'ACABACNO,R4),ACABACNO                                       
         CLI   ACABLEN,X'48'       SPECIAL BILL NO PRESENT                      
         BL    PRT9                                                             
         MVC   36(L'ACABBINO,R4),ACABBINO                                       
         CLI   ACABLEN,X'57'       BUDGET MEMO FIGURE HERE                      
         BL    PRT9                                                             
         MVC   67(3,R4),=C'BG='                                                 
         MVC   70(L'ACABBMEM,R4),ACABBMEM                                       
*                                                                               
*                  ADD ON THE USER FIELDS                                       
PRT9     LA    R1,SOFTH1                                                        
         LA    R3,SOFTAB                                                        
PRT10    CLC   0(L'SOFTH1,R1),SPACES                                            
         BE    PRT30               NO MORE SOFT HEADLINES                       
         SR    R5,R5                                                            
         ICM   R5,3,0(R3)          DISPLACEMENT TO HEADLINE FIELD               
         AR    R5,RA               R5=TO HEADLINE FIELD                         
         CLC   0(L'SOFTH1,R5),SPACES                                            
         BNE   *+14                                                             
         MVC   0(L'SOFTH1,R5),0(R1) DATA TO HEADLINE                            
         LA    R1,L'SOFTH1(R1)                                                  
         LA    R3,2(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   PRT10                                                            
*                                                                               
PRT30    GOTO1 ACREPORT                                                         
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
OCDATCK  NTR1                      DATE SELECTION IF THERE.                     
         CLC   MYSTART(6),=X'000000FFFFFF'                                      
         BE    YES                 NO DATES, TAKE ALL                           
         CLI   QOPT2,C' '                                                       
         BE    YES                 NO OPT2, TAKE ALL                            
         CLI   QOPT2,C'C'                                                       
         BE    CK26                CHECK FOR CLOSED DATE                        
*                                  ELSE CHECK FOR OPEN DATE                     
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLC   MYSTART,ACSTBFDT                                                 
         BH    NO                                                               
         CLC   MYEND,ACSTBFDT                                                   
         BL    NO                                                               
         B     YES                                                              
CK26     MVI   ELCODE,X'26'                                                     
         L     R2,ADACC                                                         
         BAS   RE,GETEL                                                         
         BNE   NO                  CLOSED, NO 26,FORGET IT.                     
         USING ACJOBD,R2                                                        
         CLC   MYSTART,ACJBCLOS                                                 
         BH    NO                                                               
         CLC   MYEND,ACJBCLOS                                                   
         BL    NO                                                               
         B     YES                                                              
NO       LA    R1,1                                                             
         B     *+6                                                              
YES      SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*              BUILD SOFT HEADLINES                                             
*                                                                               
BLDSOFT  NTR1                                                                   
         LA    R0,7                                                             
         LA    R1,SOFTH1                                                        
         MVC   0(L'SOFTH1,R1),SPACES   CLEAR SOFT HEADS                         
         LA    R1,L'SOFTH1(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         LA    R0,7                                                             
         LA    R1,SOFTH1                                                        
         L     R3,ADACC                                                         
         USING ACKEYD,R3                                                        
         LA    R4,ACRECORD                                                      
         USING ACUFD,R4                                                         
BLDSFT03 CLI   0(R4),0                                                          
         BE    XIT                 END OF RECORD                                
         CLI   0(R4),X'A2'                                                      
         BE    BLDSFT05                                                         
BLDSFT04 ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     BLDSFT03                                                         
BLDSFT05 TM    ACUFSTAT,X'20'      SHOW ON ESTIMATES                            
         BZ    BLDSFT04                                                         
         LA    R2,ACUFDATA-ACUFD    LENGTH UP TO DATA                           
         ZIC   R3,ACUFLEN           ELEMENT LENGTH                              
         SR    R3,R2                R3=LENGTH OF ACUFDATA                       
         BZ    BLDSFT04             NO DATA                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   13(0,R1),ACUFDATA    DATA TO SOFT FIELD                          
         MVC   0(12,R1),ACUFDESC    FIELD DESCRIPTION                           
         LA    R1,L'SOFTH1(R1)      BUMP TO NEXT AREA                           
         BCT   R0,BLDSFT04                                                      
         B     XIT                                                              
         EJECT                                                                  
         GETEL (R2),DATADISP,ELCODE                                             
ELCODE   DS    XL1                                                              
         SPACE 2                                                                
MYSTART  DS    XL3                                                              
MYEND    DS    XL3                                                              
*                                                                               
*              TABLE OF DISPLACEMENTS FOR SOFTHEADS                             
*                                                                               
SOFTAB   DC    AL2(HEAD6-ACWORKD+1)                                             
         DC    AL2(HEAD7-ACWORKD+1)                                             
         DC    AL2(HEAD7-ACWORKD+45)                                            
         DC    AL2(HEAD8-ACWORKD+1)                                             
         DC    AL2(HEAD8-ACWORKD+45)                                            
         DC    AL2(HEAD9-ACWORKD+1)                                             
         DC    AL2(HEAD9-ACWORKD+45)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
ACCUMS   DS    15158C                                                           
         EJECT                                                                  
*              DSECT FOR MODULE                                                 
         SPACE 3                                                                
AC1402D  DSECT                                                                  
RELO     DS    F                                                                
PRV      DS    CL1                                                              
ORGEQCUR DS    CL1                                                              
REVHEAD  DS    CL30                                                             
ANYEST   DS    CL1                                                              
CLIA     DS    CL50                                                             
PRDA     DS    CL50                                                             
MEDA     DS    CL50                                                             
JOBA     DS    CL50                                                             
INF1     DS    CL50                                                             
INF2     DS    CL50                                                             
INF3     DS    CL50                                                             
INF4     DS    CL50                                                             
BLLA     DS    CL25                                                             
CLSA     DS    CL8                                                              
ADDA     DS    CL26                                                             
ADDB     DS    CL26                                                             
ADDC     DS    CL26                                                             
ADDD     DS    CL26                                                             
CDSW     DS    CL1                                                              
JOBCDSW  DS    CL1                                                              
CDAMNT   DS    PL6                                                              
WKCDLST  DS    CL100                                                            
SAVEKEY  DS    CL49                                                             
COMMSW   DS    CL1                                                              
COMMBYTE DS    CL1                                                              
COMMKEY  DS    CL49                                                             
MYSPACE  DS    CL1                                                              
ORGBUD   DS    PL6                                                              
CURBUD   DS    PL6                                                              
PRVBUD   DS    PL6                                                              
CHANGE   DS    PL6                                                              
MYDUB    DS    D                                                                
PL13     DS    PL13                                                             
RULST    DS    CL1                                                              
PKFLD    DS    PL13                                                             
SOFTH1   DS    7CL43               SOFT HEADLINES                               
EDITPARM DS    CL24                SAVE PARMS FOR CALL TO EDITOR                
         EJECT                                                                  
EDITORD  DSECT                                                                  
       ++INCLUDE DDEBLOCK                                                       
GOBLOCKD DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACREPPROFD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
         EJECT                                                                  
COMMBUFF CSECT                                                                  
         DS    CL1000                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046ACREP1402 05/01/02'                                      
         END                                                                    
