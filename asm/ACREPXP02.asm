*          DATA SET ACREPXP02  AT LEVEL 090 AS OF 05/01/02                      
*PHASE ACXP02A,+0                                                               
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
         TITLE 'CREATE POSTING WORKER FILE'                                     
         PRINT NOGEN                                                            
ACXP02   CSECT                                                                  
         NMOD1 0,**ACXP**,R8                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXPD,RC                                                         
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   CA2                                                              
         L     RF,=A(ACCSUM)                                                    
         MVI   0(RF),X'FF'                                                      
         L     RF,=A(CLTSUM)                                                    
         MVI   0(RF),X'FF'                                                      
         ZAP   POSTREC,=P'0'                                                    
         ZAP   POSTCASH,=P'0'                                                   
         ZAP   RUNTOT,=P'0'                                                     
         ZAP   RECSIN,=P'0'                                                     
         ZAP   RECSMV,=P'0'                                                     
         XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'A08'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
*        OI    ID+13,X'01'         ALLOW DUPLICATE FILES                        
         BAS   RE,OPNPOST                                                       
         SPACE 2                                                                
XIT      XMOD1 1                                                                
         EJECT                                                                  
CA2      CLI   MODE,REQFRST                                                     
         BNE   CA10                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   REQTOT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
CA10     CLI   MODE,PROCACC                                                     
         BNE   CA30                                                             
         ZAP   ACCTOT,=P'0'                                                     
         ZAP   ACCDEB,=P'0'                                                     
         ZAP   ACCCRE,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
CA30     CLI   MODE,PROCTRNS                                                    
         BNE   CA40                                                             
         AP    RECSIN,=P'1'                                                     
*                                                                               
CA32     L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         LR    R3,R4                                                            
         SH    R3,DATADISP                                                      
         USING TRNRECD,R3                                                       
*                                                                               
         CLI   0(R4),TRNELQ                                                     
         BNE   XIT                                                              
         TM    TRNSTAT,TRNSDR                                                   
         BNO   CA35                                                             
         LR    R5,R4                                                            
         XC    OFC,OFC                                                          
         XC    CLT,CLT                                                          
         SR    R0,R0                                                            
*                                                                               
CA33     IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    XIT                                                              
         CLI   0(R5),CPJELQ                                                     
         BNE   CA33A                                                            
         USING CPJELD,R5                                                        
         CLI   CPJTYPE,CPJTJOB                                                  
         BNE   CA33A                                                            
         MVC   CLT,CPJCLI                                                       
         B     CA33X                                                            
CA33A    CLI   0(R5),MDTELQ                                                     
         BNE   CA33                                                             
         USING MDTELD,R5                                                        
         MVC   CLT,MDTCLI                                                       
CA33X    L     R6,=A(CLILST)        IRVINE CLIENTS                              
         OC    CLT,SPACES                                                       
CA34     CLC   CLT,0(R6)                                                        
         BNE   *+14                                                             
         MVC   OFC,3(R6)                                                        
         B     CA35                                                             
         LA    R6,L'CLILST(R6)                                                  
         CLI   0(R6),X'FF'                                                      
         BNE   CA34                                                             
         MVC   OFC,TRNOFFC                                                      
         BAS   RE,MISCLT                                                        
*                                                                               
CA35     MVI   ACTM,MVTO                                                        
         CLC   OFC,=C'D4'          TEST IRVINE                                  
         BE    CA35Y                                                            
         CLC   OFC,=C'CA'                                                       
         BE    CA35Y                                                            
         CLC   TRNKACT(3),=C'C03'                                               
         BE    CA36                                                             
         B     XIT                                                              
CA35Y    CLC   TRNKACT(3),=C'C0B'                                               
         BNE   XIT                                                              
*                                                                               
         USING PSHEADD,R2          BUILD POSTING ELEMENT                        
CA36     AP    RECSMV,=P'1'                                                     
         CLI   TRNOFFC,C'3'                                                     
         BNE   *+14                                                             
         MVC   TRNOFFC,=C'C3'                                                   
         B     CA36A                                                            
*                                                                               
         CLI   TRNOFFC,C'B'                                                     
         BNE   *+14                                                             
         MVC   TRNOFFC,=C'D3'                                                   
         B     CA36A                                                            
*                                                                               
         CLI   TRNOFFC,C'E'                                                     
         BNE   *+14                                                             
         MVC   TRNOFFC,=C'CE'                                                   
         B     CA36A                                                            
         DC    H'0'                                                             
CA36A    LA    R2,T                                                             
         XC    T-4(4),T-4                                                       
         MVC   PSHDEL(2),=X'5046'                                               
*                                                                               
         BAS   RE,NEWACC                                                        
*                                                                               
         MVC   PSHDACC,TRNKCULA    ACCOUNT                                      
         MVC   PSHDANAL,TRNKWORK                                                
         MVC   PSHDSBAC,TRNKCULC   CONTRA                                       
*                                                                               
         SPACE 1                                                                
         L     R6,ADSUBAC                                                       
         USING CACELD,R6                                                        
         MVC   PSHDSBNM,SPACES     NAME FROM SUB-ACCOUNT RECORD                 
         SR    R1,R1                                                            
         IC    R1,CACLN                                                         
         SH    R1,=H'18'                                                        
         BM    *+14                                                             
         EX    R1,*+4                                                           
         MVC   PSHDSBNM(0),CACNAME                                              
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         LR    R5,R4                                                            
*                                                                               
CA37     SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R5)      MOVE ELEMENTS TO AREA ONE AT A TIME           
         LA    R2,1(R1,R2)        R2 TO NEXT OUTPUT AREA                        
         MVI   0(R2),0            END OF RECORD                                 
         LA    R5,1(R1,R5)        R5 TO NEXT ELEMENT                            
         CLI   0(R5),0                                                          
         BNE   CA37                                                             
         CLI   ACTM,MVFR                                                        
         BNE   *+8                                                              
         BAS   RE,REV                                                           
         BAS   RE,PUTIT                                                         
*                                                                               
         CLI   ACTM,MVFR                                                        
         BE    XIT                                                              
         L     RF,ADACC                                                         
         MVC   TRNKCULA,0(RF)      RESTORE ACCOUNT                              
         MVI   ACTM,MVFR                                                        
         B     CA36A                                                            
         EJECT                                                                  
NEWACC   NTR1  ,                                                                
         MVC   WORK(1),ACTM                                                     
         MVC   WORK+1(3),TRNKACT                                                
         LA    RF,NEWACT                                                        
         CLC   WORK(4),0(RF)                                                    
         BE    *+18                                                             
         LA    RF,L'NEWACT(RF)                                                  
         CLI   0(RF),X'FF'                                                      
         BNE   *-18                                                             
         DC    H'0'                                                             
         MVC   TRNKACT(3),4(RF)                                                 
*                                                                               
         L     RF,=A(ACCSUM)                                                    
NEWACC1  CLI   0(RF),X'FF'                                                      
         BE    NEWACC5                                                          
         CLC   TRNKULA,0(RF)                                                    
         BNE   NEWACC3                                                          
NEWACC2  LA    R1,14(RF)                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R1,6(R1)                                                         
         AP    0(6,R1),TRNAMNT                                                  
         B     XIT                                                              
NEWACC3  LA    RF,L'ACCSUM(RF)                                                  
         B     NEWACC1                                                          
NEWACC5  MVC   0(14,RF),TRNKULA                                                 
         ZAP   14(6,RF),=P'0'                                                   
         ZAP   20(6,RF),=P'0'                                                   
         MVI   L'ACCSUM(RF),X'FF'                                               
         B     NEWACC2                                                          
*                                                                               
NEWACT   DS    XL7                                                              
         DC    AL1(MVTO),C'C03',C'CC3'                                          
         DC    AL1(MVFR),C'C03',C'CCA'                                          
         DC    AL1(MVTO),C'C0B',C'CD4'                                          
         DC    AL1(MVFR),C'C0B',C'CD3'                                          
*                                                                               
*        DC    AL1(MVTO),C'X03',C'XCA'                                          
*        DC    AL1(MVFR),C'X03',C'XC3'                                          
*        DC    AL1(MVTO),C'X0B',C'XD4'                                          
*        DC    AL1(MVFR),C'X0B',C'XD3'                                          
         DC    X'FF'                                                            
*                                                                               
MISCLT   NTR1  ,                                                                
         L     RF,=A(CLTSUM)                                                    
MISCLT1  CLI   0(RF),X'FF'                                                      
         BE    MISCLT5                                                          
         CLC   CLT,0(RF)                                                        
         BE    XIT                                                              
         LA    RF,L'CLTSUM(RF)                                                  
         B     MISCLT1                                                          
MISCLT5  MVC   0(3,RF),CLT                                                      
         MVI   L'CLTSUM(RF),X'FF'                                               
         B     XIT                                                              
         EJECT                                                                  
*              ACCLAST                                                          
CA40     CLI   MODE,ACCLAST                                                     
         BNE   CA60                                                             
         B     XIT                                                              
         EJECT                                                                  
CA60     CLI   MODE,REQLAST                                                     
         BNE   CA70                                                             
         B     XIT                                                              
         EJECT                                                                  
CA70     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         L     R2,=A(ACCSUM)                                                    
CA72     CLI   0(R2),X'FF'                                                      
         BE    CA73                                                             
         MVC   P+1(14),0(R2)                                                    
         EDIT  (P6,14(R2)),(13,P+20),2,MINUS=YES                                
         EDIT  (P6,20(R2)),(13,P+40),2,MINUS=YES                                
         GOTO1 ACREPORT                                                         
         LA    R2,L'ACCSUM(R2)                                                  
         B     CA72                                                             
*                                                                               
CA73     L     R2,=A(CLTSUM)                                                    
CA74     CLI   0(R2),X'FF'                                                      
         BE    CA75                                                             
         MVC   P+1(3),0(R2)                                                     
         GOTO1 ACREPORT                                                         
         LA    R2,L'CLTSUM(R2)                                                  
         B     CA74                                                             
*                                                                               
CA75     MVC   P+1(20),=CL20'RECORDS IN'                                        
         EDIT  RECSIN,(6,P+22)                                                  
         GOTO1 ACREPORT                                                         
         MVC   P+1(20),=CL20'RECORDS MOVED'                                     
         EDIT  RECSMV,(6,P+22)                                                  
         GOTO1 ACREPORT                                                         
*                                                                               
         CP    POSTREC,=P'0'                                                    
         BE    XIT                                                              
         XC    T(80),T                                                          
         MVC   T-4(2),=X'0021'                                                  
         MVC   T(2),=X'521D'                                                    
         MVC   T+2(15),=CL15'POSTINGS'                                          
         ZAP   T+17(6),POSTREC                                                  
         ZAP   T+23(6),POSTCASH                                                 
         GOTO1 ACREPORT                                                         
         BAS   RE,ADDPOST                                                       
         BAS   RE,CLOSPOST                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REVERSE TRANSACTION AND SUBSIDIARY ELEMENTS                         *         
***********************************************************************         
                                                                                
         DROP  R4                                                               
REV      NTR1  ,                                                                
         LA    R2,T                                                             
REV3     SR    R3,R3                                                            
         IC    R3,1(R2)            SKIP HEADER                                  
         AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    XIT                                                              
*                                                                               
REV5     LA    RE,REVTAB                                                        
         CLC   0(1,RE),0(R2)       TABLE VS. ELEMENT                            
         BE    REV7                                                             
         LA    RE,L'REVTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   REV5+L'REV5                                                      
         B     REV3                                                             
*                                                                               
REV7     SR    RF,RF                                                            
         ICM   RF,7,1(RE)          SET ADDRESS OF ROUTINE                       
         BASR  R9,RF                                                            
         B     REV3                                                             
*                                                                               
         EJECT                                                                  
         USING TRNELD,R2                                                        
RTRN     ZAP   DUB,TRNAMNT         REVERSE TRANSACTION AMOUNTS                  
         MP    DUB,=P'-1'                                                       
         ZAP   TRNAMNT,DUB                                                      
         CLC   TRNANAL,=C'99'                                                   
         BNE   RTRNX                                                            
         LA    R6,3                                                             
         LA    R5,TRNNARR+15                                                    
RTRN3    ZAP   DUB,0(6,R5)         REVERSE COMM. AMOUNT                         
         MP    DUB,=P'-1'                  BILLED AMOUNT                        
         ZAP   0(6,R5),DUB                 PAYABLE AMOUNT                       
         LA    R5,6(R5)                                                         
         BCT   R6,RTRN3                                                         
RTRNX    BR    R9                                                               
*                                                                               
*                                                                               
         USING XPYELD,R2                                                        
RXPY     ZAP   DUB,XPYCD           EXTRA PAYMENT CASH DISCOUNT                  
         MP    DUB,=P'-1'                                                       
         ZAP   XPYCD,DUB                                                        
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
         USING SCIELD,R2                                                        
RSCI     ZAP   DUB,SCIAMNT         SUBSIDIARY CASH                              
         MP    DUB,=P'-1'                                                       
         ZAP   SCIAMNT,DUB                                                      
         CLI   SCILN,SCILN2Q                                                    
         BL    RSCIX                                                            
         ZAP   DUB,SCIADMN                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   SCIADMN,DUB                                                      
RSCIX    BR    R9                                                               
*                                                                               
*                                                                               
         USING PBIELD,R2                                                        
RPBI     ZAP   DUB,PBIPST          PST BILLED ELEMENT                           
         MP    DUB,=P'-1'                                                       
         ZAP   PBIPST,DUB                                                       
         ZAP   DUB,PBIGROSS                                                     
         MP    DUB,=P'-1'                                                       
         ZAP   PBIGROSS,DUB                                                     
         ZAP   DUB,PBICOMM                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   PBICOMM,DUB                                                      
         BR    R9                                                               
         DROP  R2                                                               
         EJECT                                                                  
         USING PSSUBFD,R2                                                       
RPSSB    ZAP   DUB,PSSBCASH        SUB FILE TRAILER                             
         MP    DUB,=P'-1'                                                       
         ZAP   PSSBCASH,DUB                                                     
         BR    R9                                                               
*                                                                               
*                                                                               
RMDT     LA    RF,MDTTAB                                                        
RMDT3    CLI   0(RF),X'FF'                                                      
         BER   R9                  END-OF-TABLE                                 
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RE,4(R1)                                                         
         CLM   RE,1,1(R2)          INSURE L'EL LONG ENOUGH                      
         BH    RMDT5                                                            
         AR    R1,R2               R1=A(FIELD)                                  
         L     RE,0(R1)                                                         
         LCR   RE,RE               SET COMPLEMENT                               
         ST    RE,0(R1)                                                         
RMDT5    LA    RF,L'MDTTAB(RF)     BUMP TO NEXT FIELD IN TABLE                  
         B     RMDT3                                                            
         DROP  R2                                                               
         EJECT                                                                  
RMDP     LA    RF,MDPTAB                                                        
RMDP3    CLI   0(RF),X'FF'                                                      
         BER   R9                  END-OF-TABLE                                 
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RE,4(R1)                                                         
         CLM   RE,1,1(R2)          INSURE L'EL LONG ENOUGH                      
         BH    RMDP5                                                            
         AR    R1,R2               R1=A(FIELD)                                  
         ZAP   DUB,0(6,R1)                                                      
         MP    DUB,=P'-1'                                                       
         ZAP   0(6,R1),DUB                                                      
RMDP5    LA    RF,L'MDPTAB(RF)     BUMP TO NEXT FIELD IN TABLE                  
         B     RMDP3                                                            
         EJECT                                                                  
*              PUT WORKER RECORD TO ACPOST                                      
         SPACE 2                                                                
PUTIT    NTR1                                                                   
         AP    POSTREC,=P'1'                                                    
         LA    R2,T                                                             
         ZIC   R3,1(R2)                                                         
PUT2     AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    PUT4                                                             
         ZIC   R3,1(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   PUT2                                                             
         MVI   0(R2),0                                                          
PUT4     LA    R2,1(R2)                                                         
         LA    R3,T-4                                                           
         SR    R2,R3                                                            
         STH   R2,T-4                                                           
         BAS   RE,ADDPOST                                                       
         CLI   QOPT1,C'D'          DUMPS                                        
         BNE   *+8                                                              
         BAS   RE,DMPPUT                                                        
         B     XIT                                                              
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              WORKER INTERFACE                                                 
         SPACE 2                                                                
OPNPOST  MVC   COMMAND,=CL6'OPEN'                                               
         B     FILE                                                             
         SPACE 1                                                                
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         B     FILE                                                             
         SPACE 1                                                                
CLOSPOST MVC   COMMAND,=CL6'CLOSE'                                              
         B     FILE                                                             
         SPACE 1                                                                
FILE     NTR1                                                                   
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         LA    R3,T-4                                                           
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
DMPPUT   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R7,=C'PUT'                                                       
         LA    R5,T-4                                                           
         MVC   HALF,T-4                                                         
         SPACE 1                                                                
DUMP     LH    R9,HALF                                                          
         GOTO1 PRNTBL,DMCB,(3,(R7)),(R5),C'DUMP',(R9),=C'2D',(C'P',PRINX        
               T)                                                               
         B     XIT                                                              
         EJECT                                                                  
*        CONSTANTS                                                              
         SPACE 2                                                                
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
HEXIN    DC    V(HEXIN)                                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'300'                                                         
RECSIN   DC    PL4'0'                                                           
RECSMV   DC    PL4'0'                                                           
*                                                                               
ACTM     DS    XL1                                                              
MVTO     EQU   1                                                                
MVFR     EQU   2                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ACCSUM   DS    10CL(14+6+6)                                                     
CLTSUM   DS    50CL3                                                            
         SPACE 2                                                                
POSTBUFF DS    0D                                                               
         DC    4500X'00'                                                        
*                     '                                                         
CLILST   DS    0CL5                IRVINE CLIENTS                               
         DC    C'DLVCA'                                                         
         DC    C'CLMCA'                                                         
         DC    C'DAZCA'                                                         
         DC    C'DOKCA'                                                         
         DC    C'LR CA'                                                         
         DC    C'DBMCA'                                                         
         DC    C'DRMCA'                                                         
         DC    C'DVLCA'                                                         
         DC    C'LM CA'                                                         
         DC    C'LMSCA'                                                         
         DC    C'DMWCA'                                                         
         DC    C'DINCA'                                                         
         DC    C'DLICA'                                                         
         DC    C'DDECA'                                                         
         DC    C'DOACA'                                                         
         DC    C'DSECA'                                                         
         DC    C'DSLCA'                                                         
         DC    C'DCICA'                                                         
         DC    C'DPHCA'                                                         
         DC    C'DATCA'                                                         
         DC    C'DJACA'                                                         
         DC    C'DNECA'                                                         
         DC    C'LDACA'                                                         
         DC    C'DSYCA'                                                         
         DC    C'DCHCA'                                                         
         DC    C'DNWCA'                                                         
         DC    C'DCLCA'                                                         
         DC    C'DPICA'                                                         
         DC    C'DTXCA'                                                         
         DC    C'DSTCA'                                                         
         DC    C'DLMCA'                                                         
         DC    C'DMECA'                                                         
         DC    C'DODCA'                                                         
         DC    C'DMDCA'                                                         
         DC    C'DLACA'                                                         
         DC    C'DWACA'                                                         
         DC    C'LMDCA'                                                         
         DC    C'DSDCA'                                                         
         DC    C'GLRCA'                                                         
         DC    C'WMED4'                                                         
         DC    C'WLID4'                                                         
         DC    C'LMWD4'                                                         
         DC    C'DRNCA'                                                         
         DC    C'FMSCA'                                                         
         DC    C'TBCD4'                                                         
         DC    C'TBED4'                                                         
         DC    C'TBID4'                                                         
         DC    C'TBKD4'                                                         
         DC    C'TBND4'                                                         
         DC    C'TBPD4'                                                         
         DC    C'TBRD4'                                                         
         DC    C'TBSD4'                                                         
         DC    C'TBXD4'                                                         
         DC    C'TCSD4'                                                         
         DC    C'TIBD4'                                                         
         DC    C'TKBD4'                                                         
         DC    C'TPRD4'                                                         
         DC    C'TXBD4'                                                         
         DC    C'LMMD4'                                                         
         DC    C'LCJD4'                                                         
         DC    C'WLDD4'                                                         
         DC    C'DBPCA'                                                         
         DC    C'DCOCA'                                                         
         DC    C'DCTCA'                                                         
         DC    C'DDMCA'                                                         
         DC    C'DDOCA'                                                         
         DC    C'DESCA'                                                         
         DC    C'DEUCA'                                                         
         DC    C'DHMCA'                                                         
         DC    C'DHOCA'                                                         
         DC    C'DNJCA'                                                         
         DC    C'DSWCA'                                                         
         DC    C'HH CA'                                                         
         DC    C'HTZCA'                                                         
         DC    C'LMACA'                                                         
         DC    C'LMCCA'                                                         
         DC    C'MCDCA'                                                         
****** DEFAULTS FOR THIS RUN                                                    
         DC    C'RCLC3'                                                         
         DC    C'FLTD3'                                                         
         DC    C'FQCD3'                                                         
         DC    C'LQCD3'                                                         
         DC    C'LMLD3'                                                         
         DC    C'FDSD3'                                                         
         DC    C'FDMD3'                                                         
         DC    C'ESPD3'                                                         
         DC    C'NAOD3'                                                         
         DC    C'FCFD3'                                                         
         DC    C'FCCC3'                                                         
         DC    C'FRCD3'                                                         
         DC    C'RCCD3'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
REVTAB   DS    0XL4                REVERSE ELEMENTS                             
         DC    AL1(TRNELQ),AL3(RTRN)                                            
         DC    AL1(XPYELQ),AL3(RXPY)                                            
         DC    AL1(SCIELQ),AL3(RSCI)                                            
         DC    AL1(PBIELQ),AL3(RPBI)                                            
         DC    AL1(PSSBELQ),AL3(RPSSB)                                          
         DC    AL1(MDTELQ),AL3(RMDT)                                            
         DC    AL1(MDPELQ),AL3(RMDP)                                            
         DC    X'FF'                                                            
*                                                                               
*                                  TABLES FOR BINARY AMOUNT REVERSALS           
MDTTAB   DS    0XL4                                                             
         DC    AL1(MDTGRS-MDTELD),C'GRS'                                        
         DC    AL1(MDTNET-MDTELD),C'NET'                                        
         DC    AL1(MDTCOM-MDTELD),C'COM'                                        
         DC    AL1(MDTCD-MDTELD),C'CD '                                         
         DC    AL1(MDTINTL-MDTELD),C'INT'                                       
         DC    AL1(MDTRECV-MDTELD),C'RCV'                                       
         DC    AL1(MDTVAT-MDTELD),C'VAT'                                        
         DC    X'FF'                                                            
*                                                                               
MDPTAB   DS    0XL4                                                             
         DC    AL1(MDPGRS-MDPELD),C'GRS'                                        
         DC    AL1(MDPNET-MDPELD),C'NET'                                        
         DC    AL1(MDPCOM-MDPELD),C'COM'                                        
         DC    AL1(MDPCD-MDPELD),C'CD '                                         
         DC    AL1(MDPINTL-MDPELD),C'INT'                                       
         DC    AL1(MDPRECV-MDPELD),C'RCV'                                       
         DC    AL1(MDPVAT-MDPELD),C'VAT'                                        
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
ACXPD    DSECT                                                                  
         DS    F                                                                
T        DS    CL400                                                            
         DS    CL500                                                            
AREA     DS    CL200                                                            
ID       DS    CL16                                                             
POSTCASH DS    PL6                                                              
POSTREC  DS    PL6                                                              
*                                                                               
ACCDEB   DS    PL8                                                              
ACCCRE   DS    PL8                                                              
ACCTOT   DS    PL8                                                              
REQTOT   DS    PL8                                                              
RUNTOT   DS    PL8                                                              
*                                                                               
ELCODE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
*                                                                               
OFC      DS    CL2                                                              
CLT      DS    CL3                                                              
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINNUMB  DS    CL1                 NUMBER OF BUCKETS                            
BINFRST  DS    CL1                 DISP. TO FIRST BUCKET                        
BINSTAT  DS    CL1                 X'80' BINARY DATA                            
         DS    CL1                 SPARE                                        
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
*              DSECT FOR CLIENT SUMMARY RECORDS                                 
         SPACE 1                                                                
CLSD     DSECT                                                                  
CLSZAC   DS    CL2                 SZ ACCOUNT                                   
CLSCDE   DS    CL3                 CLIENT CODE                                  
CLSDATE  DS    CL3                 BY DATE                                      
CLSMOS   DS    CL2                 AND MOS                                      
CLSOFF   DS    CL1                 OFFICE                                       
CLSKLEN  EQU   *-CLSD                                                           
         DS    CL3                 SPARE                                        
CLSBK    EQU   *                                                                
CLSAMNT  DS    PL8                 AMOUNT                                       
CLSBKCNT EQU   (*-CLSBK)/8         NUMBER OF BUCKETS                            
CLSLEN   EQU   *-CLSD                                                           
         EJECT                                                                  
       ++INCLUDE ACMASTD                                                        
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'090ACREPXP02 05/01/02'                                      
         END                                                                    
