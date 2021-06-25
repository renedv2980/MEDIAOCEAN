*          DATA SET ACREPLA02  AT LEVEL 016 AS OF 09/21/09                      
*PHASE ACLA02A                                                                  
*INCLUDE ACTYPCHK                                                               
         TITLE 'LEDGER ANALYSIS REPORT'                                         
ACLA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACLA**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACLAD,RC                                                         
         EJECT                                                                  
*              RUNFIRST                                                         
*                                                                               
RUNF00   CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         L     RF,GETOPT           NOP GETPOTS                                  
         MVC   0(2,RF),=X'07FE'                                                 
         MVI   FCSEQ,FCSEQNEW      READ IN NATIVE SEQUENCE(NO BLDBUF!!)         
         B     XIT                                                              
         EJECT                                                                  
*              REQUEST FIRST                                                    
*                                                                               
REQF00   CLI   MODE,LEDGFRST                                                    
         BNE   PTRN00                                                           
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT1,C'Y'          SUPPRESS OFFICE SECURITY                     
         BNE   *+8                                                              
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRDMOSP,C'N'                                                    
         CLI   QUNIT,C'S'          ONLY FOR UNIT S                              
         BNE   XIT                                                              
         CLI   QOPT2,C'Y'          USE G/L MOS PASSIVES?                        
         BNE   *+8                                                              
         MVI   FCRDMOSP,C'Y'                                                    
         B     XIT                                                              
         EJECT                                                                  
*              PROCESS TRANSACTIONS                                             
*                                                                               
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   REQL00                                                           
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   0(R4),X'44'                                                      
         BNE   XIT                                                              
         LR    R3,R4                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         GOTO1 TYPCHK,DMCB,(C'O',(R3))                                          
         LA    R8,WRK              BUILD A WORK RECORD                          
         USING WRKD,R8                                                          
         ZAP   WRKDR,=P'0'                                                      
         ZAP   WRKCR,=P'0'                                                      
*                                                                               
         CLC   QSELECT,SPACES                                                   
         BE    *+14                                                             
         CLC   TRNSBTCH,QSELECT                                                 
         BNE   XIT                                                              
         LA    R1,WRKDR                                                         
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                                                              
         LA    R1,WRKCR                                                         
         ZAP   0(L'WRKDR,R1),TRNSAMNT                                           
         MVI   WRKSRT,WRKULT       SORT BY UNIT/LEDGER/TYPE                     
         MVC   WRKKEY(2),ACKEYACC+1   UNIT/LEDGER TO KEY                        
         MVC   WRKKEY+2(1),TRNSTYPE   TRANSACTION TYPE                          
         GOTO1 BINADD,DMCB,WRK,ADWRK                                            
         MVI   WRKKEY+2,X'FF'         TOTAL FOR U/L                             
         BASR  RE,RF                                                            
         MVC   WRKKEY(2),=X'FFFF'     OVERALL TOTAL                             
         BASR  RE,RF                                                            
*                                                                               
         MVI   WRKSRT,WRKTUL       SORT BY TYPE, UNIT/LEDGER                    
         MVC   WRKKEY(1),TRNSTYPE  TRANSACTION TYPE                             
         MVC   WRKKEY+1(2),ACKEYACC+1   UNIT/LEDGER TO KEY                      
         BASR  RE,RF                                                            
         MVC   WRKKEY+1(2),=X'FFFF' TOTAL FOR TYPE                              
         BASR  RE,RF                                                            
         MVI   WRKKEY,X'FF'          OVERALL TOTAL                              
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
*              REQUEST LAST                                                     
*                                                                               
REQL00   CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         L     R5,ADWRK                                                         
         USING BIND,R5                                                          
         ICM   R3,15,BININ                                                      
         BZ    XIT                                                              
         LA    R8,BINTABLE                                                      
*                                                                               
         USING WRKD,R8                                                          
REQL03   MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),WRKDR                                              
         MVC   P+27(L'EDMSK-1),WORK+1                                           
         MVC   WORK(L'EDMSK),EDMSK                                              
         ED    WORK(L'EDMSK),WRKCR                                              
         MVC   P+45(L'EDMSK-1),WORK+1                                           
         ZAP   DUB,WRKDR                                                        
         SP    DUB,WRKCR                                                        
         EDIT  (P8,DUB),(13,P+63),2,MINUS=YES                                   
         CLI   WRKSRT,WRKULT      UNIT LEDGER SORT                              
         BNE   REQL07                                                           
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVC   P+4(2),WRKKEY       UNIT LEDGER                                  
         EDIT  (B1,WRKKEY+2),(3,P+12)                                           
         CLI   WRKKEY+2,X'FF'                                                   
         BNE   *+14                                                             
         MVC   P+11(5),=C'TOTAL'                                                
         MVI   SPACING,2                                                        
         CLC   WRKKEY(2),=X'FFFF'                                               
         BNE   *+10                                                             
         MVC   P+4(3),=C'ALL'                                                   
         B     REQL10                                                           
*                                                                               
REQL07   CLI   RCSUBPRG,1                                                       
         BE    *+12                                                             
         MVI   FORCEHED,C'Y'       FIRST TIME NEW PAGE                          
         MVI   RCSUBPRG,1          TYPE SORT                                    
*                                                                               
         EDIT  (B1,WRKKEY),(3,P+3)                                              
         MVC   P+12(2),WRKKEY+1     UNIT LEDGER                                 
         CLC   WRKKEY+1(2),=X'FFFF'                                             
         BNE   *+14                                                             
         MVC   P+10(5),=C'TOTAL'                                                
         MVI   SPACING,2                                                        
         CLI   WRKKEY,X'FF'                                                     
         BNE   *+10                                                             
         MVC   P+3(3),=C'ALL'                                                   
*                                                                               
REQL10   GOTO1 ACREPORT                                                         
         LA    R8,WRKLNQ(R8)                                                    
         BCT   R3,REQL03                                                        
*                                                                               
         XC    BININ,BININ                                                      
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2                                                       
         GOTO1 TYPCHK,DMCB,(C'P',P),ACREPORT,0                                  
         GOTO1 TYPCHK,DMCB,(C'I',0)                                             
         B     XIT                                                              
         EJECT                                                                  
*              ADD ITEM TO BINARY TABLE                                         
*                                                                               
         USING BIND,R5                                                          
BINADD   NTR1  ,                                                                
         L     R5,4(R1)            BINSRCH PARAMETERS                           
         L     R3,0(R1)            A(RECORD)                                    
         MVC   PARM+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,PARM,(1,(R3)),(R2)                                       
         OC    PARM(4),PARM                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,PARM+8        UPDATE COUNT                                 
         CLI   PARM,1                                                           
         BE    XIT                 NOT FOUND - ADDED                            
         L     R4,PARM             A(RECORD FOUND)                              
         ZIC   R6,BINFRST          DISP. TO FIRST BUCKET                        
         AR    R4,R6               RECORD FOUND                                 
         AR    R3,R6               NEW RECORD                                   
         ZIC   R7,BINNUMB          NUMBER OF BUCKETS                            
         TM    BINSTAT,X'80'                                                    
         BO    BINBIN              DATA IS BINARY                               
         AP    0(8,R4),0(8,R3)     ADD NEW TO OLD                               
         LA    R4,8(R4)                                                         
         LA    R3,8(R3)                                                         
         BCT   R7,*-14                                                          
         B     XIT                                                              
*                                                                               
BINBIN   L     RE,0(R3)                                                         
         L     RF,0(R4)                                                         
         AR    RF,RE                                                            
         ST    RF,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R7,BINBIN                                                        
         B     XIT                                                              
*                                                                               
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*        DATA CONSTANTS                                                         
*                                                                               
TYPCHK   DC    V(TYPCHK)                                                        
ADWRK    DC    A(WRKTAB)                                                        
*                                                                               
EDMSK    DC    X'40202020202020202020202020214B202060'                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MXWRK    EQU   1000                                                             
         DS    0D                  TYPE SUMMARY                                 
         DC    CL8'**WRKTAB*'                                                   
WRKTAB   DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(WRKLNQ)         RECORD LENGTH                                
         DC    AL4(4)              DISP. TO KEY/ KEY LENGTH                     
         DC    AL4(MXWRK)          MAX. IN TABLE                                
         DC    AL1(2)              NUMBER OF BUCKETS                            
         DC    AL1(WRKDR-WRKD)     DISP. TO BUCKETS                             
         DC    X'00'               STATUS                                       
         DC    AL1(0)                                                           
         DS    (MXWRK*WRKLNQ)C     TABLE                                        
         EJECT                                                                  
ACLAD    DSECT                                                                  
WRK      DS    CL(WRKLNQ)                                                       
PARM     DS    6F                                                               
TODAY2   DS    CL2                                                              
ELCODE   DS    CL1                                                              
         EJECT                                                                  
*              DSECT FOR WORK RECORD                                            
*                                                                               
WRKD     DSECT                                                                  
WRKSRT   DS    XL1                 SORT TYPE                                    
WRKULT   EQU   1                   SORT BY UNIT/LEDGER/TYPE                     
WRKTUL   EQU   2                   SORT BY TYPE/LEDGER                          
WRKKEY   DS    CL3                 UNIT/LEDGER/TYPE                             
*                                  OR TYPE/UNIT/LEDGER                          
WRKDR    DS    PL8                 DEBIT                                        
WRKCR    DS    PL8                 CREDIT                                       
WRKLNQ   EQU   *-WRKD                                                           
         EJECT                                                                  
*              DSECT FOR THE BINSRCH LIST                                       
*                                                                               
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
*  ACREPWORKD                                                                   
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREPLA02 09/21/09'                                      
         END                                                                    
