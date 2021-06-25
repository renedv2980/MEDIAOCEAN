*          DATA SET ACREPX902  AT LEVEL 013 AS OF 08/16/00                      
*PHASE ACX902A                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'FIX TRANSACTION ELEMENTS'                                       
ACX902   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACX9**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX9D,RC                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST FOR RUN                                                       *         
***********************************************************************         
                                                                                
RUNF     CLI   MODE,RUNFRST                                                     
         BNE   LDGF                                                             
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         ZAP   CHATOT,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LEDGER                                                    *         
***********************************************************************         
                                                                                
LDGF     CLI   MODE,LEDGFRST                                                    
         BNE   PRAC                                                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'       NEW PAGE                                     
         MVI   FCSUPOFC,C'Y'       SUPPRESS OFFICE SECURITY                     
         MVI   FCRNTIME,C'N'       SUPPRESS TIME                                
         MVI   FCRESET,C'Y'        RESET READ SEQUENCE                          
         MVI   FCRDTRNS,C'N'       DON'T PROCESS TRANSACTIONS                   
         GOTO1 DATCON,DMCB,(0,QSTART),(1,CONDATE)                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FIRST FOR ACCOUNT                                                   *         
***********************************************************************         
                                                                                
PRAC     CLI   MODE,PROCACC                                                     
         BNE   PTRN                                                             
         ZAP   CHAACC,=P'0'                                                     
         L     R3,ADACCSTA                                                      
         USING RSTELD,R3                                                        
         CLC   RSTTDATE,CONDATE    TEST CURRENT ACTIVITY                        
         BL    XIT                                                              
         MVI   FCRDTRNS,C'Y'       READ TRANSACTIONS                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
                                                                                
PTRN     CLI   MODE,PROCTRNS                                                    
         BNE   ACCL                                                             
         L     R5,ADTRANS                                                       
         USING TRNELD,R5                                                        
         TM    TRNSTAT,TRNSDR      TEST DEBIT                                   
         BZ    XIT                 DON'T NEED CREDITS                           
         TM    TRNSTAT,TRNSREV                                                  
         BO    XIT                 SKIP REVERSALS                               
         TM    TRNSTAT,TRNSNOCM                                                 
         BO    XIT                 SKIP ALREADY MARKED NON COMM                 
         CLC   TRNBTCH+2(4),SPACES SKIP CREATED WITH AC21/AC27                  
         BE    XIT                                                              
         BAS   RE,GETEP                                                         
         MVC   BYTE,TRNSTAT                                                     
         NI    BYTE,TRNSNOCM                                                    
         NI    STATUS,TRNSNOCM                                                  
         CLC   BYTE,STATUS                                                      
         BE    XIT                 IT'S OK                                      
         NI    TRNSTAT,X'FF'-TRNSNOCM                                           
         OC    TRNSTAT,STATUS                                                   
*                                                                               
         MVI   ELCODE,TRSELQ       GET TRANSACTION STATUS ELEMENT               
         BAS   RE,FIRSTEL                                                       
         BNE   XIT                                                              
         USING TRSELD,R5                                                        
*        CLC   TRSDATE,=X'C6C1'    TEST 6/1/99                                  
*        BL    XIT                                                              
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,NEWDATE)                              
*                                                                               
         L     R2,ADTRANS          PRINT TRANSACTION DETAIL                     
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         LA    R7,P                                                             
         USING PRNTD,R7                                                         
         MVC   PRNTULA,TRNKULA     ACCOUNT                                      
         MVC   PRNTOFF,TRNKOFF     OFFICE                                       
         MVC   PRNTULC,TRNKULC     CONTRA                                       
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(8,PRNTDATE)                            
         MVC   PRNTREF,TRNKREF     REFERENCE                                    
         SR    R1,R1                                                            
         IC    R1,TRNKSBR          SUBREFERENCE                                 
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRNTSBR,DUB                                                      
*                                                                               
         L     R5,ADTRANS          EDIT AMOUNT                                  
         USING TRNELD,R5                                                        
         LA    R6,PRNTDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R6,PRNTCR                                                        
         EDIT  (P6,TRNAMNT),(13,0(R6)),2,CR=YES                                 
*                                                                               
*        GOTO1 DATCON,DMCB,(1,OLDDATE),(8,PRNTOLD)                              
         GOTO1 DATCON,DMCB,(1,NEWDATE),(8,PRNTNEW)                              
         GOTO1 ACREPORT                                                         
*                                                                               
         AP    CHAACC,=P'1'        COUNT RECORDS CHANGED                        
         CLI   QOPT1,C'D'                                                       
         BNE   *+8                                                              
         BAS   RE,DMP              DUMP CHANGED RECORDS                         
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   XIT                                                              
         MVI   MODE,WRITRANS                                                    
         B     XIT                                                              
         DROP  R2,R5,R7                                                         
         EJECT                                                                  
***********************************************************************         
* ACCOUNT LAST                                                        *         
***********************************************************************         
                                                                                
ACCL     CLI   MODE,ACCLAST                                                     
         BNE   RUNL                                                             
         CP    CHAACC,=P'0'                                                     
         BE    XIT                                                              
*        EDIT  (P4,CHAACC),(6,P+1)                                              
*        MVC   P+8(20),=C'TRANSACTIONS CHANGED'                                 
*        GOTO1 ACREPORT                                                         
         AP    CHATOT,CHAACC                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
                                                                                
RUNL     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         EDIT  (P4,CHATOT),(6,P+1)                                              
         MVC   P+8(20),=C'TRANSACTIONS CHANGED'                                 
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUNTINE TO DUMP RECORDS                                            *         
* R2 = A(TRANSACTIION)                                                *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2                                                       
DMP      NTR1  ,                                                                
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'PUT'                                                       
         SR    RF,RF                                                            
         ICM   RF,3,TRNKEY+ACCORLEN                                             
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R2),C'DUMP',(RF),=C'2D'                    
         B     XIT                                                              
         DROP  R2                                                               
*                                                                               
                                                                                
         GETEL R5,DATADISP,ELCODE                                               
                                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
GETEP    NTR1  ,                                                                
         L     R2,ADTRANS                                                       
         SH    R2,DATADISP                                                      
         USING TRNRECD,R2                                                       
         MVC   DKEY,TRNKEY                                                      
         LA    R2,DKEY                                                          
         MVI   TRNKSBR,0                                                        
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
*                                                                               
GETEP3   LA    R2,DIR                                                           
         CLC   DIR(41),DKEY                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TRNKSTYP,47                                                      
         BNE   GETEP5                                                           
         MVC   DA,TRNKDA                                                        
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,IOAREA,DMWORK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,IOAREA                                                        
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,R3           R3=TYPE 47                                   
         L     R4,ADTRANS          R4=TYPE 48                                   
         CLC   TRNBTCH,TRNBTCH-TRNELD(R4)                                       
         BNE   GETEP5                                                           
*        ZAP   DUB,TRNAMNT-TRNELD(L'TRNAMNT,R4)                                 
*        CVB   RE,DUB                                                           
*        ZAP   DUB,TRNAMNT                                                      
*        CVB   RF,DUB                                                           
*        LPR   RF,RF                                                            
*        LPR   RE,RE                                                            
*        CR    RE,RF                                                            
*        BNE   GETEP5                                                           
         MVC   STATUS,TRNSTAT      SAVE  STATUS 0F 47                           
         B     XIT                                                              
*                                                                               
GETEP5   GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         B     GETEP3                                                           
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERAL POOL                                          *         
***********************************************************************         
                                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50'                                                          
*                                                                               
ACCDIR   DC    C'ACCDIR '                                                       
ACCMST   DC    C'ACCMST '                                                       
GETREC   DC    C'GETREC '                                                       
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DSECT FOR STORAGE                                                   *         
***********************************************************************         
                                                                                
ACX9D    DSECT                                                                  
ELCODE   DS    CL1                                                              
CONDATE  DS    XL3                 CONTROL DATE                                 
OLDDATE  DS    XL3                 OLD CLEARANCE ADDED DATE                     
NEWDATE  DS    XL3                 NEW CLEARANCE ADDED DATE                     
CHAACC   DS    PL4                                                              
CHATOT   DS    PL4                                                              
*                                                                               
DKEY     DS    XL42                                                             
DIR      DS    XL60                                                             
DA       DS    F                                                                
*                                                                               
STATUS   DS    X                                                                
*                                                                               
IOAREA   DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
                                                                                
PRNTD    DSECT                                                                  
         DS    C                                                                
PRNTULA  DS    CL14                ACCOUNT                                      
         DS    C                                                                
PRNTOFF  DS    CL2                 OFFFICE                                      
         DS    C                                                                
PRNTULC  DS    CL14                CONTRA                                       
         DS    C                                                                
PRNTDATE DS    CL8                 DATE                                         
         DS    C                                                                
PRNTREF  DS    CL6                 REFERENCE                                    
         DS    C                                                                
PRNTSBR  DS    CL3                 SUB-REFERENCE                                
         DS    CL2                                                              
PRNTDR   DS    CL13                DEBIT AMOUNT                                 
         DS    CL2                                                              
PRNTCR   DS    CL13                CREDIT AMOUNT                                
         DS    CL2                                                              
PRNTOLD  DS    CL8                 OLD DATE                                     
         DS    CL1                                                              
PRNTNEW  DS    CL8                 NEW DATE                                     
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPX902 08/16/00'                                      
         END                                                                    
