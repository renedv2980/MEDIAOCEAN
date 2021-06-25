*          DATA SET ACREPZ202  AT LEVEL 069 AS OF 05/01/02                      
*PHASE ACZ202A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'FIX DECADE IN TRANSACTION RECORDS'                              
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* CHANGES YEAR TO RIGHT DECADE IF 1) TRANSACTION DATE IN 44 ELEM IS             
* <910101.  2) ACTIVITY DATE IN 60 ELEM IS <990309. 3)OFFICE CODE IS IA         
* CHANGES YEAR IN KEY STATUS, RECORD STATUS & TRSPMOS IN 60 ELEM                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         PRINT NOGEN                                                            
ACZ202   CSECT                                                                  
         NMOD1 0,**ACZ2**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ2D,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
XIT      XMOD1 1                                                                
         EJECT                                                                  
FX00     MVI   FORCEHED,C'Y'                                                    
         MVI   FIRSTIME,C'Y'       SET FIRSTIME IN                              
         MVC   PAGE,=H'1'                                                       
         MVC   AIO,=A(IO)                                                       
         MVC   AIO2,=A(IO2)                                                     
         ZAP   TOTACC,=P'0'                                                     
         ZAP   TOTLEDG,=P'0'                                                    
         ZAP   TOTDEB,=P'0'                                                     
         ZAP   TOTCR,=P'0'                                                      
         ZAP   LEDGTOT,=P'0'                                                    
         MVC   SVACCNT,SPACES                                                   
         MVC   FRSTEL,=Y(ACCRFST-ACCRECD)                                       
*                                                                               
         USING TRNRECD,R6                                                       
         LA    R6,DIO                                                           
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL                                                 
         MVC   TRNKUNT(2),QUNIT    MOVE IN UNIT/LEDGER                          
         MVC   CUL,DIO                                                          
*                                                                               
FX02     LA    R6,DIO                                                           
         OI    DMINBTS,X'08'                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRDHI),DIR,DIO,DIO                        
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX04     DS    0H                                                               
         LA    R6,DIO                                                           
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMRSEQ),DIR,DIO,DIO                        
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLC   CUL,DIO                      CHANGE IN COMPANY                   
         BNE   FX99                DONE                                         
*                                                                               
FX05     CLC   TRNKDATE,SPACES     TRANSACTION RECORD?                          
         BNH   FX04                                                             
FX05A    CLC   TRNKOFF,=C'IA'      OFFICE CODE IA ONLY                          
         BNE   FX04                                                             
         CLC   TRNKDATE,=X'910101' DO NOT READ ABOVE 1991                       
         BNL   FX04                                                             
         CLC   TRNKSMOS,TRNKDATE                                                
         BE    FX04                THIS ONE'S OKAY                              
         MVC   WORK(1),TRNKSMOS                                                 
         MVC   WORK+1(1),TRNKDATE                                               
         NI    WORK,X'0F'                                                       
         NI    WORK+1,X'0F'                                                     
         CLC   WORK(1),WORK+1                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   FIRSTIME,C'Y'                                                    
         BE    FX06                                                             
         CLC   SVACCNT,TRNKACT     SAME ACCOUNT                                 
         BE    FX06                                                             
         CP    TOTACC,=P'0'                                                     
         BE    FX06                                                             
         MVC   P(31),=C'TOTAL TRANSACTIONS FOR ACCOUNT '                        
         MVC   P+31(12),SVACCNT                                                 
         EDIT  (P6,TOTACC),(12,P+50),COMMAS=YES,ZERO=NOBLANK                    
         GOTO1 ACREPORT                                                         
         MVC   P(25),=C'TOTAL DEBITS FOR ACCOUNT '                              
         EDIT  (P8,TOTDEB),(14,P+50),2,COMMAS=YES,ZERO=NOBLANK                  
         GOTO1 ACREPORT                                                         
         MVC   P(26),=C'TOTAL CREDITS FOR ACCOUNT '                             
         EDIT  (P8,TOTCR),(14,P+50),2,COMMAS=YES,ZERO=NOBLANK                   
         GOTO1 ACREPORT                                                         
         ZAP   TOTACC,=P'0'                                                     
         ZAP   TOTDEB,=P'0'                                                     
         ZAP   TOTCR,=P'0'                                                      
FX06     MVC   SVTRNSDT,TRNKDATE   SAVE THE RIGHT YEAR                          
         MVC   SVACCNT,TRNKACT     SAVE ACCOUNT                                 
         MVI   FIRSTIME,C'N'       FIRSTIME IS OVER                             
*                                                                               
         MVC   DA,TRNKDA                                                        
         GOTO1 DATAMGR,DMCB,(DMINBTS,GETREC),ACCMST,DA,AIO,DMWRK                
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO                                                           
         AH    R5,FRSTEL                                                        
FX10     CLI   0(R5),X'44'                                                      
         BE    FX16                                                             
         DC    H'0'                FIRST EL MUST BE 44                          
FX11     CLI   0(R5),0                                                          
         BE    FX04                                                             
         CLI   0(R5),X'60'                                                      
         BE    FX20                                                             
FX12     SR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     FX11                                                             
*                                                                               
         USING TRNELD,R5                                                        
FX16     ZAP   SVCREDIT,=P'0'                                                   
         ZAP   SVDEBIT,=P'0'                                                    
         CLC   TRNDATE,=X'910101'                                               
         BNL   FX04                                                             
         TM    TRNSTAT,TRNSDR      DEBIT OR CREDIT?                             
         BO    *+14                MUST BE DEBIT                                
         AP    SVCREDIT,TRNAMNT                                                 
         B     *+10                                                             
         AP    SVDEBIT,TRNAMNT                                                  
         B     FX12                GET NEXT EL                                  
*                                                                               
         USING TRSELD,R5                                                        
FX20     XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(2,TRSDATE),(1,WORK) ACTIVITY DATE                   
         CLC   WORK(3),=X'990309'                                               
         BNE   FX04                                                             
         BAS   RE,DMPGET                                                        
         L     R6,AIO                                                           
         BAS   RE,DMPGET                                                        
         MVC   TRSPMOS(1),SVTRNSDT MOVE IN RIGHT DECADE IN 60 ELEM              
         AP    TOTACC,=P'1'                                                     
         AP    TOTLEDG,=P'1'                                                    
         AP    TOTDEB,SVDEBIT                                                   
         AP    TOTCR,SVCREDIT                                                   
         AP    LEDGTOT,SVDEBIT                                                  
         AP    LEDGTOT,SVCREDIT                                                 
*                                                                               
FX50     DS    0H                                                               
         LA    R6,DIO                                                           
         MVC   TRNKSMOS(1),SVTRNSDT  MOVE IN RIGHT DECADE KEY STATUS            
         BAS   RE,DMPPUT                                                        
         L     R6,AIO                                                           
         MVC   TRNRSMOS(1),SVTRNSDT  MOVE IN RIGHT DECADE RECORD STATUS         
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    FX02                                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,DMWRT),DIR,DIO,DIO                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATAMGR,DMCB,(DMINBTS,PUTREC),ACCMST,DA,AIO,DMWRK                
         CLI   8(R1),0                                                          
         BE    FX02                RESET DIRECTORY READS                        
         DC    H'0'                DIE WRITING A RECORD                         
*                                                                               
FX99     DS    0H                                                               
         GOTO1 ACREPORT                                                         
         CP    TOTACC,=P'0'                                                     
         BE    FX99A                                                            
         MVC   P(31),=C'TOTAL TRANSACTIONS FOR ACCOUNT '                        
         MVC   P+33(12),SVACCNT                                                 
         EDIT  (P6,TOTACC),(12,P+50),COMMAS=YES,ZERO=NOBLANK                    
         GOTO1 ACREPORT                                                         
FX99A    CP    TOTDEB,=P'0'                                                     
         BNE   FX100                                                            
         CP    TOTCR,=P'0'                                                      
         BE    XIT                                                              
FX100    MVC   P(25),=C'TOTAL DEBITS FOR ACCOUNT '                              
         EDIT  (P8,TOTDEB),(14,P+50),2,COMMAS=YES,ZERO=NOBLANK                  
         GOTO1 ACREPORT                                                         
         MVC   P(26),=C'TOTAL CREDITS FOR ACCOUNT '                             
         EDIT  (P8,TOTCR),(14,P+50),2,COMMAS=YES,ZERO=NOBLANK                   
         GOTO1 ACREPORT                                                         
         MVC   P(26),=C'TOTAL ACCOUNTS FOR LEDGER '                             
         MVC   P+28(2),CUL+1                                                    
         EDIT  (P8,TOTLEDG),(12,P+50),COMMAS=YES,ZERO=NOBLANK                   
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         MVC   P(18),=C'TOTALS FOR LEDGER '                                     
         MVC   P+20(2),CUL+1                                                    
         EDIT  (P8,LEDGTOT),(14,P+50),2,COMMAS=YES,ZERO=NOBLANK                 
         GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
DMPGET   NTR1                                                                   
         LA    R3,=C'GET'                                                       
         B     DMP02                                                            
*                                                                               
DMPPUT   NTR1                                                                   
         LA    R3,=C'PUT'                                                       
DMP02    C     R6,AIO                                                           
         BE    DUMPR                                                            
         C     R6,=A(DIO)                                                       
         BE    DUMPDIR                                                          
         DC    H'0'                                                             
*                                                                               
DUMPDIR  SR    R5,R5                                                            
         LA    R6,DIO                                                           
         LH    R5,=H'54'                                                        
         B     DUMP01                                                           
DUMPR    SR    R5,R5                                                            
         L     R6,AIO                                                           
         ICM   R5,3,ACCRLEN-ACCRECD(R6)                                         
DUMP01   DS    0H                                                               
         CLI   QOPT2,C'D'                                                       
         BNE   XIT                                                              
         GOTO1 PRNTBL,DMCB,(3,(R3)),(R6),C'DUMP',(R5),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*        CONSTANTS                                                              
*                                                                               
RELO     DC    F'0'                                                             
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
FRSTEL   DC    H'49'                                                            
ACCFIL   DC    CL8'ACCOUNT'                                                     
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCMST   DC    CL8'ACCMST'                                                      
DIR      DC    CL6'ACCDIR'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
DIO      DS    CL(CACRFST-CACRECD)                                              
DIO2     DS    CL(CACRFST-CACRECD)                                              
IO       DS    CL2048                                                           
IO2      DS    CL2048                                                           
         EJECT                                                                  
         LTORG                                                                  
MXTSH    EQU   1000                                                             
SVTRNSDT DS    XL1                 SAVED TRANSACTION YEAR                       
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
*                                                                               
ACZ2D    DSECT                                                                  
AIO      DS    A                                                                
AIO2     DS    A                                                                
ELCODE   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ACTIVE   DS    CL1                                                              
DATESV   DS    XL3                                                              
REFSV    DS    CL6                                                              
CUL      DS    CL3                                                              
SVACCNT  DS    XL12                                                             
SAVKEY   DS    CL64                                                             
*                                                                               
ELIST    DS    3F                  FOR GETL, ADDEL, DELEL                       
ELERR    DS    CL1                                                              
         ORG   ELERR               ERROR CODE FROM HELLO                        
ELADDR   DS    F                   ADDRESS OF ELEMENT FROM HELLO                
         DS    2F                                                               
*                                                                               
TOTACC   DS    PL6                                                              
TOTLEDG  DS    PL8                                                              
TOTCR    DS    PL8                                                              
TOTDEB   DS    PL8                                                              
LEDGTOT  DS    PL8                                                              
SVDEBIT  DS    PL8                                                              
SVCREDIT DS    PL8                                                              
ALLTOT   DS    PL8                                                              
DA       DS    F                                                                
FIRSTIME DS    CL1                                                              
DMWRK    DS    12D                                                              
*                                                                               
DA2      DS    F                                                                
DMWRK2   DS    12D                                                              
*                                                                               
*                                                                               
TSHD     DSECT                                                                  
TSHACC   DS    XL15                ACCOUNT                                      
TSHDTE   DS    XL3                 DATE                                         
TSHLEN   EQU   *-TSHD                                                           
         EJECT                                                                  
*        ACGENBOTH                                                              
*        ACGENFILE                                                              
*        ACGENPOST                                                              
*        ACMASTD                                                                
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'069ACREPZ202 05/01/02'                                      
         END                                                                    
**PAN#1  CSECT                                                                  
         END                                                                    
