*          DATA SET ACREPX702  AT LEVEL 032 AS OF 05/01/02                      
*PHASE ACX702A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'FIX 51 ELEMENT IN 1R TRANSACTIONS'                              
         PRINT NOGEN                                                            
ACX702   CSECT                                                                  
         NMOD1 0,**ACX7**,RR=R7                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACX7D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         BE    RUNL                                                             
XIT      XIT1                                                                   
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
*                                                                               
RUNF     ZAP   TOTCOUNT,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* REQUEST FIRST                                                       *         
***********************************************************************         
REQF     MVI   FORCEHED,C'Y'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCRESET,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
         ZAP   COUNT,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS TRANSACTIONS                                                *         
***********************************************************************         
         USING TRNRECD,R2                                                       
PTRN     L     R2,ADTRANS                                                       
         SH    R2,DATADISP                                                      
*                                                                               
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
*                                                                               
         USING PCIELD,R3                                                        
PTRN10   CLI   0(R3),0                                                          
         BE    XIT                                                              
         CLI   0(R3),PCIELQ        X'51'                                        
         BE    *+16                                                             
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PTRN10                                                           
*                                                                               
         BAS   RE,DMPGET                                                        
*                                                                               
         MVC   SVTASK,PCITSK       SAVE TASK CODE                               
         CLC   PCITSK,SPACES       IF TASK CODE IS SPACES THEN                  
         BNE   *+10                CHANGE TO BINARY ZEROS                       
         XC    SVTASK,SVTASK                                                    
*                                                                               
         MVC   SVSJ,SPACES                                                      
         CLC   PCICLI(2),=C'SJ'    BAD EL LOOKS LIKE SJCLIPRDJOBXXX             
         BNE   *+14                WITH NO CO CODE OR                           
         MVC   SVSJ,PCICLI                                                      
         B     *+16                                                             
         MVC   SVSJ(2),=C'SJ'      BAD EL LOOK LIKE 4040CLIPRDJOBXXX            
         MVC   SVSJ+2(12),PCICLI+2 WITH NO CO CODE AND NO SJ                    
*                                                                               
         MVC   PCICLI,SPACES                                                    
         MVC   PCIPRJT,SPACES                                                   
         XC    PCITSK,PCITSK                                                    
         MVC   PCICLI(1),RCCOMPFL                                               
         MVC   PCICLI+1(L'PCICLI-1),SVSJ                                        
         MVC   PCIPRJT,PCICLI                                                   
         MVC   PCITSK,SVTASK                                                    
         AP    COUNT,=P'1'                                                      
         BAS   RE,DMPPUT                                                        
*                                                                               
PT050    CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITRANS                                                    
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REQUEST LAST                                                        *         
***********************************************************************         
REQL     GOTO1 ACREPORT                                                         
         MVC   P+1(25),=C'NUMBER OF TRANSACATIONS: '                            
         EDIT  COUNT,(9,P+27)                                                   
         AP    TOTCOUNT,COUNT                                                   
         GOTO1 ACREPORT                                                         
         ZAP   DUMPCNT,=P'0'                                                    
         ZAP   PDUMP,=P'0'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   DUMP RECORDS                                                                
***********************************************************************         
*                                                                               
RUNL     GOTO1 ACREPORT                                                         
         MVC   P+1(28),=C'TOTAL TRANSACTIONS FOR RUN: '                         
         EDIT  TOTCOUNT,(9,P+30)                                                
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*   DUMP RECORDS                                                                
***********************************************************************         
*                                                                               
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET '                                                      
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         LA    R6,=C'PUT '                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         B     DUMP                                                             
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,TRNRLEN-TRNRECD(R2)                                         
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R2),C'DUMP',(R8),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
                                                                                
HELLO    DC    V(HELLO)                                                         
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
AIO      DC    A(IOA)                                                           
BIO      DC    A(IOB)                                                           
CIO      DC    A(IOC)                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'2000'                                                        
*                                                                               
TRNSACT  DC    C'TRANSACTION DA=00000000'                                       
*                                                                               
ACCFIL   DC    CL8'ACCFIL'                                                      
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* IO AREAS                                                            *         
***********************************************************************         
                                                                                
IOA      DS    XL2000                                                           
IOB      DS    XL2000                                                           
IOC      DS    XL2000                                                           
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
                                                                                
ACX7D    DSECT                                                                  
AKEY     DS    CL(L'ACCKEY)        DIRECTORY KEY                                
ADIR     DS    CL(ACCKDA-ACCKEY)   TRANSACTION DIRECTORY RECORD                 
ADA      DS    XL(L'ACCKDA)        DISK ADDRESS                                 
*                                                                               
BKEY     DS    CL(L'ACCKEY)                                                     
BDIR     DS    CL(ACCKDA-ACCKEY)                                                
BDA      DS    XL(L'ACCKDA)                                                     
*                                                                               
CKEY     DS    CL(L'ACCKEY)                                                     
CDIR     DS    CL(ACCKDA-ACCKEY)                                                
CDA      DS    XL(L'ACCKDA)                                                     
*                                                                               
SVSJ     DS    CL14                SAVED SJ ACCOUNT                             
SVTASK   DS    XL2                 SAVED TASK CODE                              
COUNT    DS    PL8                                                              
TOTCOUNT DS    PL8                                                              
*                                                                               
ELEMENT  DS    XL255                                                            
*                                                                               
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032ACREPX702 05/01/02'                                      
         END                                                                    
