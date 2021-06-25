*          DATA SET ACREPZ702  AT LEVEL 009 AS OF 08/16/00                      
*PHASE ACZ702A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'UNDELETE ACCOUNTS'                                              
ACZ702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZ7**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZ7D,RC                                                         
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,LEDGLAST                                                    
         BE    LDGL                                                             
         B     XIT                                                              
***********************************************************************         
*              RUN FIRST                                              *         
***********************************************************************         
*                                                                               
RUNF     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              LEDGER FIRST                                           *         
***********************************************************************         
*                                                                               
LDGF     DS    0H                                                               
         MVI   FCRDTRNS,C'N'       NO TRANSACTIONS                              
         MVI   FCRDACC,C'N'        JUST ACCOUNTS                                
         MVI   FORCEHED,C'Y'                                                    
         L     R2,ADLEDGER                                                      
         CLC   1(2,R2),=C'SJ'      FOR SJ                                       
         BE    XIT                                                              
         MVI   FCRDACC,C'Y'                                                     
         ZAP   CHAREC,=P'0'                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              PROCACC                                                *         
***********************************************************************         
*                                                                               
PACC     DS    0H                                                               
         L     R3,ADACC                                                         
*                                                                               
         USING ACTRECD,R3                                                       
         TM    ACCOSTAT(R3),ACTSDELT TEST DELETED RECORD                        
         BZ    XIT                                                              
         BAS   RE,DMPGET                                                        
*                                                                               
         MVC   P+1(14),ACTKULA                                                  
         GOTO1 ACREPORT                                                         
         NI    ACCOSTAT(R3),ALL-ACTSDELT                                        
         AP    CHAREC,=P'1'                                                     
         BAS   RE,DMPPUT                                                        
         CLI   RCWRITE,C'N'                                                     
         BE    XIT                                                              
         MVI   MODE,WRITACC                                                     
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              LEDGER LAST                                            *         
***********************************************************************         
*                                                                               
LDGL     DS    0H                                                               
         L     R3,ADACC                                                         
         USING ACTRECD,R3                                                       
         GOTO1 ACREPORT                                                         
         MVC   P(7),=C'LEDGER '                                                 
         MVC   P+8(1),ACTKLDG                                                   
         MVC   P+10(6),=C'TOTAL '                                               
         EDIT  (P6,CHAREC),(14,P+20),MINUS=YES                                  
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              DUMP OUT RECORDS                                       *         
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
         LA    R6,=C'GET'                                                       
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT'                                                       
*                                                                               
DUMP     CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         SR    R8,R8                                                            
         ICM   R8,3,ACCORLEN(R3)                                                
         GOTO1 PRNTBL,DMCB,(3,(R6)),(R3),C'DUMP',(R8),=C'2D'                    
XIT      XIT1                                                                   
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CONSTANTS                                                              
*-------------------------------------------------------------------*           
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
*                                                                               
CHAREC   DC    PL6'0'                                                           
*                                                                               
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'50'                                                          
         EJECT                                                                  
ACZ7D    DSECT                                                                  
ALL      EQU   X'FF'                                                            
PARM     DS    6F                                                               
TODAY2   DS    CL2                                                              
DMPSW    DS    CL1                                                              
ELCODE   DS    CL1                                                              
IO       DS    CL1000                                                           
IOB      DS    CL1000                                                           
         EJECT                                                                  
*  ACREPWORKD                                                                   
*  ACGENFILE                                                                    
*  ACGENBOTH                                                                    
*  ACGENMODES                                                                   
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPZ702 08/16/00'                                      
         END                                                                    
