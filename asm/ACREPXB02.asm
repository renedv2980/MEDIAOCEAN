*          DATA SET ACREPXB02  AT LEVEL 010 AS OF 05/20/14                      
*PHASE ACXB02A                                                                  
         TITLE 'FIX ACCOUNT BALANCES'                                           
ACXB02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**XB02**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXBD,RC                                                         
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   ACC2                                                             
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   BALDR,=P'0'                                                      
         ZAP   BALCR,=P'0'                                                      
         ZAP   TRNDR,=P'0'                                                      
         ZAP   TRNCR,=P'0'                                                      
         B     ACCX                                                             
*                                                                               
ACC2     CLI   MODE,LEDGFRST                                                    
         BNE   ACC3                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   LDGDR,=P'0'                                                      
         ZAP   LDGCR,=P'0'                                                      
         ZAP   BLEDR,=P'0'                                                      
         ZAP   BLECR,=P'0'                                                      
         B     ACCX                                                             
*                                                                               
ACC3     CLI   MODE,PROCACC                                                     
         BNE   ACC4                                                             
         ZAP   ACCDR,=P'0'                                                      
         ZAP   ACCCR,=P'0'                                                      
         B     ACCX                                                             
*                                                                               
ACC4     CLI   MODE,PROCTRNS                                                    
         BNE   ACC6                                                             
         L     R2,ADTRANS                                                       
         LR    R3,R2                                                            
         SH    R3,DATADISP                                                      
         USING TRNRECD,R3                                                       
         TM    TRNRSTAT,TRNSDELT                                                
         BO    ACCX                                                             
         DROP  R3                                                               
         USING TRNELD,R2                                                        
         CLI   TRNEL,TRNELQ        TEST TRANSACTION                             
         BNE   ACCX                                                             
         LA    R1,TRNELD                                                        
         SH    R1,=Y(ACRECORD-ACKEYD)                                           
         USING ACKEYD,R1                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   ACCX                                                             
         DROP  R1                                                               
         LA    RE,ACCDR                                                         
         TM    TRNSTAT,TRNSDR                                                   
         BNZ   *+8                                                              
         LA    RE,ACCCR                                                         
         AP    0(L'ACCDR,RE),TRNAMNT                                            
         B     ACCX                                                             
*                                                                               
ACC6     CLI   MODE,ACCLAST                                                     
         BNE   ACC17                                                            
         ICM   R2,15,ADACCBAL                                                   
         BZ    ACCX                                                             
         MVI   FLAG,0                                                           
         USING ABLELD,R2                                                        
         CP    ABLDR,ACCDR         TEST BALANCE DIFFERENCE                      
         BNE   *+14                                                             
         CP    ABLCR,ACCCR                                                      
         BE    ACCX                                                             
         CLI   QOPT1,C'Y'                                                       
         BNE   ACC8                                                             
         ZAP   DUB,ACCDR                                                        
         SP    DUB,ABLDR                                                        
         SP    DUB,ACCCR                                                        
         AP    DUB,ABLCR                                                        
         CP    DUB,ABLFRWD                                                      
         BNE   *+14                                                             
         ZAP   ABLFRWD,=P'0'                                                    
         B     ACC8                                                             
         MVI   FLAG,1                                                           
*                                                                               
ACC8     L     R1,ADACC                                                         
         MVC   P+1(L'ACTKACT),ACTKACT-ACTRECD(R1)                               
         AH    R1,=Y(ACRECORD-ACKEYD)                                           
         USING NAMELD,R1                                                        
         SR    R0,R0                                                            
ACC10    CLI   NAMEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   NAMEL,NAMELQ                                                     
         BE    *+14                                                             
         IC    R0,NAMLN                                                         
         AR    R1,R0                                                            
         B     ACC10                                                            
         SR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SH    RE,=Y(NAMLN1Q+1)                                                 
         LA    RF,P+1+L'ACTKACT-1                                               
         CLI   0(RF),C' '                                                       
         BNE   *+8                                                              
         BCT   RF,*-8                                                           
         EX    RE,*+4                                                           
         MVC   2(0,RF),NAMEREC                                                  
         DROP  R1                                                               
         CURED (P8,ABLDR),(15,P+51),2,MINUS=YES                                 
         CURED (P8,ABLCR),(15,P+66),2,MINUS=YES                                 
         CURED (P8,ACCDR),(15,P+81),2,MINUS=YES                                 
         CURED (P8,ACCCR),(15,P+96),2,MINUS=YES                                 
         AP    BALDR,ABLDR                                                      
         AP    BALCR,ABLCR                                                      
         AP    BLEDR,ABLDR                                                      
         AP    BLECR,ABLCR                                                      
         AP    TRNDR,ACCDR                                                      
         AP    TRNCR,ACCCR                                                      
         AP    LDGDR,ACCDR                                                      
         AP    LDGCR,ACCCR                                                      
         CLI   FLAG,0                                                           
         BE    ACC12                                                            
         MVC   P+106(20),=C'** Not in balance **'                               
*                                                                               
ACC12    GOTO1 ACREPORT                                                         
         ZAP   ABLDR,ACCDR                                                      
         ZAP   ABLCR,ACCCR                                                      
*                                                                               
ACC14    CLI   RCWRITE,C'N'                                                     
         BE    *+8                                                              
         MVI   MODE,WRITACC                                                     
         B     ACCX                                                             
*                                                                               
ACC17    CLI   MODE,LEDGLAST                                                    
         BNE   ACC18                                                            
         MVC   P+1(14),=CL14'Ledger Totals'                                     
         CP    LDGDR,=P'0'         Any Balances?                                
         BNE   ACC17A                                                           
         CP    LDGCR,=P'0'         Any Balances?                                
         BE    ACCX                                                             
ACC17A   CURED (P8,BALDR),(15,P+51),2,MINUS=YES                                 
         CURED (P8,BALCR),(15,P+66),2,MINUS=YES                                 
         CURED (P8,LDGDR),(15,P+81),2,MINUS=YES                                 
         CURED (P8,LDGCR),(15,P+96),2,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
                                                                                
ACC18    CLI   MODE,REQLAST                                                     
         BNE   ACCX                                                             
         MVC   P+1(14),=CL14'Request totals'                                    
         CURED (P8,BALDR),(15,P+51),2,MINUS=YES                                 
         CURED (P8,BALCR),(15,P+66),2,MINUS=YES                                 
         CURED (P8,TRNDR),(15,P+81),2,MINUS=YES                                 
         CURED (P8,TRNCR),(15,P+96),2,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
*                                                                               
ACCX     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
ACXBD    DSECT                     ** DSECT TO COVER SPACEND **                 
ACCDR    DS    PL8                 ACCOUNT TOTAL DEBITS                         
ACCCR    DS    PL8                 ACCOUNT TOTAL CREDITS                        
LDGDR    DS    PL8                 Ledger Total Debits                          
LDGCR    DS    PL8                 Ledger Total Credits                         
BALDR    DS    PL8                 BALANCE DEBITS                               
BALCR    DS    PL8                 BALANCE CREDITS                              
BLEDR    DS    PL8                 Ledger Balance Debits                        
BLECR    DS    PL8                 Ledger Balance Credits                       
TRNDR    DS    PL8                 TRANSACTION DEBITS                           
TRNCR    DS    PL8                 TRANSACTION CREDITS                          
FLAG     DS    XL1                 FLAG BYTE                                    
         SPACE 2                                                                
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
*  ACGENBOTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPXB02 05/20/14'                                      
         END                                                                    
