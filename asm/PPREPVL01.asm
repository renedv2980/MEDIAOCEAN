*          DATA SET PPREPVL01  AT LEVEL 013 AS OF 01/28/21                      
*PHASE PPVL01A,+0                                                               
         TITLE 'PPVL01 - EDI VENDOR LEVEL TAPE'                                 
PPVL01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         SPROG 1                                                                
         SSPEC H1,47,CL30'PRINT VENDOR BILLING INTERFACE'                       
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,47,30C'-'                                                     
*                                                                               
         SPROG 1,2                                                              
         PSPEC H2,95,AGYADD                                                     
         PSPEC H3,95,REPORT                                                     
         PSPEC H3,120,PAGE                                                      
*                                                                               
         PSPEC H6,1,C'                                        '                 
         PSPEC H7,1,C'   MED CLT      CLIENT NAME     PRD EST '                 
         PSPEC H8,1,C'   --- --- -------------------- --- --- '                 
*                                                                               
         PSPEC H6,41,C'   INV       INV     INV          INV      '             
         PSPEC H7,41,C'  NUMBER     MOS     DATE        AMOUNT    '             
         PSPEC H8,41,C'---------- ------ ---------- --------------'             
*                                                                               
         SPROG 1                                                                
         PSPEC H6,85,C'    PUB             PUB               PUB      '         
         PSPEC H7,85,C'   NUMBER           NAME             AMOUNT    '         
         PSPEC H8,85,C'----------- -------------------- --------------'         
*                                                                               
         SPROG 2                                                                
         PSPEC H6,85,C'  TAX      AGENCY  '                                     
         PSPEC H7,85,C' AMOUNT  COMMISSION'                                     
         PSPEC H8,85,C'-------- ----------'                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREPVL01 01/28/21'                                      
         END                                                                    
