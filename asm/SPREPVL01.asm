*          DATA SET SPREPVL01  AT LEVEL 011 AS OF 01/28/21                      
*PHASE SPVL01A,+0                                                               
         TITLE 'SPVL01 - EDI VENDOR LEVEL TAPE'                                 
SPVL01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPVL03                                                       
*                                                                               
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         SPROG 1,4                                                              
         SSPEC H1,47,CL29'SPOT VENDOR BILLING INTERFACE'                        
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,47,29C'-'                                                     
*                                                                               
         SPROG 2                                                                
         SSPEC H1,47,CL28'NET VENDOR BILLING INTERFACE'                         
         SSPEC H1,95,AGYNAME                                                    
         SSPEC H2,47,28C'-'                                                     
*                                                                               
         SPROG 1,2,4                                                            
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
         SPROG 1,2                                                              
         PSPEC H6,85,C'STATION     VENDOR    '                                  
         PSPEC H7,85,C'            AMOUNT    '                                  
         PSPEC H8,85,C'------- --------------'                                  
*                                                                               
         SPROG 4                                                                
         PSPEC H6,85,C'  TAX      AGENCY  '                                     
         PSPEC H7,85,C' AMOUNT  COMMISSION'                                     
         PSPEC H8,85,C'-------- ----------'                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPVL01 01/28/21'                                      
         END                                                                    
