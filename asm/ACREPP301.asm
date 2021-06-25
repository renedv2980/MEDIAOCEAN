*          DATA SET ACREPP301  AT LEVEL 007 AS OF 04/27/07                      
*PHASE ACP301A                                                                  
         TITLE 'SPECS FOR ORDER LIST'                                           
ACP301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ORDERS                                                      
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,98,PAGE                                                       
         ASPEC H1,45,C'UNMATCHED ORDER REPORT'                                  
         ASPEC H2,45,22C'-'                                                     
         ASPEC H2,85,REQUESTOR                                                  
         ASPEC H3,2,COMPANY                                                     
         ASPEC H6,36,C'JOB OR EXP. A/C'                                         
         ASPEC H7,36,C'---------------'                                         
         ASPEC H6,52,C'DUE DATE'                                                
         ASPEC H7,52,C'--------'                                                
         ASPEC H6,61,C'WC ORD DATE'                                             
         ASPEC H7,61,C'-- --------'                                             
         ASPEC H6,73,C'AUTHORIZER.....'                                         
         ASPEC H7,73,C'---------------'                                         
         ASPEC H6,89,C'     ORD-AMOUNT     INV-TO-DATE'                         
         ASPEC H7,89,C'--------------- ---------------'                         
                                                                                
         SPROG 0                                                                
         ASPEC H6,2,C'ORDER# SUPPLIER CODE AND NAME'                            
         ASPEC H7,2,C'------ ----------------------'                            
                                                                                
         SPROG 1                                                                
         ASPEC H6,2,C'SUPPLIER CODE AND NAME'                                   
         ASPEC H7,2,22C'-'                                                      
         ASPEC H6,28,C'ORDER#'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPP301 04/27/07'                                      
         END                                                                    
