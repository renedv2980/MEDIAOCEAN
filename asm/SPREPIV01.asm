*          DATA SET SPREPIV01  AT LEVEL 002 AS OF 02/16/05                      
*PHASE SPIV01A                                                                  
         TITLE 'SPIV01 - AUTOPAY INVOICE LISTING'                               
SPIV01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SPROG 1,2                                                              
*                                                                               
         SSPEC H1,58,C'PAID INVOICE LISTING'                                    
         SSPEC H2,58,C'--------------------'                                    
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H1,1,C'MEDIA CODE:'                                              
         SSPEC H2,1,C'MARKET GROUP:'                                            
*                                                                               
         SSPEC H7,1,C' STA'                                                     
         SSPEC H8,1,C'-----'                                                    
         SSPEC H7,7,C'MKT'                                                      
         SSPEC H8,7,C'----'                                                     
         SSPEC H7,12,C'CLT'                                                     
         SSPEC H8,12,C'---'                                                     
         SSPEC H7,16,C'PRD'                                                     
         SSPEC H8,16,C'---'                                                     
         SSPEC H7,20,C'PR2'                                                     
         SSPEC H8,20,C'---'                                                     
         SSPEC H7,24,C'EST'                                                     
         SSPEC H8,24,C'---'                                                     
         SSPEC H7,30,C'SREP'                                                    
         SSPEC H8,30,C'----'                                                    
         SSPEC H7,35,C' MOS'                                                    
         SSPEC H8,35,C'------'                                                  
         SSPEC H7,42,C'PD DATE'                                                 
         SSPEC H8,42,C'--------'                                                
         SSPEC H7,51,C'INVOICE NUMBER(S)'                                       
         SSPEC H8,51,C'----------------------------'                            
         SSPEC H8,80,C'---------------------------'                             
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC H7,2,C'MED'                                                      
         SSPEC H8,2,C'---'                                                      
         SSPEC H7,6,C'CLT'                                                      
         SSPEC H8,6,C'---'                                                      
         SSPEC H7,12,C'PRD'                                                     
         SSPEC H8,12,C'---'                                                     
         SSPEC H7,18,C'PRD2'                                                    
         SSPEC H8,18,C'----'                                                    
         SSPEC H7,24,C'PAK'                                                     
         SSPEC H8,24,C'---'                                                     
         SSPEC H7,30,C'EST'                                                     
         SSPEC H8,30,C'---'                                                     
         SSPEC H7,36,C'STA'                                                     
         SSPEC H8,36,C'----'                                                    
         SSPEC H7,42,C'SREP'                                                    
         SSPEC H8,42,C'----'                                                    
         SSPEC H7,48,C' MONTH  '                                                
         SSPEC H8,48,C'--------'                                                
         SSPEC H7,58,C'  PAID'                                                  
         SSPEC H8,58,C'--------'                                                
         SSPEC H7,68,C'INVOICE NUMBER'                                          
         SSPEC H8,68,C'--------------'                                          
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPIV01 02/16/05'                                      
         END                                                                    
