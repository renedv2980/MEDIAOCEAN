*          DATA SET SPREPSS01  AT LEVEL 001 AS OF 10/24/03                      
*PHASE SPSS01A                                                                  
         TITLE 'SPSS01 - SUPERDESK AUTHORIZATION STATUS REPORT'                 
SPSS01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SPROG 1,2                                                              
*                                                                               
         SSPEC H1,46,C'AUTHORIZATION STATUS REPORT'                             
         SSPEC H2,46,C'---------------------------'                             
*                                                                               
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,PAGE                                                       
         SSPEC H4,108,REPORT                                                    
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,PERIOD                                                      
         SSPEC H3,46,C'QUARTER'                                                 
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H7,1,C'STATUS'                                                   
         SSPEC H8,1,C'------'                                                   
         SSPEC H7,11,C'DUE DATE'                                                
         SSPEC H8,11,C'--------'                                                
         SSPEC H7,21,C'START DT'                                                
         SSPEC H8,21,C'--------'                                                
         SSPEC H7,31,C'CLT'                                                     
         SSPEC H8,31,C'---'                                                     
         SSPEC H7,36,C'PRODUCT'                                                 
         SSPEC H8,36,C'-------'                                                 
         SSPEC H7,46,C'EST'                                                     
         SSPEC H8,46,C'-----'                                                   
         SSPEC H7,52,C'MKT#'                                                    
         SSPEC H8,52,C'----'                                                    
         SSPEC H7,57,C'MARKET NAME'                                             
         SSPEC H8,57,C'--------------------'                                    
         SSPEC H7,83,C'REV#'                                                    
         SSPEC H8,83,C'----'                                                    
         SSPEC H7,88,C'BUYER COMP'                                              
         SSPEC H8,88,C'----------'                                              
         SSPEC H7,99,C'SUPV APPR'                                               
         SSPEC H8,99,C'---------'                                               
         SSPEC H7,109,C'STATION ORD'                                            
         SSPEC H8,109,C'-----------'                                            
*                                                                               
         SPROG 2                                                                
         SSPEC H7,1,C'STATUS'                                                   
         SSPEC H8,1,C'------------------------------'                           
         SSPEC H7,38,C'MARKETS'                                                 
         SSPEC H8,38,C'-------'                                                 
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPSS01 10/24/03'                                      
         END                                                                    
