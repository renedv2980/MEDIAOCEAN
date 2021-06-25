*          DATA SET SPREPSU01  AT LEVEL 006 AS OF 07/06/07                      
*PHASE SPSU01A                                                                  
         TITLE 'SPSU01 - SUPERDESK OVERNIGHT UPDATE'                            
SPSU01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,36,C'SUPERDESK OVERNIGHT UPDATE REPORT'                       
         SSPEC H2,36,C'---------------------------------'                       
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H7,1,C'M'                                                        
         SSPEC H7,3,C'CLT'                                                      
         SSPEC H7,7,C'PRD-PTR'                                                  
         SSPEC H7,15,C'EST'                                                     
         SSPEC H7,19,C'MKT'                                                     
         SSPEC H7,24,C'STAT'                                                    
         SSPEC H7,30,C'FLIGHT DATES'                                            
         SSPEC H7,48,C'NWS XFR'                                                 
         SSPEC H7,57,C'WRK ADD'                                                 
         SSPEC H7,67,C'ORD SENT'                                                
         SSPEC H7,76,C'ORD CONF'                                                
         SSPEC H8,1,C'-'                                                        
         SSPEC H8,3,C'---'                                                      
         SSPEC H8,7,C'-------'                                                  
         SSPEC H8,15,C'---'                                                     
         SSPEC H8,19,C'----'                                                    
         SSPEC H8,24,C'-----'                                                   
         SSPEC H8,30,C'-----------------'                                       
         SSPEC H8,48,C'--------'                                                
         SSPEC H8,57,C'--------'                                                
         SSPEC H8,67,C'--------'                                                
         SSPEC H8,76,C'--------'                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPSU01 07/06/07'                                      
         END                                                                    
