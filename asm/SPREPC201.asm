*          DATA SET SPREPC201  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPC201A                                                                  
         TITLE 'SPC201 - CANADIAN NETWORK BUY TRANSFER'                         
SPC201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,29,C'CANADIAN NETWORK BUY TRANSFER'                           
         SSPEC H2,29,C'-----------------------------'                           
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H7,1,C'CLT'                                                      
         SSPEC H7,9,C'PRD'                                                      
         SSPEC H7,17,C'EST'                                                     
         SSPEC H7,25,C'MARKET'                                                  
         SSPEC H7,34,C'STATION'                                                 
         SSPEC H7,43,C'LINE'                                                    
         SSPEC H8,1,C'---'                                                      
         SSPEC H8,9,C'---'                                                      
         SSPEC H8,17,C'---'                                                     
         SSPEC H8,25,C'------'                                                  
         SSPEC H8,34,C'-------'                                                 
         SSPEC H8,43,C'----'                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPC201 08/29/00'                                      
         END                                                                    
