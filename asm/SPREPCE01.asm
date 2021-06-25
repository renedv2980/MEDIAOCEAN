*          DATA SET SPREPCE01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPCE01A                                                                  
         TITLE 'SPCE01 - SEARCH RECOVERY FOR COKEAT FILE TRANSACTIONS'          
SPCE01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,38,C'COKEAT CLIENT, PRODUCT AND ESTIMATE CHANGES'             
         SSPEC H2,38,C'-------------------------------------------'             
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H7,1,C'ACTION'                                                   
         SSPEC H7,12,C'MEDIA'                                                   
         SSPEC H7,22,C'CLIENT'                                                  
         SSPEC H7,32,C'PRODUCT'                                                 
         SSPEC H7,44,C'ESTIMATE'                                                
         SSPEC H8,1,C'------'                                                   
         SSPEC H8,12,C'-----'                                                   
         SSPEC H8,22,C'------'                                                  
         SSPEC H8,32,C'-------'                                                 
         SSPEC H8,44,C'--------'                                                
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPCE01 08/29/00'                                      
         END                                                                    
