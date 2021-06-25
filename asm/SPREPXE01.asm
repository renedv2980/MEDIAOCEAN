*          DATA SET SPREPXE01  AT LEVEL 007 AS OF 08/29/00                      
*PHASE SPXE01A                                                                  
         TITLE 'SPREPXE01 - SPTFILE CLOSEOUT CONTROL'                           
         PRINT NOGEN                                                            
SPXE01   CSECT                                                                  
***********************************************************************         
         FSPEC USE,SP0003                                                       
         SPROG 0                                                                
         SSPEC H1,43,C'SPTFILE CLOSEOUT CONTROL'                                
*                                                                               
         SSPEC H2,5,REQUESTOR                                                   
         SSPEC H2,43,C'------------------------'                                
         SSPEC H2,85,RUN                                                        
         SSPEC H2,84,REPORT                                                     
*                                                                               
         SSPEC H4,100,PAGE                                                      
*                                                                               
         SSPEC H9,5,C'MEDIA'                                                    
         SSPEC H10,5,C'-----'                                                   
         SSPEC H9,14,C'CLIENT'                                                  
         SSPEC H10,14,C'------'                                                 
         SSPEC H9,39,C'PRODUCT'                                                 
         SSPEC H10,39,C'-------'                                                
         SSPEC H9,64,C'ESTIMATE'                                                
         SSPEC H10,64,C'--------'                                               
         SSPEC H8,92,C'START'                                                   
         SSPEC H9,92,C'DATE'                                                    
         SSPEC H10,92,C'----'                                                   
         SSPEC H8,104,C'END'                                                    
         SSPEC H9,104,C'DATE'                                                   
         SSPEC H10,104,C'----'                                                  
*                                                                               
         SPROG 1                                                                
         SSPEC H1,43,C'SPOTPAK AGENCY LIST -'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,43,C'------------------------'                                
         SSPEC H2,85,RUN                                                        
         SSPEC H2,84,REPORT                                                     
*                                                                               
         SSPEC H4,100,PAGE                                                      
*                                                                               
         SSPEC H8,3,C'AGY ALPHA CODE'                                           
         SSPEC H9,3,C'--------------'                                           
         SSPEC H8,21,C'AGY NUM CODE'                                            
         SSPEC H9,21,C'------------'                                            
         SSPEC H8,41,C'AGENCY NAME'                                             
         SSPEC H9,41,C'-----------'                                             
         SSPEC H8,81,C'AGENCY ADDRESS'                                          
         SSPEC H9,81,C'--------------'                                          
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPXE01 08/29/00'                                      
         END                                                                    
