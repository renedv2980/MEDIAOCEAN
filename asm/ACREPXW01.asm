*          DATA SET ACREPXW01  AT LEVEL 004 AS OF 02/24/20                      
*PHASE ACXW01B                                                                  
         TITLE 'ACXW01 - SPECS FOR MEDIA/WORKCODE RECORD PURGE'                 
ACXW01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
         SPACE 1                                                                
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H3,2,COMPANY                                                     
         ASPEC H4,2,UNIT                                                        
         ASPEC H5,2,LEDGER                                                      
         ASPEC H1,83,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H3,83,REQUESTOR                                                  
         ASPEC F1,2,REQDETS                                                     
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,46,C'MEDIA RECORD PURGE'                                      
         ASPEC H2,46,C'------------------'                                      
         ASPEC H8,1,C'MEDIA CODE'                                               
         ASPEC H9,1,C'----------'                                               
         ASPEC H8,12,C'MEDIA NAME'                                              
         ASPEC H9,12,C'----------'                                              
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H1,45,C'WORKCODE RECORD PURGE'                                   
         ASPEC H2,45,C'---------------------'                                   
         ASPEC H8,1,C'WORKCODE'                                                 
         ASPEC H9,1,C'--------'                                                 
         ASPEC H8,11,C'WORKCODE NAME'                                           
         ASPEC H9,11,C'-------------'                                           
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC H1,38,C'MEDIA/WORKCODE RECORD PURGE'                             
         ASPEC H2,38,C'---------------------------'                             
         ASPEC H6,38,C'*******CONTROL SHEET*******'                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPXW01 02/24/20'                                      
         END                                                                    
