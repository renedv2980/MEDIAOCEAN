*          DATA SET ACREPPI01  AT LEVEL 001 AS OF 03/15/02                      
*PHASE ACPI01A,+0                                                               
         TITLE 'ACPI - POSPAY/A57 TABLE INFO REPORT'                            
ACPI01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         SPROG 1,2                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,2,ORIGIN                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H1,45,C'POSPAY/A57 TABLE ENTRY INFORMATION'                      
         ACDEF H2,45,C'----------------------------------'                      
         ACDEF H8,2,C'CMP'                                                      
         ACDEF H10,2,C'---'                                                     
         ACDEF H8,7,C'DDS CASH/SC'                                              
         ACDEF H9,7,C' ACCOUNT   '                                              
         ACDEF H10,7,C'-----------'                                             
         ACDEF H8,20,C'  BANK ACCOUNT      '                                    
         ACDEF H10,20,C'--------------------'                                   
         ACDEF H8,42,C'EYE CATCHER'                                             
         ACDEF H10,42,C'-----------'                                            
         ACDEF H8,55,C'POSPAY'                                                  
         ACDEF H9,55,C' (Y/N)'                                                  
         ACDEF H10,55,C'------'                                                 
         ACDEF H8,63,C' BLOCK'                                                  
         ACDEF H9,63,C' SIZE '                                                  
         ACDEF H10,63,C'------'                                                 
         ACDEF H8,71,C' RECORD '                                                
         ACDEF H9,71,C'  SIZE  '                                                
         ACDEF H10,71,C'--------'                                               
         ACDEF H8,82,C'DATA SET NAME'                                           
         ACDEF H10,82,C'-------------'                                          
         ACDEF H8,110,C'ROUTINE/BANK NAME'                                      
         ACDEF H10,110,C'-----------------'                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPPI01 03/15/02'                                      
         END                                                                    
