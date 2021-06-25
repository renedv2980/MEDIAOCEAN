*          DATA SET ACREP5601  AT LEVEL 004 AS OF 12/13/00                      
*PHASE AC5601A                                                                  
         TITLE 'SPECS FOR CHECK REGISTER'                                       
AC5601   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF NOREP,REQDETS                                                    
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5                                                
         ACDEF H1,43,C'ACCPAK CHECK REGISTER'                                   
         ACDEF H2,43,C'---------------------'                                   
         ACDEF H1,2,RUN                                                         
         ACDEF H1,93,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,93,C'BANK A/C -'                                              
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H6,2,C'CHECK NO'                                                 
         ACDEF H7,2,C'--------'                                                 
         ACDEF H6,20,C'AMOUNT'                                                  
         ACDEF H7,20,C'------'                                                  
         ACDEF H6,34,C'PAYEE NO'                                                
         ACDEF H7,34,C'--------'                                                
         ACDEF H6,55,C'PAYEE NAME/ADDRESS'                                      
         ACDEF H7,55,C'------------------'                                      
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H6,2,C'TOTALS FOR RUN'                                           
         ACDEF H7,2,C'--------------'                                           
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H6,2,C'MEDIA/OFFICE TOTALS'                                      
         ACDEF H7,2,C'-------------------'                                      
         ACDEF H9,2,C'MEDIA'                                                    
         ACDEF H9,11,C'OFFICE'                                                  
         ACDEF H9,25,C'AMOUNT'                                                  
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H6,2,C'OFFICE TOTALS'                                            
         ACDEF H7,2,C'-------------'                                            
         ACDEF H9,2,C'OFFICE'                                                   
         ACDEF H9,22,C'AMOUNT'                                                  
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H4,70,C'(ACCOUNT ORDER)'                                         
         ACDEF H6,2,C'PAYEE NO'                                                 
         ACDEF H7,2,C'--------'                                                 
         ACDEF H6,22,C'CHECK NO'                                                
         ACDEF H7,22,C'--------'                                                
         ACDEF H6,42,C'AMOUNT'                                                  
         ACDEF H7,42,C'------'                                                  
         ACDEF H6,52,C'PAYEE NAME'                                              
         ACDEF H7,52,C'----------'                                              
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H6,2,C'REGISTER COMMENTS'                                        
         ACDEF H7,2,C'-----------------'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP5601 12/13/00'                                      
         END                                                                    
