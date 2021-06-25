*          DATA SET REREP7701  AT LEVEL 008 AS OF 04/02/03                      
*PHASE RE7701A,+0                                                               
         TITLE 'SPECS FOR POINT PERSON LISTING'                                 
RE7701   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'POINT PERSON LISTING'                                    
         ASPEC H2,47,19C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,C'REPRESENTATIVE'                                           
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H7,02,C'CODE'                                                    
         ASPEC H9,02,C'----'                                                    
         ASPEC H7,09,C'NAME'                                                    
         ASPEC H9,09,C'----'                                                    
         ASPEC H7,27,C'EDI?'                                                    
         ASPEC H9,27,C'----'                                                    
         ASPEC H7,33,C'TELEPHONE'                                               
         ASPEC H9,33,C'---------'                                               
         ASPEC H7,54,C'REP'                                                     
         ASPEC H9,54,C'---'                                                     
         ASPEC H7,58,C'OFF'                                                     
         ASPEC H9,58,C'---'                                                     
         ASPEC H7,63,C'S/P'                                                     
         ASPEC H8,63,C'CODE'                                                    
         ASPEC H9,63,C'----'                                                    
         ASPEC H7,69,C'SALESPERSON'                                             
         ASPEC H9,69,C'-----------'                                             
         ASPEC H7,86,C'LEAVE DATE'                                              
         ASPEC H8,86,C'EMAIL ADDRESS'                                           
         ASPEC H9,86,C'-------------'                                           
         ASPEC H7,108,C'FAX#'                                                   
         ASPEC H9,108,C'----'                                                   
         ASPEC H7,128,C'FAX?'                                                   
         ASPEC H9,128,C'----'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREP7701 04/02/03'                                      
         END                                                                    
