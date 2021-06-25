*          DATA SET ACREPCN01  AT LEVEL 013 AS OF 06/04/93                      
*PHASE ACCN01A,+0                                                               
         TITLE 'NEW PERSON RECORD REPORT'                                       
ACCN01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN,WIDE=198                                                
         ACDEF H1,136,PAGE,WIDE=198                                             
         ACDEF H2,1,C'COMPANY',WIDE=198                                         
         ACDEF H3,136,C'REPORT ACCN',WIDE=198                                   
         ACDEF H4,1,C'OFFICE',WIDE=198                                          
         ACDEF H4,136,REQUESTOR,WIDE=198                                        
*                                                                               
*              REGULAR REPORT                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,62,C'PERSON RECORD REPORT',WIDE=198                           
         ACDEF H2,62,C'--------------------',WIDE=198                           
         ACDEF H9,2,C'CODE',WIDE=198                                            
         ACDEF H8,18,C'FILT',WIDE=198                                           
         ACDEF H9,18,C'12345',WIDE=198                                          
         ACDEF H9,25,C'NAME',WIDE=198                                           
         ACDEF H9,63,C'LOC',WIDE=198                                            
         ACDEF H8,70,C'HIRE ',WIDE=198                                          
         ACDEF H9,70,C'DATE ',WIDE=198                                          
         ACDEF H8,80,C'TERM ',WIDE=198                                          
         ACDEF H9,80,C'DATE ',WIDE=198                                          
         ACDEF H6,90,C'S  E  P      ',WIDE=198                                  
         ACDEF H7,90,C'T  X  R  J  A',WIDE=198                                  
         ACDEF H8,90,C'A  E  O  O  C',WIDE=198                                  
         ACDEF H9,90,C'T  C  D  B  T',WIDE=198                                  
         ACDEF H8,105,C'LOCK ',WIDE=198                                         
         ACDEF H9,105,C'DATE ',WIDE=198                                         
         ACDEF H7,115,C'LOCATION',WIDE=198                                      
         ACDEF H8,115,C'START',WIDE=198                                         
         ACDEF H9,115,C'DATE',WIDE=198                                          
         ACDEF H7,125,C'LOCATION',WIDE=198                                      
         ACDEF H8,125,C'END',WIDE=198                                           
         ACDEF H9,125,C'DATE',WIDE=198                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPCN01 06/04/93'                                      
         END                                                                    
