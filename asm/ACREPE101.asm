*          DATA SET ACREPE101  AT LEVEL 009 AS OF 03/22/90                      
*PHASE ACE101A,+0                                                               
         TITLE 'INTERAGENCY ESTIMATE STATEMENT'                                 
ACE101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         ACDEF MAXLINES,56                                                      
         ACDEF SPROG,0                                                          
*                                                                               
*        LEFT HEADLINES                                                         
*                                                                               
         ACDEF H1,2,RUN,WIDE=198                                                
         ACDEF H3,2,C'A.O.R.:',WIDE=198                                         
         ACDEF H4,2,C'CLIENT:',WIDE=198                                         
         ACDEF H5,2,C'PRODUCT:',WIDE=198                                        
*                                                                               
*        RIGHT HEADLINES                                                        
*                                                                               
         ACDEF H1,122,PAGE,WIDE=198                                             
         ACDEF H3,122,C'REPORT ACE1',WIDE=198                                   
         ACDEF H4,122,REQUESTOR,WIDE=198                                        
         ACDEF H5,122,PERIOD,WIDE=198                                           
*                                                                               
*        REPORT TITLE                                                           
*                                                                               
         ACDEF H1,54,C'INTERAGENCY ESTIMATE STATEMENT',WIDE=198                 
         ACDEF H2,54,30C'_',WIDE=198                                            
*                                                                               
*        COLUMN HEADINGS                                                        
*                                                                               
         ACDEF H8,2,C'-------- ESTIMATE -----------',WIDE=198                   
         ACDEF H9,2,C'NUMBER',WIDE=198                                          
         ACDEF H9,20,C'DESCRIPTION',WIDE=198                                    
*                                                                               
         ACDEF H8,49,C'MEDIA',WIDE=198                                          
*                                                                               
         ACDEF H8,57,C' ADV ',WIDE=198                                          
         ACDEF H9,57,C'MONTH',WIDE=198                                          
*                                                                               
         ACDEF H8,66,C'POSTING',WIDE=198                                        
         ACDEF H9,66,C'  DATE',WIDE=198                                         
*                                                                               
         ACDEF H8,77,C'A.O.R.',WIDE=198                                         
         ACDEF H9,77,C'FEE %',WIDE=198                                          
*                                                                               
         ACDEF H8,85,C'CREATIVE',WIDE=198                                       
         ACDEF H9,85,C'   %',WIDE=198                                           
*                                                                               
         ACDEF H8,95,C'   GROSS',WIDE=198                                       
         ACDEF H9,95,C'BILLING EST',WIDE=198                                    
*                                                                               
         ACDEF H8,111,C'RECEIVABLE',WIDE=198                                    
         ACDEF H9,111,C'AMOUNT EST',WIDE=198                                    
*                                                                               
         ACDEF H8,127,C' PAID',WIDE=198                                         
         ACDEF H9,127,C'TO DATE',WIDE=198                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPE101 03/22/90'                                      
         END                                                                    
