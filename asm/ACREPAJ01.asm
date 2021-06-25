*          DATA SET ACREPAJ01  AT LEVEL 036 AS OF 03/01/12                      
*PHASE ACAJ01A,+0                                                               
         PRINT NOGEN                                                            
         TITLE 'SPECS FOR AUTO-JOURNAL'                                         
ACAJ01   CSECT                                                                  
*                                                                               
         ACDEF WIDTH,198                                                        
         SPROG 0,1,2                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,123,REPORT                                                    
         ACDEF H1,141,PAGE                                                      
         ACDEF H3,2,C'COMPANY'                                                  
*                                                                               
         SPROG 0                                                                
         ACDEF H6,2,C'REF '                                                     
         ACDEF H7,2,C'----'                                                     
         ACDEF H6,9,C'DATE'                                                     
         ACDEF H7,9,C'----'                                                     
         ACDEF H6,19,C'ACCOUNT NAME'                                            
         ACDEF H7,19,C'------------'                                            
         ACDEF H6,56,C'ACCOUNT NUMBER'                                          
         ACDEF H7,56,C'--------------'                                          
         ACDEF H6,71,C'OFF CODE'                                                
         ACDEF H7,71,C'--- ----'                                                
         ACDEF H6,92,C'DEBIT             CREDIT'                                
         ACDEF H7,92,C'-----             ------'                                
         ACDEF H6,122,C'         NARRATIVE            '                         
         ACDEF H7,122,C'------------------------------'                         
*                                                                               
         SPROG 1                                                                
         ACDEF H6,2,C'THE FOLLOWING RECORDS HAVE INVALID DATA'                  
         ACDEF H9,2,C'REF'                                                      
         ACDEF H10,2,C'---'                                                     
         ACDEF H9,9,C'DATE'                                                     
         ACDEF H10,9,C'----'                                                    
         ACDEF H9,19,C'CLIENT PRODUCT JOB   '                                   
         ACDEF H10,19,C'------ ------- ------'                                  
         ACDEF H9,56,C'VENDOR      '                                            
         ACDEF H10,56,C'------'                                                 
         ACDEF H9,71,C'CODE'                                                    
         ACDEF H10,71,C'----'                                                   
         ACDEF H9,94,C'AMOUNT'                                                  
         ACDEF H10,94,C'------'                                                 
         ACDEF H9,114,C'         NARRATIVE            '                         
         ACDEF H10,114,C'------------------------------'                        
*                                                                               
         SPROG 2                                                                
         ACDEF H6,45,C'DDS CONTROL SHEET'                                       
         ACDEF H7,45,C'-----------------'                                       
         ACDEF H9,30,C'DEBITS          CREDITS       RECORDS'                   
         ACDEF H10,30,C'------          -------       -------'                  
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036ACREPAJ01 03/01/12'                                      
         END                                                                    
