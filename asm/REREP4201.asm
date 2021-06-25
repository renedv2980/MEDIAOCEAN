*          DATA SET REREP4201  AT LEVEL 010 AS OF 08/31/00                      
*          DATA SET REREP4201  AT LEVEL 009 AS OF 06/04/80                      
*PHASE RE4201A                                                                  
         TITLE 'SPECS FOR SALESMAN SUMMARY'                                     
RE4201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         RSPEC MAXLINES,56                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC F1,1,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'SALESPERSON SUMMARY'                                     
         ASPEC H2,48,19C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,OFFICE                                                      
         ASPEC H4,46,PERIOD                                                     
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H5,46,BASIS                                                      
         ASPEC H7,46,DIVISION                                                   
         ASPEC H8,46,TEAM                                                       
         ASPEC H10,2,C'INIT      SALESPERSON NAME'                              
         ASPEC H11,2,C'----      ----------------'                              
         ASPEC H10,105,C'TOTAL'                                                 
         ASPEC H11,105,C'-----'                                                 
         SPACE 1                                                                
         SPROG 2                                                                
         ASPEC F1,1,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REREP4201 08/31/00'                                      
         END                                                                    
