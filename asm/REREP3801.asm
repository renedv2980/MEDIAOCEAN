*          DATA SET REREP3801  AT LEVEL 005 AS OF 08/31/00                      
*          DATA SET REREP3801  AT LEVEL 004 AS OF 06/04/80                      
*PHASE RE3801A                                                                  
         TITLE 'SPECS FOR WEEKLY SALES REPORT'                                  
RE3801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,CONTRACTS                                                   
         RSPEC MAXLINES,56                                                      
         SPROG 0                                                                
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,2,RUN                                                         
         ASPEC H1,50,C'WEEKLY SALES REPORT'                                     
         ASPEC H2,50,19C'-'                                                     
         ASPEC H1,88,RENUM                                                      
         ASPEC H1,101,PAGE                                                      
         ASPEC H4,2,REP                                                         
         ASPEC H4,88,REQUESTOR                                                  
         ASPEC H8,92,C'OTHERS       TOTAL'                                      
         ASPEC H9,92,C'------       -----'                                      
         SPROG 1                                                                
         ASPEC F1,2,REQDETS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP3801 08/31/00'                                      
         END                                                                    
