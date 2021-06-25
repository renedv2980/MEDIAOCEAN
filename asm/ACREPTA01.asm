*          DATA SET ACREPTA01  AT LEVEL 001 AS OF 06/28/01                      
*PHASE ACTA01A,+0                                                               
         TITLE 'ACTA - TIMESHEET AUDIT REPORT'                                  
ACTA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         SPROG 0,1,2                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,C'TIMESHEET AUDIT REPORT'                                  
         ACDEF H2,86,C'----------------------'                                  
         ACDEF H3,87,PERIOD                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H1,160,REPORT                                                    
         ACDEF H1,180,PAGE                                                      
         ACDEF H2,160,REQUESTOR                                                 
*                                                                               
         SPROG 0,1                                                              
         ACDEF H10,3,C'PERSON'                                                  
         ACDEF H10,12,C'PERIOD  '                                               
         ACDEF H11,12,C'END DATE'                                               
         ACDEF H10,22,C'TEMPO'                                                  
         ACDEF H11,22,C'LINE#'                                                  
         ACDEF H10,28,C'TMS  '                                                  
         ACDEF H11,28,C'LINE#'                                                  
         ACDEF H10,35,C' HEADER INFO BUCKETS (TSX-BD)  '                        
         ACDEF H12,35,C'   B       R       N      N/C  '                        
         ACDEF H10,67,C'   TIME INFO BUCKETS (TIM-8B)  '                        
         ACDEF H12,67,C'   B       R       N      N/C  '                        
         ACDEF H10,99,C' DETAIL INFO BUCKETS(TSI-BF01) '                        
         ACDEF H12,99,C'   B       R       N      N/C  '                        
         ACDEF H10,131,C' ALLOCATION HOURS (TSI-BF03)   '                       
         ACDEF H12,131,C'   B       R       N      N/C  '                       
         ACDEF H10,161,C'   LIST OF ERRORS    '                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPTA01 06/28/01'                                      
         END                                                                    
