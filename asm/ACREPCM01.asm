*          DATA SET ACREPCM01  AT LEVEL 006 AS OF 08/16/00                      
*PHASE ACCM01A,+0                                                               
         TITLE 'ACCM - MISSING TIMESHEET - SPECS'                               
ACCM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
*                                                                               
         SPROG 0,1,2,3,4                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,55,C'MISSING TIMESHEET REPORT'                                
         ACDEF H2,55,C'------------------------'                                
         ACDEF H3,56,PERIOD                                                     
         ACDEF H4,2,COMPANY                                                     
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H2,101,REQUESTOR                                                 
*                                                                               
         ACDEF H11,2,C'NAME'                                                    
         ACDEF H12,2,C'----'                                                    
         ACDEF H10,33,C'          TOTAL   *- - - - - - - - -  T I M E '         
         ACDEF H10,79,C'S H E E T S   M I S S I N G  - - - - - - - - -'         
         ACDEF H10,125,C' - *'                                                  
         ACDEF H11,33,C'EMP. #   MISSING'                                       
         ACDEF H12,33,C'------   -------'                                       
         ACDEF H11,51,C'(ST) #  WEEK DATES'                                     
         ACDEF H12,51,C'------------------'                                     
         ACDEF H11,71,C'(ST) #  WEEK DATES'                                     
         ACDEF H12,71,C'------------------'                                     
         ACDEF H11,91,C'(ST) #  WEEK DATES'                                     
         ACDEF H12,91,C'------------------'                                     
         ACDEF H11,111,C'(ST) #  WEEK DATES'                                    
         ACDEF H12,111,C'------------------'                                    
*                                                                               
         SPROG 2                                                                
         ACDEF H5,54,C'*** FLAGGED ITEMS ONLY ***'                              
*                                                                               
         SPROG 0,2                                                              
         ACDEF H6,101,C'**FLAGS**  (H)= PRE HIRE '                              
         ACDEF H7,101,C'           (T)= POST TERM'                              
         ACDEF H8,101,C'           (Z)= ZERO HRS '                              
         ACDEF H9,101,C'           (L)= OUTSIDE LOC'                            
*                                                                               
         SPROG 3                                                                
         ACDEF H6,101,C'**FLAGS**  (H)= PRE HIRE '                              
         ACDEF H7,101,C'           (T)= POST TERM'                              
         ACDEF H8,101,C'           (L)= OUTSIDE LOC'                            
*                                                                               
         SPROG 4                                                                
         ACDEF H6,101,C'**FLAGS**  (Z)= ZERO HRS '                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPCM01 08/16/00'                                      
         END                                                                    
