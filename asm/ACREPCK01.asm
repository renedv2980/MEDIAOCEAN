*          DATA SET ACREPCK01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACCK01A,+0                                                               
         TITLE 'ACCK01 - SPECS FOR CLEARANCE CHECK REPORT'                      
ACCK01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF GETOPT,N                                                         
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,60,C'CLEARANCE CHECK REPORT'                                  
         ACDEF H2,60,C'----------------------'                                  
         ACDEF H1,117,REPORT                                                    
         ACDEF H2,117,PAGE                                                      
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H5,031,C'  CLEARANCES  '                                         
         ACDEF H6,031,C'--------------'                                         
         ACDEF H4,048,C'     CASH     '                                         
         ACDEF H5,048,C'   RECEIPTS   '                                         
         ACDEF H6,048,C'--------------'                                         
         ACDEF H5,065,C' CASHPAK TOTAL'                                         
         ACDEF H6,065,C'--------------'                                         
         ACDEF H6,082,C'--------------'                                         
         ACDEF H5,099,C'  DIFFERENCE  '                                         
         ACDEF H6,099,C'--------------'                                         
         ACDEF H4,116,C'  COMMISSION  '                                         
         ACDEF H5,116,C'     ONLY     '                                         
         ACDEF H6,116,C'--------------'                                         
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H5,014,C'TOTAL CLEARANCES'                                       
         ACDEF H6,014,C'----------------'                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPCK01 08/16/00'                                      
         END                                                                    
