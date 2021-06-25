*          DATA SET ACREPBD01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACBD01A,+0                                                               
         TITLE 'BBDO - BALANCE RESTORE'                                         
ACBD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,133,PAGE                                                      
         ACDEF H4,133,C'REPORT ACBD'                                            
         ACDEF H5,133,REQUESTOR                                                 
*                                                                               
         ACDEF H2,60,C'BALANCE RESTORE REPORT'                                  
         ACDEF H3,60,C'----------------------'                                  
         ACDEF H5,02,C'ACCOUNT:'                                                
         ACDEF H6,02,C'NAME:'                                                   
*                                                                               
         ACDEF H08,02,C'OFFICE'                                                 
         ACDEF H08,09,C'CONTRA'                                                 
         ACDEF H08,24,C'CONTRA NAME'                                            
         ACDEF H08,061,C'    BALANCE'                                           
         ACDEF H09,061,C'    FORWARD'                                           
         ACDEF H08,079,C'   ACTIVITY'                                           
         ACDEF H09,079,C'  THRU 12/95'                                          
         ACDEF H08,097,C'   12/95   '                                           
         ACDEF H09,097,C' ADJUSMENTS'                                           
         ACDEF H08,115,C'      NEW  '                                           
         ACDEF H09,115,C'    BALANCE'                                           
         ACDEF H08,133,C'      OLD  '                                           
         ACDEF H09,133,C'    BALANCE'                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPBD01 08/16/00'                                      
         END                                                                    
