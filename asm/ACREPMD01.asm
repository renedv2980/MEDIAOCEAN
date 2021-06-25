*          DATA SET ACREPMD01  AT LEVEL 024 AS OF 02/10/21                      
*PHASE ACMD01A                                                                  
         TITLE 'SCAN FOR ORPHANED MEDIA POSTING RECORDS '                       
ACMD01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,002,C'CC',2,L                                                 
         ACDEF H2,002,C'--',2,L                                                 
         ACDEF H1,005,C'SMCLIPROESTM',12,L                                      
         ACDEF H2,005,C'------------',12,L                                      
         ACDEF H1,019,C'MOS ',04,L                                              
         ACDEF H2,019,C'----',04,L                                              
         ACDEF H1,025,C'INVOICE ',08,L                                          
         ACDEF H2,025,C'--------',08,L                                          
         ACDEF H1,035,C'SR ACCOUNT     ',15,L                                   
         ACDEF H2,035,C'---------------',15,L                                   
         ACDEF H1,051,C'SR CONTRA ACC  ',15,L                                   
         ACDEF H2,051,C'---------------',15,L                                   
         ACDEF H1,067,C'OFF CODE',8,L                                           
         ACDEF H2,067,C'--------',8,L                                           
         ACDEF H1,076,C'DA       ',08,L                                         
         ACDEF H2,076,C'---------',08,L                                         
         ACDEF H1,086,C'SR ACC FOUND',12,L                                      
         ACDEF H2,086,C'------------',12,L                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024ACREPMD01 02/10/21'                                      
         END                                                                    
