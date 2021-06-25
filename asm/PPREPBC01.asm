*          DATA SET PPREPBC01  AT LEVEL 042 AS OF 08/07/03                      
*PHASE PPBC01A,+0                                                               
         TITLE 'PPBC-01 - PRINT BILLING SUMMARY'                                
PPBC01   CSECT                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
         SPACE 1                                                                
         FSPEC READ,BILLS                                                       
         RSPEC MAXLINES,56                                                      
*                                                                               
         SPROG 10,11,30,31                                                      
         SPACE 1                                                                
         SSPEC H1,1,REQUESTOR                                                   
         SSPEC H1,99,C'DONOVAN DATA SYSTEMS, INC.'                              
         SSPEC H2,1,REPORT                                                      
         SSPEC H4,99,PAGE                                                       
         SSPEC H6,1,C'CLIENT'                                                   
         SSPEC H7,1,C'------'                                                   
         SSPEC H7,26,C'------------'                                            
         SSPEC H6,45,C'CURRENT MONTH'                                           
         SSPEC H7,45,C'-------------'                                           
         SSPEC H6,65,C'MANUAL BILLS'                                            
         SSPEC H7,65,C'------------'                                            
         SSPEC H5,83,C'CURRENT MONTH'                                           
         SSPEC H6,83,C' MANUAL BILLS'                                           
         SSPEC H7,83,C'-------------'                                           
         SPACE 2                                                                
*                                  AGENCY SUMMARY                               
         SPROG 10,11                                                            
         SPACE 1                                                                
         SSPEC H1,55,C'AGENCY BILLING SUMMARY'                                  
         SSPEC H2,55,C'----------------------'                                  
         SPACE 2                                                                
*                                  DDS SUMMARY                                  
         SPROG 30,31                                                            
         SPACE 1                                                                
         SSPEC H1,61,C'FILE SUMMARY'                                            
         SSPEC H2,61,C'------------'                                            
         SPACE 2                                                                
         SPROG 10,30                                                            
         SSPEC H6,26,C'YEAR TO DATE'                                            
         SSPEC H5,65,C'YEAR TO DATE'                                            
         SPROG 11,31                                                            
         SSPEC H6,26,C'FILE TO DATE'                                            
         SSPEC H5,65,C'FILE TO DATE'                                            
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PPREPBC01 08/07/03'                                      
         END                                                                    
