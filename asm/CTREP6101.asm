*          DATA SET CTREP6101  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT6101A                                                                  
         TITLE 'SPECS FOR LIBRARY/JCL/FILE BOOK LISTINGS'                       
CT6101   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1,2,3                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,55,REPORT                                                     
         ASPEC H1,70,PAGE                                                       
         ASPEC H2,55,REQUESTOR                                                  
         SPROG 1                                                                
         ASPEC H1,31,C'JCL BOOK LISTING'                                        
         ASPEC H2,31,16C'-'                                                     
         SPROG 2                                                                
         ASPEC H1,31,C'LIBRARY BOOK LISTING'                                    
         ASPEC H2,31,20C'-'                                                     
         SPROG 3                                                                
         ASPEC H1,31,C'FILE NAME BOOK LISTING'                                  
         ASPEC H2,31,22C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP6101 08/22/00'                                      
         END                                                                    
