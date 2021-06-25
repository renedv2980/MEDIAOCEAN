*          DATA SET CTREP5001  AT LEVEL 007 AS OF 08/22/00                      
*PHASE CT5001A                                                                  
         TITLE 'SPECS FOR ID REPORT'                                            
CT5001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,IDS                                                         
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,51,C'ID REPORT'                                               
         ASPEC H2,51,C'---------'                                               
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         SPROG 1,3                                                              
         ASPEC H4,2,C'IDENTIFICATION'                                           
         ASPEC H5,2,C'ID NUMBER'                                                
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H5,85,C'ACTIVE ON'                                               
         SPROG 3                                                                
         ASPEC H8,2,C'SYSTEM   SYSTEM   FILE   AGENCY  AGENCY  ACCESS'          
         ASPEC H9,2,C'------   NUMBER  NUMBER  BINARY  ALPHA.  LIMIT'           
         ASPEC H8,52,C'PROGRAM AUTHORIZATION LIST'                              
         ASPEC H9,52,26C'-'                                                     
         SPROG 2                                                                
         ASPEC H4,2,C'PROGRAM CROSS-REFERENCE'                                  
         ASPEC H5,2,23C'-'                                                      
         ASPEC H8,2,C'SYSTEM       PROGRAM       PROGRAM'                       
         ASPEC H9,2,C'------        CODE          NAME  '                       
         ASPEC H8,45,C'EXCEPTION LIST'                                          
         ASPEC H9,45,C'--------------'                                          
         ASPEC H4,85,REQUESTOR                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTREP5001 08/22/00'                                      
         END                                                                    
