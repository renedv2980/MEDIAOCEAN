*          DATA SET CTREP2001  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT2001A                                                                  
         TITLE 'SPECS FOR COMMENT REPORT'                                       
CT2001   CSECT                                                                  
         FSPEC READ,COMMENTS                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'COMMENT REPORT'                                          
         ASPEC H2,45,C'--------------'                                          
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,C'USER'                                                     
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H5,2,C'SYSTEM'                                                   
         ASPEC H8,2,C'CLIENT  COMMENT ID    LINE   COMMENT TEXT'                
         ASPEC H9,2,C'------  ----------   NUMBER  ------------'                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP2001 08/22/00'                                      
         END                                                                    
