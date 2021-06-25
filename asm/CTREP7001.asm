*          DATA SET CTREP7001  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT7001A                                                                  
         TITLE 'SPECS FOR FIELD DESCRIPTION LISTING'                            
CT7001   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,USERS                                                       
         ASPEC H1,2,RUN                                                         
         ASPEC H1,43,C'FIELD DESCRIPTION LISTING'                               
         ASPEC H2,43,25C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,C'SYSTEM/PROGRAM'                                           
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H5,85,C'ACTIVE ON'                                               
         ASPEC H8,14,C'FIELD      FIELD DESCRIPTION'                            
         ASPEC H9,14,C'NUMBER     -----------------'                            
         ASPEC H8,57,C'FIELD     ACCEPTABLE VALUES     DEFAULT'                 
         ASPEC H9,57,C'TYPE      -----------------      VALUE '                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP7001 08/22/00'                                      
         END                                                                    
