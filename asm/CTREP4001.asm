*          DATA SET CTREP4001  AT LEVEL 002 AS OF 08/22/00                      
*PHASE CT4001A                                                                  
         TITLE 'SPECS FOR ERROR REPORT'                                         
CT4001   CSECT                                                                  
         FSPEC READ,ERRORS                                                      
         ASPEC H1,2,RUN                                                         
         ASPEC H1,31,C'ERROR LISTING'                                           
         ASPEC H2,31,C'-------------'                                           
         ASPEC H1,55,REPORT                                                     
         ASPEC H1,70,PAGE                                                       
         ASPEC H4,2,C'SYSTEM -'                                                 
         ASPEC H4,55,REQUESTOR                                                  
         ASPEC H7,2,C'ERROR    ERROR DESCRIPTION'                               
         ASPEC H8,2,C'NUMBER   -----------------'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTREP4001 08/22/00'                                      
         END                                                                    
