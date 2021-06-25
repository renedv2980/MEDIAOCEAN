*          DATA SET CTREP8001  AT LEVEL 010 AS OF 08/22/00                      
*PHASE CT8001A                                                                  
         TITLE 'CT8001 - SPECS FOR TERMINAL REPORT'                             
CT8001   CSECT                                                                  
         PRINT NOGEN                                                            
         TITLE 'SPECS FOR TERMINAL REPORT'                                      
         FSPEC READ,TERMS                                                       
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,47,C'TERMINAL REPORT'                                         
         ASPEC H2,47,15C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         SPROG 1                                                                
         ASPEC H4,2,C'LINE ADDRESS'                                             
         ASPEC H5,2,C'TERMINAL ADDRESS'                                         
         ASPEC H6,2,C'TERMINAL DESC.'                                           
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H6,85,C'ACTIVE ON'                                               
         ASPEC H10,2,C'SYSTEM    PROGRAM AUTHORIZATION LIST'                    
         ASPEC H11,2,C'------    --------------------------'                    
         SPROG 2                                                                
         ASPEC H4,2,C'PROGRAM CROSS-REFERENCE'                                  
         ASPEC H5,2,23C'-'                                                      
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H8,2,C'SYSTEM       PROGRAM       PROGRAM'                       
         ASPEC H9,2,C'------        CODE          NAME  '                       
         ASPEC H8,45,C'EXCEPTION LIST'                                          
         ASPEC H9,45,C'--------------'                                          
         SPROG 3                                                                
         ASPEC H4,2,C'ID/TERMINAL CROSS REFERENCE'                              
         ASPEC H5,2,27C'-'                                                      
         ASPEC H8,2,C'ID           TERMINAL(,PASSWORD) LIST'                    
         ASPEC H9,2,C'--           ------------------------'                    
         ASPEC H4,85,REQUESTOR                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTREP8001 08/22/00'                                      
         END                                                                    
