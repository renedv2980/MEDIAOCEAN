*          DATA SET CTREP5401  AT LEVEL 007 AS OF 08/22/00                      
*PHASE CT5401A                                                                  
         PRINT NOGEN                                                            
         TITLE 'SPECS FOR OUTPUT TYPE REPORT'                                   
CT5401   CSECT                                                                  
         FSPEC READ,OUTPUT                                                      
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,C'OUTPUT TYPE LISTING'                                     
         ASPEC H2,45,19C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H8,2,C'OUTPUT CODE  OUTPUT   OUTPUT    FORM '                    
         ASPEC H9,2,C'-----------  CLASS   PRIORITY   CODE '                    
         ASPEC H8,40,C'CARRIAGE   NUMBER OF    OUTPUT     SEPARATOR'            
         ASPEC H9,40,C'CONTROL     COPIES    DISPOSITION   OPTION  '            
         ASPEC H8,87,C' TAPE '                                                  
         ASPEC H9,87,C'DETAIL'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTREP5401 08/22/00'                                      
         END                                                                    
