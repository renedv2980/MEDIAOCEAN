*          DATA SET CTREP7401  AT LEVEL 003 AS OF 12/05/97                      
*PHASE CT7401A,+0                                                               
         TITLE 'CT7201 - SPECS FOR EFFECTIVE REPORT PROFILES'                   
CT7201   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,USERS                                                       
         ASPEC H1,2,RUN                                                         
         ASPEC H1,43,C'EFFECTIVE REPORT PROFILES'                               
         ASPEC H2,43,25C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,C'SYSTEM/PROGRAM'                                           
         ASPEC H4,85,REQUESTOR                                                  
         ASPEC H8,2,C'FIELD   FIELD DESCRIPTION'                                
         ASPEC H9,2,C'NUMBER  -----------------'                                
         ASPEC H8,40,C'FIELD  ACCEPTABLE VALUES   DEFAULT'                      
         ASPEC H9,40,C' TYPE  -----------------    VALUE '                      
         ASPEC H8,77,C'AGENCY   MEDIA    CLIENT  PROFILE'                       
         ASPEC H9,77,C'------   -----    ------   VALUE '                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTREP7401 12/05/97'                                      
         END                                                                    
