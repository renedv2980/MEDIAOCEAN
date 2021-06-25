*          DATA SET CTREP7301  AT LEVEL 007 AS OF 06/14/01                      
*PHASE CT7301A                                                                  
         TITLE 'CT7301 - SPECS FOR EFFECTIVE REPORT PROFILES'                   
CT7301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,USERS                                                       
         ASPEC H1,2,RUN                                                         
         ASPEC H1,43,C'EFFECTIVE REPORT PROFILES'                               
         ASPEC H2,43,25C'-'                                                     
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H2,85,REQUESTOR                                                  
         ASPEC H5,11,C'SYS  PRG  AGY      MED  CLT'                             
         ASPEC H6,11,C'---  ---  ---      ---  ---'                             
         ASPEC H5,40,C'---------------- VALUE OF PROFIL'                        
         ASPEC H5,72,C'E BYTES 01-16 ----------------'                          
         ASPEC H6,40,C'01  02  03  04  05  06  07  08'                          
         ASPEC H6,72,C'09  10  11  12  13  14  15  16'                          
         ASPEC H5,105,C'LAST ACTIVITY'                                          
         ASPEC H6,105,C'-------------'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007CTREP7301 06/14/01'                                      
         END                                                                    
