*          DATA SET ACREP7401  AT LEVEL 003 AS OF 11/02/93                      
*PHASE AC7401A                                                                  
         TITLE 'SPECS FOR USER PROFILE LISTING'                                 
AC7401   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,46,C'REPORT PROFILE LISTING'                                  
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H2,46,22C'-'                                                     
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H5,2,C'REPORT'                                                   
*                                                                               
         SPROG 0                                                                
         ASPEC M1,2,C'FIELD   DESCRIPTION'                                      
         ASPEC M1,42,C'ACCEPTABLE VALUES     DEFAULT  DDS'                      
         ASPEC M2,2,C'NUMBER  -----------'                                      
         ASPEC M2,42,C'-----------------      VALUE   ---'                      
*                                                                               
         SPROG 1                                                                
         ASPEC M1,2,C'USERID      UNIT/   OFFICE/        -----------'           
         ASPEC M1,48,C'------------EFFECTIVE OVERRIDE FIELD VALUES'             
         ASPEC M1,91,C'-----------------------'                                 
         ASPEC M2,2,C'------      LEDGER  ACCOUNT        01   02   0'           
         ASPEC M2,48,C'3   04   05   06   07   08   09   10   11  '             
         ASPEC M2,91,C' 12   13   14   15   16'                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREP7401 11/02/93'                                      
         END                                                                    
