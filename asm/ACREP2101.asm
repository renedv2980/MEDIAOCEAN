*          DATA SET ACREP2101  AT LEVEL 017 AS OF 05/20/16                      
*PHASE AC2101A                                                                  
         TITLE 'SPECS FOR PRODUCTION BILLS'                                     
AC2101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF WCNAMES                                                          
         ACDEF PRORATA,BILL                                                     
         ACDEF FJOBCOL,CE,CEG,PEBCE,PEBCEG                                      
*                                                                               
         SPROG 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21          
         RSPEC REQUEST,NOSUM                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF U1,2,C' '           THIS IS A BINARY ZERO                        
         ACDEF U2,2,C' '           THIS IS A BINARY ZERO                        
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         SPROG 0,6,8,9,17,18                                                    
         ACDEF H1,2,AC#BILDT,9,L                                                
         ACDEF H1,45,AC#BIL,4,L                                                 
         ACDEF H2,45,AC#BIL,4,LU                                                
         ACDEF H1,79,PAGE                                                       
         ACDEF H3,2,AC#CLINT,6,L                                                
         ACDEF H4,2,AC#PRO,7,L                                                  
         ACDEF H6,2,AC#JOB,3,L                                                  
         ACDEF H5,2,AC#MED,5,L                                                  
         ACDEF H11,42,AC#NETAM,11,R                                             
         ACDEF H11,58,AC#CMN,10,R                                               
         ACDEF H11,79,AC#TOTAL,5,R                                              
*                                                                               
         SPROG 0,8                                                              
         ACDEF H11,2,AC#DESC,11,L                                               
         ACDEF H11,24,AC#VNDR,7,L                                               
         ACDEF H11,32,AC#NAME,4,L                                               
*                                                                               
         SPROG 17,18                                                            
         ACDEF H11,2,AC#DESC,11,L                                               
*                                                                               
         SPROG 6,9                                                              
         ACDEF H11,2,AC#WC,9,L                                                  
         ACDEF H11,23,C'           '                                            
*                                                                               
         SPROG 8,9,18                                                           
         ACDEF H11,42,C'           '                                            
         ACDEF H11,58,C'                  '                                     
         ACDEF H11,79,AC#TOTAL,5,R                                              
*                                                                               
         SPROG 1                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,40,AC#SMYBL,19,C                                              
         ACDEF H2,40,AC#SMYBL,19,CU                                             
         ACDEF H1,79,PAGE                                                       
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#JOBNU,10,L                                              
         ACDEF H11,27,AC#BILC,11,L                                              
         ACDEF H11,42,AC#NETAM,11,R                                             
         ACDEF H11,58,AC#CMN,10,R                                               
         ACDEF H11,79,AC#TOTAL,5,R                                              
*                                                                               
         SPROG 2,3,7,12,13,15,16,19,20                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
*                                                                               
         SPROG 7                                                                
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#CLI,3,L                                                 
         ACDEF H11,11,C'/'                                                      
         ACDEF H11,12,AC#PROC,3,L                                               
         ACDEF H11,15,C'/'                                                      
         ACDEF H11,16,AC#JOBC,3,L                                               
         ACDEF H11,23,AC#DATE,4,L                                               
         ACDEF H11,30,AC#BILC,7,L                                               
*                                                                               
         SPROG 3,13                                                             
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#CLI,3,L                                                 
         ACDEF H11,11,C'/'                                                      
         ACDEF H11,12,AC#PROC,3,L                                               
         ACDEF H11,15,C'/'                                                      
         ACDEF H11,16,AC#JOBC,3,L                                               
         ACDEF H11,23,AC#DATE,4,L                                               
         ACDEF H11,30,AC#BILC,7,L                                               
*                                                                               
         SPROG 2,12                                                             
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#MED,5,L                                                 
*                                                                               
         SPROG 3,2                                                              
         ACDEF H2,50,AC#PRBIR,35,C                                              
         ACDEF H11,41,AC#BILAM,11,R                                             
         ACDEF H11,57,AC#NETCT,12,R                                             
         ACDEF H11,73,AC#AGCOM,11,R                                             
         ACDEF H11,91,AC#GROSS,5,R                                              
         ACDEF H11,98,AC#CSHDS,13,R                                             
*                                                                               
         SPROG 4                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H7,2,COMPANY                                                     
         ACDEF H9,2,C'DDS CONTROL SHEET'                                        
         ACDEF H11,2,30C' '                                                     
         ACDEF H11,32,30C' '                                                    
         ACDEF H11,62,30C' '                                                    
         ACDEF H12,40,60C' '                                                    
*                                                                               
         SPROG 5                                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,C'INTERNAL INVOICE BILLING SUMMARY'                        
         ACDEF H11,2,30C' '                                                     
         ACDEF H11,32,30C' '                                                    
         ACDEF H11,62,30C' '                                                    
         ACDEF H12,40,60C' '                                                    
         ACDEF H11,2,AC#JOBNU,10,L                                              
         ACDEF H11,15,AC#INCAC,14,L                                             
         ACDEF H11,34,AC#DATE,4,L                                               
         ACDEF H11,43,AC#BILC,7,L                                               
         ACDEF H11,56,AC#REF,4,L                                                
         ACDEF H11,65,AC#WC,3,L                                                 
         ACDEF H11,74,AC#AMT,6,R                                                
*                                                                               
         SPROG 7                                                                
         ACDEF H2,50,AC#RTAG,33,C                                               
         ACDEF H11,41,AC#RSRCV,6,L                                              
         ACDEF H11,45,AC#ACC,7,L                                                
         ACDEF H11,56,AC#RSCSN,12,L                                             
         ACDEF H11,72,AC#SLSA,13,L                                              
*                                                                               
         SPROG 10                                                               
         ACDEF H1,2,AC#BILDT,9,L                                                
         ACDEF H1,79,PAGE                                                       
*                                                                               
         SPROG 11                                                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,79,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,40,C'SUMMARY OF NON-BILLABLE JOBS'                            
         ACDEF H11,2,30C' '                                                     
         ACDEF H11,32,30C' '                                                    
         ACDEF H11,62,30C' '                                                    
         ACDEF H12,40,60C' '                                                    
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,5,C'/'                                                       
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,9,C'/'                                                       
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,18,AC#RSN,6,L                                                
*                                                                               
         SPROG 12,13                                                            
         ACDEF H2,50,AC#PRBIR,35,C                                              
         ACDEF H10,44,AC#ACL,6,L                                                
         ACDEF H10,76,AC#AGY,6,L                                                
         ACDEF H10,97,AC#GSTAM,11,R                                             
         ACDEF H11,100,AC#PSTAM,11,R                                            
         ACDEF H11,41,AC#BILAM,11,R                                             
         ACDEF H11,54,AC#NETCT,12,R                                             
         ACDEF H11,74,AC#CMN,10,R                                               
         ACDEF H11,89,AC#GROSS,5,R                                              
*                                                                               
         SPROG 14                                                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,40,AC#SMYBL,19,C                                              
         ACDEF H2,40,AC#SMYBL,19,CU                                             
         ACDEF H1,79,PAGE                                                       
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#JOBNU,10,L                                              
         ACDEF H11,27,AC#BILC,11,L                                              
         ACDEF H11,42,AC#NETAM,11,R                                             
         ACDEF H11,58,AC#CMN,10,R                                               
         ACDEF H11,79,AC#TOTAL,5,R                                              
         ACDEF H11,87,AC#GSTAM,11,R                                             
         ACDEF H12,90,AC#PSTAM,11,R                                             
*                                                                               
         SPROG 15                                                               
         ACDEF H2,50,C'INTERNAL INCOME REGISTER'                                
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,24,AC#BILC,7,L                                               
         ACDEF H11,37,AC#WC,2,L                                                 
         ACDEF H11,42,AC#INCAM,13,L                                             
         ACDEF H11,56,AC#INCAC,14,L                                             
         ACDEF H11,72,AC#INPST,13,L                                             
         ACDEF H11,87,AC#DRA,14,L                                               
*                                                                               
         SPROG 16                                                               
         ACDEF H2,50,C'STUDIO INTERCOMPANY POSTING REGISTER'                    
         ACDEF H11,34,40C' '                                                    
         ACDEF H10,2,AC#STDIO,6,L                                               
         ACDEF H10,9,AC#TYPE,4,L                                                
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#CLI,3,L                                                 
         ACDEF H11,11,C'/'                                                      
         ACDEF H11,12,AC#PROC,3,L                                               
         ACDEF H11,15,C'/'                                                      
         ACDEF H11,16,AC#JOBC,3,L                                               
         ACDEF H11,25,AC#DATE,4,L                                               
         ACDEF H11,35,AC#BILC,7,L                                               
         ACDEF H11,54,AC#AMT,6,R                                                
         ACDEF H11,64,AC#AGJBL,10,L                                             
         ACDEF H11,80,AC#CRA,14,L                                               
*                                                                               
         SPROG 19                                                               
         ACDEF H2,50,C'INTERCOMPANY POSTING EXCEPTION REGISTER'                 
         ACDEF H11,34,40C' '                                                    
         ACDEF H10,2,AC#STDIO,6,L                                               
         ACDEF H10,9,AC#JOBC,3,L                                                
         ACDEF H10,18,AC#STDIO,6,L                                              
         ACDEF H11,19,AC#TYPE,4,L                                               
         ACDEF H10,26,AC#AGJBL,10,L                                             
         ACDEF H10,42,AC#DATE,4,L                                               
         ACDEF H10,50,AC#BILC,7,L                                               
         ACDEF H10,62,AC#RSN,6,L                                                
*                                                                               
         SPROG 20                                                               
         ACDEF H2,50,C'PERCENT OF BILLING REGISTER'                             
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,19,AC#VNDR,7,L                                               
         ACDEF H11,34,AC#WC,2,L                                                 
         ACDEF H11,39,AC#BILC,7,L                                               
         ACDEF H11,54,AC#AMT,6,R                                                
*                                                                               
         DC    100X'00'                                                         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREP2101 05/20/16'                                      
         END                                                                    
