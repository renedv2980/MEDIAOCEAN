*          DATA SET ACREP2701  AT LEVEL 012 AS OF 05/20/16                      
*PHASE AC2701A                                                                  
         TITLE 'SPECS FOR PROFESSIONAL BILLING'                                 
AC2701   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WCNAMES                                                          
         ACDEF PRORATA,BILL                                                     
         ACDEF FJOBCOL,CE                                                       
         ACDEF JOBBER,PROCACC                                                   
*                                                                               
         SPROG 0,1,2,3,4,5,6,10,11,12,13,14,15,17,18,19                         
         RSPEC REQUEST,NOSUM                                                    
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ASPEC U1,2,C' '           THIS IS A BINARY ZERO                        
         ASPEC U2,2,C' '           THIS IS A BINARY ZERO                        
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         SPROG 0,6                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,45,AC#BIL,4,L                                                 
         ASPEC H2,45,AC#BIL,4,LU                                                
         ASPEC H1,56,AC#BILC,11,L '                                             
         ASPEC H1,79,PAGE                                                       
         ASPEC H3,2,AC#CLINT,6,L                                                
         ASPEC H4,2,AC#PRO,7,L                                                  
         ASPEC H6,2,AC#JOB,3,L                                                  
         ASPEC H5,2,AC#MED,5,L                                                  
         ASPEC H11,43,AC#NETAM,11,R                                             
         ASPEC H11,60,AC#CMN,10,R                                               
         ASPEC H11,79,AC#TOTAL,5,R                                              
*                                                                               
         SPROG 0                                                                
         ASPEC H11,2,AC#DESC,11,L                                               
         ASPEC H11,23,AC#VNDR,6,L                                               
         ASPEC H11,30,AC#NAME,4,L                                               
*                                                                               
         SPROG 6                                                                
         ASPEC H11,2,AC#WC,9,L                                                  
         ASPEC H11,23,C'           '                                            
*                                                                               
         SPROG 1                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,AC#SMYBL,19,C                                              
         ASPEC H1,79,PAGE                                                       
         ASPEC H2,40,AC#SMYBL,19,CU                                             
         ASPEC H11,2,AC#MED,5,L                                                 
         ASPEC H11,7,C'/'                                                       
         ASPEC H11,8,AC#JOBNU,10,L                                              
         ASPEC H11,27,AC#BILC,11,L                                              
         ASPEC H11,43,AC#NETAM,11,R                                             
         ASPEC H11,60,AC#CMN,10,R                                               
         ASPEC H11,79,AC#TOTAL,5,R                                              
*                                                                               
         SPROG 2,3,7,12,13,17,18                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H2,2,COMPANY                                                     
         ASPEC H3,2,COMPADD                                                     
*                                                                               
         SPROG 7                                                                
         ASPEC H11,34,40C' '                                                    
         ASPEC H11,2,AC#MED,5,L                                                 
         ASPEC H11,7,C'/'                                                       
         ASPEC H11,8,AC#CLI,3,L                                                 
         ASPEC H11,11,C'/'                                                      
         ASPEC H11,12,AC#PROC,3,L                                               
         ASPEC H11,15,C'/'                                                      
         ASPEC H11,16,AC#JOBC,3,L                                               
         ASPEC H11,23,AC#DATE,4,L                                               
         ASPEC H11,30,AC#BILC,7,L                                               
*                                                                               
         SPROG 3,13                                                             
         ASPEC H10,34,40C' '                                                    
         ASPEC H10,23,AC#DATE,4,L                                               
         ASPEC H10,30,AC#BILC,7,L                                               
         ASPEC H11,2,AC#MED,5,L                                                 
         ASPEC H11,7,C'/'                                                       
         ASPEC H11,8,AC#CLI,3,L                                                 
         ASPEC H11,11,C'/'                                                      
         ASPEC H11,12,AC#PROC,3,L                                               
         ASPEC H11,15,C'/'                                                      
         ASPEC H11,16,AC#JOBC,3,L                                               
*                                                                               
         SPROG 2,11                                                             
         ASPEC H11,34,40C' '                                                    
         ASPEC H11,2,AC#MED,5,L                                                 
*                                                                               
         SPROG 3,2                                                              
         ASPEC H2,50,AC#CLGRP,37,C                                              
         ASPEC H3,50,AC#CLGRP,37,CU                                             
         ASPEC H10,45,AC#ACL,6,L                                                
         ASPEC H10,77,AC#AGY,6,L                                                
         ASPEC H10,100,AC#CASH,4,R                                              
         ASPEC H11,42,AC#BILAM,11,R                                             
         ASPEC H11,55,AC#NETCT,12,R                                             
         ASPEC H11,75,AC#CMN,10,R                                               
         ASPEC H11,88,AC#GROSS,5,R                                              
         ASPEC H11,98,AC#DISS,8,R                                               
*                                                                               
         SPROG 5                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H2,2,COMPANY                                                     
         ASPEC H3,2,COMPADD                                                     
         ASPEC H2,50,AC#IIBS,32,C                                               
         ASPEC H3,50,AC#IIBS,32,CU                                              
         ASPEC H11,2,30C' '                                                     
         ASPEC H11,32,30C' '                                                    
         ASPEC H11,62,30C' '                                                    
         ASPEC H12,40,60C' '                                                    
         ASPEC H11,2,AC#JOBNU,10,L                                              
         ASPEC H11,24,AC#INCAC,14,L                                             
         ASPEC H11,42,AC#DATE,4,L                                               
         ASPEC H11,56,AC#REF,9,L                                                
         ASPEC H11,71,AC#AMT,6,L                                                
*                                                                               
         SPROG 7                                                                
         ASPEC H1,40,30C' '                                                     
         ASPEC H2,50,AC#RTAG,33,C                                               
         ASPEC H3,50,AC#RTAG,33,CU                                              
         ASPEC H11,41,AC#RCV,10,L                                               
         ASPEC H11,56,AC#CSG,7,L                                                
         ASPEC H11,72,AC#SALES,5,L                                              
         ASPEC H12,41,AC#ACC,7,L                                                
         ASPEC H12,56,AC#ACC,7,L                                                
         ASPEC H12,72,AC#ACC,7,L                                                
*                                                                               
         SPROG 8                                                                
         ASPEC H1,79,PAGE                                                       
*                                                                               
         SPROG 9                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,50,C'GROUP BILLING TRACE RECORD SUMMARY'                      
         ASPEC H2,50,C'----------------------------------'                      
         ASPEC H6,2,AC#CLI,6,L                                                  
         ASPEC H6,22,AC#BILDT,9,L                                               
         ASPEC H11,2,AC#BILC,5,L                                                
         ASPEC H12,2,AC#BILC,5,LU                                               
         ASPEC H11,13,AC#SJAMT,6,L                                              
         ASPEC H12,13,AC#SJAMT,6,LU                                             
         ASPEC H11,25,AC#SRAMT,6,L                                              
         ASPEC H12,25,AC#SRAMT,6,LU                                             
         ASPEC H11,37,AC#12AMT,6,L                                              
         ASPEC H12,37,AC#12AMT,6,LU                                             
         ASPEC H11,49,AC#11AMT,6,L                                              
         ASPEC H12,49,AC#11AMT,6,LU                                             
         ASPEC H11,61,AC#DSAMT,6,L                                              
         ASPEC H12,61,AC#DSAMT,6,LU                                             
         ASPEC H11,72,AC#INAMT,7,L                                              
         ASPEC H12,72,AC#INAMT,7,LU                                             
         ASPEC H11,84,AC#TMAMT,7,L                                              
         ASPEC H12,84,AC#TMAMT,7,LU                                             
         ASPEC H11,96,AC#OPAMT,7,L                                              
         ASPEC H12,96,AC#OPAMT,7,LU                                             
         ASPEC H11,108,AC#PEAMT,7,L                                             
         ASPEC H12,108,AC#PEAMT,7,LU                                            
         ASPEC H11,120,AC#REAMT,7,L                                             
         ASPEC H12,120,AC#REAMT,7,LU                                            
*                                                                               
         SPROG 11                                                               
         ASPEC H1,2,RUN                                                         
         ASPEC H2,2,COMPANY                                                     
         ASPEC H3,2,COMPADD                                                     
         ASPEC H2,40,C'SUMMARY OF NON-BILLABLE JOBS'                            
         ASPEC H11,2,30C' '                                                     
         ASPEC H11,32,30C' '                                                    
         ASPEC H11,62,30C' '                                                    
         ASPEC H12,40,60C' '                                                    
         ASPEC H11,2,AC#CLI,3,L                                                 
         ASPEC H11,5,C'/'                                                       
         ASPEC H11,6,AC#PROC,3,L                                                
         ASPEC H11,9,C'/'                                                       
         ASPEC H11,10,AC#JOBC,3,L                                               
         ASPEC H11,18,AC#RSN,6,L                                                
*                                                                               
         SPROG 12,13                                                            
         ASPEC H2,50,AC#CLGRP,37,C                                              
         ASPEC H3,50,AC#CLGRP,37,CU                                             
         ASPEC H10,45,AC#ACL,6,L                                                
         ASPEC H10,77,AC#AGY,6,L                                                
         ASPEC H10,97,AC#GSTAM,11,R                                             
         ASPEC H11,100,AC#PSTAM,11,R                                            
         ASPEC H11,42,AC#BILAM,11,R                                             
         ASPEC H11,55,AC#NETCT,12,R                                             
         ASPEC H11,75,AC#CMN,10,R                                               
         ASPEC H11,90,AC#GROSS,5,R                                              
*                                                                               
         SPROG 14                                                               
         ASPEC H1,40,AC#SMYBL,19,C                                              
         ASPEC H2,40,AC#SMYBL,19,CU                                             
         ASPEC H1,61,PAGE                                                       
         ASPEC H11,2,AC#MED,5,L                                                 
         ASPEC H11,7,C'/'                                                       
         ASPEC H11,8,AC#JOBNU,10,L                                              
         ASPEC H11,27,AC#BILC,11,L                                              
         ASPEC H11,43,AC#NETAM,11,R                                             
         ASPEC H11,60,AC#CMN,10,R                                               
         ASPEC H11,79,AC#TOTAL,5,R                                              
         ASPEC H11,87,AC#GSTAM,11,R                                             
         ASPEC H12,90,AC#PSTAM,11,R                                             
*                                                                               
         SPROG 15                                                               
         ASPEC H1,2,RUN                                                         
         ASPEC H2,2,COMPANY                                                     
         ASPEC H3,2,COMPADD                                                     
         ASPEC H2,40,C'SUMMARY OF ASP POSTINGS'                                 
         ASPEC H11,2,30C' '                                                     
         ASPEC H11,32,30C' '                                                    
         ASPEC H11,62,30C' '                                                    
         ASPEC H12,40,60C' '                                                    
         ASPEC H11,2,AC#CLI,3,L                                                 
         ASPEC H11,5,C'/'                                                       
         ASPEC H11,6,AC#PROC,3,L                                                
         ASPEC H11,9,C'/'                                                       
         ASPEC H11,10,AC#JOBC,3,L                                               
         ASPEC H11,19,AC#INCAC,14,L                                             
         ASPEC H11,36,AC#WC,2,L                                                 
         ASPEC H11,41,AC#BILC,11,L                                              
         ASPEC H11,58,AC#AMT,6,R                                                
*                                                                               
         SPROG 17                                                               
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
         SPROG 18                                                               
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
         SPROG 19                                                               
         ASPEC H1,79,PAGE                                                       
         ASPEC H12,2,AC#DESC,11,L                                               
         ASPEC H12,23,AC#VNDR,6,L                                               
         ASPEC H12,30,AC#NAME,4,L                                               
         ASPEC H12,43,AC#NETAM,11,R                                             
         ASPEC H12,60,AC#CMN,10,R                                               
         ASPEC H12,79,AC#TOTAL,5,R                                              
*                                                                               
         SPROG 20                                                               
         ASPEC H1,79,PAGE                                                       
         ASPEC H12,2,AC#DESC,11,L                                               
         ASPEC H12,43,AC#NETAM,11,R                                             
         ASPEC H12,60,AC#CMN,10,R                                               
         ASPEC H12,79,AC#TOTAL,5,R                                              
*                                                                               
         DC    100X'00'                                                         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREP2701 05/20/16'                                      
         END                                                                    
