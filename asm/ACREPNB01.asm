*          DATA SET ACREPNB01  AT LEVEL 006 AS OF 05/20/16                      
*PHASE ACNB01A                                                                  
         TITLE 'SPECS FOR PRODUCTION BILLING'                                   
ACNB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
*                                                                               
         ACDEF NOSUM,REQDETS                                                    
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF WCNAMES                                                          
         ACDEF RESET                                                            
         ACDEF FJOBCOL,CE,CEG,HR,HRG                                            
         ACDEF JOBBER,PROCACC                                                   
         ACDEF GETOPT,W                                                         
         ACDEF PRORATA,BILL                                                     
*                                                                               
         ACDEF SPROG,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15                        
         ACDEF U1,2,C' '           THIS IS A BINARY ZERO                        
         ACDEF U2,2,C' '                                                        
*                                                                               
*                              CLIENT/PRODUCT SUMMARIES                         
         ACDEF SPROG,1                                                          
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
*                              CLIENT/PRODUCT SUMMARIES WITH GST                
         ACDEF SPROG,2                                                          
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
*                               INVOICE REGISTER(21)                            
         ACDEF SPROG,3,4                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H2,50,AC#PRBIR,35,C                                              
         ACDEF H3,2,COMPADD                                                     
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#CLI,3,L                                                 
         ACDEF H11,11,C'/'                                                      
         ACDEF H11,12,AC#PROC,3,L                                               
         ACDEF H11,15,C'/'                                                      
         ACDEF H11,16,AC#JOBC,3,L                                               
         ACDEF H11,23,AC#DATE,4,L                                               
         ACDEF H11,30,AC#BILC,7,L                                               
         ACDEF H11,41,AC#BILAM,11,R                                             
*                                                                               
*                               INVOICE REGISTER (A21) WITH CD                  
         ACDEF SPROG,3                                                          
         ACDEF H11,57,AC#NETCT,12,R                                             
         ACDEF H11,73,AC#AGCOM,11,R                                             
         ACDEF H11,91,AC#GROSS,5,R                                              
         ACDEF H11,98,AC#CSHDS,13,R                                             
*                                                                               
*                               INVOICE REGISTER (A21) WITH GST                 
         ACDEF SPROG,4                                                          
         ACDEF H10,44,AC#ACL,6,L                                                
         ACDEF H10,76,AC#AGY,6,L                                                
         ACDEF H10,97,AC#GSTAM,11,R                                             
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,54,AC#NETCT,12,R                                             
         ACDEF H11,74,AC#CMN,10,R                                               
         ACDEF H11,89,AC#GROSS,5,R                                              
         ACDEF H11,100,AC#PSTAM,11,R                                            
*                                                                               
*                               INVOICE REGISTER(A27)                           
         ACDEF SPROG,17,18,19,20                                                
         ACDEF H1,2,RUN                                                         
         ACDEF H2,2,COMPANY                                                     
         ACDEF H2,50,AC#CLGRP,37,C                                              
         ACDEF H3,50,AC#CLGRP,37,CU                                             
         ACDEF H3,2,COMPADD                                                     
         ACDEF H10,34,40C' '                                                    
         ACDEF H10,23,AC#DATE,4,L                                               
         ACDEF H10,30,AC#BILC,7,L                                               
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#CLI,3,L                                                 
         ACDEF H11,11,C'/'                                                      
         ACDEF H11,12,AC#PROC,3,L                                               
         ACDEF H11,15,C'/'                                                      
         ACDEF H11,16,AC#JOBC,3,L                                               
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H10,45,AC#ACL,6,L                                                
         ACDEF H10,77,AC#AGY,6,L                                                
         ACDEF H11,42,AC#BILAM,11,R                                             
         ACDEF H11,55,AC#NETCT,12,R                                             
         ACDEF H11,75,AC#CMN,10,R                                               
         ACDEF H11,88,AC#GROSS,5,R                                              
*                                                                               
*                               INVOICE REGISTER(A27) WITH CD                   
         ACDEF SPROG,17,19                                                      
         ACDEF H10,100,AC#CASH,4,R                                              
         ACDEF H11,98,AC#DISS,8,R                                               
*                                                                               
*                               INVOICE REGISTER(A27) WITH GST                  
         ACDEF SPROG,18,20                                                      
         ACDEF H10,97,AC#GSTAM,11,R                                             
         ACDEF H11,100,AC#PSTAM,11,R                                            
*                                                                               
*                               MEDIA SUMMARY(A21)                              
         ACDEF SPROG,5,6                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,AC#PRBIR,35,C                                              
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,34,40C' '                                                    
*                                                                               
*                               MEDIA SUMMARY(A21) WITH CD                      
         ACDEF SPROG,5                                                          
         ACDEF H11,41,AC#BILAM,11,R                                             
         ACDEF H11,57,AC#NETCT,12,R                                             
         ACDEF H11,73,AC#AGCOM,11,R                                             
         ACDEF H11,91,AC#GROSS,5,R                                              
         ACDEF H11,98,AC#CSHDS,13,R                                             
*                                                                               
*                               MEDIA SUMMARY(A21) WITH GST                     
         ACDEF SPROG,6                                                          
         ACDEF H10,44,AC#ACL,6,L                                                
         ACDEF H10,76,AC#AGY,6,L                                                
         ACDEF H10,97,AC#GSTAM,11,R                                             
         ACDEF H11,74,AC#CMN,10,R                                               
         ACDEF H11,89,AC#GROSS,5,R                                              
         ACDEF H11,100,AC#PSTAM,11,R                                            
*                                                                               
*                               TARGET REGISTER                                 
         ACDEF SPROG,7                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,AC#RTAG,33,C                                               
         ACDEF H11,2,AC#MED,5,L                                                 
         ACDEF H11,7,C'/'                                                       
         ACDEF H11,8,AC#CLI,3,L                                                 
         ACDEF H11,11,C'/'                                                      
         ACDEF H11,12,AC#PROC,3,L                                               
         ACDEF H11,15,C'/'                                                      
         ACDEF H11,16,AC#JOBC,3,L                                               
         ACDEF H11,23,AC#DATE,4,L                                               
         ACDEF H11,30,AC#BILC,7,L                                               
         ACDEF H11,41,AC#RSRCV,6,L                                              
         ACDEF H11,45,AC#ACC,7,L                                                
         ACDEF H11,56,AC#RSCSN,12,L                                             
         ACDEF H11,72,AC#SLSA,13,L                                              
*                                                                               
*                               INTERNAL REGISTER                               
         ACDEF SPROG,8                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,C'INTERNAL INVOICE BILLING SUMMARY'                        
         ACDEF H11,2,30C' '                                                     
         ACDEF H11,32,30C' '                                                    
         ACDEF H11,62,30C' '                                                    
         ACDEF H11,2,AC#JOBNU,10,L                                              
         ACDEF H11,15,AC#INCAC,14,L                                             
         ACDEF H11,34,AC#DATE,4,L                                               
         ACDEF H11,43,AC#BILC,7,L                                               
         ACDEF H11,56,AC#REF,4,L                                                
         ACDEF H11,65,AC#WC,3,L                                                 
         ACDEF H11,74,AC#AMT,6,R                                                
         ACDEF H12,40,60C' '                                                    
*                                                                               
         ACDEF SPROG,9                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,C'INTERNAL INCOME REGISTER'                                
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,24,AC#BILC,7,L                                               
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,37,AC#WC,2,L                                                 
         ACDEF H11,42,AC#INCAM,13,L                                             
         ACDEF H11,56,AC#INCAC,14,L                                             
         ACDEF H11,72,AC#INPST,13,L                                             
         ACDEF H11,87,AC#DRA,14,L                                               
*                                                                               
         ACDEF SPROG,10                                                         
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
         ACDEF SPROG,11                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,C'INTERCOMPANY POSTING EXCEPTION REGISTER'                 
         ACDEF H11,34,40C' '                                                    
         ACDEF H10,2,AC#STDIO,6,L                                               
         ACDEF H10,9,AC#JOBC,3,L                                                
         ACDEF H10,18,AC#STDIO,6,L                                              
         ACDEF H10,26,AC#AGJBL,10,L                                             
         ACDEF H10,42,AC#DATE,4,L                                               
         ACDEF H10,50,AC#BILC,7,L                                               
         ACDEF H10,62,AC#RSN,6,L                                                
         ACDEF H11,19,AC#TYPE,4,L                                               
*                                                                               
         ACDEF SPROG,12                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H1,79,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,40,C'SUMMARY OF NON-BILLABLE JOBS'                            
         ACDEF H11,2,30C' '                                                     
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,5,C'/'                                                       
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,9,C'/'                                                       
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,18,AC#RSN,6,L                                                
         ACDEF H11,32,30C' '                                                    
         ACDEF H11,62,30C' '                                                    
         ACDEF H12,40,60C' '                                                    
*                                                                               
         ACDEF SPROG,13                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H7,2,COMPANY                                                     
         ACDEF H9,2,C'DDS CONTROL SHEET'                                        
         ACDEF H11,2,30C' '                                                     
         ACDEF H11,32,30C' '                                                    
         ACDEF H11,62,30C' '                                                    
         ACDEF H12,40,60C' '                                                    
*                                                                               
         ACDEF SPROG,14                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,C'PERCENT OF BILLING REGISTER'                             
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,19,AC#VNDR,6,L                                               
         ACDEF H11,34,AC#WC,2,L                                                 
         ACDEF H11,39,AC#BILC,7,L                                               
         ACDEF H11,54,AC#AMT,6,R                                                
*                                                                               
         ACDEF SPROG,21                                                         
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,PAGE                                                       
         ACDEF H2,2,COMPANY                                                     
         ACDEF H3,2,COMPADD                                                     
         ACDEF H2,50,C'SUMMARY OF ASP POSTINGS'                                 
         ACDEF H11,34,40C' '                                                    
         ACDEF H11,2,AC#CLI,3,L                                                 
         ACDEF H11,6,AC#PROC,3,L                                                
         ACDEF H11,10,AC#JOBC,3,L                                               
         ACDEF H11,19,AC#INCAC,14,L                                             
         ACDEF H11,34,AC#WC,2,L                                                 
         ACDEF H11,39,AC#BILC,11,L                                              
         ACDEF H11,54,AC#AMT,6,R                                                
*                                                                               
         SPROG 15                  GROUP BILL                                   
         ACDEF H1,79,PAGE                                                       
*                                                                               
         SPROG 16                                                               
         ACDEF H1,2,RUN                                                         
         ACDEF H1,50,C'GROUP BILLING TRACE RECORD SUMMARY'                      
         ACDEF H2,50,C'----------------------------------'                      
         ACDEF H6,2,AC#CLI,6,L                                                  
         ACDEF H6,22,AC#BILDT,9,L                                               
         ACDEF H11,2,AC#BILC,5,L                                                
         ACDEF H12,2,AC#BILC,5,LU                                               
         ACDEF H11,13,AC#SJAMT,6,L                                              
         ACDEF H12,13,AC#SJAMT,6,LU                                             
         ACDEF H11,25,AC#SRAMT,6,L                                              
         ACDEF H12,25,AC#SRAMT,6,LU                                             
         ACDEF H11,37,AC#12AMT,6,L                                              
         ACDEF H12,37,AC#12AMT,6,LU                                             
         ACDEF H11,49,AC#11AMT,6,L                                              
         ACDEF H12,49,AC#11AMT,6,LU                                             
         ACDEF H11,61,AC#DSAMT,6,L                                              
         ACDEF H12,61,AC#DSAMT,6,LU                                             
         ACDEF H11,72,AC#INAMT,7,L                                              
         ACDEF H12,72,AC#INAMT,7,LU                                             
         ACDEF H11,84,AC#TMAMT,7,L                                              
         ACDEF H12,84,AC#TMAMT,7,LU                                             
         ACDEF H11,96,AC#OPAMT,7,L                                              
         ACDEF H12,96,AC#OPAMT,7,LU                                             
         ACDEF H11,108,AC#PEAMT,7,L                                             
         ACDEF H12,108,AC#PEAMT,7,LU                                            
         ACDEF H11,120,AC#REAMT,7,L                                             
         ACDEF H12,120,AC#REAMT,7,LU                                            
*                                                                               
         DC    X'00'                                                            
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,X        
               20,21                                                            
         ACDEF F1,2,REQDETS                                                     
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPNB01 05/20/16'                                      
         END                                                                    
