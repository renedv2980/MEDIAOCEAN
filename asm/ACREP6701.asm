*          DATA SET ACREP6701  AT LEVEL 002 AS OF 11/11/97                      
*PHASE AC6701A,+0                                                               
         TITLE 'SPECS FOR JOB STATUS REPORT'                                    
AC6701   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,ESTIMATES                                                   
         FSPEC READ,TRANSACTIONS                                                
         RSPEC MAXLINES,56                                                      
         ACDEF PRORATA,BILL                                                     
         ASPEC F1,2,REQDETS                                                     
*                                                                               
         SPROG 0,1,2,3,4,5,6,7                                                  
         ACDEF H8,56,MOSFILT                                                    
         ACDEF H1,2,RUN                                                         
         ACDEF H4,2,COMPANY                                                     
         SPROG 0,1,2,3,4,6,7                                                    
         ACDEF H5,2,C'CLIENT'                                                   
         SPROG 0,1,2,3,6,7                                                      
         ACDEF H6,2,C'PRODUCT'                                                  
         SPROG 0,1,2,6,7                                                        
         ACDEF H7,2,C'MEDIA'                                                    
         ACDEF H8,2,C'JOB'                                                      
*                                                                               
         SPROG 2                                                                
         ACDEF H11,2,130C' '                                                    
         ACDEF H12,2,130C' '                                                    
         ACDEF M1,2,130C' '                                                     
         ACDEF M2,2,130C' '                                                     
*                                                                               
         SPROG 3,4,5                                                            
         ACDEF H8,1,132X'00'                                                    
         ACDEF H9,1,132X'00'                                                    
*                                                                               
         SPROG 1                                                                
         ACDEF H5,59,16X'BF'                                                    
         ACDEF H6,59,C'WORKCODE SUMMARY'                                        
         ACDEF H7,59,16X'BF'                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP6701 11/11/97'                                      
         END                                                                    
