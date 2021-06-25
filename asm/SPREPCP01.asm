*          DATA SET SPREPCP01  AT LEVEL 031 AS OF 01/09/02                      
*PHASE SPCP01A                                                                  
         TITLE 'SPCP01 - CPPRS REPORT'                                          
SPCP01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,C'EXCLUSION REPORT'                                         
         SSPEC H1,60,RUN                                                        
*                                                                               
         SSPEC H2,60,AGYNAME                                                    
*                                                                               
         SSPEC H3,1,SPACES                                                      
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H4,1,C'CLT'                                                      
         SSPEC H4,5,C'CLIENT DESCRIPTION'                                       
         SSPEC H4,30,C'PRD'                                                     
         SSPEC H4,34,C'PRODUCT DESCRIPTION'                                     
         SSPEC H4,59,C'EST'                                                     
         SSPEC H4,65,C'START'                                                   
         SSPEC H4,74,C'END'                                                     
*                                                                               
         SSPEC H5,1,C'---'                                                      
         SSPEC H5,5,C'------------------'                                       
         SSPEC H5,30,C'---'                                                     
         SSPEC H5,34,C'-------------------'                                     
         SSPEC H5,59,C'---'                                                     
         SSPEC H5,65,C'-----'                                                   
         SSPEC H5,74,C'---'                                                     
*                                                                               
         SPROG 2                                                                
*                                                                               
         SSPEC H4,1,C'Daypart'                                                  
         SSPEC H4,10,C'Type'                                                    
         SSPEC H4,19,C'Code'                                                    
         SSPEC H4,26,C'Daypart'                                                 
         SSPEC H4,35,C'Type'                                                    
         SSPEC H4,44,C'Code'                                                    
         SSPEC H4,51,C'Daypart'                                                 
         SSPEC H4,60,C'Type'                                                    
         SSPEC H4,69,C'Code'                                                    
*                                                                               
         SSPEC H5,1,C'------'                                                   
         SSPEC H5,26,C'------'                                                  
         SSPEC H5,51,C'------'                                                  
         SSPEC H5,10,C'------'                                                  
         SSPEC H5,35,C'------'                                                  
         SSPEC H5,60,C'------'                                                  
         SSPEC H5,19,C'----'                                                    
         SSPEC H5,44,C'----'                                                    
         SSPEC H5,69,C'----'                                                    
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPREPCP01 01/09/02'                                      
         END                                                                    
