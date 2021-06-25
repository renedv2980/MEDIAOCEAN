*          DATA SET SPREP4101  AT LEVEL 055 AS OF 04/14/20                      
*PHASE SP4101A,+0                                                               
         TITLE 'SP41-01 - CLT/PRD HEADER PRINT'                                 
SP4101   CSECT                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
         SPACE 1                                                                
         FSPEC USE,SP0003                                                       
**NOP**  RSPEC REQUEST,NOREP                                                    
         SPACE 1                                                                
         SPROG 10,20                                                            
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H3,99,REPORT                                                     
         SSPEC H4,99,PAGE                                                       
         SPACE                                                                  
         SPROG 10                                                               
         SSPEC H1,55,C'CLIENT HEADER PRINT'                                     
         SSPEC H2,55,C'-------------------'                                     
         SPACE                                                                  
         SPROG 20                                                               
         SSPEC H3,1,CLIENT                                                      
         SSPEC H1,55,C'PRODUCT HEADER PRINT'                                    
         SSPEC H2,55,C'--------------------'                                    
         SPACE 1                                                                
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055SPREP4101 04/14/20'                                      
         END                                                                    
