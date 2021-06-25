*          DATA SET ACREPWF01  AT LEVEL 008 AS OF 09/24/01                      
*PHASE ACWF01A,*+0                                                              
         TITLE 'ACWF01 - READ WORKER FILE AND CHANGE ELEM ON FILE'              
ACWF01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0                                                                
         RSPEC REQUEST,NOSUM                                                    
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,99,PAGE                                                       
         ASPEC H1,45,C'WORKER LISTING'                                          
         ASPEC H2,45,C'--------------'                                          
         ASPEC H3,2,C'MEDIA  CLIENT  PRODUCT  ESTIMATE  INVOICE'                
         ASPEC H4,2,C'-----  ------  -------  --------  -------'                
         ASPEC H3,45,C'OLD U/L/A       NEW U/L/A'                               
         ASPEC H4,45,C'---------       ---------'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPWF01 09/24/01'                                      
         END                                                                    
