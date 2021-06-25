*          DATA SET ACREPTU01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACTU01A,+0                                                               
         TITLE 'TIME UPDATE PROGRAM'                                            
ACTU01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,1,RUN                                                         
         ACDEF H1,60,C'TIME UPDATE REPORT'                                      
         ACDEF H2,60,C'------------------'                                      
         ACDEF H3,1,C'COMPANY'                                                  
         ACDEF H3,133,PAGE                                                      
         ACDEF H4,133,C'REPORT ACTU'                                            
*                                                                               
         ACDEF H8,02,C'ACCOUNT CODE'                                            
         ACDEF H8,15,C'ACCOUNT NAME'                                            
         ACDEF H8,52,C'TYPE'                                                    
*                                                                               
         ACDEF H7,63,CL40'PERIOD\     FIRST       FIRST     PERIOD  '           
         ACDEF H8,63,CL40'END DATE     MOA        HOURS      HOURS  '           
*                                                                               
         ACDEF H8,107,CL24'   TOTAL            '                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPTU01 08/16/00'                                      
         END                                                                    
