*          DATA SET PPREPFS01  AT LEVEL 008 AS OF 07/08/96                      
*PHASE PPFS01A,+0                                                               
         TITLE 'PPFS01 - STATUS FIX (CHECK#/DATE)'                              
PPFS01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         PSPEC H1,36,C'PRINTPAK CLEARANCE STATUS CHECK CHANGE'                  
         PSPEC H2,36,C'--------------------------------------'                  
         PSPEC H1,99,AGYNAME                                                    
         PSPEC H2,99,AGYADD                                                     
         PSPEC H4,99,REPORT                                                     
         PSPEC H4,119,PAGE                                                      
         PSPEC H5,99,RUN                                                        
         PSPEC H8,2,C'****        KEY FIELDS         ****'                      
         PSPEC H9,2,C'AGY M   CLT   PUB         DATE  SEQ'                      
         PSPEC H10,2,C'-----------------------------------'                     
         PSPEC H8,56,C'****  OLD  ****   ****  NEW  ****'                       
         PSPEC H9,56,C'CHECK# * DATE *   CHECK# * DATE *'                       
         PSPEC H10,56,C'------ --------   ------ --------'                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPREPFS01 07/08/96'                                      
         END                                                                    
