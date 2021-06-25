*          DATA SET SRDSPUB01  AT LEVEL 029 AS OF 10/13/98                      
*PHASE PPSD01A,+0                                                               
         TITLE 'PPSD01 - PRTFIX SPECS'                                          
PPSD01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
*                                                                               
         SPROG 0,10                                                             
         PSPEC H1,40,C'PRINTPAK GENFILE SIZE-RECORD CREATE PROGRAM'             
         PSPEC H2,40,C'-------------------------------------------'             
         PSPEC H3,49,PERIOD                                                     
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 10                                                               
         PSPEC H7,1,C'RECORD            MID NUMBER'                             
         PSPEC H7,41,C'*RECORD DATA*'                                           
         PSPEC H7,83,C'PUBLICATION NAME'                                        
         PSPEC H8,1,C'----              --------- '                             
         PSPEC H8,41,C'-----------------------------------'                     
         PSPEC H8,83,C'----------------'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029SRDSPUB01 10/13/98'                                      
         END                                                                    
