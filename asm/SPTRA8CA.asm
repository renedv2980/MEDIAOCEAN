*          DATA SET SPTRA8CA   AT LEVEL 008 AS OF 05/12/16                      
*PHASE T2168CA   A FOR NET/C FOR SPOT                                           
         TITLE 'SPTRA8C - DUMMY PHASE FOR COMMERCIAL SAVE AREA'                 
T2168C   CSECT                                                                  
         DC    C'*T2168C*'         OVERLAY ID                                   
         DC    A(T2168CX-T2168CA)  LENGTH OF OVERLAY                            
         DC    F'0'                NOT USED                                     
* WAS 26000                                                                     
T2168CA  DS    (40*1024)C          OVERLAY STORAGE                              
T2168CX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPTRA8CA  05/12/16'                                      
         END                                                                    
