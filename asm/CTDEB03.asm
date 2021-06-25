*          DATA SET CTDEB03    AT LEVEL 003 AS OF 01/09/13                      
*PHASE TA0F03A                                                                  
         TITLE 'CTDEB03 - PHASE PATCH MODULE'                                   
CTDEB03  CSECT                                                                  
         DC    A(PATCHAR-CTDEB03)                                               
         DC    A(PATCHWK-CTDEB03)                                               
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    C'***PATCH AREA***'                                              
PATCHAR  DC    (4096-32)X'00'      COVERED BY PATCHWKD                          
*                                                                               
         DC    C'***PATCH WORK***'                                              
PATCHWK  DS    (MAXPATCH*K)C                                                    
PATCHWKX EQU   *                                                                
         EJECT                                                                  
*CTDEBWORK                                                                      
       ++INCLUDE CTDEBWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTDEB03   01/09/13'                                      
         END                                                                    
