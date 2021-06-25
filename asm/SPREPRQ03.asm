*          DATA SET SPREPRQ03  AT LEVEL 015 AS OF 08/29/00                      
*PHASE SPRQ03A                                                                  
         TITLE 'SPRQ03 - SPONSOR FILE SUB-CONTROLLER 00'                        
         PRINT NOGEN                                                            
SPRQ03   CSECT                                                                  
         NMOD1 0,SPRQ03                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R8                                                    
         LA    R8,2048(RA)                                                      
         LA    R8,2048(R8)                                                      
*                                                                               
         EJECT                                                                  
         MVI   MODE,PROCREC                                                     
         GOTO1 GO                                                               
         B     EXIT                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPRQ03 08/29/00'                                      
         END                                                                    
