*          DATA SET DEITNCO    AT LEVEL 002 AS OF 01/22/15                      
*PHASE DEITNCOA                                                                 
                                                                                
*---------------------------------------------------------------------*         
* OUTPUT PHASE FOR FAKE CONVERSION THAT GENERATES NTI CODES FOR                 
* A LIST OF NIELSEN PROGRAM NUMBERS FOR ITN.                                    
*                                                                               
* IPHASE: DEITNCI                                                               
* OPHASE: DEITNCO                                                               
*---------------------------------------------------------------------*         
                                                                                
         TITLE 'ITN CODES OUTPUT PHASE'                                         
DEITNCO  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DEITNCO                                                        
         USING DEMCOND,R8          R8=A(GLOBAL W/S)                             
                                                                                
         B     *+4(R1)                                                          
         B     CNV10               PROCESS A RECORD                             
         B     CNV20               LAST TIME HOOK                               
                                                                                
CNV10    B     CNVX                NOTHING TO PROCESS                           
                                                                                
CNV20    DS    0H                  RELEASE UPDATED BIT MAP                      
         GOTO1 VNTIPRG,DMCB,=C'WRTM',(0,VBITMAP1),0                             
         DS    0H                  GENERATE J-RECS                              
         GOTO1 VNTIPRG,DMCB,=C'JREC',(0,VBITMAP1),0                             
         B     CNVX                                                             
                                                                                
CNVX     XMOD1 1                                                                
                                                                                
         LTORG                                                                  
         EJECT                                                                  
* DEDEMCNVD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMCNVD                                                      
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DEITNCO   01/22/15'                                      
         END                                                                    
