*          DATA SET SPREPFX02A AT LEVEL 057 AS OF 07/12/00                      
*PHASE SPFX02A                                                                  
*INCLUDE MEDPRDRD                                                               
         TITLE 'SPFX02 - TEST NEW SPMEDPRDRD'                                   
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
*                                                                               
FX10     OC    EORDN+8(8),EORDN+8   TEST ANY MAR OR APR 01 DOLLARS              
         BZ    FX20                                                             
                                                                                
         BAS   RE,PRTIT                                                         
*                                                                               
FX20     MVI   MODE,ESTLAST                                                     
         B     EXIT                                                             
         EJECT                                                                  
PRTIT    NTR1                                                                   
         LA    R5,P                                                             
         USING PLINED,R5                                                        
*                                                                               
         MVC   PMED,QMED                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,EKEYCLT,PCLT                                         
*                                                                               
         MVC   PPRD,EKEYPRD                                                     
*                                                                               
         SR    R0,R0                                                            
         IC    R0,EKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         MVC   PSTART,ESTART                                                    
         MVI   PSTART,C'0'                                                      
         MVC   PEND,EEND                                                        
         MVI   PEND,C'0'                                                        
*                                                                               
         L     R0,EORDN+8          GET MARCH DOLLARS                            
         EDIT  (R0),(10,PDOLMAR),2,ZERO=NOBLANK                                 
*                                                                               
         L     R0,EORDN+12         GET APRIL DOLLARS                            
         EDIT  (R0),(10,PDOLAPR),2,ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL2                                                              
PSTART   DS    CL6                                                              
         DS    CL2                                                              
PEND     DS    CL6                                                              
         DS    CL2                                                              
PDOLMAR  DS    CL10                                                             
         DS    CL2                                                              
PDOLAPR  DS    CL10                                                             
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057SPREPFX02A07/12/00'                                      
         END                                                                    
