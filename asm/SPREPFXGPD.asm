*          DATA SET SPREPFXGPD AT LEVEL 005 AS OF 12/10/98                      
*PHASE SPFX02D,+0                                                               
         TITLE 'SPFXGPD'                                                        
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         LA    RC,SPACEND                                                       
         USING SPFXWRKD,RC                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
FX10     DS    0H                                                               
         MVI   FCRDBUYS,C'N'                                                    
*                                                                               
         PACK  WA16,Q2USER(16)                                                  
         GOTO1 HEXOUT,DMCB,WA16,P,16,=C'N'                                      
         GOTO1 REPORT                                                           
         SRP   WA16,64-6,5                                                      
         GOTO1 HEXOUT,DMCB,WA16,P,16,=C'N'                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
SPFXWRKD DSECT                                                                  
WA16     DS    XL16                                                             
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPFXGPD12/10/98'                                      
         END                                                                    
