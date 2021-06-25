*          DATA SET SPREPLR12  AT LEVEL 008 AS OF 07/30/90                      
*PHASE SPLR02T,+0,NOAUTO                                                        
         TITLE 'SPREPLR12 RADIO DIRECTORY REPORT'                               
SPLR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLR02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING BSKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   BSCODE,C'M'         SET TO READ PRIMARY DIRECTORY                
         MVI   BSMEDIA,C'R'                                                     
         MVI   BSSRC,C'A'          SET RATING SERVICE                           
         MVI   BSIND,X'02'                                                      
         MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR6    CLI   BSMEDIA,C'R'                                                     
         BNE   EXIT                                                             
         CLI   BSSRC,C'A'                                                       
         BNE   EXIT                                                             
         CLI   BSIND,X'02'                                                      
         BNE   SPLR2                                                            
         CLC   BSSTAT,PRVSTAT                                                   
         BNE   SPLR8                                                            
         MVC   HALF,BSBOOK                                                      
         XC    HALF,=X'FFFF'                                                    
         ZIC   R3,HALF                                                          
         EDIT  (R3),(2,P)                                                       
         ZIC   R3,HALF+1                                                        
         EDIT  (R3),(2,P+2)                                                     
         MVC   P+5(5),BSSTAT                                                    
         SR    R3,R3                                                            
         ICM   R3,3,BSRMKT                                                      
         EDIT  (R3),(4,P+11)                                                    
         SR    R3,R3                                                            
         ICM   R3,3,BSKMKT                                                      
         EDIT  (R3),(4,P+16)                                                    
         MVI   P+22,C'*'                                                        
         ICM   R3,3,PRVMKT                                                      
         EDIT  (R3),(4,P+24)                                                    
         GOTO1 REPORT                                                           
SPLR8    MVC   PRVSTAT,BSSTAT                                                   
         MVC   PRVMKT,BSRMKT                                                    
         B     SPLR2                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
RANKCTR  DC    F'0'                                                             
SPILCTR  DC    F'0'                                                             
LOOPCTR  DC    F'0'                                                             
USTOTAL  DC    F'0'                                                             
INDXCTR  DC    F'0'                                                             
LASTRANK DC    F'0'                                                             
PRVSTAT  DS    CL5                                                              
PRVMKT   DS    CL2                                                              
PVSMS    DS    CL7                                                              
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
         SPACE 2                                                                
RANKTAB  DS    10000C                                                           
SPILTAB  DS    100000C                                                          
*                                                                               
RANKTABD DSECT                                                                  
RMKT     DS    CL2                                                              
RNKUNV   DS    CL4                                                              
RNKPCT   DS    CL4                                                              
RANK     DS    CL2                                                              
SMARANK  DS    CL2                                                              
RMKTNAM  DS    CL30                                                             
RNKTABEN DS    0C                                                               
RANKLEN  EQU   RNKTABEN-RMKT                                                    
         SPACE 2                                                                
SPILTABD DSECT                                                                  
SMS      DS    0CL7                                                             
SMKT     DS    CL2                                                              
SSTA     DS    CL5                                                              
SBOOK    DS    CL2                                                              
SPILEND  DS    0C                                                               
SPILLEN  EQU   SPILEND-SMKT                                                     
         SPACE 2                                                                
HOMETABD DSECT                                                                  
HSTA     DS    CL5                                                              
HMKT     DS    CL2                                                              
HOMTABEN DS    0C                                                               
HOMELEN  EQU   HOMTABEN-HSTA                                                    
         SPACE 2                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPLR12 07/30/90'                                      
         END                                                                    
