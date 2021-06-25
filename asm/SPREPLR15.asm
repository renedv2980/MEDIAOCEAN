*          DATA SET SPREPLR15  AT LEVEL 021 AS OF 05/01/02                      
*PHASE SPLR02T,+0,NOAUTO                                                        
         TITLE 'SPREPLR15 CREATE RADIO M POINTERS'                              
         PRINT NOGEN                                                            
SPLR02   CSECT                                                                  
         NMOD1 0,SPLR02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         OPEN  (OUT,(OUTPUT))                                                   
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVC   DRCODE(3),=C'DRA'                                                
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR ',SVSPLKEY,SPLKEY              
SPLR6    CLC   DRCODE(3),=C'DRA'                                                
         BNE   EXIT1                                                            
         MVC   SVSPLKEY,SPLKEY                                                  
         MVC   HALF,DRBOOK                                                      
         XC    HALF,=X'FFFF'                                                    
         CLC   HALF,=X'5707'                                                    
         BNE   SPLR2                                                            
         MVC   PRVKEY,SPLKEY                                                    
         XC    SVSPLKEY,SVSPLKEY                                                
         LA    R7,SVSPLKEY                                                      
         USING BSKEY,R7                                                         
         MVC   BSCODE(3),DRCODE                                                 
         MVI   BSCODE,C'M'                                                      
         MVC   BSBOOK,DRBOOK                                                    
         MVI   BSIND,X'02'                                                      
         MVC   BSSTAT,DRSTAT                                                    
         MVC   BSKMKT(4),DRKMKT    MARKET/STYP/BTYP                             
         MVC   BSRMKT,DRKMKT                                                    
         MVC   BSHOME,DRHOME                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         CLC   SVSPLKEY(L'BSKMAJOR),SPLKEY                                      
         BE    SPLR8                                                            
         MVC   SVSPLKEY+20(4),=X'FFFF0000'                                      
         MVC   RECIOLN,=X'001B0000'                                             
         MVC   RECIO(24),SVSPLKEY                                               
         BAS   RE,PUTOUT                                                        
         LA    R6,PRVKEY                                                        
         MVC   P(5),DRSTAT                                                      
         EDIT  (B2,DRKMKT),(4,P+6)                                              
         ZIC   R9,HALF                                                          
         EDIT  (R9),(2,P+11)                                                    
         ZIC   R9,HALF+1                                                        
         EDIT  (R9),(2,P+13)                                                    
         MVC   P+20(5),=C'BSKEY'                                                
         GOTO1 REPORT                                                           
         DROP  R7                                                               
*                                                                               
SPLR8    XC    SVSPLKEY,SVSPLKEY                                                
         LA    R6,PRVKEY                                                        
         LA    R7,SVSPLKEY                                                      
         USING MLKEY,R7                                                         
         MVC   MLCODE(3),DRCODE                                                 
         MVI   MLCODE,C'M'                                                      
         MVC   MLBOOK,DRBOOK                                                    
         MVI   MLIND,X'00'                                                      
         MVC   MLSTAT,DRSTAT                                                    
         MVC   MLKMKT(4),DRKMKT    MARKET/STYP/BTYP                             
         MVC   MLRMKT,DRKMKT                                                    
         MVC   MLHOME,DRHOME                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         CLC   SVSPLKEY(15),SPLKEY                                              
         BE    SPLR18                                                           
         MVC   SVSPLKEY+20(4),=X'FFFF0000'                                      
         MVC   RECIOLN,=X'001B0000'                                             
         MVC   RECIO(24),SVSPLKEY                                               
         BAS   RE,PUTOUT                                                        
         LA    R6,PRVKEY                                                        
         MVC   P(5),DRSTAT                                                      
         EDIT  (B2,DRKMKT),(4,P+6)                                              
         ZIC   R9,HALF                                                          
         EDIT  (R9),(2,P+11)                                                    
         ZIC   R9,HALF+1                                                        
         EDIT  (R9),(2,P+13)                                                    
         MVC   P+20(5),=C'MLKEY'                                                
         GOTO1 REPORT                                                           
SPLR18   MVC   SVSPLKEY,PRVKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         LA    R6,SPLKEY                                                        
         B     SPLR2                                                            
         SPACE 2                                                                
         EJECT                                                                  
PUTOUT   NTR1                                                                   
         LA    R5,RECIOLN                                                       
         L     R7,=A(OUT)                                                       
         PUT   (R7),(R5)                                                        
         L     RE,OCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OCOUNT                                                        
         C     RE,=F'30'                                                        
         BH    PUTOUTX                                                          
         GOTO1 HEXOUT,DMCB,RECIOLN,P,60                                         
         GOTO1 REPORT                                                           
PUTOUTX  XIT1                                                                   
         EJECT                                                                  
EXIT1    CLOSE (OUT)                                                            
         EDIT  (B4,OCOUNT),(6,P)                                                
         GOTO1 REPORT                                                           
EXIT     XMOD1 1                                                                
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
OCOUNT DC      F'0'                                                             
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
PRVKEY   DS    CL24                                                             
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
RECIOLN  DS    CL4                                                              
RECIO    DS    2000C                                                            
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2000,                X        
               BLKSIZE=32760,MACRF=PM                                           
         LTORG                                                                  
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
**PAN#1  DC    CL21'021SPREPLR15 05/01/02'                                      
         END                                                                    
