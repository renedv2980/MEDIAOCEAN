*          DATA SET SPREPLR14  AT LEVEL 014 AS OF 12/16/87                      
*PHASE SPLR02T,+0,NOAUTO                                                        
         TITLE 'SPREPLR14 RADIO CHECK POINTERS'                                 
SPLR13   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLR13,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         SPACE 2                                                                
         LA    R6,SPLKEY                                                        
         USING DRKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   DRCODE,C'D'         SET TO READ PRIMARY DIRECTORY                
         MVI   DRMEDIA,C'R'                                                     
         MVI   DRSRC,C'N'          SET RATING SERVICE                           
         MVC   SVSPLKEY,SPLKEY                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR3                                                            
         SPACE 2                                                                
SPLR2    MVC   PREVKEY,SPLKEY                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR3    CLC   DRCODE(3),=C'DRN'                                                
         BNE   EXIT                                                             
         LA    R6,SPLKEY                                                        
         MVC   SVSPLKEY,SPLKEY                                                  
         CLC   PREVKEY(DRHOME-DRKEY),SPLKEY                                     
         BNE   SPLR2                                                            
         MVC   P(5),DRSTAT                                                      
         EDIT  (B2,DRKMKT),(4,P+6)                                              
         GOTO1 REPORT                                                           
         B     SPLR2                                                            
*                                                                               
EXIT     XMOD1 1                                                                
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
         SPACE 2                                                                
         LTORG                                                                  
COUNT    DC    F'0'                                                             
OCOUNT   DC    F'0'                                                             
RCOUNT   DC    F'0'                                                             
DCOUNT   DC    F'0'                                                             
SPLRRA   DC    F'0'                                                             
SPLRRB   DC    F'0'                                                             
SPLRRC   DC    F'0'                                                             
PREVKEY  DS    CL20                                                             
BOOKFLTR DS    CL2                                                              
BKLIST   DS    CL30                                                             
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
*                                                                               
ASTATAB  DC    A(STATAB)                                                        
SVDA     DS    F                                                                
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
RPTYPE   DS    C                                                                
RECIOLN  DS    CL4                                                              
RECIO    DS    2000C                                                            
         SPACE 2                                                                
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,LRECL=2000,                X        
               BLKSIZE=32760,MACRF=PM                                           
STATAB   DS    750000C                                                          
         SPACE 2                                                                
STATABD  DSECT                                                                  
SMS      DS    0CL10                                                            
SSTA     DS    CL5                                                              
SRSMKT   DS    CL2                                                              
SBOOK    DS    CL2                                                              
SSHOME   DS    CL1                                                              
STATEND  DS    0C                                                               
STATLEN  EQU   STATEND-SBOOK                                                    
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SPREPLR14 12/16/87'                                      
         END                                                                    
