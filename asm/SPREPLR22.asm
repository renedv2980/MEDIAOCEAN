*          DATA SET SPREPLR22  AT LEVEL 125 AS OF 05/01/02                      
*PHASE SPLR02T,+0,NOAUTO                                                        
         TITLE 'SPREPLR22 TELEVISION ARB AND NSI'                               
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
         BNE   EXIT_XIT                                                         
         MVC   H6,=CL132'STATIONS'                                              
         MVC   H8,=CL132'ARBITRON  NIELSON'                                     
AFTHEAD  MVC   H9,=CL132'--------  -------'                                     
         MVI   FORCEHED,C'Y'                                                    
         MVI   CLEARHED,C'N'       DON'T CLEAR HEADLINES                        
         MVI   FRSTTIME,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING SBKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVC   SBCODE(3),=C'STA'   SET TO READ PRIMARY DIRECTORY                
         MVC   SVSPLKEY,SPLKEY                                                  
         LA    R7,NPLKEY                                                        
         USING NBKEY,R7                                                         
         XC    NPLKEY,NPLKEY                                                    
         MVC   NBCODE(3),=C'STN'   SET TO READ PRIMARY DIRECTORY                
         MVC   NVSPLKEY,NPLKEY                                                  
         XC    PRVSTAT,PRVSTAT                                                  
         XC    NPRVSTAT,NPRVSTAT                                                
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',NVSPLKEY,NPLKEY              
         MVI   PRINTIT,C'N'                                                     
         MVI   SHOWSTAT,C'N'                                                    
         B     SPLR6                                                            
         SPACE 2                                                                
*                                                                               
GETARB   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
         XIT1                                                                   
*                                                                               
GETNSI   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',NVSPLKEY,NPLKEY               
         XIT1                                                                   
*                                                                               
SPLR6    CLC   SBCODE(2),=C'STA'   MAKE SURE IT'S A TV STATION                  
         BNE   FIN_NSI                                                          
         TM    SBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_NSI                                                          
         CLC   NBCODE(2),=C'STN'   DITTO                                        
         BNE   FIN_ARB                                                          
         TM    NBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_ARB                                                          
*                                                                               
SPLR7    CLC   PRVSTAT,SBSTAT                                                   
         BNE   SPLR8                                                            
         BAS   RE,GETARB                                                        
         CLC   SBCODE(2),=C'STA'   MAKE SURE IT'S A TV STATION                  
         BNE   FIN_NSI                                                          
         TM    SBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_NSI                                                          
         B     SPLR7                                                            
*                                                                               
SPLR8    MVC   PRVSTAT,SBSTAT                                                   
SPLR8A   CLC   NPRVSTAT,NBSTAT                                                  
         BNE   SPLR9                                                            
         BAS   RE,GETNSI                                                        
         CLC   NBCODE(2),=C'STN'   MAKE SURE IT'S A TV STATION                  
         BNE   FIN_ARB                                                          
         TM    NBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_ARB                                                          
         B     SPLR8A                                                           
*                                                                               
SPLR9    MVC   NPRVSTAT,NBSTAT                                                  
SPLR9A   CLC   SBSTAT,NBSTAT       EITHER (A)RBITRON OR (N)IELSON               
         BL    SPLRARBS                                                         
         BE    SPLR7               BOTH EQUAL GOT NEXT ONES                     
*                                                                               
         MVC   P+10(5),NBSTAT                                                   
         GOTO1 REPORT                                                           
         B     SPLR8A                                                           
*                                                                               
SPLRARBS MVC   P(5),SBSTAT                                                      
         GOTO1 REPORT                                                           
SP_ARBSL CLC   PRVSTAT,SBSTAT                                                   
         BNE   SP_ARBSA                                                         
         BAS   RE,GETARB                                                        
         CLC   SBCODE(2),=C'STA'   MAKE SURE IT'S A TV STATION                  
         BNE   FIN_NSI                                                          
         TM    SBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_NSI                                                          
         B     SP_ARBSL                                                         
*                                                                               
SP_ARBSA MVC   PRVSTAT,SBSTAT                                                   
         B     SPLR9A                                                           
* ----------------------------------------------------------------              
FIN_NSI  CLC   SBSTAT,NBSTAT                                                    
         BE    FIN_NSIL                                                         
         MVC   P+10(5),NBSTAT                                                   
         GOTO1 REPORT                                                           
         MVC   NPRVSTAT,NBSTAT                                                  
FIN_NSIL CLC   NPRVSTAT,NBSTAT                                                  
         BNE   FIN_NSI                                                          
         BAS   RE,GETNSI                                                        
         CLC   NBCODE(2),=C'STN'   MAKE SURE IT'S A TV STATION                  
         BNE   EXIT_XIT                                                         
         TM    NBSTAT,X'F0'        SEE IF NUMBER                                
         BO    EXIT_XIT                                                         
         B     FIN_NSIL                                                         
*                                                                               
FIN_ARB  CLC   SBSTAT,NBSTAT                                                    
         BE    FIN_ARBL                                                         
         MVC   P(5),SBSTAT                                                      
         GOTO1 REPORT                                                           
         MVC   PRVSTAT,SBSTAT                                                   
FIN_ARBL CLC   PRVSTAT,SBSTAT                                                   
         BNE   FIN_ARB                                                          
         BAS   RE,GETARB                                                        
         CLC   SBCODE(2),=C'STA'   MAKE SURE IT'S A TV STATION                  
         BNE   EXIT_XIT                                                         
         TM    SBSTAT,X'F0'        SEE IF NUMBER                                
         BO    EXIT_XIT                                                         
         B     FIN_ARBL                                                         
* ----------------------------------------------------------------              
EXIT_XIT XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* P DISPLACEMENT FOR DIFFERENT BOOKS                                            
TABLE0   DC    AL1(87,11,14)       NOV '87                                      
         DC    AL1(88,02,22)       FEB '88                                      
         DC    AL1(88,05,30)       MAY '88                                      
         DC    AL1(88,07,38)       JUL '88                                      
         DC    AL1(88,11,46)       NOV '88                                      
         DC    AL1(89,02,54)       FEB '89                                      
         DC    AL1(89,05,62)       MAY '89                                      
         DC    AL1(89,07,70)       JUL '89                                      
         DC    AL1(89,11,78)       NOV '89                                      
         DC    AL1(90,02,86)       FEB '90                                      
         DC    AL1(90,05,94)       MAY '90                                      
         DC    AL1(90,07,102)      JUL '90                                      
         DC    AL1(90,11,110)      NOV '90                                      
         DC    X'FFFF'                                                          
         SPACE 2                                                                
TABLE1   DC    AL1(88,02,14)       FEB '88                                      
         DC    AL1(89,02,22)       FEB '89                                      
         DC    AL1(90,02,30)       FEB '90                                      
         DC    AL1(88,05,38)       MAY '88                                      
         DC    AL1(89,05,46)       MAY '89                                      
         DC    AL1(90,05,54)       MAY '90                                      
         DC    AL1(88,07,62)       JUL '88                                      
         DC    AL1(89,07,70)       JUL '89                                      
         DC    AL1(90,07,78)       JUL '90                                      
         DC    AL1(87,11,86)       NOV '87                                      
         DC    AL1(88,11,94)       NOV '88                                      
         DC    AL1(89,11,102)      NOV '89                                      
         DC    AL1(90,11,110)      NOV '90                                      
         DC    X'FFFF'                                                          
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
NPRVSTAT DS    CL5                                                              
PRVMKT   DS    CL2                                                              
PRINTIT  DS    C                                                                
SHOWSTAT DS    C                                                                
FRSTTIME DS    C                                                                
WANTSRCE DS    C                                                                
PVSMS    DS    CL7                                                              
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
NPLKEY   DS    CL24                                                             
NVSPLKEY DS    CL24                                                             
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
NBKEY    DSECT                                                                  
NBKMAJOR DS    0CL18                                                            
NBCODE   DS    CL1                                                              
NBCODEQU EQU   C'S'                                                             
NBMEDIA  DS    CL1                                                              
NBSRC    DS    CL1                                                              
NBSTAT   DS    CL5                                                              
NBKMKT   DS    XL2                                                              
NBSTYP   DS    XL1                                                              
NBBTYP   DS    XL1                                                              
NBRMKT   DS    XL2                                                              
NBBOOK   DS    XL2                                                              
NBHOME   DS    XL1                                                              
         DS    XL1                                                              
NBKSTAT  DS    XL1                                                              
NBNDXDA  DS    XL4                                                              
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125SPREPLR22 05/01/02'                                      
         END                                                                    
