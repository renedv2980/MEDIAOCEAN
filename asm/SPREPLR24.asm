*          DATA SET SPREPLR24  AT LEVEL 038 AS OF 05/01/02                      
*PHASE SPLR02T,+0,NOAUTO                                                        
         TITLE 'SPREPLR24 TELEVISION HOME STATIONS'                             
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
         MVC   H8,=CL132'ARBITRON       NIELSON'                                
AFTHEAD  MVC   H9,=CL132'--------       -------'                                
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
         MVI   FOUNDARB,C'N'                                                    
         MVI   FOUNDNSI,C'N'                                                    
         MVI   ENDARB,C'N'                                                      
         MVI   ENDNSI,C'N'                                                      
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
         B     MAINCHK                                                          
*                                                                               
FINDARB  NTR1                                                                   
SPLR7    CLC   PRVSTAT,SBSTAT                                                   
         BNE   SPLR8                                                            
SPLR7A   BAS   RE,GETARB                                                        
         CLC   SBCODE(2),=C'STA'   MAKE SURE IT'S A TV STATION                  
         BNE   FIN_ARBX                                                         
         TM    SBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_ARBX                                                         
         CLC   PRVSTAT,SBSTAT                                                   
         BNE   SPLR8                                                            
         CLI   ASRCHNG,C'Y'                                                     
         BNE   SPLR7                                                            
         CLC   SBKMKT(2),=X'0000'                                               
         BE    COMPARB                                                          
         MVI   ASRCHNG,C'N'                                                     
         B     SPLR7                                                            
COMPARB  CLC   PA_MKTN,SBRMKT                                                   
         BE    SPLR7                                                            
         MVI   ASRCHNG,C'N'                                                     
         B     FINDARBX                                                         
*                                                                               
SPLR8    CLC   SBKMKT(2),=X'0000'                                               
         BNE   SPLR7A                                                           
         MVC   PRVSTAT,SBSTAT                                                   
         MVC   PA_MKTN,SBRMKT                                                   
         MVI   ASRCHNG,C'Y'                                                     
         MVI   FOUNDARB,C'N'                                                    
         B     SPLR7A                                                           
FINDARBX XIT1                                                                   
FIN_ARBX MVI   ENDARB,C'Y'                                                      
         XIT1                                                                   
*---------------------------------------------------------------                
FINDNSI  NTR1                                                                   
SPLR9    CLC   NPRVSTAT,NBSTAT                                                  
         BNE   SPLR10                                                           
SPLR9A   BAS   RE,GETNSI                                                        
         CLC   NBCODE(2),=C'STN'   MAKE SURE IT'S A TV STATION                  
         BNE   FIN_NSIX                                                         
         TM    NBSTAT,X'F0'        SEE IF NUMBER                                
         BO    FIN_NSIX                                                         
         CLC   NPRVSTAT,NBSTAT                                                  
         BNE   SPLR10                                                           
         CLI   NSRCHNG,C'Y'                                                     
         BNE   SPLR9                                                            
         CLC   NBKMKT(2),=X'0000'                                               
         BE    COMPNSI                                                          
         MVI   NSRCHNG,C'N'                                                     
         B     SPLR9                                                            
COMPNSI  CLC   PN_MKTN,NBRMKT                                                   
         BE    SPLR9                                                            
         MVI   NSRCHNG,C'N'                                                     
         B     FINDNSIX                                                         
*                                                                               
SPLR10   CLC   NBKMKT(2),=X'0000'                                               
         BNE   SPLR9A                                                           
         MVC   NPRVSTAT,NBSTAT                                                  
         MVC   PN_MKTN,NBRMKT                                                   
         MVI   NSRCHNG,C'Y'                                                     
         MVI   FOUNDNSI,C'N'                                                    
         B     SPLR9A                                                           
FINDNSIX XIT1                                                                   
FIN_NSIX MVI   ENDNSI,C'Y'                                                      
         XIT1                                                                   
*---------------------------------------------------------------                
MAINCHK  BAS   RE,FINDARB                                                       
         CLI   ENDARB,C'Y'                                                      
         BE    FIN_NSI                                                          
MAINCHKN BAS   RE,FINDNSI                                                       
         CLI   ENDNSI,C'Y'                                                      
         BE    FIN_ARB                                                          
*---------------------------------------------------------------                
MAINPR   CLC   SBSTAT,NBSTAT                                                    
         BL    PR_ARB                                                           
         BH    PR_NSI                                                           
         MVC   P(5),SBSTAT                                                      
         EDIT  (2,PA_MKTN),(3,P+6),ZERO=NOBLANK                                 
         EDIT  (2,SBRMKT),(3,P+10),ZERO=NOBLANK                                 
         MVC   P+15(5),NBSTAT                                                   
         EDIT  (2,PN_MKTN),(3,P+21),ZERO=NOBLANK                                
         EDIT  (2,NBRMKT),(3,P+25),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         B     MAINCHK                                                          
*                                                                               
PR_ARB   MVC   P(5),SBSTAT                                                      
         EDIT  (2,PA_MKTN),(3,P+6),ZERO=NOBLANK                                 
         EDIT  (2,SBRMKT),(3,P+10),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         BAS   RE,FINDARB                                                       
         CLI   ENDARB,C'Y'                                                      
         BE    FIN_NSI                                                          
         B     MAINPR                                                           
*                                                                               
PR_NSI   MVC   P+15(5),NBSTAT                                                   
         EDIT  (2,PN_MKTN),(3,P+21),ZERO=NOBLANK                                
         EDIT  (2,NBRMKT),(3,P+25),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         B     MAINCHKN                                                         
*---------------------------------------------------------------                
FIN_NSI  CLI   ENDNSI,C'Y'                                                      
         BE    EXIT_XIT                                                         
         MVC   P+15(5),NBSTAT                                                   
         EDIT  (2,PN_MKTN),(3,P+21),ZERO=NOBLANK                                
         EDIT  (2,NBRMKT),(3,P+25),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         BAS   RE,FINDNSI                                                       
         B     FIN_NSI                                                          
*----------------------------------------------------------------               
FIN_ARB  CLI   ENDARB,C'Y'                                                      
         BE    EXIT_XIT                                                         
         MVC   P(5),SBSTAT                                                      
         EDIT  (2,PA_MKTN),(3,P+6),ZERO=NOBLANK                                 
         EDIT  (2,SBRMKT),(3,P+10),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
         BAS   RE,FINDARB                                                       
         B     FIN_ARB                                                          
*----------------------------------------------------------------               
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
PA_MKTN  DS    XL2'0000'                                                        
ASRCHNG  DS    C'N'                                                             
PN_MKTN  DS    XL2'0000'                                                        
NSRCHNG  DS    C'N'                                                             
FOUNDARB DS    C'N'                                                             
FOUNDNSI DS    C'N'                                                             
ENDARB   DS    C'N'                                                             
ENDNSI   DS    C'N'                                                             
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
**PAN#1  DC    CL21'038SPREPLR24 05/01/02'                                      
         END                                                                    
