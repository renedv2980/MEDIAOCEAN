*          DATA SET SPREPLR26  AT LEVEL 044 AS OF 05/01/02                      
*PHASE SPLR02B,+0,NOAUTO                                                        
         TITLE 'SPREPLR26 ANALYZE 93/94 CABLE FILE'                             
         PRINT NOGEN                                                            
SPLR02   CSECT                                                                  
         NMOD1 0,SPLR02,RR=R5                                                   
*                                                                               
         LA    R4,2048(RB)         R4 IS SECOND BASE REGISTER                   
         LA    R4,2048(R4)                                                      
         USING SPLR02,RB,R4                                                     
*                                                                               
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
OPEN3    L     R2,ADAGY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',FILELIST,(R2)                     
         B     OPEN4                                                            
         EJECT                                                                  
FILELIST DS    0C                                                               
         DC    CL8'NPAVDIR '                                                    
         DC    CL8'NPAVFIL '                                                    
         DC    C'X '                                                            
*                                                                               
OPEN4    DS    0H                                                               
         EJECT                                                                  
SPLD0    LA    R6,SPLKEY                                                        
         USING PMKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVC   PMCODE(3),=C'QCN'                                                
         MVC   SVSPLKEY,SPLKEY                                                  
         XC    RCOUNT,RCOUNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PAVDIR ',SVSPLKEY,SPLKEY              
         MVC   HLDSTA,PMSTAT                                                    
         B     SPLD6                                                            
         SPACE 1                                                                
SPLD2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PAVDIR ',SVSPLKEY,SPLKEY              
         CLI   8(R1),0                                                          
         BNE   SPRR0                                                            
SPLD6    OC    RCOUNT,RCOUNT                                                    
*        BZ    SPLD7                                                            
         MVC   P(5),HLDSTA                                                      
         MVC   P+7(4),=CL4'TOT '                                                
         EDIT  (B4,RCOUNT),(6,P+20)                                             
         LA    RE,CABTBL                                                        
CNVNET   CLI   0(RE),X'FF'                                                      
         BE    CNVNETX                                                          
         CLC   4(4,RE),HLDSTA                                                   
         BE    *+12                                                             
         LA    RE,L'CABTBL(RE)                                                  
         B     CNVNET                                                           
         MVC   P+27(4),0(RE)                                                    
CNVNETX  XC    RCOUNT,RCOUNT                                                    
         MVC   HLDSTA,PMSTAT                                                    
         GOTO1 REPORT                                                           
SPLD7    XC    CCOUNT,CCOUNT                                                    
         CLC   PMCODE(3),=C'QCN'                                                
         BNE   SPRR0                                                            
         MVC   SVDA,PMNDXDA                                                     
         BAS   RE,SPLF0                                                         
         B     SPLD2                                                            
         SPACE 1                                                                
SPLF0    NTR1                                                                   
         MVC   RECIO(20),SPLKEY                                                 
         XC    RCOUNT,RCOUNT                                                    
         XC    CCOUNT,CCOUNT                                                    
         XC    DCOUNT,DCOUNT                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'PAVFIL ',SVDA,RECIO                   
         B     SPLF2                                                            
SPLF1    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'PAVFIL ',SVDA,RECIO                   
SPLF2    CLI   8(R1),0                                                          
         BNE   SPLFEX                                                           
         LA    R6,RECIO                                                         
         USING PMKEY,R6                                                         
         LA    R7,PMDATA                                                        
         USING MARELEM,R7                                                       
*                                                                               
*                                                                               
SPLF5    LA    R6,RECIO                                                         
         XC    RECIOLN,RECIOLN                                                  
         SR    RE,RE                                                            
         ICM   RE,3,PMRLEN                                                      
         LA    RE,4(RE)                                                         
         STCM  RE,3,RECIOLN                                                     
*                                                                               
         L     RE,RCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,RCOUNT                                                        
         BAS   RE,PUTOUT                                                        
         L     RE,CCOUNT                                                        
         C     RE,=F'20'                                                        
         BH    SPLF1                                                            
         LA    R4,PMDATA                                                        
PR0F     CLI   0(R4),0                                                          
         BE    SPLF1                                                            
         CLI   0(R4),X'0F'                                                      
         BE    PR0F3                                                            
PR0F2    ZIC   R5,1(R4)                                                         
         CLI   QOPT1,C'N'                                                       
         BE    PROF2A                                                           
         GOTO1 HEXOUT,DMCB,(R4),P+4,(R5)                                        
         GOTO1 REPORT                                                           
PROF2A   AR    R4,R5                                                            
         B     PR0F                                                             
PR0F3    GOTO1 HEXOUT,DMCB,(R4),P+2,10                                          
         MVC   P+20(80),0(R4)                                                   
         GOTO1 REPORT                                                           
         B     PR0F2                                                            
*                                                                               
         DROP  R6,R7                                                            
SPLFEX   XIT1                                                                   
         EJECT                                                                  
PUTOUT   NTR1                                                                   
         LA    R5,RECIOLN                                                       
         L     RE,OCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,OCOUNT                                                        
         L     RE,CCOUNT                                                        
         LA    RE,1(RE)                                                         
         ST    RE,CCOUNT                                                        
         C     RE,=F'10'                                                        
         BH    PUTOUTX                                                          
         GOTO1 HEXOUT,DMCB,RECIOLN,P,60                                         
         GOTO1 REPORT                                                           
PUTOUTX  XIT1                                                                   
         EJECT                                                                  
SPRR0    DS    0H                                                               
EXIT1    EDIT  (B4,OCOUNT),(6,P)                                                
         GOTO1 REPORT                                                           
EXIT     XMOD1 1                                                                
         EJECT                                                                  
CABTBL   DS    0CL8                                                             
         DC    CL4'TOT ',CL4'0000'    TOTAL USA                                 
         DC    CL4'CAB ',CL4'0001'    TOTAL CABLE                               
         DC    CL4'WTBS',CL4'5830'    TBS                                       
         DC    CL4'LIF ',CL4'6196'    LIFETIME                                  
         DC    CL4'MTV ',CL4'6198'    MTV                                       
         DC    CL4'HLN ',CL4'6199'    HEADLINE NEWS                             
         DC    CL4'FAM ',CL4'6201'    THE FAMILY CHANNEL                        
         DC    CL4'CNN ',CL4'6202'    CABLE NEWS NETWORK                        
         DC    CL4'ESPN',CL4'6204'    ESPN                                      
         DC    CL4'NICK',CL4'6212'    NICKELODEON                               
         DC    CL4'USAN',CL4'6217'    USA NETWORK                               
         DC    CL4'AEN ',CL4'6218'    A & E CABLE NETWORK                       
         DC    CL4'TLC ',CL4'6635'    THE LEARNING CHANNEL                      
         DC    CL4'TNN ',CL4'6221'    THE NASHVILLE NETWORK                     
         DC    CL4'LMT ',CL4'6381'    LIFETIME MEDICAL                          
         DC    CL4'TNT ',CL4'6390'    TNT                                       
         DC    CL4'TWC ',CL4'6523'    THE WEATHER CHANNEL                       
         DC    CL4'DSCV',CL4'6530'    THE DISCOVERY CHANNEL                     
         DC    CL4'VH1 ',CL4'6546'    VH-1                                      
         DC    CL4'CNBC',CL4'6521'    CNBC                                      
         DC    CL4'CMTV',CL4'6647'    CMT                                       
         DC    CL4'CMD ',CL4'7133'    COMEDY CENTRAL                            
         DC    CL4'HBO ',CL4'6510'    HOME BOX OFFICE                           
         DC    CL4'SHO ',CL4'6511'    SHOWTIME                                  
         DC    CL4'CMX ',CL4'6513'    CINEMAX                                   
         DC    CL4'BET ',CL4'6200'    BET                                       
         DC    X'FFFF'                                                          
         EJECT                                                                  
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
OCOUNT DC      F'0'                                                             
CCOUNT DC      F'0'                                                             
RCOUNT DC      F'0'                                                             
DCOUNT DC      F'0'                                                             
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
SVDA     DS    CL4                                                              
HLDSTA   DS    CL5                                                              
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
BKTYPE   DS    C                                                                
RECIOLN  DS    CL4                                                              
RECIO    DS    2000C                                                            
         LTORG                                                                  
         EJECT                                                                  
*     BOOK TYPES ARE ASSIGNED AS FOLLW                                          
*         H=HISPANIC                                                            
*         B=BLACK                                                               
*         T=TRADING AREA                                                        
*         M=METRO                                                               
*         N=NETWORK AFFILIATES ONLY                                             
*         S=SCANAMERICA / TMG SCANNER (ARB ONLY)                                
*         E=EXTENDED AREA                                                       
*                                                                               
* TABLE OF MARKETS                                                              
*                                                                               
MARKETS  DS    0XL06                                                            
*        DC    CL4'CHBX',AL2(5531)                                              
*        DC    CL4'CHMI',AL2(6119)                                              
*        DC    CL4'CFRE',AL2(7071)                                              
*        DC    CL4'CFAP',AL2(4199)                                              
*        DC    CL4'CBLN',AL2(5369)                                              
*        DC    CL4'CFPL',AL2(5369)                                              
*        DC    CL4'CIPA',AL2(7153)                                              
*        DC    CL4'CFSK',AL2(7109)                                              
*        DC    CL4'CJPM',AL2(4120)                                              
*        DC    CL4'CKRS',AL2(4120)                                              
*        DC    CL4'CIHF',AL2(2080)                                              
*        DC    CL4'CKKX',AL2(8069)                                              
*        DC    CL4'CFAC',AL2(8069)                                              
         DC    CL4'CFTF',AL2(4101)                                              
*                                                                               
MARKETSX DC    AL2(0)                                                           
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
RANKTAB  DS    10000C                                                           
SPILTAB  DS    0C                                                               
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
**PAN#1  DC    CL21'044SPREPLR26 05/01/02'                                      
         END                                                                    
