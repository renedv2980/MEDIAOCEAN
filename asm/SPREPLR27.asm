*          DATA SET SPREPLR27  AT LEVEL 034 AS OF 11/09/94                      
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
         L     RE,=A(STATAB)                                                    
         L     RF,=F'10000'                                                     
         XCEF                                                                   
         L     RE,=A(SPILTAB)                                                   
         L     RF,=F'30000'                                                     
         XCEF                                                                   
         XC    PRVBK,PRVBK                                                      
         MVI   FORCEHED,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING PIKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   PIKCODE,C'I'        SET TO READ PRIMARY DIRECTORY                
         MVI   PIMEDIA,C'T'                                                     
         MVI   PISRC,C'N'          SET RATING SERVICE                           
         MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
SPLRRH   GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR6    CLI   PIKCODE,C'I'                                                     
         BNE   SPEXIT                                                           
         CLI   PIMEDIA,C'T'                                                     
         BNE   SPEXIT                                                           
         CLI   PISRC,C'N'                                                       
         BNE   SPEXIT                                                           
         OC    PRVBK,PRVBK                                                      
         BNZ   *+10                                                             
         MVC   PRVBK,PIBOOK                                                     
         CLC   PRVBK,PIBOOK                                                     
         BNE   SPEXIT                                                           
         L     R1,=A(SPILTAB)                                                   
SRCHPNO  OC    0(3,R1),0(R1)                                                    
         BZ    SRCHPNO1                                                         
         CLC   0(3,R1),PIPNUM                                                   
         BE    SPLRINC                                                          
         LA    R1,3(R1)                                                         
         B     SRCHPNO                                                          
SRCHPNO1 MVC   0(3,R1),PIPNUM                                                   
         ZIC   RE,STACNT                                                        
         LA    RE,1(RE)                                                         
         STC   RE,STACNT                                                        
*        CLI   STACNT,20                                                        
*        BH    SRCHPNOX                                                         
         LA    RE,DRKWORK                                                       
         USING DRKEY,RE                                                         
         XC    SPILCTR,SPILCTR                                                  
         XC    DRKEY,DRKEY                                                      
         MVC   DRKEY(3),PIKEY                                                   
         MVI   DRKEY,C'R'                                                       
         MVC   DRBOOK,PIBOOK                                                    
         MVC   DRSTAT,PISTA                                                     
         MVC   DRRECW(L'DRKWORK),DRKWORK                                        
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',DRKWORK,DRRECW               
         LA    RE,DRRECW                                                        
         MVC   SVNDXDA,DRNDXDA                                                  
*                                                                               
         CLI   SRCHPNO,20                                                       
         BL    FRDHI                                                            
         MVC   DRHIGHD,PIDAY                                                    
         MVC   DRHIQHR,PISQH                                                    
*                                                                               
FRDHI    GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMFIL ',SVNDXDA,DRRECW               
         B     SRCHPN2A                                                         
         SPACE 2                                                                
SRCHPNO2 CLI   STACNT,20                                                        
         BH    SRCHPNOX                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMFIL ',SVNDXDA,DRRECW               
SRCHPN2A LA    RE,DRRECW                                                        
         CLI   8(R1),0                                                          
         BNE   SRCHPNOX                                                         
         LA    RE,DRFRSTEL                                                      
SRCHPNO3 DS    0C                                                               
         CLI   0(RE),0                                                          
         BE    SRCHPNO2                                                         
         TM    2(RE),X'80'                                                      
         BO    *+12                                                             
         CLI   0(RE),X'21'                                                      
         BE    SRCHPNO4                                                         
SRCHPN3A SR    RF,RF                                                            
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     SRCHPNO3                                                         
SRCHPNO4 L     R1,=A(SPILTAB)                                                   
         OC    2(3,RE),2(RE)                                                    
         BZ    SRCHPN3A                                                         
SRCHPNO5 OC    0(3,R1),0(R1)                                                    
         BZ    SRCHPNO7                                                         
         CLC   0(3,R1),2(RE)                                                    
         BE    SRCHPN3A                                                         
         LA    R1,3(R1)                                                         
         B     SRCHPNO5                                                         
SRCHPNO7 MVC   0(3,R1),2(RE)                                                    
         ST    RE,SVRE                                                          
         MVC   P+3(5),PISTA                                                     
         SR    RF,RF                                                            
         ICM   RF,7,2(RE)                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+10(7),DUB+4(4)                                                 
         MVC   P+20(6),7(RE)                                                    
         MVC   P+30(5),13(RE)                                                   
         MVC   P+36(2),19(RE)                                                   
         GOTO1 REPORT                                                           
         L     RE,SVRE                                                          
         L     RF,SPILCTR                                                       
         LA    RF,1(RF)                                                         
         ST    RF,SPILCTR                                                       
         B     SRCHPN3A                                                         
*                                                                               
SRCHPNOX DS    0H                                                               
         MVC   P+1(5),PISTA                                                     
         L     RF,SPILCTR                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+7(7),DUB+4(4)                                                  
         GOTO1 REPORT                                                           
         ICM   RE,15,NYADDR                                                     
         BNZ   *+8                                                              
         LA    RE,NYLIST-5                                                      
         CLI   0(RE),X'FF'                                                      
         BE    NY1                                                              
         LA    RE,5(RE)                                                         
         ST    RE,NYADDR                                                        
         MVC   PISTA(5),0(RE)                                                   
         B     SRCHPNO1                                                         
NY1      GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR',SVSPLKEY,SPLKEY               
         MVI   STACNT,40                                                        
*                                                                               
         L     R1,=A(STATAB)                                                    
SRCHTAB  CLI   0(R1),0                                                          
         BNE   *+14                                                             
         MVC   0(5,R1),PISTA                                                    
         B     SPLRINC                                                          
         CLC   PISTA,0(R1)                                                      
         BE    SPLRINC                    FOUND JUST GET NEXT PRG               
         LA    R1,5(R1)                                                         
         B     SRCHTAB                                                          
SPLRINC  SR    R1,R1                                                            
         ICM   R1,7,PIPNUM                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,7,PIPNUM                                                      
         XC    PISTA(11),PISTA                                                  
         MVC   SVSPLKEY,SPLKEY                                                  
         B     SPLRRH                                                           
SPEXIT   L     R6,=A(STATAB)                                                    
SPEXITP  MVC   P+1(5),0(R6)                                                     
         GOTO1 REPORT                                                           
         LA    R6,5(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   SPEXITP                                                          
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
NYADDR   DC    F'0'                                                             
NYLIST   DC    C'WABCT'                                                         
         DC    C'WCBST'                                                         
         DC    C'WLIGT'                                                         
         DC    C'WLIWT'                                                         
         DC    C'WNBCT'                                                         
         DC    C'WNETT'                                                         
         DC    C'WNYCT'                                                         
         DC    C'WNYWT'                                                         
         DC    C'WPIXT'                                                         
         DC    C'WWORT'                                                         
         DC    C'WYCCT'                                                         
         DC    C'WAJAT'                                                         
         DC    C'WYEDT'                                                         
         DC    C'KAB T'                                                         
         DC    C'KBNTT'                                                         
         DC    C'KFCBT'                                                         
         DC    X'FF'                                                            
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
STACNT   DC    X'0'                                                             
SVRE     DC    F'0'                                                             
RELO     DC    F'0'                                                             
RANKCTR  DC    F'0'                                                             
SPILCTR  DC    F'0'                                                             
LOOPCTR  DC    F'0'                                                             
USTOTAL  DC    F'0'                                                             
INDXCTR  DC    F'0'                                                             
LASTRANK DC    F'0'                                                             
PRVSTAT  DS    CL5                                                              
PRVMKT   DS    CL2                                                              
PRVBK    DS    CL2                                                              
PVSMS    DS    CL7                                                              
SPLKEY   DS    CL24                                                             
SVSPLKEY DS    CL24                                                             
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
SVNDXDA  DS    CL4                                                              
DRKWORK  DS    CL24                                                             
DRRECW   DS    1400C                                                            
         SPACE 2                                                                
STATAB   DS    10000C                                                           
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
**PAN#1  DC    CL21'034SPREPLR27 11/09/94'                                      
         END                                                                    
