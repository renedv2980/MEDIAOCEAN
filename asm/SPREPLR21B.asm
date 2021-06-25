*          DATA SET SPREPLR21B AT LEVEL 063 AS OF 05/01/02                      
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
         MVC   H8,=CL132'STATION      NOV97   FEB98   MAY98   JUL98   NX        
               OV98'                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   SHOWPLUS,C'Y'                                                    
         MVI   SPLUSNOW,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING SBKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVC   SBCODE(3),=C'STN'   SET TO READ PRIMARY DIRECTORY                
         MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR6    CLI   SBCODE,C'S'         STATIONS ONLY                                
         BNE   EXIT                                                             
         CLI   SBMEDIA,C'T'        TELEVISION ONLY                              
         BNE   EXIT                                                             
         CLI   SBSRC,C'N'                                                       
         BNE   EXIT                                                             
         OC    SBKMKT,SBKMKT       NOT A SPILL MARKET?                          
         BZ    SPLR2               YES, GET NEXT ONE                            
         CLI   SBBTYP,0            INCLUDE REGULAR BOOK TYPE                    
         BZ    GOODBOOK                                                         
         CLI   SBBTYP,X'E0'        AND EXTENDED BOOK TYPE                       
         BE    GOODBOOK                                                         
         B     SPLR2                                                            
GOODBOOK CLC   SBBOOK,=X'6107'                                                  
         BL    SPLR2                                                            
         BH    GDBOOK1                                                          
         MVC   PPPSTAT,PLSSTAT                                                  
         MVC   PPPMKT,PLSMKT                                                    
         MVC   PLSSTAT,SBSTAT                                                   
         MVC   PLSMKT,SBKMKT                                                    
         MVI   SHOWPLUS,C'N'       SPILL PRIOR TO FIRST BOOK                    
         B     SPLR2                                                            
GDBOOK1  CLC   SBBOOK,=X'610B'     CHECK ONLY BOOKS AFTER NOV '89               
         BL    SPLR2               BOOKS BEFORE THAT, FORGET IT                 
*                                                                               
         CLC   SBSTAT,PRVSTAT                                                   
         BE    STA_EQL                                                          
         BAS   RE,SPLR8                                                         
         MVC   P(5),SBSTAT                                                      
STA_EQL  CLC   SBKMKT,PRVMKT                                                    
         BE    USEITNOW                                                         
         BAS   RE,SPLR8                                                         
USEITNOW MVC   HALF,SBBOOK                                                      
         MVC   SPLUSNOW,SHOWPLUS                                                
*        MVC   P(5),SBSTAT         SHOW STATION                                 
*                                                                               
         LA    R2,P                                                             
         LA    R4,TABLE                                                         
DISPLOOP CLC   =X'FFFF',0(R4)                                                   
         BNE   CHECK1                                                           
         LA    R4,TABLE            NOT IN TABLE, USE FIRST ONE                  
         B     GETDISP                                                          
CHECK1   CLC   0(2,R4),SBBOOK                                                   
         BE    GETDISP                                                          
         LA    R4,3(R4)                                                         
         B     DISPLOOP                                                         
GETDISP  ZIC   R3,2(R4)                                                         
         AR    R2,R3               CALCULATE DISPLACEMENT                       
         SR    R3,R3                                                            
         ICM   R3,3,SBKMKT                                                      
         EDIT  (R3),(4,(R2))                                                    
         MVC   PRVSTAT,SBSTAT                                                   
         MVC   PRVMKT,SBKMKT                                                    
         B     SPLR2                                                            
         SPACE 2                                                                
SPLR8    NTR1                                                                   
         OC    PRVSTAT,PRVSTAT                                                  
         BZ    SKIPREP                                                          
         MVC   H8,=CL132'STATION      NOV97   FEB98   MAY98   JUL98   NX        
               OV98'                                                            
*        CLI   SHOWPLUS,C'Y'                                                    
*        BNE   NOPLUSP                                                          
         CLC   PPPSTAT(7),PRVSTAT  COMPARE STATION AND MARKET                   
         BE    NOPLUSP                                                          
         MVI   P+12,C'+'           NO SPILL PRIOR TO FIRST BOOK                 
NOPLUSP  GOTO1 REPORT                                                           
SKIPREP  MVC   PRVSTAT,SBSTAT                                                   
         MVC   PRVMKT,SBKMKT                                                    
*        MVC   PLSSTAT,SBSTAT                                                   
*        MVC   PLSMKT,SBKMKT                                                    
         MVI   SHOWPLUS,C'Y'                                                    
         XIT1                                                                   
* ---------------------------------------------------------------               
         SPACE 3                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* P DISPLACEMENT FOR DIFFERENT BOOKS                                            
TABLE    DC    AL1(97,11,14)       NOV '97                                      
         DC    AL1(98,02,22)       FEB '98                                      
         DC    AL1(98,05,30)       MAY '98                                      
         DC    AL1(98,07,38)       JUL '98                                      
         DC    AL1(98,11,46)       NOV '98                                      
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
PRVMKT   DS    CL2                                                              
PLSSTAT  DS    CL5                                                              
PLSMKT   DS    CL2                                                              
PPPSTAT  DS    CL5                                                              
PPPMKT   DS    CL2                                                              
SHOWPLUS DS    C                                                                
SPLUSNOW DS    C                                                                
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
**PAN#1  DC    CL21'063SPREPLR21B05/01/02'                                      
         END                                                                    
