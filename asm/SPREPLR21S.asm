*          DATA SET SPREPLR21S AT LEVEL 105 AS OF 05/01/02                      
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
         MVC   H6,=CL132'ARBITRON SOURCE'                                       
         MVC   H8,=CL132'STATION      NOV89   FEB90   MAY90   JUL90   N*        
               OV90'                                                            
         MVC   H9,=CL132'-------      -----   -----   -----   -----   -*        
               ----'                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVI   CLEARHED,C'N'       DON'T CLEAR HEADLINES                        
         MVI   FRSTTIME,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING SBKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   WANTSRCE,C'A'       ARBITRON                                     
         MVC   SBCODE(3),=C'STA'   SET TO READ PRIMARY DIRECTORY                
NIEL_1   MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         MVI   P+12,C'+'           INITIALIZE AS NO PREVIOUS BOOK               
         MVI   PRINTIT,C'N'                                                     
         MVI   SHOWSTAT,C'N'                                                    
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR6    CLI   SBCODE,C'S'         STATIONS ONLY                                
         BNE   EXIT                                                             
         CLI   SBMEDIA,C'T'        TELEVISION ONLY                              
         BNE   EXIT                                                             
         CLC   SBSRC,WANTSRCE      EITHER (A)RBITRON OR (N)IELSON               
         BNE   EXIT                                                             
         OC    SBKMKT,SBKMKT       NOT A SPILL MARKET?                          
         BZ    SPLR2               YES, GET NEXT ONE                            
         CLI   SBBTYP,0            INCLUDE REGULAR BOOK TYPE                    
         BZ    GOODBOOK                                                         
         CLI   SBBTYP,X'E0'        AND EXTENDED BOOK TYPE                       
         BE    GOODBOOK                                                         
         B     SPLR2                                                            
GOODBOOK CLC   SBBOOK,=X'5907'                                                  
         BL    SPLR2                                                            
         CLC   SBSTAT,PRVSTAT                                                   
         BE    MRKMEQL                                                          
         BAS   RE,SPLR8                                                         
         MVI   SHOWSTAT,C'Y'                                                    
MRKMEQL  CLC   SBKMKT,PRVMKT                                                    
         BE    STA_EQL0                                                         
         BAS   RE,SPLR8                                                         
STA_EQL0 MVI   PRINTIT,C'N'                                                     
         CLC   SBBOOK,=X'5907'                                                  
         BH    GDBOOK1                                                          
STA_EQL1 MVI   P+12,C' '           PREVIOUS BOOK EXISTS                         
         MVC   PRVSTAT,SBSTAT                                                   
         MVC   PRVMKT,SBKMKT                                                    
         B     SPLR2                                                            
*                                                                               
GDBOOK1  CLC   SBBOOK,=X'590B'     CHECK ONLY BOOKS AFTER NOV '89               
         BL    SPLR2               BOOKS BEFORE THAT, FORGET IT                 
*                                                                               
         MVI   PRINTIT,C'Y'                                                     
         CLI   SHOWSTAT,C'Y'                                                    
         BNE   *+10                                                             
         MVC   P(5),SBSTAT                                                      
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
         CLI   PRINTIT,C'Y'                                                     
         BNE   SKIPREP                                                          
NOPLUSP  GOTO1 REPORT                                                           
SKIPREP  MVC   PRVSTAT,SBSTAT                                                   
         MVC   PRVMKT,SBKMKT                                                    
         CLI   PRINTIT,C'Y'                                                     
         BNE   SKIPREP1                                                         
         MVI   SHOWSTAT,C'N'                                                    
SKIPREP1 MVI   P+12,C'+'           NO SPILL PRIOR TO FIRST BOOK                 
         XIT1                                                                   
* ---------------------------------------------------------------               
         SPACE 3                                                                
EXIT     CLI   FRSTTIME,C'Y'                                                    
         BNE   EXIT_RL                                                          
         MVI   FRSTTIME,C'N'                                                    
         XC    SPLKEY,SPLKEY                                                    
         MVC   SBCODE(3),=C'STN'   NIELSON'S                                    
         MVC   H6,=CL132'NIELSON SOURCE'                                        
         MVI   WANTSRCE,C'N'                                                    
         MVI   FORCEFUT,C'Y'       PAGE BREAK BEFORE NIELSON                    
WRAPUP   CLI   PRINTIT,C'Y'        MAKE SURE LAST ONE IS PRINTED                
         BE    PRNTREPT                                                         
         XC    P,P                                                              
PRNTREPT GOTO1 REPORT                                                           
         MVI   FORCEFUT,C'N'                                                    
         B     NIEL_1                                                           
*                                                                               
EXIT_RL  CLI   PRINTIT,C'Y'                                                     
         BNE   EXIT_XIT                                                         
         GOTO1 REPORT                                                           
EXIT_XIT XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* P DISPLACEMENT FOR DIFFERENT BOOKS                                            
TABLE    DC    AL1(89,11,14)       NOV '89                                      
         DC    AL1(90,02,22)       FEB '90                                      
         DC    AL1(90,05,30)       MAY '90                                      
         DC    AL1(90,07,38)       JUL '90                                      
         DC    AL1(90,11,46)       NOV '90                                      
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
PRINTIT  DS    C                                                                
SHOWSTAT DS    C                                                                
FRSTTIME DS    C                                                                
WANTSRCE DS    C                                                                
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
**PAN#1  DC    CL21'105SPREPLR21S05/01/02'                                      
         END                                                                    
