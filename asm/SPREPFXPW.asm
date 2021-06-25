*          DATA SET SPREPFXPW  AT LEVEL 013 AS OF 08/28/98                      
*PHASE SPFX02V                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'SPFX02 - ADD CLT TAX $$ FIELD INTO ELEMENTS'                    
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         USING PLINED,P                                                         
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
FX       DS    0H                                                               
         L     RF,ADCONLST                                                      
         L     RF,VBRDMON-SPADCONS(RF)                                          
         ST    RF,BRDMON                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   MID1,MYHEAD                                                      
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D7A'     PW RECORDS ONLY                              
         MVC   KEY+2(4),KEYSAVE    A-M/CLT/PRD                                  
         MVC   KEY+6(1),KEYSAVE+9  EST                                          
         MVC   KEY+7(2),KEYSAVE+4  MKT                                          
         GOTO1 HIGH                                                             
         B     FX22                                                             
*                                                                               
FX20     DS    0H                                                               
         GOTO1 SEQ                                                              
                                                                                
FX22     DS    0H                                                               
         CLC   KEY(9),KEYSAVE                                                   
         BNE   FXEND                                                            
*                                                                               
FX30     DS    0H                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING PWRECD,R6                                                        
         GOTO1 MSUNPK,DMCB,(X'80',PWKMKT),P1+2,P1+8                             
         OC    PWKSTA,PWKSTA                                                    
         BNZ   *+10                                                             
         MVC   P1+8(8),SPACES                                                   
         GOTO1 HEXOUT,DMCB,PWFKEY,P+20,18,=C'SEP',0                             
         GOTO1 REPORT                                                           
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,PWDOLCDQ                                                  
         BAS   RE,GETEL                                                         
         BNE   FX20                                                             
*                                                                               
         USING PWDOLEL,R6                                                       
*                                                                               
         GOTO1 BRDMON,DMCB,(X'FF',PWDOLWK),HALF                                 
         GOTO1 DATCON,DMCB,(2,HALF),SVBRDYM                                     
         B     FX54                                                             
*                                                                               
FX52     DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BE    FX54                                                             
         LA    R7,=X'FFFFFFFF'                                                  
         B     FX56                                                             
*PWDOLWK                                                                        
FX54     LR    R7,R6               SAVE CURRENT ELEM ADDRESS                    
         GOTO1 BRDMON,DMCB,(X'FF',PWDOLWK),HALF                                 
         GOTO1 DATCON,DMCB,(2,HALF),DUB                                         
         CLC   DUB(4),SVBRDYM                                                   
         BE    FX60                                                             
* PRINT MONTH TOTALS                                                            
FX56     LA    R6,TOTEL                                                         
         MVC   PDATE(2),SVBRDYM+2                                               
         MVC   PDATE+3(3),=C'TOT'                                               
         BAS   RE,FMT                                                           
         GOTO1 REPORT                                                           
*                                                                               
         LA    R0,TOTEL            CLEAR TOTALS                                 
         LA    R1,TOTELX-TOTEL                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   0(R7),X'FF'         TEST EOR                                     
         BE    FX20                                                             
*                                                                               
FX60     LR    R6,R7               RESTORE ELEM POINTER                         
         GOTO1 DATCON,DMCB,(2,PWDOLWK),(5,PDATE)                                
         BAS   RE,FMT                                                           
         GOTO1 REPORT                                                           
* ADD ELEMENT TO TOTALS                                                         
         LA    RE,PWDOLSPT                                                      
         LA    RF,WIMSPOTS                                                      
         SR    R4,R4                                                            
         LA    R5,7                                                             
FX62     L     R0,0(R4,RE)                                                      
         A     R0,0(R4,RF)                                                      
         ST    R0,0(R4,RF)                                                      
         LA    4,4(R4)                                                          
         BCT   R5,FX62                                                          
         B     FX52                                                             
         EJECT                                                                  
FMT      NTR1                                                                   
*PWDOLSPT                                                                       
         ICM   R0,15,PWDOLSPT                                                   
         EDIT  (R0),(6,PSPOTS)                                                  
*PWDOLWG                                                                        
         ICM   R0,15,PWDOLWG                                                    
         EDIT  (R0),PWIMGRS,2                                                   
*PWDOLWN                                                                        
         ICM   R0,15,PWDOLWN                                                    
         EDIT  (R0),PWIMNET,2                                                   
*PWDOLTAX                                                                       
         ICM   R0,15,PWDOLTAX                                                   
         EDIT  (R0),PWIMTAX,2                                                   
*PWDOLCG                                                                        
         ICM   R0,15,PWDOLCG                                                    
         EDIT  (R0),PCLTGRS,2                                                   
*PWDOLCN                                                                        
         ICM   R0,15,PWDOLCN                                                    
         EDIT  (R0),PCLTNET,2                                                   
*PWDOLCTX                                                                       
         ICM   R0,15,PWDOLCTX                                                   
         EDIT  (R0),PCLTTAX,2                                                   
         B     EXIT                                                             
*                                                                               
FXEND    GOTO1 AENDREQ                                                          
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
*                                                                               
BRDMON   DS    F                                                                
SVBRDYM  DS    CL6                                                              
ELCODE   DS    X                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'**TOTEL*'                                                    
TOTEL    DC    H'0'                                                             
TOTWEEK  DC    H'0'                                                             
WIMSPOTS DC    F'0'                                                             
WIMTOTG  DC    F'0'                                                             
WIMTOTN  DC    F'0'                                                             
CLTTOTG  DC    F'0'                                                             
CLTTOTN  DC    F'0'                                                             
WIMTOTTX DC    F'0'                                                             
CLTTOTTX DC    F'0'                                                             
TOTELX   EQU   *                                                                
*                                                                               
MYHEAD   DS    CL132                                                            
         ORG   MYHEAD                                                           
         DS    CL8'  DATE  '                                                    
         DS    C                                                                
         DC    CL5'SPOTS'                                                       
         DS    CL1                                                              
         DC    CL10'  WIMGRS'                                                   
         DS    CL1                                                              
         DC    CL10'  WIMNET'                                                   
         DS    CL1                                                              
         DC    CL10'  WIMTAX'                                                   
         DS    CL1                                                              
         DC    CL10'  CLTGRS'                                                   
         DS    CL1                                                              
         DC    CL10'  CLTNET'                                                   
         DS    CL1                                                              
         DC    CL10'  CLTTAX'                                                   
         ORG                                                                    
         LTORG                                                                  
         EJECT                                                                  
PLINED   DSECT                                                                  
*                                                                               
PDATE    DS    CL8                                                              
         DS    C                                                                
PSPOTS   DS    CL5                                                              
         DS    CL1                                                              
PWIMGRS  DS    CL10                                                             
         DS    CL1                                                              
PWIMNET  DS    CL10                                                             
         DS    CL1                                                              
PWIMTAX  DS    CL10                                                             
         DS    CL1                                                              
PCLTGRS  DS    CL10                                                             
         DS    CL1                                                              
PCLTNET  DS    CL10                                                             
         DS    CL1                                                              
PCLTTAX  DS    CL10                                                             
*                                                                               
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPFXPW 08/28/98'                                      
         END                                                                    
