*          DATA SET SPREPFXAZ  AT LEVEL 020 AS OF 10/18/00                      
*          DATA SET SPREPFXMK  AT LEVEL 003 AS OF 05/03/99                      
*PHASE SPFX02Z                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'SPFX02Z - FIX BAD DEMOS IN AZME BUYS'                           
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         OPEN  (TAPEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
FX10     LA    R0,TAPERECL                                                      
         GET   TAPEIN,(0)                                                       
*                                                                               
         XC    SAVEL,SAVEL                                                      
         LA    R6,TAPEREC+24                                                    
         MVI   ELCDLO,2                                                         
         MVI   ELCDHI,2                                                         
         BAS   RE,NEXTEL                                                        
         CLI   25(R6),C'I'         TEST IMPRESSION                              
         BNE   FX10                NO - NEXT                                    
         MVC   SAVEL,24(R6)        SAVE DEMO/VALUE                              
* NOW READ RECORD FROM SPTFILE                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(10),TAPEREC     MOVE A-M/CLT/PRD/MKT/STA/EST                 
         MVC   KEY+11(1),TAPEREC+10 MOVE LINE NUMBER                            
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    FX12                                                             
         XC    SAVEL,SAVEL                                                      
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEYARG                      
         MVC   PCOM(9),=C'NOT FOUND'                                            
         BAS   RE,PRTBUY                                                        
         B     FX10                                                             
*                                                                               
FX12     GOTO1 GETBUY                                                           
         USING BUYRECD,R6                                                       
         L     R6,ADBUY                                                         
         MVI   ELCDLO,X'02'                                                     
         MVI   ELCDHI,X'02'                                                     
         MVI   FIXED,C'N'                                                       
*                                                                               
FX14     LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
*                                                                               
         MVC   24(8,R6),SAVEL      MOVE OLD DEMO ELEMENT                        
         MVI   25(R6),C'R'         CHANGE RATING TO IMPRESSION                  
         MVI   FIXED,C'Y'                                                       
*                                                                               
FX20     CLI   FIXED,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
         L     R0,COUNT                                                         
         AHI   R0,1                                                             
         ST    R0,COUNT                                                         
*                                                                               
         GOTO1 PUTBUY                                                           
*                                                                               
         BAS   RE,PRTBUY                                                        
*                                                                               
         L     R0,COUNT                                                         
         SRDA  R0,32                                                            
         D     R0,=F'50'                                                        
         CHI   R0,1                TEST REMAINDER 1                             
         B     FX10                *** NOP ***                                  
         BNE   FX10                                                             
         GOTO1 =V(PRTREC),DMCB,ADBUY,(24,13),PRINT,HEXOUT                       
         B     FX10                                                             
*                                                                               
INPUTEOF CLOSE TAPEIN                                                           
*                                                                               
         L     R0,COUNT                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVC   P(14),=C'RECORDS FIXED'                                          
         UNPK  P+20(8),DUB                                                      
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
         LA    R6,KEY        *** NOTE ORIGIN IS KEY                             
         USING BUYRECD,R6                                                       
*                                                                               
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BH    *+8                                                              
         MVI   PSTA+5,C'T'                                                      
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,11(R6)                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         OC    SAVEL,SAVEL                                                      
         BZ    PRTB10                                                           
         LH    R0,SAVEL+6                                                       
         EDIT  (R0),(6,PCOM),1,ALIGN=LEFT                                       
*                                                                               
PRTB10   GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
COUNT    DS    F                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVSEC    DS    X                                                                
FIXED    DS    C                                                                
         DS    0D                                                               
         DC    CL8'**SAVEL*'                                                    
SAVEL    DS    D                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
*                                                                               
TAPEIN   DCB   DDNAME=TAPEIN,DSORG=PS,MACRF=(GM),EODAD=INPUTEOF,       X        
               RECFM=VB,LRECL=4004,BUFNO=2                                      
         SPACE 2                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'TAPEREC*'                                                    
TAPERECL DS    F                                                                
TAPEREC  DS    4000C                                                            
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
SDEFRECD DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         ORG   P                   PRINT LINE DSECT                             
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL1                                                              
PCOM     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPREPFXAZ 10/18/00'                                      
         END                                                                    
