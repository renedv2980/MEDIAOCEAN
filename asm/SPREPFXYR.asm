*          DATA SET SPREPFXYR  AT LEVEL 045 AS OF 06/22/00                      
*PHASE SPFX02Y                                                                  
         TITLE 'SPFX02 - DELETE BAD BUYS YRTO'                                  
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BNE   EXIT                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         OC    BDREP,BDREP                                                      
         BZ    EXIT                                                             
         XC    BDREP,BDREP                                                      
         BRAS  RE,PRTBUY                                                        
         GOTO1 PUTBUY                                                           
         B     EXIT                                                             
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX50                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
* PROCBUY                                                                       
*                                                                               
FX50     DS    0H                                                               
         XC    KEY,KEY                                                          
         L     RE,ADCLT                                                         
         MVC   KEY(3),1(RE)                                                     
         MVI   KEY+3,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     FX54                                                             
*                                                                               
FX52     MVC   KEY,KEY1            RESTORE KEY                                  
         SR    R0,R0                                                            
         ICM   R0,7,KEY+10                                                      
         AHI   R0,1                                                             
         STCM  R0,7,KEY+10         FORCE NEXT KEY                               
         GOTO1 HIGH                                                             
*                                                                               
FX54     CLC   KEY(4),KEYSAVE                                                   
         BNE   REQL                                                             
         MVC   KEY1,KEY            SAVE CURRENT KEY                             
         CLI   KEY+10,0            TEST SPILL POINTER                           
         BNE   FX52                YES - IGNORE                                 
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   FX60                                                             
*                                                                               
         OC    BUYKMSTA(2),BUYKMSTA   TEST MARKET 0 BUY                         
         BZ    FX52                   YES - BUY IS GOOD                         
* NEED TO SEE IF NETWORK BUY ON FILE                                            
         MVC   DUB(4),2(R6)        MOVE NETWORK                                 
         MVI   DUB+4,C'N'                                                       
         GOTO1 MSPACK,DMCB,=C'0000',DUB,KEY+4                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    FX52                                                             
         MVC   KEY,KEY1            RESTORE KEY FOR PRINTING                     
*                                                                               
FX60     MVI   ELCDLO,X'0B'        TEST ANY SPOTS PAID                          
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
FX62     BAS   RE,NEXTEL                                                        
         BNE   FX70                                                             
         OC    4(2,R6),4(R6)                                                    
         BZ    FX62                                                             
         MVC   IDEL(4),=C'PAID'                                                 
* MISSING 68 ELEM                                                               
FX70     AP    RCNT,=P'1'                                                       
         BAS   RE,PRTBUY                                                        
*                                                                               
         OI    KEY+13,X'80'        DELETE DIRECTORY                             
         GOTO1 WRITE                                                            
*                                                                               
         OI    BUYREC+15,X'80'                                                  
         GOTO1 PUTBUY                                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST TO TRACE                                
         BNE   FX52                NO                                           
*                                                                               
         CLC   LASTKEY(6),KEY      SAME A-M/CLT/PRD/MKT                         
         BE    FX52                                                             
*                                                                               
         MVC   LASTKEY(6),KEY                                                   
         SR    RE,RE                                                            
         ICM   RE,3,13(R2)                                                      
         ST    RE,DMCB+12                                                       
*                                                                               
***      GOTO1 PRNTBL,DMCB,(8,=C'*RECORD*'),(R2),C'DUMP',,=C'2D',      X        
***            (C'P',PRINT)                                                     
*                                                                               
***      LA    R2,KEY                                                           
***      GOTO1 PRNTBL,DMCB,(8,=C'*POINTER'),(R2),C'DUMP',18,=C'2D',    X        
***            (C'P',PRINT)                                                     
*                                                                               
         B     FX52                                                             
LASTKEY  DC    XL6'00'                                                          
*                                                                               
FXX      DS    0H                                                               
         B     EXIT                                                             
         SPACE 2                                                                
*        REQUEST LAST                                                           
*                                                                               
REQL     DS    0H                                                               
         GOTO1 REPORT                                                           
         MVC   P(13),=C'RECORD TOTAL='                                          
         EDIT  (P6,RCNT),(5,P+13)                                               
         GOTO1 REPORT                                                           
*                                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R2,ADBUY                                                         
         USING BUYRECD,R2                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(5),WORK                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         MVC   PLID,IDEL                                                        
         GOTO1 HEXOUT,DMCB,KEY+14,PLDA,4,=C'N'                                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
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
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
ELEM     DS    XL256                                                            
ELCODE   DS    X                                                                
FIXSW    DS    C                                                                
AELMTAB  DC    A(ELMTAB)                                                        
AFRSTEL  DS    A                                                                
LASTDTE  DS    XL2                                                              
IDEL     DC    CL15' '                                                          
RCNT     DC    PL6'0'                                                           
AMOUNT   DC    PL6'0'                                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
ELMTAB   DS    50XL50                                                           
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
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
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
         DS    CL2                                                              
PLID     DS    CL15                                                             
         DS    CL5                                                              
PLDA     DS    CL8                                                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPREPFXYR 06/22/00'                                      
         END                                                                    
