*          DATA SET SPREPFXBS  AT LEVEL 005 AS OF 11/15/00                      
*PHASE SPFX02M                                                                  
*INCLUDE PRTREC                                                                 
         SPACE 1                                                                
         TITLE 'SPFX02 - FIX BAD BUY ALLOCATION LENGTHS'                        
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
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
* PROCBUY                                                                       
FX10     DS    0H                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         USING BUYRECD,R6                                                       
         MVC   SVSEC,BDSEC         SAVE SLN                                     
         MVI   FIXED,C'N'                                                       
*                                                                               
FX14     MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
FX20     BAS   RE,NEXTEL                                                        
         BNE   FX40                                                             
*                                                                               
         CLI   1(R6),10            TEST ALLOCATED                               
         BNH   FX20                                                             
*                                                                               
         CLI   1(R6),14            TEST P/B                                     
         BH    FX30                YES                                          
*                                                                               
         CLC   SVSEC,11(R6)        MATCH SLN                                    
         BE    FX20                                                             
         OC    4(2,R6),4(R6)       TEST PAID                                    
         BZ    *+10                                                             
         MVC   PKEY(8),=C'PAID-UGH'                                             
         BAS   RE,PRTBUY                                                        
         MVI   FIXED,C'Y'                                                       
         MVC   11(1,R6),SVSEC      FIX THE STUPID SPOT                          
         B     FX20                                                             
*                                                                               
FX30     SR    R0,R0                                                            
         IC    R0,11(R6)                                                        
         SR    R1,R1                                                            
         IC    R1,15(R6)                                                        
         AR    R0,R1                                                            
         CLM   R0,1,SVSEC                                                       
         BE    FX20                                                             
         TM    SVSEC,X'01'         TEST EVEN SLN                                
         BZ    FX32                                                             
         MVC   PKEY(14),=C'CANNOT BE FIXED'                                     
*                                                                               
FX32     BAS   RE,PRTBUY                                                        
         TM    SVSEC,X'01'                                                      
         BO    FX20                                                             
*                                                                               
         MVI   FIXED,C'Y'                                                       
         SR    R0,R0                                                            
         IC    R0,SVSEC                                                         
         SRL   R0,1                                                             
         STC   R0,11(R6)                                                        
         STC   R0,15(R6)                                                        
         B     FX20                                                             
*                                                                               
FX40     CLI   FIXED,C'Y'                                                       
         BNE   EXIT                                                             
*                                                                               
         GOTO1 PUTBUY                                                           
*                                                                               
         BC    0,EXIT                                                           
         OI    *-3,X'F0'                                                        
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),PRINT,HEXOUT                
         B     EXIT                                                             
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
         BC    0,PRTB2                                                          
         OI    *-3,X'F0'                                                        
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),PRINT,HEXOUT                
*                                                                               
         CLI   FIXED,C'Y'          TEST BEEN HERE BEFORE                        
         BE    EXIT                                                             
*                                                                               
PRTB2    L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         MVC   PAGY(2),BUYALPHA                                                 
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
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
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
*                                                                               
         EJECT                                                                  
*                                                                               
COUNT    DS    F                                                                
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVSEC    DS    X                                                                
FIXED    DS    C                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
INRECS   DC    F'0',CL20'RECORDS IN'                                            
OUTRECS  DC    F'0',CL20'RECORDS OUT'                                           
         SPACE 2                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
*                                                                               
       ++INCLUDE SPGENEST                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         ORG   P                                                                
* DSECT FOR PRINT LINE                                                          
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
PLINOLD  DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPFXBS 11/15/00'                                      
         END                                                                    
