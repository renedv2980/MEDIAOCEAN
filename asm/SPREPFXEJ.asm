*          DATA SET SPREPFXEJ  AT LEVEL 003 AS OF 04/14/99                      
*PHASE SPFX02E                                                                  
         SPACE 1                                                                
         TITLE 'SPFX02 - FIX BAD 680B ELEM LENGTHS'                             
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
         MVI   FIXED,C'N'                                                       
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    FX10                                                             
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         XC    COUNT,COUNT                                                      
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
         MVI   FIXED,C'N'                                                       
*                                                                               
*&&DO                                                                           
FX14     MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
FX16     BAS   RE,NEXTEL                                                        
         BNE   FX20                                                             
*                                                                               
         CLI   1(R6),X'0B'         IGNORE CORRECT LENGTH                        
         BE    FX16                                                             
*&&                                                                             
*                                                                               
* PRINT KEY & ELEM                                                              
FX16     DS    0H                                                               
         CLC   COUNT,=F'100'                                                    
         BNL   FX20                                                             
         L     RF,COUNT                                                         
         AHI   RF,1                                                             
         ST    RF,COUNT                                                         
*                                                                               
         BAS   RE,PRTBUY                                                        
         GOTO1 HEXOUT,DMCB,(R6),P+8,11,=C'TOG'                                  
         GOTO1 REPORT                                                           
         B     FX16                                                             
*                                                                               
FX20     CLI   FIXED,C'Y'                                                       
         BNE   EXIT                                                             
*&&DO                                                                           
         GOTO1 PUTBUY                                                           
         BAS   RE,PRTBUY                                                        
*                                                                               
         BC    0,EXIT                                                           
         OI    *-3,X'F0'                                                        
         BAS   RE,PRTELS                                                        
*&&                                                                             
         B     EXIT                                                             
         EJECT                                                                  
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
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
*===============================================================*               
* SUBROUTINE PRINTS BUY ELEMENTS                                *               
*===============================================================*               
         SPACE 1                                                                
PRTELS   NTR1                                                                   
         CLI   QOPT1,C'Y'          PRINT ELEMENT TRACE                          
         BNE   PRTELX                                                           
*                                                                               
PRTEL0   MVC   P(5),=C'AFTER'                                                   
         CLI   FIXED,C'Y'                                                       
         BE    *+10                                                             
         MVC   P(6),=C'BEFORE'                                                  
* PRINT KEY AND BDELEM                                                          
         L     R6,ADBUY                                                         
         GOTO1 HEXOUT,DMCB,(R6),P+8,24,=C'TOG'                                  
         LA    R6,24(R6)                                                        
         GOTO1 (RF),(R1),(R6),P2+8,40                                           
         GOTO1 (RF),(R1),40(R6),P3+8,30                                         
         GOTO1 REPORT                                                           
*                                                                               
         SR    R0,R0                                                            
PRTEL2   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    PRTELX                                                           
         CLI   0(R6),X'06'         FIND FIRST REGEL                             
         BL    PRTEL2                                                           
*                                                                               
PRTEL4   LH    R4,MAXCHARS         SET MAX CHARS TO PRINT                       
         LA    R5,P+8              SET FIRST PRINT POSITION                     
*                                                                               
PRTEL6   IC    R0,1(R6)            GET ELEMENT LENGTH                           
         SR    R4,R0                                                            
         BP    PRTEL10                                                          
         AR    R4,R0               RESTORE R4 FOR NEXT TEST                     
         CH    R4,MAXCHARS         TEST ELEM WON'T EVER FIT                     
         BNE   PRTEL8              IT MIGHT, BUT THERE'S STUFF HERE             
         LH    R0,MAXCHARS         ELSE JUST PRINT MAXCHARS                     
         B     PRTEL10                                                          
*                                                                               
PRTEL8   GOTO1 REPORT                                                           
         B     PRTEL4                                                           
*                                                                               
PRTEL10  GOTO1 HEXOUT,DMCB,(R6),(R5),(R0),=C'TOG'                               
         A     R5,DMCB+16          ADD OUTPUT LENGTH                            
         LA    R5,1(R5)                                                         
*                                                                               
PRTEL12  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   PRTEL6                                                           
*                                                                               
         CH    R4,MAXCHARS         TEST ANYTHING MORE TO PRINT                  
         BE    PRTELX                                                           
         GOTO1 REPORT                                                           
*                                                                               
PRTELX   XIT1                                                                   
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
PRTCOUNT DC    PL4'0'                                                           
MAXCHARS DC    H'60'               MAX INPUT CHARS PER LINE                     
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
SPILLELS DS    1024C                                                            
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
         DS    CL1                                                              
PLINOLD  DS    CL3                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPFXEJ 04/14/99'                                      
         END                                                                    
