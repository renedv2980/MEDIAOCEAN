*          DATA SET SPREPFXRST AT LEVEL 004 AS OF 11/15/00                      
*PHASE SPFX02R                                                                  
*INCLUDE PRTREC                                                                 
         TITLE 'SPFX02 - WRITE AN EXTRACTED RECORD BACK TO SPTFILE'             
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FXOPEN                                                           
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
FXOPEN   OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DCHO                                                                   
*                                                                               
FX10     L     R0,=A(MYIO)                                                      
         LR    R6,R0               SAVE THE ADDRESS OF OLD RECORD               
         AHI   R0,-4                                                            
         GET   FILEIN,(0)                                                       
*                                                                               
         OC    0(12,R6),0(R6)                                                   
         BZ    FX10                                                             
         CLI   0(R6),X'FF'                                                      
         BE    FX10                                                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),0(R6)                                                    
         MVC   KEY+11(1),10(R6)                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETBUY              READ THE OLD RECORD                          
*                                                                               
         L     R6,=A(MYIO)         POINT TO THE OLD RECORD                      
         SR    R7,R7                                                            
         ICM   R7,3,13(R6)         GET LENGTH OF OLD BUY                        
         L     R0,ADBUY                                                         
         LR    R1,R7                                                            
         AHI   R1,2                CLEAR TWO BYTES FOLLOWING BUY                
         MVCL  R0,R6                                                            
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),PRINT,HEXOUT                
         GOTO1 REPORT                                                           
         BAS   RE,PRTBUY                                                        
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX20                                                             
         GOTO1 PUTBUY                                                           
*                                                                               
FX20     B     FX10                                                             
*                                                                               
FXX      CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DCHO                                                                   
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
PRTBUY   NTR1                                                                   
         L     R6,ADBUY                                                         
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
                                                                                
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,KEY+11           GET LINE NUMBER                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLIN,DUB                                                         
         GOTO1 REPORT                                                           
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
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,MACRF=(GM),                      X        
               RECFM=VB,LRECL=4096,EODAD=FXX                                    
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'**MYIO**'                                                    
         DS    A                                                                
MYIO     DS    4000C                                                            
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
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
         DS    CL2                                                              
PCML     DS    CL4                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPFXRST11/15/00'                                      
         END                                                                    
