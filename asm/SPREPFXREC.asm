*          DATA SET SPREPFXREC AT LEVEL 015 AS OF 08/24/00                      
*          DATA SET SPREPFXME2 AT LEVEL 004 AS OF 08/20/99                      
*PHASE SPFX02M                                                                  
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
FX       XC    KEY,KEY                                                          
         MVC   KEY(10),=X'21BE60FF007D5C07E2D1'                                 
         MVI   KEY+11,1                                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETBUY              READ THE OLD RECORD                          
*                                                                               
FX10     L     R0,ADBUY                                                         
         SHI   R0,4                                                             
         GET   FILEIN,(0)                                                       
*                                                                               
         L     R6,ADBUY                                                         
         CLC   24(12,R6),=X'21BE60FF007D5C07E2D10101'                           
         BNE   FX10                                                             
         L     R0,ADBUY                                                         
         SR    R1,R1                                                            
         ICM   R1,3,37(R6)                                                      
         L     RE,ADBUY                                                         
         AHI   RE,24                                                            
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(RF)                                                      
         AR    RF,R0                                                            
         XC    0(2,RF),0(RF)       CTSFB                                        
*                                                                               
         GOTO1 =V(PRTREC),DMCB,(C'E',ADBUY),(24,13),PRINT,HEXOUT                
*                                                                               
         CLI   RCWRITE,C'Y'                                                     
         BNE   FXX                                                              
         GOTO1 PUTBUY                                                           
         CLOSE FILEIN                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DCHO                                                                   
*                                                                               
FXX      GOTO1 AENDREQ                                                          
         EJECT                                                                  
PRTBUY   NTR1                                                                   
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
PRTB2    LA    R5,P+2                                                           
         USING PLINED,R5                                                        
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
               RECFM=VB,LRECL=4096                                              
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
MYFLAG   DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    0D                                                               
         DS    256XL4                                                           
ESTTABX  EQU   *                                                                
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
PCML     DS    CL4                                                              
         DS    CL1                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSLK                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPFXREC08/24/00'                                      
         END                                                                    
