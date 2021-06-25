*          DATA SET SPREPFXNBR AT LEVEL 009 AS OF 06/17/99                      
*PHASE SPFX02N                                                                  
         SPACE 1                                                                
         TITLE 'SPREPFXNBR - FIX BAD PROGRAMS FROM BUY REVISIONS'               
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
         BE    FX00                                                             
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
FX00     DS    0H                                                               
         L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
*                                                                               
         USING BUYRECD,R6                                                       
         CLI   BDPROGRM+L'BDPROGRM-1,0                                          
         BNE   EXIT                NO PROBLEM WITH SPECIAL HERE                 
*                                                                               
         LA    RF,BDPROGRM+L'BDPROGRM-2   CHECK IF WE HAVE A '-S'               
         LA    R0,BDPROGRM                                                      
*                                                                               
FX10     CLI   0(RF),C' '                                                       
         BNH   FX15                                                             
         CLI   0(RF),C'S'          REAL SPECIAL?                                
         BNE   FX30                                                             
         BCTR  RF,0                                                             
         CR    RF,R0                                                            
         BL    FX30                                                             
         CLI   0(RF),C'-'                                                       
         BE    EXIT                YES, LEAVE PROGRAM ALONE                     
         B     FX30                                                             
*                                                                               
FX15     BCTR  RF,0                                                             
         CR    RF,R0                                                            
         BH    FX10                IF EQUAL, THEN WE CAN'T HAVE A '-S'          
*                                                                               
FX30     MVI   BDPROGRM+L'BDPROGRM-1,C' '                                       
*                                                                               
         L     R1,COUNT            NUMBER OF BUY RECORDS WITH SPECIAL           
         LA    R1,1(R1)               THAT SHOULDN'T BE                         
         ST    R1,COUNT                                                         
*                                                                               
FX60     GOTO1 PUTBUY              SAVE THE RECORD                              
*                                                                               
         BAS   RE,PRTBUY           SHOW THE RECORD                              
*                                                                               
*****    BC    0,EXIT                                                           
*****    OI    *-3,X'F0'                                                        
*****    BAS   RE,PRTELS                                                        
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
**PAN#1  DC    CL21'009SPREPFXNBR06/17/99'                                      
         END                                                                    
