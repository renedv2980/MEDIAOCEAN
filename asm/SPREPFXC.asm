*          DATA SET SPREPFXC   AT LEVEL 025 AS OF 10/05/98                      
*PHASE SPFX02C                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPFX02 - LIST MARKETS FOR TBS/CVS CONVERSION'                   
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,STAFRST                                                     
         BE    FX20                                                             
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
FX10     DS    0H                                                               
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,4,BI,A),WORK=1'                              
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=30'                                    
         EJECT                                                                  
FX20     DS    0H                                                               
         GOTO1                                                                  
         EJECT                                                                  
PRTBUY   NTR1                                                                   
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   PAGY,BUYALPHA                                                    
         MVC   BYTE,BUYKAM                                                      
         NI    BYTE,X'0F'                                                       
         ZIC   RF,BYTE                                                          
         LA    RE,MEDTAB(RF)                                                    
         MVC   PMED,0(RE)                                                       
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
*                                                                               
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKEY+10                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,BUYKEY,PKEY,13,=C'TOG'                               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
         SPACE                                                                  
ELCODE   DS    X                                                                
MEDTAB   DC    C' TRNX'                                                         
VSORTER  DC    V(SORTER)                                                        
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
* DSECT FOR PRINT LINE                                                          
PLINED   DSECT                                                                  
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PMED     DS    CL1                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL6                                                              
PKEY     DS    CL26                BUY LINE KEY                                 
         SPACE 3                                                                
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPFXC  10/05/98'                                      
         END                                                                    
