*          DATA SET PPRCVLOOK  AT LEVEL 022 AS OF 08/09/00                      
*PHASE PPLOOKA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE CLUNPK                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'PPRCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
RCVLOOK  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RCVLOOK,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVLOOK+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCVLOOK          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
IN4      DS    0H                                                               
         CLC   =C'NEN',RKEY                                                     
         BNE   IN2                                                              
         CLI   RKEY+3,X'20'                                                     
         BNE   IN2                                                              
         CLC   =C'BCY',RKEY+4                                                   
         BNE   IN2                                                              
****     CLC   =C'ENC',RKEY+7                                                   
****     BNE   IN2                                                              
         B     PRINTIT                                                          
*                                                                               
PRINTIT  DS    0H                                                               
         SR    R0,R0                                                            
         ICM   R0,3,RECVHDR-4      GET RECORD LENGTH                            
         GOTO1 =V(PRNTBL),DMCB,0,RECVHDR-4,C'DUMP',(R0),=C'1D'                  
         B     IN2                                                              
COUNT    DC    PL4'0'                                                           
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2100,            X        
               BLKSIZE=2104,MACRF=PM                                            
         EJECT                                                                  
DUB      DS    D                                                                
WORK     DS    XL64                                                             
*                                                                               
WORKX    DS    0X                                                               
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
OUTCNT   DC    PL4'0'                                                           
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
RSORTKEY DS    0XL13                                                            
*                                                                               
RSORTAM  DS    XL1                                                              
RSORTCLT DS    XL2                                                              
RSORTMKT DS    XL2                                                              
RSORTSTA DS    XL3                                                              
RSORTPRD DS    XL1                                                              
RSORTEST DS    XL1                                                              
RSORTBUY DS    XL3                                                              
*                                                                               
RSORTSEQ DS    XL3                                                              
RSPARE   DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RKEY     DS    0CL13                                                            
         DS    4628C                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022PPRCVLOOK 08/09/00'                                      
         END                                                                    
