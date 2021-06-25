*          DATA SET TARCVPEEL  AT LEVEL 008 AS OF 08/10/00                      
*PHASE TAPEELA                                                                  
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
         TITLE 'TARCVLOOK - SEARCH RECOVERY FILE AND PRINT'                     
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
         MVC   TITLE(20),=C'TALENT RECOVERY LOOK'                               
         GOTO1 =V(PRINTER)                                                      
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
         OPEN  (RECVOUT,(OUTPUT))                                               
*********OPEN  (RECVCPY,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY LOOK PROGRAM'                           
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,X'72'        TALFIL?                                      
         BNE   CHKF1                                                            
         CLC   RSIN(8),=X'0000BAA90065954C'                                     
         BL    IN2                                                              
         OC    RTRM,RTRM           IGNORE URGENT CHECK RUN                      
         BZ    IN2                                                              
         MVI   FLAGTF1,C'Y'                                                     
         B     PUTIT                                                            
*                                                                               
CHKF1    CLI   RFILTY,X'76'        CHKFIL?                                      
         BNE   IN2                                                              
         CLC   RSIN(8),=X'0000BB0D0070037C'                                     
         BL    IN2                                                              
         OC    RTRM,RTRM           IGNORE URGENT CHECK RUN                      
         BZ    IN2                                                              
         MVI   FLAGCF1,C'Y'                                                     
         B     PUTIT                                                            
*                                                                               
PUTIT    LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(0)                                                      
*********PUT   RECVCPY,(0)                                                      
         B     IN2                                                              
*                                                                               
COUNT    DC    PL4'0'                                                           
*                                                                               
ENDIN    DS    0H                                                               
         CLOSE RECVIN                                                           
         CLOSE (RECVOUT,)                                                       
*********CLOSE (RECVCPY,)                                                       
*                                                                               
         CLI   FLAGTF1,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   FLAGCF1,C'Y'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               MACRF=GM,EODAD=ENDIN                                             
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2100,            X        
               MACRF=PM,BLKSIZE=2104                                            
****CPY  DCB   DDNAME=RECVCPY,DSORG=PS,RECFM=VB,LRECL=2100,                     
****           MACRF=PM,BLKSIZE=2104                                            
         EJECT                                                                  
DMCB     DS    6F                                                               
MYSEQ    DC    F'0'                                                             
FLAGTF1  DC    C'N'                                                             
FLAGCF1  DC    C'N'                                                             
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
         DS    2050C                                                            
*                                                                               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
*                                                                               
WORKX    DS    0X                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008TARCVPEEL 08/10/00'                                      
         END                                                                    
