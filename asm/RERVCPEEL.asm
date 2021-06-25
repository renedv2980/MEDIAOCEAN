*          DATA SET RERVCPEEL  AT LEVEL 057 AS OF 08/31/00                      
*          DATA SET RERVCPEEL  AT LEVEL 056 AS OF 03/21/89                      
*PHASE RCVPEELA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'RERCVPEEL - PEEL/SORT/SAVE REP RECOVERY DATA'                   
RCVPEEL  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RCVPEEL,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVPEEL+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCVPEEL          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY PEEL PROGRAM'                           
         GOTO1 =V(PRINTER)                                                      
*                                                                               
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,X'82'        REPFILE                                      
         BNE   IN2                                                              
         CLI   RRECTY,X'03'        TRANSACTION TYPE, 3=ADD                      
         BNE   IN2                                                              
         CLI   RPRG,X'02'          PROGRAM, 2=CONTRACT                          
         BNE   IN2                                                              
         CLC   =X'14',RKEY         AVAIL RECORDS, RECID = X'14'                 
         BNE   IN2                                                              
*                                                                               
IN4      LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(0)                                                      
         AP    OUTCNT,=P'1'                                                     
         B     IN2                                                              
*                                                                               
ENDIN    CLOSE (RECVIN,)                                                        
         CLOSE (RECVOUT,)                                                       
*                                                                               
         MVC   P(21),=C'RECOVERY RECORDS OUT '                                  
         UNPK  P+25(4),OUTCNT                                                   
         OI    P+28,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         XBASE                                                                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               BLKSIZE=2104,MACRF=GM,EODAD=ENDIN                                
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2100,            X        
               BLKSIZE=2104,MACRF=PM                                            
         EJECT                                                                  
DMCB     DS    6F                                                               
OUTCNT   DC    PL4'0'                                                           
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RSPARE   DS    XL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    0CL13                                                            
         DS    2050C                                                            
*                                                                               
DUB      DS    D                                                                
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057RERVCPEEL 08/31/00'                                      
         END                                                                    
