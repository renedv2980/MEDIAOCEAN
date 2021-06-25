*          DATA SET RCYPEEL    AT LEVEL 001 AS OF 12/03/91                      
*PHASE RCYPEEL,*                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'RCYPEEL - PEEL/SORT/SAVE SPOT RECOVERY DATA'                    
RCYPEEL  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RCYPEEL,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCYPEEL+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
         EJECT                                                                  
INIT2    DS    0H                                                               
         LA    RE,RCYPEEL          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY PEEL PROGRAM'                           
         SR    R3,R3                                                            
         B     IN2                                                              
*                                                                               
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
         LA    R3,1(R3)                                                         
*                                                                               
*                                                                               
OUTREQ   LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(R0)                                                     
         CH    R3,=H'1000'                                                      
         BL    IN2                                                              
         B     ENDIN                                                            
*                                                                               
*                                                                               
ENDIN    CLOSE (RECVIN,)                                                        
         CLOSE (RECVOUT,)                                                       
*                                                                               
EOJ      DS    0H                                                               
         XBASE                                                                  
         LTORG                                                                  
FORMAT   DC    C'1D'                                                            
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,                        X        
               MACRF=GM,EODAD=ENDIN                                             
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=8200,            X        
               BLKSIZE=8204,MACRF=PM                                            
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
WORK     DS    XL64                                                             
DSKADDR  DC    X'00000000'                                                      
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
         DS    9000C                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001RCYPEEL   12/03/91'                                      
         END                                                                    
