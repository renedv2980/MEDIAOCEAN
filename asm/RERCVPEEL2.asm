*          DATA SET RERCVPEEL2 AT LEVEL 017 AS OF 09/25/00                      
*PHASE RERCVP1A                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PDUMPER                                                                
*INCLUDE HEXOUT                                                                 
         TITLE 'RERCVPEEL - PEEL REP RECOVERY RECORDS TO DISK'                  
         PRINT NOGEN                                                            
RCVPEEL  CSECT                                                                  
         NBASE 0,RCVPEEL,VREGSAVE                                               
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RCVPEEL+4096,RC                                                  
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         LA    RE,RCVPEEL                                                       
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
******** PROCESS INPUT  FILE                                                    
*                                                                               
INPUT10  EQU   *                                                                
         LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
         AP    INCNT,=P'1'                                                      
*                                                                               
         CLI   RFILTY,X'82'        REP FILE?                                    
         BNE   INPUT10                                                          
         CLI   RPRG,X'18'          PROGRAM = SFM?                               
         BNE   INPUT10                                                          
         CLC   =C'HN',RKEY+16      REP = HN?                                    
         BNE   INPUT10                                                          
         CLC   =X'13',RKEY         BUDGET RECORDS (ID=X'13')                    
         BNE   INPUT10                                                          
*                                                                               
OUTPUT10 EQU   *                                                                
         LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(0)                                                      
         AP    OUTCNT,=P'1'                                                     
         B     INPUT10                                                          
*                                                                               
ENDIN    EQU   *                                                                
         CLOSE (RECVIN,)                                                        
         CLOSE (RECVOUT,)                                                       
*                                                                               
         MVC   P(21),=C'RECOVERY RECORDS IN  '                                  
         UNPK  P+25(8),INCNT                                                    
         OI    P+32,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(21),=C'RECOVERY RECORDS OUT '                                  
         UNPK  P+25(8),OUTCNT                                                   
         OI    P+32,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
*                                                                               
RCVPEXIT EQU   *                                                                
         SR    R0,R0                                                            
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
******** REGSAVE                                                                
*                                                                               
         DS    0F                                                               
VREGSAVE DC    V(REGSAVE)                                                       
*                                                                               
******** DCB'S                                                                  
*                                                                               
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,RECFM=VB,LRECL=2100,             X        
               BLKSIZE=2104,MACRF=GM,EODAD=ENDIN                                
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=2100,            X        
               BLKSIZE=2104,MACRF=PM                                            
*                                                                               
******** LTORG                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
******** DATA AND DSECTS                                                        
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
INCNT    DC    PL8'0'                                                           
OUTCNT   DC    PL8'0'                                                           
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RSPARE   DS    XL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
*                                                                               
RKEY     DS    0CL17                                                            
         DS    2050C                                                            
*                                                                               
         EJECT                                                                  
*                                                                               
******** DDDPRINT                                                               
*                                                                               
       ++INCLUDE DDDPRINT                                                       
*                                                                               
******** END                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017RERCVPEEL209/25/00'                                      
         END                                                                    
