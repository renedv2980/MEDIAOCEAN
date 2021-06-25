*          DATA SET REACEPEEL  AT LEVEL 017 AS OF 08/31/00                      
*PHASE REACPEEA REACPEEL                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
         TITLE 'REACEPEEL - PEEL/SORT/SAVE REP RECOVERY DATA'                   
REACPEEL CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,REACPEEL,VREGSAVE                                              
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING REACPEEL+4096,RC                                                 
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
PEELREP  DC    CL2'JB'                                                          
         EJECT                                                                  
INIT2    DS    0H                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY PEEL PROGRAM'                           
         B     IN2                                                              
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,X'82'        TEST REPFILE                                 
         BNE   IN2                                                              
         CLI   RKEY,X'0C'          TEST CONREC                                  
         BNE   IN2                                                              
*        CLI   RRECTY,2            TEST CHANGE                                  
*        BNE   IN2                                                              
*                                                                               
IN4      CLC   RKEY+2(2),PEELREP                                                
         BNE   IN2                                                              
         CLC   RKEY+4(2),=C'RW'                                                 
         BNE   IN2                                                              
         SPACE 1                                                                
*        CLC   RKEY+23(4),=X'00008552'                                          
*        BNE   IN2                                                              
*        LA    R2,RKEY+34          POINT TO 1ST ELEMENT                         
*        TM    41(R2),X'80'        ACE                                          
*        BZ    IN2                                                              
IN5      AP    CONCNT,=P'1'                                                     
         B     IN8                                                              
*                                                                               
IN8      LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(R0)                                                     
         CP    CONCNT,=P'500'      ONLY GET 500 RECORDS                         
         BH    ENDIN                                                            
         B     IN2                                                              
*                                                                               
ENDIN    CLOSE (RECVIN,)                                                        
         B     EOJ                 *****************                            
         EJECT                                                                  
EOJ      MVC   P(21),=C'CONTRACT RECORDS OUT='                                  
         OI    CONCNT+3,X'0F'                                                   
         UNPK  P+21(6),CONCNT                                                   
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         CLOSE (RECVOUT,)                                                       
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
CONCNT   DC    PL4'0'                                                           
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RSPARE   DS    XL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 1                                                                
RKEY     DS    0CL13                                                            
         DS    2050C                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REACEPEEL 08/31/00'                                      
         END                                                                    
