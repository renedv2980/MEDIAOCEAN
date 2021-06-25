*          DATA SET RERRGPEEL  AT LEVEL 009 AS OF 08/31/00                      
*          DATA SET RERRGPEEL  AT LEVEL 008 AS OF 04/22/86                      
*PHASE RERGPEEA RERGPEEL                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
         TITLE 'RERRGPEEL - PEEL/SORT/SAVE REP RECOVERY DATA'                   
RERGPEEL CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,RERGPEEL,VREGSAVE                                              
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING RERGPEEL+4096,RC                                                 
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
         BE    *+12                                                             
         CLI   RKEY,X'13'          TEST BUDREC                                  
         BNE   IN2                                                              
         CLI   RRECTY,3            TEST ADD                                     
         BE    IN4                                                              
         OC    RSIN,RSIN           TEST SIN=0 (CHANGE W/O COPY)                 
         BZ    IN2                 YES - IGNORE                                 
*                                                                               
IN4      CLI   RKEY,X'0C'          CONTRACT                                     
         BNE   IN6                                                              
         CLC   RKEY+2(2),PEELREP                                                
         BNE   IN2                                                              
         CLC   RKEY+6(5),PEELSTA                                                
         BNE   IN2                                                              
*        CLC   RKEY+11(2),PEELOFF                                               
*        BNE   IN2                                                              
         AP    CONCNT,=P'1'                                                     
         B     IN8                                                              
*                                                                               
IN6      CLC   RKEY+16(2),PEELREP  BUDGET                                       
         BNE   IN2                                                              
         CLC   RKEY+20(5),PEELSTA                                               
         BNE   IN2                                                              
         AP    BUDCNT,=P'1'                                                     
*                                                                               
IN8      LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(R0)                                                     
         B     IN2                                                              
*                                                                               
ENDIN    CLOSE (RECVIN,)                                                        
         B     EOJ                 *****************                            
         EJECT                                                                  
EOJ      MVC   P(21),=C'CONTRACT RECORDS OUT='                                  
         OI    CONCNT+3,X'0F'                                                   
         UNPK  P+23(6),CONCNT                                                   
         GOTO1 =V(PRINTER)                                                      
         MVC   P(19),=C'BUDGET RECORDS OUT='                                    
         OI    BUDCNT+3,X'0F'                                                   
         UNPK  P+21(6),BUDCNT                                                   
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
BUDCNT   DC    PL4'0'                                                           
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
**PAN#1  DC    CL21'009RERRGPEEL 08/31/00'                                      
         END                                                                    
