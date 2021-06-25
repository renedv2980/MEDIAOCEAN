*          DATA SET REATPEEL   AT LEVEL 016 AS OF 08/31/00                      
*          DATA SET REATPEEL   AT LEVEL 015 AS OF 03/26/87                      
*PHASE REATPEEA REATPEEL                                                        
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PDUMPER                                                                
         TITLE 'REATPEEL - PEEL/SAVE BUY/CONTRACT RECOVERY DATA'                
REAPEEL  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,REAPEEL,VREGSAVE                                               
*                                                                               
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
*                                                                               
INIT2    DS    0H                                                               
         LA    RE,REAPEEL          SET FOR STXITER                              
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         OPEN  (RECVOUT,(OUTPUT))                                               
*                                                                               
         MVC   TITLE(30),=CL30'RECOVERY PEEL PROGRAM'                           
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN100    LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,X'82'        TEST REP                                     
         BNE   IN100                                                            
         CLI   RKEY,X'0B'          TEST BUY                                     
         BNE   IN200                                                            
         CLC   =C'SJ',RKEY+16      TEST SJR                                     
         BE    IN300                                                            
         B     IN100                                                            
*                                                                               
IN200    CLC   RKEY(2),=X'0C00'    TEST CONTRACT                                
         BNE   IN100                                                            
         CLC   =C'SJ',RKEY+2       TEST SJR                                     
         BNE   IN100                                                            
*                                                                               
IN300    LA    RE,RECVHDR-4                                                     
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZEROES AT END OF RECORD                  
*                                                                               
         LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(0)                                                      
         AP    OUTCNT,=P'1'                                                     
         B     IN100                                                            
*                                                                               
ENDIN    CLOSE (RECVIN,)                                                        
         CLOSE (RECVOUT,)                                                       
*                                                                               
         MVC   P(21),=C'RECOVERY RECORDS OUT '                                  
         UNPK  P+25(4),OUTCNT                                                   
         OI    P+28,X'F0'                                                       
         GOTO1 =V(PRINTER)                                                      
EOJ      DS    0H                                                               
         XBASE                                                                  
VREGSAVE DC    V(REGSAVE)                                                       
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
DUB      DS    D                                                                
OUTCNT   DC    PL4'0'                                                           
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
       ++INCLUDE DMRCVRHDR                                                      
         SPACE                                                                  
RKEY     DS    0CL27                                                            
         DS    1024C                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016REATPEEL  08/31/00'                                      
         END                                                                    
