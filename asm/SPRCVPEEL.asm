*          DATA SET SPRCVPEEL  AT LEVEL 107 AS OF 07/14/98                      
*PHASE RCVPEEL,*                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'SPRCVPEEL - PEEL/SORT/SAVE SPOT RECOVERY DATA'                  
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
         B     IN2                                                              
*                                                                               
         EJECT                                                                  
*** PROCESS INPUT FILE ***                                                      
*                                                                               
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*&&DO                                                                           
         CLI   RECVHDR,X'21'       TEST SPTFILE                                 
         BNE   IN2                                                              
         CLI   RECVHDR+1,1         TEST COPY                                    
         BNE   IN2                                                              
*&&                                                                             
         CLI   RECVHDR,X'25'       TEST REQUESTS                                
         BNE   IN2                                                              
         CLC   =C'30MINKGF',RECVHDR+104                                         
         BNE   IN2                                                              
         LH    R0,RECVHDR-4        GET RECORD LENGTH                            
         SH    R0,=H'24'           ADJUST FOR RECOVERY HEADER                   
         SLL   R0,16                                                            
         ST    R0,RECVHDR+20                                                    
         LA    R0,RECVHDR+20       POINT TO LENGTH JUST SET                     
         PUT   RECVOUT,(0)                                                      
         B     IN2                                                              
*                                                                               
         CLC   RECVHDR+24(4),=X'23D9FBFF'  TEST WITO/NET/WP2                    
         BNE   IN2                                                              
         CLC   RECVHDR+24+9(2),=X'C40101'  EST 196/LINE 1                       
         BNE   IN2                                                              
         B     PUT                                                              
*                                                                               
IN4      CLI   RECVHDR,X'25'       TEST REQ FILE                                
         BNE   IN2                                                              
         CLC   RECVHDR+24+80(5),=C'30YRN'                                       
         BNE   IN2                                                              
*                                                                               
PUT      LA    R0,RECVHDR-4                                                     
         PUT   RECVOUT,(0)                                                      
         B     IN2                                                              
*======================================================                         
         CLI   RFILTY,X'37'                                                     
         BNE   IN2                                                              
         CLI   RRECTY,X'02'                                                     
         BNE   IN2                                                              
         CLI   RPRG,X'13'          TEST PAY PROGRAM                             
         BNE   IN2                                                              
         CLC   RTIME,=P'0094350'                                                
         BL    IN2                                                              
         B     PUT                                                              
*                                                                               
*======================================================*                        
         CLI   RFILTY,X'63'        TEST ACC REQUEST                             
         BNE   IN2                 NO                                           
*                                                                               
         CLC   =C'BD',RECVHDR+24   TEST BBDO                                    
         BNE   IN2                 NO                                           
*                                                                               
         CLC   =C'99',RECVHDR+50   TEST CHANGE OF '21' REQUEST                  
         BNE   IN6                                                              
*                                                                               
         CLC   DSKADDR,RVCHR       TEST SAME DISK ADDRESS                       
         BNE   IN2                                                              
         B     IN10                                                             
*                                                                               
IN6      CLC   =C'21',RECVHDR+50   TEST '21' REQUEST                            
         BNE   IN2                 NO                                           
         MVC   DSKADDR,RVCHR       SAVE DISK ADDRESS                            
*                                                                               
IN10     GOTO1 =V(PRNTBL),DMCB,0,RECVHDR,C'DUMP',130,FORMAT                     
         B     IN2                 NO DUPES                                     
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
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,                       X        
               BLKSIZE=16000,MACRF=PM                                           
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
**PAN#1  DC    CL21'107SPRCVPEEL 07/14/98'                                      
         END                                                                    
