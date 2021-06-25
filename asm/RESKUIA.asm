*          DATA SET RESKUIA    AT LEVEL 064 AS OF 05/01/02                      
*          DATA SET RESKUIA    AT LEVEL 062 AS OF 04/07/99                      
*PHASE RESKUIA                                                                  
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'RESKUI - REPFILE RECONSTRUCT PROGRAM'                           
RESKUI   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,RESKUI,=V(REGSAVE)                                             
*                                                                               
         L     RC,=A(RECONWKC)                                                  
         USING RECONWK,RC                                                       
         LR    R8,RC                                                            
         AHI   R8,4096                                                          
         USING RECONWK+4096,R8                                                  
*                                                                               
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
*                                                                               
         LA    RE,RESKUI            SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         XC    KEY1(27),KEY1                                                    
         XC    KEY2(27),KEY2                                                    
OUTPUT   DS    0H                                                               
*                                                                               
         OPEN  (FILEIN,(INPUT),FILEOUT,(OUTPUT))                                
*                                                                               
OUTPUT10 DS    0H                                                               
         BAS   R9,READF                GET FILE REC                             
*                                                                               
         CLI   KEY2,X'0C'                                                       
         BNE   OUTPUT20                                                         
         CLC   =X'05639207',KEY2+23                                             
         BNE   OUTPUT20                                                         
*                                                                               
         MVC   P(132),KEY2                                                      
         GOTO1 =V(PRINTER)                                                      
*                                                                               
OUTPUT20 DS    0H                                                               
         CLC   KEY1(27),KEY2                                                    
         BNE   OUTPUT30                                                         
         LA    R4,KEY1-4                                                        
         BAS   R9,PUT                                                           
         LA    R4,KEY2-4                                                        
         BAS   R9,PUT                                                           
*                                                                               
OUTPUT30 DS    0H                                                               
*        MVC   P(6),=C'MOVING'                                                  
*        GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R2,KEY2-4                                                        
         ZICM  R3,0(R2),2                                                       
         LA    RE,KEY1-4                                                        
         LR    RF,R3                                                            
         MVCL  (RE),(R2)                                                        
*                                                                               
*&&DO                                                                           
         LA    R4,KEY1-4                                                        
         MVC   P(132),0(R4)                                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         LA    R4,KEY2-4                                                        
         MVC   P(132),0(R4)                                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         B     OUTPUT10                                                         
*                                                                               
         EJECT                                                                  
READF    DS    0H                                                               
         CLC   KEY2(L'ROLDKEY),KEYFF                                            
         BCR   8,R9                                                             
*        MVC   P(7),=C'READING'                                                 
*        GOTO1 =V(PRINTER)                                                      
         LA    R4,KEY2-4                                                        
         GET   FILEIN,(R4)                                                      
         AP    FILIN,=P'1'                                                      
         BR    R9                                                               
*                                                                               
PUT      DS    0H                                                               
         PUT   FILEOUT,(R4)                                                     
         AP    FILOUT,=P'1'                                                     
         BR    R9                                                               
         EJECT                                                                  
ENDIN    CLOSE (FILEIN,)                                                        
OPDONE   CLOSE (FILEOUT,)                                                       
         SPACE 2                                                                
         MVC   LINE,=PL2'75'                                                    
         MVC   TITLE(19),=C'REPFILE RECONSTRUCT'                                
*                                                                               
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
OPD2     MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 =V(PRINTER)                                                      
         BXLE  R3,R4,OPD2                                                       
         XBASE                                                                  
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
CNTRS    DS    0D                                                               
RCVADD   DC    PL4'0',CL14'RECOVERY ADDS'                                       
RCVCPY   DC    PL4'0',CL14'RECOVERY CPYS'                                       
RCVCHG   DC    PL4'0',CL14'RECOVERY CHGS'                                       
CHGKEY   DC    PL4'0',CL14'CHANGED KEYS'                                        
REUSEKEY DC    PL4'0',CL14'RE-USED KEYS'                                        
FILIN    DC    PL4'0',CL14'FILE IN'                                             
FILOUT   DC    PL4'0',CL14'FILE OUT'                                            
CNTRSX   EQU   *-1                                                              
*                                                                               
         EJECT                                                                  
DUB      DS    D                                                                
SAVR1    DS    F                                                                
MYSEQ    DS    F                                                                
SAVSIN   DS    F                                                                
NEWKEYSW DC    X'00'                                                            
ANSWER   DS    D                                                                
DMCB     DS    6F                                                               
ELCODE   DS    X                                                                
WORK     DS    XL100                                                            
KEYFF    DC    50X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               MACRF=GM,                                               X        
               EODAD=ENDIN                                                      
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,         DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
RECONWK  DSECT                                                                  
*                                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL27     ***** HARD CODE KEY LENGTH HERE *****                   
RSEQ     DS    CL3                                                              
RSPARE   DS    CL4                                                              
*                                                                               
         DS    D                   ROOM FOR LENGTH                              
KEY1     DS    0C                                                               
         DS    4200C                                                            
         SPACE 2                                                                
         DS    D                   ROOM FOR LENGTH                              
KEY2     DS    0C                                                               
REC2     DS    4200C                                                            
*REC     DS    2100C                                                            
         EJECT                                                                  
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    CL27     ***** HARD CODE KEY LENGTH HERE *****                   
NSEQ     DS    CL3                                                              
NSPARE   DS    CL4                                                              
*                                                                               
NKEY     DS    0CL27                                                            
NREC     DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
COND     DSECT                                                                  
       ++INCLUDE REGENCON                                                       
         SPACE 2                                                                
RECONWKC CSECT                                                                  
         DS    4000D                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064RESKUIA   05/01/02'                                      
         END                                                                    
