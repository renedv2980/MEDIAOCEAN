*          DATA SET SCHTFIX    AT LEVEL 079 AS OF 05/06/99                      
*          DATA SET REFIX      AT LEVEL 031 AS OF 04/02/99                      
*PHASE REFIX,*,NOAUTO                                                           
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CLUNPK                                                                 
         TITLE 'REFIX - REPFILE RECOVERY EXTRACT PROGRAM'                       
REFIX    CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,REFIX,=V(REGSAVE)                                              
*                                                                               
         L     RC,=A(RECONWKC)                                                  
         USING RECONWK,RC                                                       
         LR    R2,RC                                                            
         AHI   R2,4096                                                          
         USING RECONWK+4096,R2                                                  
*                                                                               
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
*                                                                               
         LA    RE,REFIX             SET FOR STXITER                             
         L     RF,=V(STXITER)                                                   
         STM   RE,RF,DUB                                                        
         OI    DUB+4,X'80'                                                      
         GOTO1 =V(STXITER),DMCB,DUB                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
*                                                                               
         LA    R0,L'ROLDKEY+3                                                   
         CVD   R0,DUB                                                           
         UNPK  SORTCARD+15(2),DUB                                               
         OI    SORTCARD+16,X'F0'                                                
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD                                 
         B     OPEN                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,  ,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4200'                                  
*                                                                               
OPEN     DS    0H                                                               
         OPEN  (RECVIN,(INPUT))                                                 
         EJECT                                                                  
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,X'82'        TEST REPFILE                                 
         BNE   IN2                                                              
         CLI   RKEY,X'0C'          CONTRACTS ONLY                               
         BNE   IN2                                                              
         LA    R6,CONLIST                                                       
IN5      CLC   RKEY+2(2),0(R6)                                                  
         BNE   IN10                                                             
         CLC   RKEY+23(4),2(R6)                                                 
         BE    IN20                                                             
*                                                                               
IN10     DS    0H                                                               
         AHI   R6,6                                                             
         CLI   0(R6),X'FF'                                                      
         BE    IN2                                                              
         B     IN5                                                              
*                                                                               
IN20     DS    0H                                                               
*                                                                               
         ZICM  R1,RECVHDR-4,2                                                   
         LA    R1,RECVHDR-4(R1)                                                 
         MVI   0(R1),0             SET EOR FLAG                                 
         CLI   RRECTY,1                                                         
         BE    CPY                                                              
*                                                                               
         ZICM  RE,RECVHDR-4,2                                                   
         LA    RE,L'ROLDKEY+7(RE)  ADD EXPANSION LENGTH                         
         SLL   RE,16                                                            
         ST    RE,RLEN                                                          
         L     RE,MYSEQ                                                         
         LA    RE,1(RE)                                                         
         ST    RE,MYSEQ                                                         
         MVC   RSEQ,MYSEQ+1        SET SEQUENCE NUMBER                          
         XC    RSPARE,RSPARE                                                    
*                                                                               
         CLI   RRECTY,2                                                         
         BE    CHG                                                              
         CLI   RRECTY,3                                                         
         BE    ADD                                                              
         ZICM  R4,RECVHDR-4,2                                                   
         GOTO1 =V(PRNTBL),DMCB,RECVHDR-4,C'DUMP',(R4),=C'0D'                    
         EJECT                                                                  
ADD      DS    0H                                                               
         MVC   ROLDKEY,RKEY        SET NEW/NEW REC                              
         AP    RCVADD,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN                                     
         B     IN2                                                              
         SPACE 2                                                                
CPY      MVC   ROLDKEY,RKEY        MOVE COPY KEY FOR OLD/NEW                    
         MVC   SAVSIN,RSIN         SAVE THIS SIN                                
         AP    RCVCPY,=P'1'                                                     
         B     IN2                                                              
         SPACE 2                                                                
CHG      AP    RCVCHG,=P'1'                                                     
         OC    RSIN,RSIN           TEST OFF-LINE CHANGE (NO COPY)               
         BZ    CHGNEW                                                           
         CLC   SAVSIN,RSIN                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN                                     
*                                                                               
         CLC   ROLDKEY,RKEY        TEST TRANS CHANGED KEY                       
         BE    IN2                 NO                                           
*                                                                               
CHGNEW   MVC   ROLDKEY,RKEY        SET FOR NEW/NEW REC                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN                                     
         B     IN2                                                              
         EJECT                                                                  
ENDIN1   CLOSE (RECVIN,)                                                        
         EJECT                                                                  
         USING NEXTREC,R3                                                       
OUTPUT   DS    0H                                                               
         OPEN  (FILEOUT,(OUTPUT))                                               
*                                                                               
         XC    RLEN(L'ROLDKEY+11),RLEN                                          
         XC    RECVHDR,RECVHDR                                                  
         XC    RKEY(L'ROLDKEY),RKEY                                             
OP10     DS    0H                                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET' GET FIRST OUTPUT REC                     
         ICM   R3,15,4(R1)                                                      
         BZ    OPDONE                                                           
*                                                                               
         OC    ROLDKEY,ROLDKEY     FIRST TIME SAVE REC                          
         BNZ   OP30                                                             
*                                                                               
OP20     DS    0H                                                               
         LA    R0,RLEN                                                          
         LH    R1,NLEN                                                          
         LA    R4,NLEN                                                          
         LH    R5,NLEN                                                          
         MVCL  R0,R4                                                            
*                                                                               
         LH    RE,RLEN             SET RCV FOR PUT                              
         LA    R0,L'ROLDKEY+7                                                   
         SR    RE,R0                                                            
         SH    RE,=H'24'           ADJUST FOR RECV HDR                          
         SLL   RE,16                                                            
         ST    RE,DUB                                                           
         MVC   RKEY-4(4),DUB                                                    
         B     OP10                                                             
*                                                                               
OP30     DS    0H                                                               
         CLC   ROLDKEY,NOLDKEY     TEST FOR BREAK                               
         BE    OP20                                                             
         LA    R0,RKEY-4                                                        
         PUT   FILEOUT,(0)                                                      
         AP    FILOUT,=P'1'                                                     
         B     OP20                                                             
*                                                                               
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
CONNUM   DS    XL4                                                              
KEYFF    DC    50X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECONLIST                                                      
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,          DOS SYS014                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04200,                                            X        
               MACRF=GM,                                               X        
               EODAD=ENDIN1                                                     
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04200,                                            X        
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
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0C                                                               
         DS    4200C                                                            
*        DS    2050C                                                            
         SPACE 2                                                                
         DS    D                   ROOM FOR LENGTH                              
FKEY     DS    0C                                                               
FREC     DS    4200C                                                            
*REC     DS    2100C                                                            
         EJECT                                                                  
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
RECONWKC CSECT                                                                  
         DS    4000D                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079SCHTFIX   05/06/99'                                      
         END                                                                    
