*          DATA SET CKRECONS   AT LEVEL 003 AS OF 05/01/02                      
*PHASE CKRECONA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'CKRECON - CHKDIR/CHKFIL RECONSTRUCT PROGRAM'                    
CKRECON  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,CKRECON,=V(REGSAVE)                                            
*                                                                               
         L     RC,=A(RECONWKC)                                                  
         USING RECONWK,RC                                                       
*                                                                               
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
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
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=2100'                                  
*                                                                               
OPEN     OPEN  (RECVIN,(INPUT))                                                 
         EJECT                                                                  
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RFILTY,QCHKFIL      TEST CHKFIL                                  
         BNE   IN2                                                              
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,RECVHDR-4                                                   
         LA    R1,RECVHDR-4(R1)                                                 
         MVI   0(R1),0             SET EOR FLAG                                 
*                                                                               
         CLI   RRECTY,1                                                         
         BE    CPY                                                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RECVHDR-4                                                   
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
         SR    R4,R4                                                            
         ICM   R4,3,RECVHDR-4                                                   
         GOTO1 =V(PRNTBL),DMCB,0,RECVHDR-4,C'DUMP',(R4),=C'0D'                  
         EJECT                                                                  
ADD      MVC   ROLDKEY,RKEY        SET NEW/NEW REC                              
         AP    RCVADD,=P'1'                                                     
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN                                     
         B     IN2                                                              
*                                                                               
CPY      MVC   ROLDKEY,RKEY        MOVE COPY KEY FOR OLD/NEW                    
*********MVC   SAVSIN,RSIN         SAVE THIS SIN                                
         AP    RCVCPY,=P'1'                                                     
         B     IN2                                                              
*                                                                               
CHG      AP    RCVCHG,=P'1'                                                     
         OC    RSIN,RSIN           TEST OFF-LINE CHANGE (NO COPY)               
         BZ    CHGNEW                                                           
*********CLC   SAVSIN,RSIN         DAVID E. SAYS THIS IS FUCKED                 
*********BE    *+6                                                              
*********DC    H'0'                                                             
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
OUTPUT   BC    0,OP2                                                            
         OI    *-3,X'F0'                                                        
         OPEN  (FILEIN,(INPUT),FILEOUT,(OUTPUT))                                
*                                                                               
         XC    RLEN(L'ROLDKEY+11),RLEN                                          
         XC    RECVHDR,RECVHDR                                                  
         XC    RKEY(L'ROLDKEY),RKEY                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET' GET FIRST OUTPUT REC                     
         ICM   R3,15,4(R1)                                                      
         BNZ   *+8                                                              
         LA    R3,KEYFF                                                         
*                                                                               
         BAS   R9,READF                GET FIRST FILE REC                       
*                                                                               
OP2      CLC   FKEY(L'ROLDKEY),ROLDKEY                                          
         BL    OP6                                                              
         BH    OP8                                                              
         CLI   FKEY,X'FF'          TEST RECV/FILE AT EOF                        
         BE    OPDONE                                                           
         B     OP8                                                              
*                                                                               
* FILE LOW - WRITE FILE                                                         
*                                                                               
OP6      LA    R0,FKEY-4                                                        
         BAS   R9,PUT                                                           
         BAS   R9,READF                                                         
         B     OP2                                                              
*                                                                               
         EJECT                                                                  
* FILE HIGH OR EQUAL - READ TILL CHANGE IN RKEY/NKEY, THEN PUT                  
*                                                                               
OP8      CLC   ROLDKEY,RKEY        TEST TRANS CHANGED KEY                       
         BE    OP8A                NO                                           
         AP    CHGKEY,=P'1'                                                     
         B     OP8B                                                             
*                                                                               
OP8A     CLC   FKEY(L'ROLDKEY),RKEY  TEST REC PREVIOUSLY ON FILE                
         BNE   OP8B                NO                                           
*                                                                               
         CLI   RRECTY,3            TEST RECV TYPE = ADD                         
         BNE   OP8B                NO                                           
         AP    REUSEKEY,=P'1'      RECYCLED KEY                                 
*                                                                               
OP8B     CLC   ROLDKEY,NOLDKEY     TEST FOR BREAK                               
         BE    OPSV                NO                                           
         OC    ROLDKEY,ROLDKEY     TEST FIRST TIME                              
         BZ    OPSV                YES                                          
*                                                                               
         CLC   ROLDKEY,FKEY        TEST REC ADDED TODAY                         
         BE    OP8D                NO                                           
         CLC   ROLDKEY,RKEY        YES - TEST KEY CHANGE                        
         BNE   OPSV                LAST ACTIVITY WAS KEY CHANGE - SKIP          
         LA    R0,RKEY-4           ELSE PUT RECV                                
         BAS   R9,PUT                                                           
         B     OPSV                                                             
*                                                                               
* REC PREVIOUSLY ON FILE *                                                      
*                                                                               
OP8D     BAS   R9,READF            SKIP TO NEXT FILE REC                        
*                                                                               
         CLC   ROLDKEY,RKEY        TEST KEY CHANGE                              
         BNE   OPSV                YES - SKIP RECV TOO                          
         LA    R0,RKEY-4                                                        
         BAS   R9,PUT                                                           
*                                                                               
OPSV     CLI   NEXTREC,X'FF'       CHECK EOF                                    
         BNE   OPSV1               NO                                           
         MVI   RKEY,X'FF'                                                       
         MVC   RKEY+1(L'ROLDKEY-1),RKEY                                         
         MVC   ROLDKEY,RKEY                                                     
         B     OP2                                                              
*                                                                               
OPSV1    LA    R0,RLEN             SAVE NEW RECOVERY REC                        
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
*                                                                               
OPX      GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,4(R1)                                                      
         BNZ   *+12                                                             
         LA    R3,KEYFF                                                         
         B     OP2                                                              
         CLC   RTIME-RLEN(R3),=X'0081400C'                                      
         BL    OP2                                                              
         CLC   NOLDKEY(32),BADKEY1                                              
         BE    OPX                                                              
         B     OP2                                                              
*                                                                               
BADKEY1  DC    X'A0',C'4937  ',X'1991040001008A00C4000004',C'562623460'         
         DC    C'P  ',X'FF'                                                     
         EJECT                                                                  
READF    CLC   FKEY(L'ROLDKEY),KEYFF                                            
         BCR   8,R9                                                             
         LA    R0,FKEY-4                                                        
         GET   FILEIN,(0)                                                       
         AP    FILIN,=P'1'                                                      
         BR    R9                                                               
*                                                                               
PUT      PUT   FILEOUT,(0)                                                      
         AP    FILOUT,=P'1'                                                     
         BR    R9                                                               
         EJECT                                                                  
ENDIN    CLOSE (FILEIN,)                                                        
OPDONE   CLOSE (FILEOUT,)                                                       
*                                                                               
         MVC   LINE,=PL2'75'                                                    
         MVC   TITLE(25),=C'CHKDIR/CHKFIL RECONSTRUCT'                          
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
KEYFF    DC    50X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
QCHKFIL  EQU   X'76'               CHKFILE NUMBER FROM DMFILES                  
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,          DOS SYS014                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=08200,                                            X        
               BLKSIZE=8204,          DOS BLKSIZE=02100                X        
               MACRF=GM,                                               X        
               EODAD=ENDIN1                                                     
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,          DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,                                          X        
               MACRF=GM,                                               X        
               EODAD=ENDIN                                                      
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,         DOS SYS012                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=08200               X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
RECONWK  DSECT                                                                  
*                                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL32     ***** HARD CODE KEY LENGTH HERE *****                   
RSEQ     DS    CL3                                                              
RSPARE   DS    CL4                                                              
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0C                                                               
         DS    2050C                                                            
*                                                                               
         DS    D                   ROOM FOR LENGTH                              
FKEY     DS    0C                                                               
FREC     DS    2100C                                                            
         EJECT                                                                  
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
RECONWKC CSECT                                                                  
         DS    1100D                                                            
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CKRECONS  05/01/02'                                      
         END                                                                    
