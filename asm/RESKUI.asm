*          DATA SET RESKUI     AT LEVEL 045 AS OF 05/01/02                      
*PHASE RESKUIA,*,NOAUTO                                                         
*INCLUDE REGSAVE                                                                
*INCLUDE STXITER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE CLUNPK                                                                 
         TITLE 'RESKUI - REPFILE RECONSTRUCT PROGRAM'                           
RESKUI   CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,RESKUI,=V(REGSAVE)                                             
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
         LA    RE,RESKUI            SET FOR STXITER                             
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
*                                                                               
         CLI   RKEY,X'0C'          CONTRACTS ONLY                               
         BNE   IN2                                                              
         CLC   =X'05639207',RKEY+23                                             
         BNE   IN2                                                              
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
*                                                                               
         MVC   P(4),=C'ADD:'                                                    
         MVC   P+4(128),RLEN                                                    
         GOTO1 VPRINTER                                                         
*                                                                               
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
         MVC   P(4),=C'CHG:'                                                    
         MVC   P+4(128),RLEN                                                    
         GOTO1 VPRINTER                                                         
*                                                                               
*                                                                               
         CLC   ROLDKEY,RKEY        TEST TRANS CHANGED KEY                       
         BE    IN2                 NO                                           
*                                                                               
CHGNEW   MVC   ROLDKEY,RKEY        SET FOR NEW/NEW REC                          
         GOTO1 =V(SORTER),DMCB,=C'PUT',RLEN                                     
         MVC   P(4),=C'CNW:'                                                    
         MVC   P+4(128),RLEN                                                    
         GOTO1 VPRINTER                                                         
*                                                                               
         B     IN2                                                              
         EJECT                                                                  
ENDIN1   CLOSE (RECVIN,)                                                        
         EJECT                                                                  
         USING NEXTREC,R3                                                       
OUTPUT   DS    0H                                                               
         BC    0,OP2                                                            
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
         BAS   R9,PUTCHK                                                        
         B     OPSV                                                             
         SPACE 1                                                                
* REC PREVIOUSLY ON FILE *                                                      
         SPACE 1                                                                
OP8D     BAS   R9,READF            SKIP TO NEXT FILE REC                        
*                                                                               
         CLC   ROLDKEY,RKEY        TEST KEY CHANGE                              
         BNE   OPSV                YES - SKIP RECV TOO                          
         LA    R0,RKEY-4                                                        
         BAS   R9,PUTCHK                                                        
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
         BNZ   *+8                                                              
         LA    R3,KEYFF                                                         
         B     OP2                                                              
         EJECT                                                                  
READF    CLC   FKEY(L'ROLDKEY),KEYFF                                            
         BCR   8,R9                                                             
         LA    R0,FKEY-4                                                        
         GET   FILEIN,(0)                                                       
         AP    FILIN,=P'1'                                                      
         BR    R9                                                               
*                                                                               
PUT      DS    0H                                                               
         PUT   FILEOUT,(0)                                                      
         AP    FILOUT,=P'1'                                                     
         MVC   P(4),=C'PL :'                                                    
         MVC   P+4(128),RKEY                                                    
         GOTO1 =V(PRINTER)                                                      
         BR    R9                                                               
*                                                                               
PUTCHK   NTR1                                                                   
         PUT   FILEOUT,(0)                                                      
         AP    FILOUT,=P'1'                                                     
*                                                                               
*&&DO                                                                           
         CLI   FKEY,X'0C'                                                       
         BNE   PUTCHKX                                                          
         LA    R6,FKEY                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONSEND,R6                                                      
         CLC   RCONSRD3,=X'C681'      APRIL 1/1999                              
         BE    PUTCHKX                                                          
         CLC   RCONSRD2,=X'C681'                                                
         BE    PUTCHKX                                                          
         CLC   RCONSRD1,=X'C681'                                                
         BE    PUTCHKX                                                          
         CLC   RCONS2D3,=X'C681'      APRIL 1/1999                              
         BE    PUTCHKX                                                          
         CLC   RCONS2D2,=X'C681'                                                
         BE    PUTCHKX                                                          
         CLC   RCONS2D1,=X'C681'                                                
         BE    PUTCHKX                                                          
         DROP  R6                                                               
*                                                                               
         MVC   P(2),=C'F='                                                      
         MVC   P+4(27),FKEY                                                     
         GOTO1 =V(HEXOUT),DMCB,FKEY,P+36,27                                     
         GOTO1 =V(PRINTER)                                                      
*                                                                               
         MVC   P(2),=C'R='                                                      
         MVC   P+4(27),RKEY                                                     
         GOTO1 =V(HEXOUT),DMCB,RKEY,P+36,27                                     
         GOTO1 =V(PRINTER)                                                      
*&&                                                                             
         MVC   P(4),=C'PUT:'                                                    
         MVC   P+4(128),RKEY                                                    
         GOTO1 =V(PRINTER)                                                      
*                                                                               
PUTCHKX  DS    0H                                                               
         XIT1                                                                   
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
         DS    2000D                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045RESKUI    05/01/02'                                      
         END                                                                    
