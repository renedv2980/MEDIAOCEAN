*          DATA SET SXRECON    AT LEVEL 014 AS OF 06/07/11                      
*PHASE SXRECONA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE SXLDDEFN                                                               
         TITLE 'SXRECON - XSPDIR/XSPFIL RECONSTRUCT PROGRAM'                    
SXRECON  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,SXRECON,=V(REGSAVE)                                            
*                                                                               
         L     RC,=A(RECONWKC)                                                  
         USING RECONWK,RC                                                       
*                                                                               
         L     R8,=V(LDDEFN)                                                    
         USING LDDEFND,R8               ESTABLISH FILE DEFINITION AREA          
*                                                                               
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
*                                                                               
         LA    R0,L'ROLDKEY+L'RSEQ                                              
         CVD   R0,DUB                                                           
         UNPK  SORTCARD+15(2),DUB                                               
         OI    SORTCARD+16,X'F0'                                                
         GOTO1 LSORTER,DMCB,SORTCARD,RECCARD                                    
         B     OPEN                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,  ,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=4100'                                  
*                                                                               
OPEN     OPEN  (RECVIN,(INPUT))                                                 
         EJECT                                                                  
IN2      LA    R0,RECVHDR-4                                                     
         GET   RECVIN,(0)                                                       
*                                                                               
         CLI   RRECTY,X'81'        IGNORE POINTER COPY/CHANGES                  
         BE    IN2                                                              
         CLI   RRECTY,X'82'                                                     
         BE    IN2                                                              
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    IN2                                                              
         CLI   RSIN,X'FE'          OFFLINE COPY/CHANGE PAIR                     
         BNE   *+10                                                             
         MVC   RSIN,=F'1'                                                       
         CLI   RSIN,X'FD'          OFFLINE CHANGES ONLY                         
         BNE   *+10                                                             
         MVC   RSIN,=F'0'                                                       
*                                                                               
         CLC   RFILTY,LDDDTFDA+3   TEST RIGHT DA FILE                           
         BE    IN3                                                              
*                                                                               
         TM    LDDMULTI,LDDQIPTR   DROP IF NO DIR ONLY RECS                     
         BNO   IN2                                                              
*                                                                               
         CLC   RFILTY,LDDDTFIS+3   DROP IF NOT IS FILE RECORD                   
         BNE   IN2                                                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,LDDIPDSP       DISPLACEMENT TO DIR ONLY INDICATOR           
*                                                                               
         LA    R1,RKEY(RE)         POINT TO DIRECTORY ONLY INDICATOR            
*                                                                               
         LLC   RF,LDDIPLEN         LENGTH OF DIR ONLY INDICATOR                 
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),LDDIPARG    DROP IF NOT DIR ONLY RECORD                  
         BNE   IN2                                                              
*                                                                               
IN3      SR    R1,R1               SET R1 TO POINT TO END OF RECORD             
         ICM   R1,3,RECVHDR-4                                                   
         LA    R1,RECVHDR-4(R1)                                                 
         TM    RTIME,X'40'         TEST IF THERE IS A TRAILER                   
         BZ    IN4                 NO                                           
         NI    RTIME,255-X'40'     YES-REMOVE TRAILER                           
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         IC    RF,0(R1)            RF=L'TRAILER IN LAST BYTE                    
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=A(END OF RECORD WITHOUT TRAILER)          
         LA    RF,RECVHDR-4                                                     
         LR    RE,R1                                                            
         SR    RE,RF                                                            
         STCM  RE,3,RECVHDR-4      SET NEW RECORD LENGTH                        
*                                                                               
IN4      MVI   0(R1),0             SET ZERO AT END OF RECORD                    
*                                                                               
         CLI   RRECTY,1                                                         
         BE    CPY                                                              
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,RECVHDR-4                                                   
         LA    RE,L'ROLDKEY+L'RSEQ+L'RSPARE(RE)                                 
         SLL   RE,16                                                            
         ST    RE,RLEN                                                          
*                                                                               
         L     RE,MYSEQ                                                         
         LA    RE,1(RE)                                                         
         ST    RE,MYSEQ                                                         
         MVC   RSEQ,MYSEQ          SET SEQUENCE NUMBER                          
         XC    RSPARE,RSPARE                                                    
*                                                                               
         CLI   RRECTY,2                                                         
         BE    CHG                                                              
         CLI   RRECTY,3                                                         
         BE    ADD                                                              
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,RLEN                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'BAD RECORD',RLEN,C'DUMP',(R4),=C'2D'          
         B     IN2                                                              
         EJECT                                                                  
ADD      MVC   ROLDKEY,RKEY        SET NEW/NEW REC                              
         AP    RCVADD,=P'1'                                                     
         GOTO1 LSORTER,DMCB,=C'PUT',RLEN                                        
         B     IN2                                                              
*                                                                               
CPY      MVC   ROLDKEY,RKEY        MOVE COPY KEY FOR OLD/NEW                    
         MVC   SAVSIN,RSIN         SAVE THIS SIN                                
         AP    RCVCPY,=P'1'                                                     
         B     IN2                                                              
*                                                                               
CHG      AP    RCVCHG,=P'1'                                                     
         OC    RSIN,RSIN           TEST OFF-LINE CHANGE (NO COPY)               
         BZ    CHGNEW                                                           
         CLI   CHKSIN,C'N'                                                      
         BE    CHG1                                                             
         CLC   SAVSIN,RSIN         SIN MUST MATCH FOR CPY/CHG PAIR              
         BE    *+6                                                              
         DC    H'0'                                                             
CHG1     GOTO1 LSORTER,DMCB,=C'PUT',RLEN                                        
*                                                                               
         CLC   ROLDKEY,RKEY        TEST TRANS CHANGED KEY                       
         BE    IN2                 NO                                           
*                                                                               
CHGNEW   MVC   ROLDKEY,RKEY        SET FOR NEW/NEW REC                          
         GOTO1 LSORTER,DMCB,=C'PUT',RLEN                                        
         B     IN2                                                              
         EJECT                                                                  
ENDIN1   CLOSE (RECVIN,)                                                        
         EJECT                                                                  
         USING NEXTREC,R3                                                       
OUTPUT   BC    0,OP2                                                            
         OI    *-3,X'F0'                                                        
         OPEN  (FILEIN,(INPUT),FILEOUT,(OUTPUT))                                
*                                                                               
         XC    RLEN(4+L'ROLDKEY+L'RSEQ+L'RSPARE),RLEN                           
         XC    RECVHDR,RECVHDR                                                  
         XC    RKEY(L'ROLDKEY),RKEY                                             
*                                                                               
         GOTO1 LSORTER,DMCB,=C'GET' GET FIRST OUTPUT REC                        
         ICM   R3,15,4(R1)                                                      
         BNZ   *+8                                                              
         LA    R3,KEYFF                                                         
*                                                                               
         BAS   R9,READF            GET FIRST FILE REC                           
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
OPSV2    SR    R0,R0               RECOVERY TRAILER WAS REMOVED                 
*                                                                               
OPSV3    LH    RE,RLEN                                                          
         LA    RF,L'ROLDKEY+L'RSEQ+L'RSPARE+L'RECVHDR                           
         AR    RF,R0               R0=L'TRAILER                                 
         SR    RE,RF               RE=L'RECORD+4                                
         SLL   RE,16                                                            
         ST    RE,DUB                                                           
         MVC   RKEY-4(4),DUB                                                    
*                                                                               
OPX      GOTO1 LSORTER,DMCB,=C'GET'                                             
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
PUT      PUT   FILEOUT,(0)                                                      
         AP    FILOUT,=P'1'                                                     
         BR    R9                                                               
         EJECT                                                                  
ENDIN    CLOSE (FILEIN,)                                                        
OPDONE   CLOSE (FILEOUT,)                                                       
*                                                                               
         MVC   LINE,=PL2'75'                                                    
         MVC   TITLE(25),=C'XSPDIR/XSPFIL RECONSTRUCT'                          
*                                                                               
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
OPD2     MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 LPRINTER                                                         
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
         EJECT                                                                  
DUB      DS    D                                                                
ANSWER   DS    D                                                                
SAVR1    DS    F                                                                
MYSEQ    DC    F'0'                                                             
SAVSIN   DC    F'0'                                                             
DMCB     DS    6F                                                               
KEYFF    DC    50X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
NEWKEYSW DC    X'00'                                                            
CHKSIN   DC    C'N'                                                             
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=GM,                                               X        
               EODAD=ENDIN1                                                     
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=GM,                                               X        
               EODAD=ENDIN                                                      
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=8200,                                             X        
               BLKSIZE=27648,                                          X        
               MACRF=PM                                                         
         EJECT                                                                  
RECONWK  DSECT                                                                  
*                                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL32     ***** HARD CODE KEY LENGTH HERE *****                   
RSEQ     DS    XL4                                                              
RSPARE   DS    CL4                                                              
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0C                                                               
         DS    4000C                                                            
*                                                                               
         DS    D                   ROOM FOR LENGTH                              
FKEY     DS    0C                                                               
FREC     DS    4100C                                                            
         EJECT                                                                  
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    0C                                                               
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         SPACE 3                                                                
         DS    0D                                                               
RECONWKC CSECT                                                                  
         DS    9200X                                                            
         EJECT                                                                  
       ++INCLUDE DMLDDEFN                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SXRECON   06/07/11'                                      
         END                                                                    
