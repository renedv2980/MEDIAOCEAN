*          DATA SET PERECON    AT LEVEL 007 AS OF 08/31/11                      
*PHASE PERECONA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'RECONSTRUCT - PERFIL'                                           
         PRINT NOGEN                                                            
RECON    CSECT                                                                  
         NBASE 600,RECON,=V(REGSAVE)                                            
         USING RECONWK,RC                                                       
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
         LA    R3,SORTCARD                                                      
         LA    RE,L'ROLDKEY+L'RSEQ                                              
         CVD   RE,DUB                                                           
         UNPK  15(2,R3),DUB+6(2)                                                
         OI    16(R3),X'F0'                                                     
         BAS   RE,SETSORT                                                       
*                                                                               
         OPEN  (IN1,(INPUT))                                                    
*                                                                               
INPUT    LA    R0,RECVHDR-4                                                     
         GET   IN1,(0)                                                          
*                                                                               
         CLI   RRECTY,X'81'        IGNORE POINTER COPY/CHANGES                  
         BE    INPUT                                                            
         CLI   RRECTY,X'82'                                                     
         BE    INPUT                                                            
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    INPUT                                                            
         CLI   RSIN,X'FE'          OFFLINE COPY/CHANGE PAIR                     
         BNE   *+10                                                             
         MVC   RSIN,=F'1'                                                       
         CLI   RSIN,X'FD'          OFFLINE CHANGES ONLY                         
         BNE   *+10                                                             
         MVC   RSIN,=F'0'                                                       
*                                                                               
         CLI   RFILTY,FILNUM       TEST FOR FILE                                
         BNE   INPUT                                                            
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
         DC    H'0'                                                             
         EJECT                                                                  
ADD      DS    0H                                                               
         MVC   ROLDKEY,RKEY        SET NEW/NEW REC                              
         AP    RCVADD,=P'1'                                                     
         B     INX                                                              
         SPACE 2                                                                
CPY      MVC   ROLDKEY,RKEY        MOVE COPY KEY FOR OLD/NEW                    
         MVC   SAVSIN,RSIN         SAVE THIS SIN                                
         AP    RCVCPY,=P'1'                                                     
         B     INPUT                                                            
         SPACE 2                                                                
CHG      CLI   CHKSIN,C'N'                                                      
         BE    INX                                                              
         CLC   SAVSIN,RSIN                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    RCVCHG,=P'1'                                                     
         SPACE 2                                                                
INX      DS    0H                  PUT TO SORTER                                
         BAS   RE,PUTSORT                                                       
         CLC   ROLDKEY,RKEY                                                     
         BE    INPUT               AND AGAIN IF KEY CHANGE                      
         MVC   ROLDKEY,RKEY                                                     
         B     INX                                                              
         SPACE 2                                                                
ENDIN1   LA    R1,IN1              END-OF-FILE ON RECOVERY FILE                 
         CLOSE (R1)                                                             
         B     OUTPUT                                                           
         EJECT                                                                  
* GET RECOVERY RECORDS FROM SORTER AND UPDATE PERFILE                           
*                                                                               
OUTPUT   LA    R1,IN                                                            
         OPEN  ((R1),INPUT)                                                     
         LA    R1,OUT                                                           
         OPEN  ((R1),OUTPUT)                                                    
         XC    RLEN(4+L'ROLDKEY+L'RSEQ+L'RSPARE),RLEN                           
         XC    RECVHDR,RECVHDR                                                  
         XC    RKEY,RKEY                                                        
         BAS   R9,READF            GET FIRST FILE REC                           
*                                                                               
OP1      GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R3,15,DMCB+4                                                     
         USING NEXTREC,R3                                                       
         BNZ   OP2                                                              
         LA    R3,KEYFF                                                         
         B     OP8B                                                             
*                                                                               
OP2      CLC   FKEY,ROLDKEY                                                     
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
* FILE HIGH OR EQUAL - READ TILL CHANGE IN RKEY/NKEY VIA XMOD, THEN PUT         
*                                                                               
OP8      CLC   ROLDKEY,RKEY        TEST TRANS CHANGED KEY                       
         BE    OP8A                NO                                           
         AP    CHGKEY,=P'1'                                                     
         B     OP8B                                                             
*                                                                               
OP8A     CLC   FKEY,RKEY           TEST REC PREVIOUSLY ON FILE                  
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
         CLC   FKEY,ROLDKEY        TEST REC ADDED TODAY                         
         BE    OP8D                NO                                           
         CLC   ROLDKEY,RKEY        YES - TEST KEY CHANGE                        
         BNE   OPSV                LAST ACTIVITY WAS KEY CHANGE - SKIP          
         LA    R0,RKEY-4           ELSE PUT RECV                                
         BAS   R9,PUT                                                           
         B     OPSV                                                             
* REC PREVIOUSLY ON FILE                                                        
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
         MVC   RKEY+1(L'RKEY-1),RKEY                                            
         MVC   ROLDKEY,RKEY                                                     
         B     OP2                                                              
*                                                                               
OPSV1    LH    RF,NLEN             SAVE NEW RECOVERY REC                        
         LA    R0,RLEN                                                          
         LA    RE,NLEN                                                          
         MVCL  R0,RE                                                            
*                                                                               
OPSV2    SR    R0,R0               R0=LEN OF RECOVERY TRAILER                   
*                                                                               
OPSV3    LH    RE,RLEN                                                          
         LA    RF,L'ROLDKEY+L'RSEQ+L'RSPARE+L'RECVHDR                           
         AR    RF,R0                                                            
         SR    RE,RF               RE=L'RECORD+4                                
         SLL   RE,16                                                            
         ST    RE,DUB                                                           
         MVC   RKEY-4(4),DUB                                                    
         B     OP1                 GET NEXT RECOVERY REC                        
         EJECT                                                                  
READF    CLC   FKEY,KEYFF                                                       
         BCR   8,R9                                                             
         LA    R0,FKEY-4                                                        
         LA    R1,IN                                                            
         GET   (R1),(R0)                                                        
         AP    FILIN,=P'1'                                                      
         BR    R9                                                               
*                                                                               
PUT      LA    R1,OUT                                                           
         PUT   (R1),(R0)                                                        
         AP    FILOUT,=P'1'                                                     
         BR    R9                                                               
         SPACE 2                                                                
ENDIN    MVI   FKEY,X'FF'          END OF PERFIL TAPE                           
         MVC   FKEY+1(L'FKEY-1),FKEY                                            
         BR    R9                                                               
*                                                                               
OPDONE   LA    R1,IN               END OF RUN                                   
         CLOSE ((R1))                                                           
         LA    R1,OUT                                                           
         CLOSE ((R1))                                                           
         BAS   RE,ENDSORT                                                       
         SPACE 2                                                                
         MVC   TITLE(7),FILNAME                                                 
         MVC   TITLE+7(12),=C' RECONSTRUCT'                                     
*                                                                               
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
OPD2     MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 PRINTER                                                          
         BXLE  R3,R4,OPD2                                                       
         XBASE                                                                  
         EJECT                                                                  
*              SORTER INTERFACE                                                 
         SPACE 2                                                                
SETSORT  NTR1                                                                   
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECDCARD                                
         B     EXIT                                                             
         SPACE 2                                                                
PUTSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'PUT',RLEN                                         
         B     EXIT                                                             
         SPACE 2                                                                
ENDSORT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         SPACE 1                                                                
SORTCARD DC    CL80'SORT FIELDS=(05,XX,A),FORMAT=BI,WORK=1'                     
RECDCARD DC    CL80'RECORD TYPE=V,LENGTH=2100'                                  
         EJECT                                                                  
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
         SPACE 1                                                                
SORTER   DC    V(SORTER)                                                        
PRINTER  DC    V(PRINTER)                                                       
LOGIO    DC    V(LOGIO)                                                         
*                                                                               
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
MSGAREA  DS    CL10                                                             
MYSEQ    DC    F'0'                                                             
SAVSIN   DC    F'0'                                                             
CHKSIN   DC    C'Y'                                                             
NEWKEYSW DC    X'00'                                                            
KEYFF    DC    50X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
         LTORG                                                                  
*                                                                               
KEYLEN   EQU   36                  *****  USER DEFINED  *****                   
FILNUM   EQU   X'E2'               *****  USER DEFINED  *****                   
FILNAME  DC    C'PERFIL '          *****  USER DEFINED  *****                   
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
IN1      DCB   DDNAME=IN1,DSORG=PS,MACRF=(GM),RECFM=VB,                X        
               EODAD=ENDIN1,BUFNO=2                                             
*                                                                               
IN       DCB   DDNAME=IN,DSORG=PS,MACRF=(GM),RECFM=VB,                 X        
               EODAD=ENDIN,BUFNO=2                                              
*                                                                               
OUT      DCB   DDNAME=OUT,DSORG=PS,MACRF=(PM),RECFM=VB,                X        
               BLKSIZE=27648,LRECL=8200,BUFNO=2                                 
         EJECT                                                                  
RECONWK  DSECT                                                                  
*                                                                               
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL36                *****  USER DEFINED  *****                   
RSEQ     DS    XL4                                                              
RSPARE   DS    CL4                                                              
*                                                                               
* DMRCVRHDR                                                                     
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0CL36               *****  USER DEFINED  *****                   
         DS    2050C                                                            
         SPACE 2                                                                
         DS    D                   ROOM FOR LENGTH                              
FKEY     DS    0CL36               *****  USER DEFINED  *****                   
FREC     DS    2200C                                                            
         EJECT                                                                  
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    CL36                *****  USER DEFINED  *****                   
NTRTYPE  DS    C                                                                
NSEQ     DS    H                                                                
*                                                                               
NKEY     DS    0CL36               *****  USER DEFINED  *****                   
NREC     DS    0C                                                               
*                                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PERECON   08/31/11'                                      
         END                                                                    
