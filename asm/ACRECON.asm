*          DATA SET ACRECON    AT LEVEL 012 AS OF 06/29/11                      
*PHASE ACRECONA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
ACRECON  TITLE 'RECONSTRUCT - ACCMST'                                           
ACRECON  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE (WORKX-WORKD),**RECON,=V(REGSAVE)                                
         USING WORKD,RC                                                         
         L     R7,=V(CPRINT)                                                    
         USING DPRINT,R7                                                        
         LA    R0,L'ROLDKEY+L'RSEQ                                              
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  SORTCARD+15(2),DUB                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         LA    R5,RECVIN                                                        
         OPEN  ((R5),INPUT)                                                     
         B     INPUT                                                            
*                                                                               
SORTCARD DC    C'SORT FIELDS=(5,??,A),FORMAT=BI,WORK=1 '                        
RECCARD  DC    C'RECORD TYPE=V,LENGTH=(2100,,,,) '                              
         EJECT                                                                  
* READ RECOVERY FILE, BUILD KEY AND PUT TO SORTER                               
*                                                                               
INPUT    LA    R1,RECVIN                                                        
         LA    R0,RECVHDR-4                                                     
         GET   (R1),(R0)                                                        
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
         CLI   RFILTY,ACCMSTQ      TEST FOR FILE                                
         BE    INP02                                                            
         CLI   RFILTY,ACCDIRQ      TEST FOR DIRECTORY                           
         BNE   INPUT                                                            
         CLI   RKEY,PLAKTYPQ       TEST INDIRECT POINTER RECORD                 
         BNE   INPUT                                                            
*                                                                               
INP02    CLC   RKEY(KEYLEN-1),KEY00 IGNORE ANY FILEHDR CHANGES, AS THEY         
         BNH   INPUT                WILL WIPE 'MODE=' ON DUMP FILE HDR          
*                                                                               
INP03    SR    R1,R1               SET R1 TO POINT TO END OF RECORD             
         ICM   R1,3,RCVREC                                                      
         LA    R1,RCVREC(R1)                                                    
         TM    RTIME,X'40'         TEST IF THERE IS A TRAILER                   
         BZ    INP04               NO                                           
         NI    RTIME,255-X'40'     YES-REMOVE TRAILER                           
         BCTR  R1,0                                                             
         SR    RF,RF                                                            
         IC    RF,0(R1)            RF=L'TRAILER IN LAST BYTE                    
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=A(END OF RECORD WITHOUT TRAILER)          
         LA    RF,RCVREC                                                        
         LR    RE,R1                                                            
         SR    RE,RF                                                            
         STCM  RE,3,RCVREC         SET NEW RECORD LENGTH                        
*                                                                               
INP04    MVI   0(R1),0             SET ZERO AT END OF RECORD                    
*                                                                               
         CLI   RRECTY,1                                                         
         BE    CPY                                                              
*                                                                               
         SR    RE,RE               SET RECORD LEN                               
         ICM   RE,3,RCVREC                                                      
         AHI   RE,L'ROLDKEY+L'RSEQ+L'RSPARE                                     
         SLL   RE,16                                                            
         ST    RE,RLEN                                                          
*                                                                               
         L     RE,MYSEQ            SET SEQUENCE NUMBER                          
         AH    RE,=H'1'                                                         
         ST    RE,MYSEQ                                                         
         MVC   RSEQ,MYSEQ                                                       
         XC    RSPARE,RSPARE                                                    
*                                                                               
         CLI   RRECTY,2            CHECK FOR CHANGE OR ADD                      
         BE    CHG                 CHANGE                                       
         CLI   RRECTY,3                                                         
         BE    ADD                 ADD                                          
         DC    H'0'                                                             
*                                                                               
ADD      MVC   ROLDKEY,RKEY        SET NEW/NEW REC                              
         AP    RCVADD,=P'1'                                                     
         B     INX                                                              
*                                                                               
CPY      MVC   ROLDKEY,RKEY        MOVE COPY KEY FOR OLD/NEW                    
         MVC   SAVSIN,RSIN         SAVE THIS SIN                                
         AP    RCVCPY,=P'1'                                                     
         B     INPUT                                                            
*                                                                               
CHG      AP    RCVCHG,=P'1'                                                     
         OC    RSIN,RSIN           TEST OFF-LINE CHANGE(NO COPY)                
         BZ    CHGNEW                                                           
         CLI   CHKSIN,C'N'                                                      
         BE    INX                                                              
         CLC   SAVSIN,RSIN         CHECK CHANGE IS PAIR OF COPY                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
INX      DS    0H                  PUT TO SORTER                                
         GOTO1 SORTER,DMCB,=C'PUT',RLEN                                         
         CLC   ROLDKEY,RKEY        AND AGAIN IF KEY CHANGE                      
         BE    INPUT                                                            
         MVC   ROLDKEY,RKEY        THIS TIME WITH NEW KEY                       
         B     INX                                                              
*                                                                               
CHGNEW   MVC   ROLDKEY,RKEY        PUT TO SORTER                                
         GOTO1 SORTER,DMCB,=C'PUT',RLEN                                         
         B     INPUT                                                            
*                                                                               
ENDRECV  LA   R5,RECVIN            END OF RECOVERY FILE                         
         CLOSE ((R5))                                                           
         B     OUTPUT                                                           
         EJECT                                                                  
* GET RECOVERY RECORDS FROM SORTER AND UPDATE ACCMST                            
*                                                                               
OUTPUT   LA    R5,FILEIN           OPEN ACCMST TAPES AND INITIALISE             
         OPEN  ((R5),INPUT)                                                     
         LA    R5,FILEOUT                                                       
         OPEN  ((R5),OUTPUT)                                                    
         XC    RLEN(4+L'ROLDKEY+L'RSEQ+L'RSPARE),RLEN                           
         XC    RECVHDR,RECVHDR                                                  
         XC    RKEY,RKEY                                                        
         BAS   R9,READF            GET FIRST FILE REC                           
*                                                                               
OP1      GOTO1 SORTER,DMCB,=C'GET' GET FIRST/NEXT RECOVERY REC                  
         ICM   R3,15,4(R1)                                                      
         USING NEXTREC,R3                                                       
         BNZ   OP2                                                              
         LA    R3,KEYFF            END OF RECOVERY                              
         B     OP8B                                                             
*                                                                               
OP2      CLC   FKEY,ROLDKEY        COMPARE FILE/RECOVERY KEYS                   
         BL    OP6                                                              
         BH    OP8                                                              
         CLI   FKEY,X'FF'          TEST RECV/FILE AT EOF                        
         BE    OPDONE                                                           
         B     OP8                                                              
*                                                                               
OP6      LA    R0,FKEY-4           FILE LOW - WRITE FILE                        
         BAS   R9,PUT                                                           
         BAS   R9,READF                                                         
         B     OP2                                                              
         EJECT                                                                  
* FILE HIGH OR EQUAL - READ TILL CHANGE IN RKEY/NKEY VIA XMOD, THEN PUT         
*                                                                               
OP8      CLC   ROLDKEY,RKEY        TEST KEY WAS CHANGED IN RCV COPY/CHG         
         BE    OP8A                NO                                           
         AP    CHGKEY,=P'1'                                                     
         B     OP8B                                                             
*                                                                               
OP8A     CLC   FKEY,RKEY           TEST REC PREVIOUSLY ON FILE                  
         BNE   OP8B                NO                                           
         CLI   RRECTY,3            TEST RECOVERY ADD                            
         BNE   OP8B                NO                                           
         AP    REUSEKEY,=P'1'      YES - RECYCLED KEY                           
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
         LA    R0,RKEY-4                                                        
         BAS   R9,PUT              ELSE PUT RECOVERY REC                        
         B     OPSV                                                             
         EJECT                                                                  
* REC PREVIOUSLY ON FILE                                                        
*                                                                               
OP8D     BAS   R9,READF            SKIP TO NEXT FILE REC                        
         CLC   ROLDKEY,RKEY        TEST KEY CHANGE                              
         BNE   OPSV                YES - SKIP RECV TOO                          
         LA    R0,RKEY-4                                                        
         BAS   R9,PUT              ELSE PUT RECOVERY REC                        
                                                                                
OPSV     CLI   NEXTREC,X'FF'       CHECK END OF RECOVERY RECS                   
         BNE   OPSV1               NO                                           
         MVI   RKEY,X'FF'          YES - MAKE RECOVERY KEY HIGH                 
         MVC   RKEY+1(L'RKEY-1),RKEY                                            
         MVC   ROLDKEY,RKEY        SO ALL REMAINING FILE RECS ARE ADDED         
         B     OP2                                                              
*                                                                               
OPSV1    LH    RF,NLEN             SAVE NEW RECOVERY REC                        
         LR    R1,RF                                                            
         LA    RE,NLEN                                                          
         LA    R0,RLEN                                                          
         MVCL  R0,RE                                                            
*                                                                               
OPSV2    LH    RE,RLEN                                                          
         LA    RF,L'ROLDKEY+L'RSEQ+L'RSPARE+L'RECVHDR                           
         SR    RE,RF               RE=L'RECORD+4                                
         SLL   RE,16                                                            
         STCM  RE,15,RKEY-4                                                     
         B     OP1                 GO BACK FOR NEXT RCV GET                     
         EJECT                                                                  
* SUBROUTINES ETC                                                               
*                                                                               
READF    CLC   FKEY,KEYFF          READ ACCOUNT FILE DUMP TAPE                  
         BER   R9                                                               
         LA    R0,FKEY-4                                                        
         LA    R1,FILEIN                                                        
         GET   (R1),(R0)                                                        
         AP    FILIN,=P'1'                                                      
         LH    R1,FKEY-4                                                        
         CHI   R1,ACCKLEN+4        TEST ACCDIR REC                              
         BH    READF02             NO - MST (OR ARC)                            
         TM    FKEY+KEYLEN+0,X'80' TEST DELETED                                 
         BNOR  R9                                                               
         B     *+10                                                             
READF02  TM    FKEY+KEYLEN+2,X'80' IGNORE DELETES WHICH MAY NOT EVEN BE         
         BNOR  R9                  IN KEY SEQUENCE IF RESULTING                 
         AP    FILDEL,=P'1'        FROM KEY CHANGES - AND SO CAN UPSET          
         B     READF               KEY MATCHING                                 
*                                                                               
PUT      LA    R1,FILEOUT          WRITE ACCMST                                 
         PUT   (R1),(R0)                                                        
         AP    FILOUT,=P'1'                                                     
         BR    R9                                                               
*                                                                               
ENDIN    MVI   FKEY,X'FF'          END OF ACCMST INPUT                          
         MVC   FKEY+1(L'FKEY-1),FKEY                                            
         BR    R9                                                               
*                                                                               
OPDONE   LA    R5,FILEIN           END OF RUN                                   
         CLOSE ((R5))                                                           
         LA    R5,FILEOUT                                                       
         CLOSE ((R5))                                                           
         MVC   TITLE(7),FILNAME                                                 
         MVC   TITLE+7(12),=C' RECONSTRUCT'                                     
         LA    R3,CNTRS                                                         
         LA    R4,20                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
OPD2     MVC   SPACING,=C'BL02'                                                 
         OI    5(R3),X'0F'                                                      
         UNPK  P+1(12),0(6,R3)                                                  
         MVC   P+15(14),6(R3)                                                   
         GOTO1 PRINTER                                                          
         BXLE  R3,R4,OPD2                                                       
         XBASE                                                                  
*                                                                               
CNTRS    DS    0D                                                               
RCVADD   DC    PL6'0',CL14'RECOVERY ADDS'                                       
RCVCPY   DC    PL6'0',CL14'RECOVERY CPYS'                                       
RCVCHG   DC    PL6'0',CL14'RECOVERY CHGS'                                       
CHGKEY   DC    PL6'0',CL14'CHANGED KEYS'                                        
REUSEKEY DC    PL6'0',CL14'RE-USED KEYS'                                        
FILIN    DC    PL6'0',CL14'FILE IN'                                             
FILDEL   DC    PL6'0',CL14'FILE DELETES'                                        
FILOUT   DC    PL6'0',CL14'FILE OUT'                                            
CNTRSX   EQU   *-1                                                              
*                                                                               
LOGIO    DC    V(LOGIO)                                                         
PRINTER  DC    V(PRINTER)                                                       
SORTER   DC    V(SORTER)                                                        
         EJECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
MSGAREA  DS    CL10                                                             
SAVSIN   DC    F'0'                                                             
MYSEQ    DC    F'0'                                                             
KEY00    DC    64X'00'   *****  AT LEAST KEYLEN X'00'S *****                    
KEYFF    DC    64X'FF'   *****  AT LEAST KEYLEN X'FF'S *****                    
CHKSIN   DC    C'Y'                                                             
         LTORG                                                                  
*                                                                               
KEYLEN   EQU   L'ACCKEY            *****  USER DEFINED  *****                   
ACCMSTQ  EQU   X'6A'               *****  USER DEFINED  *****                   
FILNAME  DC    C'ACCMST '          *****  USER DEFINED  *****                   
ACCDIRQ  EQU   X'69'                                                            
DIRNAME  DC    C'ACCDIR '                                                       
*                                                                               
         EJECT                                                                  
         PRINT NOGEN                                                            
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,MACRF=(GM),RECFM=VB,             X        
               BUFNO=2,EODAD=ENDRECV                                            
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,MACRF=(GM),RECFM=VB,             X        
               BUFNO=2,EODAD=ENDIN                                              
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            X        
               BLKSIZE=27648,LRECL=8200,BUFNO=2                                 
         EJECT                                                                  
* DSECTS                                                                        
*                                                                               
WORKD    DSECT                                                                  
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL(KEYLEN)          * SORT KEY                                   
RSEQ     DS    CL4                 *                                            
RCVREC   DS    0X                  RECOVERY RECORD STARTS HERE                  
RSPARE   DS    XL4                 RECORD LENGTH                                
       ++INCLUDE DMRCVRHDR                                                      
RKEY     DS    0XL(KEYLEN)         *****  USER DEFINED  *****                   
         DS    2050X                                                            
         SPACE 2                                                                
         DS    D                   ROOM FOR LENGTH                              
FKEY     DS    0XL(KEYLEN)         *****  USER DEFINED  *****                   
FREC     DS    2200X                                                            
WORKX    EQU   *                                                                
         SPACE 2                                                                
NEXTREC  DSECT                                                                  
NLEN     DS    H                                                                
         DS    H                                                                
NOLDKEY  DS    CL(KEYLEN)          *****  USER DEFINED  *****                   
         DS    0C                                                               
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACRECON   06/29/11'                                      
         END                                                                    
