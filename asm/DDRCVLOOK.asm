*          DATA SET DDRCVLOOK  AT LEVEL 003 AS OF 08/13/00                      
*PHASE RCVLOOKA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE FATAB                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE REGSAVE                                                                
       ++INCLUDE DMGREQUS                                                       
         TITLE 'GENERALIZED RECOVERY EXTRACT'                                   
RCVLOOK  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,*RCVL**,=V(REGSAVE)                                            
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         LA    R6,RCVOUT                                                        
         OPEN  ((R6),OUTPUT)                                                    
         GOTO1 =V(DATAMGR),DMCB,=C'OPEN',=C'CONTROL',=C'NCTFILE X',IO           
         GOTO1 =V(DMOD000),PARAS,V(FINDSYS),(1,0)                               
         MVC   ASYS,4(R1)                                                       
*                                                                               
RCV2     GOTO1 =V(CARDS),PARAS,IO,=C'RE00'                                      
         CLC   IO(2),=C'/*'                                                     
         BE    RCVX                                                             
         CLC   IO(5),=C'TRACE'                                                  
         BNE   *+12                                                             
         MVI   TRACE,C'Y'                                                       
         B     RCV2                                                             
*                                                                               
         MVC   FILEID,IO                                                        
         L     R1,ASYS             R1=A(SYSFLES)                                
RCV4     ZIC   R0,3(R1)            R0=NUMBER OF FILES                           
         STC   R0,NSYSFLES                                                      
         MVC   SYSNUM,0(R1)        SAVE SE NUMBER                               
         LA    R1,4(R1)            POINT TO FIRST ENTRY                         
         ST    R1,ASYSFLES                                                      
RCV6     ICM   RE,15,4(R1)                                                      
         BZ    *+14                                                             
         CLC   FILEID,22(RE)                                                    
         BE    RCV8                                                             
         LA    R1,8(R1)                                                         
         BCT   R0,RCV6                                                          
         CLI   0(R1),X'FF'         TEST ANY MORE SYSTEMS                        
         BNE   RCV4                                                             
         CLI   TRACE,C'Y'                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 =V(LOGIO),PARAS,1,=C'FILE NAME NOT RECOGNIZED'                   
         GOTO1 (RF),(R1),,(8,IO)                                                
         B     RCVX                                                             
*                                                                               
RCV8     L     R1,=V(SELIST)                                                    
         LH    RE,0(R1)                                                         
         L     RF,4(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,SYSNUM                                                     
         BE    RCV10                                                            
         BXLE  R1,RE,*-10                                                       
         MVC   SYSNAME,=C'SYS=XX '                                              
         GOTO1 =V(HEXOUT),DMCB,SYSNUM,SYSNAME+4,1,=C'TOG'                       
         XC    APRG,APRG                                                        
         B     RCV20                                                            
*                                                                               
RCV10    MVC   SYSNAME,SENAME                                                   
         MVC   APRG,SEPGMS                                                      
         B     RCV20                                                            
         EJECT                                                                  
* OPEN FILE AND READ AND SORT RECOVERY RECORDS                                  
*                                                                               
RCV20    LA    R9,RCVFILE                                                       
         USING DTFPHD,R9                                                        
         MVC   DTFFID,FILEID                                                    
         XC    P1(24),P1                                                        
         ST    R9,P4                                                            
         GOTO1 =V(DADDS),PARAS,A(DAOPEN)                                        
         LA    R1,RECVHDR                                                       
         ST    R1,P2               A(RECORD)                                    
         MVC   TTB,=X'00010000'                                                 
         LA    R1,TTB                                                           
         ST    R1,P5               A(DISK ADDRESS)                              
         L     R1,=A(TRKBUFF)                                                   
         ST    R1,P6                                                            
         XC    SEQ,SEQ                                                          
*                                                                               
RCV21    ZIC   R1,TTB+2            BUMP BLOCK                                   
         LA    R1,1(R1)                                                         
         STC   R1,TTB+2                                                         
         CLI   TTB+2,0                                                          
         BE    RCV22                                                            
         GOTO1 =V(DADDS),PARAS,A(RDTRK)                                         
         CLI   P3+1,X'09'          TEST E-O-T                                   
         BNE   RCV23                                                            
RCV22    LH    R1,TTB              BUMP TRACK                                   
         LA    R1,1(R1)                                                         
         SLL   R1,16                                                            
         ST    R1,TTB                                                           
         B     RCV21                                                            
RCV23    CLI   P3+1,X'04'          TEST E-O-F                                   
         BE    RCV26                                                            
         OC    P3(2),P3                                                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   RSIN,0              TEST OFFLINE RECOVERY RECORD                 
         BE    RCV21                                                            
         L     R1,SEQ              BUMP SEQUENCE NUMBER                         
         LA    R1,1(R1)                                                         
         ST    R1,SEQ                                                           
         CLI   TRACE,C'Y'                                                       
         BNE   *+14                                                             
         CLC   SEQ,=F'250'                                                      
         BH    RCV26                                                            
         XC    SORTREC,SORTREC                                                  
         MVC   SSIN,RSIN+1                                                      
         MVC   SSEQ,SEQ+1                                                       
         MVC   SDATE,RDATE                                                      
         MVC   STIME,RTIME                                                      
         MVC   SFILTY,RFILTY                                                    
         MVC   SRECTY,RRECTY                                                    
         MVC   SPRG,RPRG                                                        
         MVC   SUSER,RUSER                                                      
         MVC   STRM,RTRM                                                        
         CLI   SORTSW,0                                                         
         BNE   RCV24                                                            
         MVI   SORTSW,1                                                         
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
RCV24    GOTO1 =V(SORTER),DMCB,=C'PUT',SORTREC                                  
         B     RCV21                                                            
*                                                                               
RCV26    GOTO1 =V(DADDS),PARAS,A(DACLOSE)                                       
         CLI   SORTSW,0            TEST ANY RECORDS SORTED                      
         BE    RCV2                NO                                           
         MVI   SORTSW,0                                                         
*                                                                               
RCV28    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R1,15,4(R1)                                                      
         BZ    RCV2                                                             
         MVC   SORTREC,0(R1)                                                    
         XC    OUTREC,OUTREC       BUILD OUTPUT RECORD                          
         MVC   OSYS,SYSNAME                                                     
         MVC   OSIN,SSIN                                                        
         ZIC   R1,SDATE+0          SET EBCDIC DATE                              
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ODATE+0(2),DUB                                                   
         IC    R1,SDATE+1                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ODATE+2(2),DUB                                                   
         IC    R1,SDATE+2                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ODATE+4(2),DUB                                                   
         MVC   OTIME,STIME                                                      
         OI    OTIME+3,X'0F'                                                    
         L     R1,ASYSFLES         SET FILE NAME                                
         ZIC   R0,NSYSFLES                                                      
RCV30    CLC   SFILTY,3(R1)                                                     
         BE    *+14                                                             
         LA    R1,8(R1)                                                         
         BCT   R0,RCV30                                                         
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,7,5(R1)          R1=A(DTF)                                    
         MVC   OFILE,22(RE)                                                     
         MVC   ORECTY,SRECTY                                                    
         ICM   R1,15,APRG                                                       
         BZ    RCV32                                                            
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1                                                       
         CLC   PGMNUM,SPRG                                                      
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     RCV32                                                            
         MVC   OPRG,PGMNAME                                                     
         B     RCV34                                                            
RCV32    MVC   OPRG,=C'PGM=XX '                                                 
         GOTO1 =V(HEXOUT),DMCB,SPRG,OPRG+4,1,=C'TOG'                            
RCV34    XC    IO(25),IO                                                        
         MVI   IO,C'I'                                                          
         MVC   IO+23(2),SUSER                                                   
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',IO,IO                     
         CLI   8(R1),0                                                          
         BNE   RCV38                                                            
         LA    R1,IO+28                                                         
         SR    R0,R0                                                            
RCV36    CLI   0(R1),0                                                          
         BE    RCV38                                                            
         CLI   0(R1),X'02'                                                      
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     RCV36                                                            
         MVC   OUSER,2(R1)                                                      
         B     RCV40                                                            
RCV38    SR    R1,R1                                                            
         ICM   R1,3,SUSER                                                       
         MVC   OUSER,=C'UID=    '                                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  OUSER+4(4),DUB                                                   
RCV40    MVC   OTRM,STRM                                                        
         CLI   TRACE,C'Y'                                                       
         BE    RCV42                                                            
         PUT   (R6),OUTREC                                                      
         B     RCV28                                                            
*                                                                               
RCV42    MVC   P+0(7),OSYS                                                      
         SR    R1,R1                                                            
         ICM   R1,7,OSIN                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+9(7),DUB                                                       
         XC    DUB(4),DUB                                                       
         MVC   DUB+4(4),OTIME                                                   
         OI    DUB+7,X'0F'                                                      
         UNPK  P+18(6),DUB                                                      
         MVC   P+26(7),OFILE                                                    
         GOTO1 =V(HEXOUT),DMCB,ORECTY,P+35,1,=C'TOG'                            
         MVC   P+39(7),OPRG                                                     
         MVC   P+48(8),OUSER                                                    
         SR    R1,R1                                                            
         ICM   R1,3,OTRM                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+58(4),DUB                                                      
         GOTO1 =V(PRINTER)                                                      
         B     RCV28                                                            
*                                                                               
RCVX     CLOSE ((R6))                                                           
         XBASE ,                                                                
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
SORTCARD DC    C'SORT FIELDS=(1,8,A),FORMAT=BI,WORK=1 '                         
RECCARD  DC    C'RECORD TYPE=F,LENGTH=(32,,,,) '                                
RCVFILE  DMDA  DSKXTNT=16                                                       
RCVOUT   DCB   DDNAME=RCVOUT,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=64                
         EJECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PARAS    DS    0F                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
WORK     DS    CL32                                                             
ASYS     DS    A                                                                
ASYSFLES DS    A                                                                
APRG     DS    A                                                                
SEQ      DS    F                                                                
TTB      DS    F                                                                
TRACE    DC    C'N'                                                             
SORTSW   DC    X'00'                                                            
NSYSFLES DS    X                                                                
SYSNUM   DS    X                                                                
SYSNAME  DS    CL7                                                              
FILEID   DS    CL7                                                              
IO       DS    1000C                                                            
         EJECT                                                                  
SORTREC  DS    0XL32                                                            
SSIN     DS    XL3                 SYSTEM INPUT NUMBER                          
SSEQ     DS    XL3                 SEQUENCE NUMBER                              
SDATE    DS    XL3                 DATE (BINARY YMD)                            
STIME    DS    XL4                 TIME (STANDARD)                              
SFILTY   DS    X                   FILE TYPE                                    
SRECTY   DS    X                   RECOVERY RECORD TYPE                         
SPRG     DS    X                   PROGRAM NUMBER                               
SUSER    DS    XL2                 USER-ID NUMBER                               
STRM     DS    XL2                 TERMINAL NUMBER                              
         ORG   SORTREC+L'SORTREC                                                
         SPACE 1                                                                
OUTREC   DS    0XL64                                                            
OSYS     DS    CL7                 SYSTEM (SPOT1, PRINT2 ETC)                   
OSIN     DS    XL3                 SYSTEM INPUT NUMBER                          
ODATE    DS    CL6                 SYSTEM DATE (EBCDIC)                         
OTIME    DS    XL4                 TIME (STANDARD)                              
OFILE    DS    CL7                 FILE NAME                                    
ORECTY   DS    XL1                 RECORD TYPE                                  
OPRG     DS    CL7                 PROGRAM NAME                                 
OUSER    DS    CL8                 USER-ID NAME                                 
OTRM     DS    XL2                 TERMINAL NUMBER                              
         ORG   OUTREC+L'OUTREC                                                  
         SPACE 1                                                                
       ++INCLUDE DMRCVRHDR                                                      
         DS    4000C                                                            
         EJECT                                                                  
TRKBUFF  DS    50000C                                                           
         SPACE 1                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DDRCVLOOK 08/13/00'                                      
         END                                                                    
