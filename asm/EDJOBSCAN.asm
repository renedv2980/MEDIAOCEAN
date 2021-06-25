*          DATA SET EDJOBSCAN  AT LEVEL 002 AS OF 05/01/02                      
*PHASE JOBSCAN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE XSORT                                                                  
*CHANGE  WORKER(WORKX)                                                          
*INCLUDE DMDMGRL                                                                
*INCLUDE WORKER                                                                 
         TITLE 'EDJOBSCAN - PROGRAM TO SCAN EOD JOBS'                           
         PRINT NOGEN                                                            
JOBSCAN  CSECT                                                                  
         NBASE 0,JOBSCAN,WORK=A(WRKWRK)                                         
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(13),=C'EOD JOBS SCAN'                                      
         MVC   MID1+00(6),=C'ORIGIN'                                            
         MVC   MID1+10(4),=C'DEST'                                              
         MVC   MID1+20(5),=C'COUNT'                                             
         MVC   MID2+00(6),=16C'-'                                               
         MVC   MID2+10(4),=16C'-'                                               
         MVC   MID2+20(5),=16C'-'                                               
*                                                                               
CARDS    GOTO1 =V(CARDS),PARM,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    CARDSX                                                           
         GOTO1 =V(PRINTER)                                                      
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
         B     CARDS                                                            
CARDSX   EQU   *                                                                
*                                                                               
INIT     GOTO1 =V(DATAMGR),DMCB,DMOPEN,DMSYSTEM,DMFILES                         
         L     R7,=A(JOBIO)                                                     
         LA    R7,4(R7)                                                         
         USING EODREQD,R7                                                       
*                                                                               
LOOP     BAS   RE,INDXJOB          READ EODJOBS INDEX                           
         BNE   FINAL                                                            
*                                                                               
LOOP1    BAS   RE,READJOB          READ EODJOBS DATA RECORD                     
         CLI   EOFSW,C'Y'                                                       
         BNE   *+12                                                             
         MVI   EOFSW,C'N'                                                       
         B     LOOP                                                             
         MVC   ORIGIN,SPACES                                                    
         MVC   DEST,SPACES                                                      
         AP    JOBS,=P'1'                                                       
         CLC   QORIGIN,QDEST       TEST IF DESTINATION SAME AS ORIGIN           
         BE    LOOP1                                                            
         AP    JOBS1,=P'1'                                                      
         L     R2,=A(IDTAB)                                                     
*                                                                               
LOOP2    CLC   0(2,R2),=X'FFFF'    TEST END OF TABLE                            
         BE    LOOP1                                                            
         CLC   0(2,R2),QORIGIN                                                  
         BNE   LOOP2A                                                           
         CLC   2(2,R2),QDEST                                                    
         BNE   LOOP2A                                                           
         SR    RF,RF               ALREADY IN THE TABLE SO BUMP COUNT           
         ICM   RF,3,4(R2)                                                       
         LA    RF,1(RF)                                                         
         STH   RF,4(R2)                                                         
         B     LOOP1                                                            
LOOP2A   OC    0(2,R2),0(R2)       TEST FREE ENTRY                              
         BZ    LOOP2B                                                           
         LA    R2,22(R2)                                                        
         B     LOOP2                                                            
LOOP2B   ST    R2,AIDTAB           SAVE A(FREE IDTAB ENTRY)                     
*                                                                               
LOOP3    L     R3,=A(IO)           READ ORIGIN ID RECORD                        
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),QORIGIN                                              
         MVC   CTKEY,0(R3)                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',CTKEY,(R3)                
         CLI   DMCB+8,0                                                         
         BNE   LOOP3X                                                           
         LA    RE,CTIDATA                                                       
         SR    RF,RF                                                            
LOOP3A   CLI   0(RE),0             TEST END OF RECORD                           
         BE    LOOP3X                                                           
         CLI   0(RE),X'02'         DESCRIPTION ELEMENT                          
         BNE   LOOP3B                                                           
         IC    RF,1(RE)                                                         
         SH    RF,=H'1'                                                         
         BM    LOOP3X                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ORIGIN(0),2(RE)                                                  
         B     LOOP3X                                                           
LOOP3B   IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     LOOP3A                                                           
LOOP3X   EQU   *                                                                
*                                                                               
LOOP4    L     R3,=A(IO)           READ DESTINATION ID RECORD                   
         USING CTIREC,R3                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),QDEST                                                
         MVC   CTKEY,0(R3)                                                      
         GOTO1 =V(DATAMGR),DMCB,=C'DMREAD',=C'CTFILE',CTKEY,(R3)                
         CLI   DMCB+8,0                                                         
         BNE   LOOP4X                                                           
         LA    RE,CTIDATA                                                       
         SR    RF,RF                                                            
LOOP4A   CLI   0(RE),0             TEST END OF RECORD                           
         BE    LOOP4X                                                           
         CLI   0(RE),X'02'         DESCRIPTION ELEMENT                          
         BNE   LOOP4B                                                           
         IC    RF,1(RE)                                                         
         SH    RF,=H'1'                                                         
         BM    LOOP4X                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DEST(0),2(RE)                                                    
         B     LOOP4X                                                           
LOOP4B   IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     LOOP4A                                                           
LOOP4X   EQU   *                                                                
*                                                                               
LOOP5    L     R2,AIDTAB           POINT TO SPARE ID TABLE ENTRY                
         L     RF,CIDTAB                                                        
         LA    RF,1(RF)                                                         
         ST    RF,CIDTAB           BUMP NUMBER OF ENTRIES                       
         MVC   0(2,R2),QORIGIN                                                  
         MVC   2(2,R2),QDEST                                                    
         MVC   4(2,R2),=H'1'                                                    
         MVC   6(8,R2),ORIGIN                                                   
         MVC   14(8,R2),DEST                                                    
         B     LOOP1                                                            
*                                                                               
FINAL    L     R2,=A(IDTAB)                                                     
         ICM   R0,15,CIDTAB                                                     
         BZ    FINAL1                                                           
         GOTO1 =V(XSORT),DMCB,(R2),(R0),22,16,6                                 
         L     R2,=A(IDTAB)                                                     
*                                                                               
FINAL1   OC    0(2,R2),0(R2)                                                    
         BZ    FINAL2                                                           
         CLC   0(2,R2),=X'FFFF'                                                 
         BE    FINAL2                                                           
         MVC   P+00(8),6(R2)       ORIGIN ID NAME                               
         MVC   P+10(8),14(R2)      DESTINATION ID NAME                          
         SR    R0,R0                                                            
         ICM   R0,3,4(R2)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+20(5),DUB         NUMBER OF OCCURANCES                         
         GOTO1 =V(PRINTER)                                                      
         LA    R2,22(R2)                                                        
         B     FINAL1                                                           
*                                                                               
FINAL2   GOTO1 =V(PRINTER)                                                      
         UNPK  P(8),JOBS                                                        
         OI    P+7,C'0'                                                         
         UNPK  P+10(8),JOBS1                                                    
         OI    P+17,C'0'                                                        
         GOTO1 =V(PRINTER)                                                      
*                                                                               
EOJ      XBASE                                                                  
         EJECT                                                                  
INDXJOB  NTR1                                                                   
         L     RF,=A(EODJOBS)                                                   
         GOTO1 =V(WORKER),DMCB,=C'INDEX',(RF),JD                                
         B     INDXCHK                                                          
         SPACE 2                                                                
INDXREQ  NTR1                                                                   
         L     RF,=A(EODREQS)                                                   
         GOTO1 =V(WORKER),DMCB,=C'INDEX',(RF),QD                                
         B     INDXCHK                                                          
         SPACE 2                                                                
INDXCHK  SR    R2,R2               TEST FOR ERRORS                              
         ICM   R2,1,DMCB+8         EXIT WITH CC=EQL IF GOOD INDEX READ          
         B     XIT                                                              
         SPACE 2                                                                
READJOB  NTR1                                                                   
         L     RF,=A(EODJOBS)                                                   
         LR    R2,R7                                                            
         SH    R2,=H'4'                                                         
         GOTO1 =V(WORKER),DMCB,=C'READ',(RF),JD,(R2)                            
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8         EXIT WITH CC=EQL IF VALID FILE READ          
         BZ    XIT                                                              
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BO    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVI   EOFSW,C'Y'          SET END OF DATA FLAG                         
         ICM   R2,1,DMCB+8                                                      
         B     XIT                                                              
         SPACE 2                                                                
READREQ  NTR1                                                                   
         XC    REQIOMAP,REQIOMAP   CLEAR MAP OF CARDS                           
         LA    RF,REQIOMAP                                                      
         ST    RF,REQIOADR         POINT TO FIRST CARD IN MAP                   
         SPACE 1                                                                
READREQA L     RF,=A(EODREQS)                                                   
         LA    R2,REQIO                                                         
         MVC   0(80,R2),SPACES                                                  
         MVI   80(R2),X'11'        DEFAULT TO SINGLE CARD REQUEST               
         SH    R2,=H'4'                                                         
         GOTO1 =V(WORKER),DMCB,=C'READ',(RF),QD,(R2)                            
         SPACE 1                                                                
READREQB SR    R2,R2               TEST FOR ERRORS                              
         ICM   R2,1,DMCB+8                                                      
         BZ    READREQC                                                         
         TM    DMCB+8,X'80'        TEST FOR EOF                                 
         BO    *+6                                                              
         DC    H'0'                DIE IF DISK ERROR                            
         MVI   EOFSW,C'Y'          SET END OF DATA FLAG                         
         B     READREQX                                                         
         SPACE 1                                                                
READREQC CLC   REQIO(4),=C'JOB='   TEST JOB= RECORD                             
         BE    READREQX                                                         
         SPACE 1                                                                
READREQD CLI   REQIO+80,X'00'      TEST RDHDR= RECORD                           
         BNE   READREQE                                                         
         L     RE,=A(REQIOH)                                                    
         ST    RE,REQIOMAP+0                                                    
         MVC   0(81,RE),REQIO                                                   
         B     READREQA                                                         
         SPACE 1                                                                
READREQE CLI   REQIO+80,X'11'      TEST SINGLE CARD REQUEST                     
         BNE   READREQF                                                         
         L     RE,=A(REQIO1)                                                    
         ST    RE,REQIOMAP+4                                                    
         MVC   0(81,RE),REQIO                                                   
         B     READREQX                                                         
         SPACE 1                                                                
READREQF CLI   REQIO+80,X'20'      PAIR OF REQUEST CARDS                        
         BNE   READREQG                                                         
         L     RE,=A(REQIO1)                                                    
         ST    RE,REQIOMAP+4                                                    
         MVC   0(81,RE),REQIO                                                   
         CLC   REQ2(80),SPACES     TEST SECOND CARD                             
         BE    READREQX                                                         
         L     RE,=A(REQIO2)                                                    
         ST    RE,REQIOMAP+8                                                    
         MVC   0(80,RE),REQ2                                                    
         B     READREQX                                                         
         SPACE 1                                                                
READREQG CLI   REQIO+80,X'21'      TEST FIRST CARD OF PAIR                      
         BNE   READREQH                                                         
         L     RE,=A(REQIO1)                                                    
         ST    RE,REQIOMAP+4                                                    
         MVC   0(81,RE),REQIO                                                   
         B     READREQA                                                         
         SPACE 1                                                                
READREQH CLI   REQIO+80,X'22'      TEST SECOND CARD OF PAIR                     
         BNE   READREQI                                                         
         L     RE,=A(REQIO2)                                                    
         ST    RE,REQIOMAP+8                                                    
         MVC   0(81,RE),REQIO                                                   
         B     READREQX                                                         
         SPACE 1                                                                
READREQI DC    H'0'                REQUESTS ALL OUT OF STEP                     
         SPACE 1                                                                
READREQX ICM   R2,1,DMCB+8         EXIT WITH CC=EQL IF VALID FILE READ          
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
C        DS    CL80                                                             
CTKEY    DS    CL25                                                             
         DS    CL3                                                              
ORIGIN   DS    CL10                                                             
         DS    CL20                                                             
DEST     DS    CL10                                                             
         DS    CL20                                                             
*                                                                               
AIDTAB   DC    A(0)                                                             
CIDTAB   DC    F'0'                                                             
JOBS     DC    PL4'0'                                                           
JOBS1    DC    PL4'0'                                                           
*                                                                               
DMOPEN   DC    CL8'DMOPEN'                                                      
DMSYSTEM DC    CL8'CONTROL'                                                     
DMFILES  DC    C'N'                                                             
CTFILE   DC    CL7'CTFILE'                                                      
         DC    C'X'                                                             
EOFSW    DC    C'N'                                                             
*                                                                               
JD       DC    XL32'00'                                                         
QD       DC    XL32'00'                                                         
*                                                                               
REQLEN   DC    F'0'                                                             
REQIO    DS    0CL81                                                            
REQID    DS    CL04                                                             
REQKEY   DS    CL30                                                             
         DS    CL46                                                             
REQTYPE  DS    CL1                                                              
REQ2     DS    CL81                                                             
*                                                                               
REQIOADR DC    A(0)                A(NEXT CARD)                                 
REQIOMAP DC    XL44'00'            A(CARDS PRESENT FOR THIS REQUEST)            
         DC    XL4'FFFFFFFF'       END OF LIST                                  
*                                                                               
REQIOH   DC    CL81' '             SET OF CARDS THAT COMPISE A REQUEST          
REQIO1   DC    CL81' '                                                          
REQIO2   DC    CL81' '                                                          
REQIO3   DC    CL81' '                                                          
REQIO4   DC    CL81' '                                                          
REQIO5   DC    CL81' '                                                          
REQIO6   DC    CL81' '                                                          
REQIO7   DC    CL81' '                                                          
REQIO8   DC    CL81' '                                                          
REQIO9   DC    CL81' '                                                          
REQIOA   DC    CL81' '                                                          
         EJECT                                                                  
*TAPE AND WORKER FILES DTFS/DCBS                                                
*                                                                               
EODREQS  DMWK  BUFFER=WKRQBUF                                                   
         SPACE 1                                                                
EODJOBS  DMWK  BUFFER=WKRJBUF                                                   
         SPACE 1                                                                
         DC    0D'0'                                                            
         DC    CL8'I/O AREA'                                                    
IO       DS    2000C                                                            
         SPACE 1                                                                
         DC    0D'0'                                                            
         DC    CL8'JOBIO   '                                                    
JOBIO    DS    1000C                                                            
         SPACE 1                                                                
         DC    0D'0'                                                            
         DC    CL8'JOBIO   '                                                    
IDTAB    DC    2000XL22'00'                                                     
IDTABX   DC    22X'FF'                                                          
         SPACE 1                                                                
         DC    0D'0'                                                            
         DC    CL8'WKRQBUF '                                                    
WKRQBUF  DS    50000C                                                           
         SPACE 1                                                                
         DC    0D'0'                                                            
         DC    CL8'WKRJBUF '                                                    
WKRJBUF  DS    50000C                                                           
         SPACE 1                                                                
         DC    CL8'WRKWRK  '                                                    
WRKWRK   DC    5000D'0'                                                         
*                                                                               
UTL      CSECT                                                                  
         DC    F'0',X'0A000000'                                                 
         EJECT                                                                  
*EDREQD                                                                         
       ++INCLUDE EDREQD                                                         
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 1                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002EDJOBSCAN 05/01/02'                                      
         END                                                                    
