*          DATA SET DDSTATSHT  AT LEVEL 005 AS OF 07/24/08                      
*PHASE STATSHTA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE SORTER                                                                 
*INCLUDE REGSAVE                                                                
         TITLE 'STATSHT- PRINT SHUTTLE ADRFILE STATISTICS, LINE ORDER'          
STATSHT  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**STATSH,=V(REGSAVE),R9                                        
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         MVC   TITLE(32),REPORTID                                               
*                                                                               
INIT     GOTO1 =V(CARDS),DMCB,C,=C'RE00'                                        
         CLC   C(2),=C'/*'                                                      
         BE    INITX                                                            
         MVC   P(80),C                                                          
         GOTO1 =V(PRINTER)                                                      
*                                                                               
INIT1    CLC   C(5),=C'LUID='      LUID=LUIDNAME                                
         BNE   INIT1X                                                           
         L     RF,ANXTLUID         GET ADDR OF NEXT ENTRY                       
         CLI   0(RF),X'FF'                                                      
         BE    INIT1X              TABLE IS FULL                                
         MVC   0(8,RF),C+5                                                      
         LA    RF,8(RF)                                                         
         ST    RF,ANXTLUID                                                      
         OI    INOPTS,X'80'        SET LUID LIST PRESENT                        
         CLI   C+12,C'P'                                                        
         BNE   *+8                                                              
         OI    INOPTS,X'40'        SET PRINTER LUID PRESENT                     
         CLI   C+12,C'S'                                                        
         BNE   *+8                                                              
         OI    INOPTS,X'20'        SET SHUTTLE LUID PRESENT                     
INIT1X   EQU   *                                                                
*                                                                               
INIT2    CLC   C(7),=C'DEVICE='    DEVICE=PS WHERE P=PRTR AND S=SHUT            
         BNE   INIT2X                                                           
         MVC   DEVICE(2),C+7                                                    
         OI    INOPTS,X'08'        SET DEVICE INPUT                             
         CLI   DEVICE,C'P'                                                      
         BE    INIT2A                                                           
         CLI   DEVICE+1,C'P'                                                    
         BNE   *+8                                                              
INIT2A   OI    INOPTS,X'04'        SET DEVICE=P PRESENT                         
         CLI   DEVICE,C'S'                                                      
         BE    INIT2B                                                           
         CLI   DEVICE+1,C'S'                                                    
         BNE   *+8                                                              
INIT2B   OI    INOPTS,X'02'        SET DEVICE=S PRESENT                         
INIT2X   EQU   *                                                                
*                                                                               
INIT3    CLC   C(6),=C'TRACE='     TRACE=ALL/BLOCKS/RECORDS/$PQ                 
         BNE   INIT3X                                                           
         MVC   TRACE,C+6                                                        
INIT3X   EQU   *                                                                
*                                                                               
         B     INIT                                                             
*                                                                               
INITX    MVC   MID1(79),DISPHDR    SET SHUTTLE TRACE MID LINES                  
         MVC   MID2(79),DISPUND                                                 
         TM    INOPTS,X'44'        TEST IF PRINTERS SPECIFIED                   
         BZ    INITX1                                                           
         MVC   MID1(79),DISPPHDR   SET PRINTER TRACE MID LINES                  
         MVC   MID2(79),DISPPUND                                                
*                                                                               
INITX1   TM    INOPTS,X'88'        TEST ANY OPTIONS INPUT                       
         BZ    *+8                                                              
         OI    INOPTS,X'02'        DEFAULT IS DEVICE=S                          
*                                                                               
INITX2   OPEN  (ADRIN,INPUT)                                                    
         GOTO1 =V(SORTER),P1,SORTCARD,RECCARD                                   
         EJECT                                                                  
* SORT ON LUID AND TRANSACTION TIME                                             
*                                                                               
INPUT    CLC   NXTREC,BLKFAC       TEST IF END OF BLOCK                         
         BL    INPUT1                                                           
INPUT0   XC    NXTREC,NXTREC       RESET TO START OF BLOCK                      
         L     R3,=A(BUFF)                                                      
         GET   ADRIN,BUFF          READ NEXT ADRFILE RECORD                     
*                                                                               
INPUT0A  CLI   TRACE,C'B'          TRACING BLOCKS                               
         BE    *+12                                                             
         CLI   TRACE,C'A'                                                       
         BNE   INPUT0B                                                          
         MVI   P,C'B'              PRINT FIRST RECORD OF THIS BLOCK             
         MVI   P+1,C'='                                                         
         MVC   P+2(64),0(R3)                                                    
         GOTO1 =V(PRINTER)                                                      
INPUT0B  EQU   *                                                                
*                                                                               
         CLI   0(R3),C'*'                                                       
         BE    INPUT0              IGNORE BLOCKS WITH SPECIAL DATA              
         CLC   0(4,R3),=C'$PQS'                                                 
         BNE   INPUT1                                                           
         TM    39(R3),X'80'        THIS IS SET ON IN 80-CHR CODE                
         BZ    INPUT1                                                           
         MVC   RECLEN,=H'80'                                                    
         MVC   BLKFAC,=H'80'                                                    
*                                                                               
INPUT1   LH    R1,NXTREC           MOVE NEXT RECORD OUT OF BLOCK                
         MH    R1,RECLEN                                                        
         L     R3,=A(BUFF)                                                      
         AR    R3,R1                                                            
         MVC   REC,0(R3)                                                        
*                                                                               
INPUT1A  CLI   TRACE,C'R'          TRACING RECORDS                              
         BE    *+12                                                             
         CLI   TRACE,C'A'                                                       
         BNE   INPUT1B                                                          
         MVI   P,C'R'              PRINT THIS RECORD                            
         MVI   P+1,C'='                                                         
         MVC   P+2(64),REC                                                      
         GOTO1 =V(PRINTER)                                                      
INPUT1B  EQU   *                                                                
*                                                                               
         LH    R1,NXTREC           BUMP NEXT RECORD NUMBER                      
         AHI   R1,1                                                             
         STH   R1,NXTREC                                                        
*                                                                               
         LA    R3,REC              R3=A(ADRFILE LOGICAL RECORD)                 
         USING LOGRECD,R3                                                       
         CLC   LOGID(3),=C'$PQ'    ONLY WANT PRNT/SHUT RECORDS                  
         BNE   INPUT                                                            
         OI    LOGTIME+3,X'0F'     CLEAN SIGN OF PACKED TIME                    
*                                                                               
INPUT1C  CLI   TRACE,C'$'          TRACING $PQ ONLY                             
         BE    *+12                                                             
         CLI   TRACE,C'A'                                                       
         BNE   INPUT1D                                                          
         MVI   P,C'$'              PRINT THIS RECORD                            
         MVI   P+1,C'='                                                         
         MVC   P+2(64),REC                                                      
         GOTO1 =V(PRINTER)                                                      
INPUT1D  EQU   *                                                                
*                                                                               
INPUT2   L     RE,=A(LUIDTAB)      TEST IF ANY LUID=CARDS INPUT                 
         L     RF,ANXTLUID                                                      
         CR    RE,RF                                                            
         BE    INPUT2X                                                          
INPUT2A  CLC   0(8,RE),LOGLUID     SEARCH TABLE OF LUIDS                        
         BE    INPUT4                                                           
         LA    RE,8(RE)                                                         
         CR    RE,RF                                                            
         BL    INPUT2A                                                          
         B     INPUT               BYPASS IF NOT IN LUID TABLE                  
INPUT2X  EQU   *                                                                
*                                                                               
INPUT3   CLI   LOGID+3,C'Q'        TEST IF PRINTER TRACE RECORD                 
         BNE   INPUT3A                                                          
         TM    INOPTS,X'44'        TEST IF WANT PRINTER RECORDS                 
         BNZ   INPUT3X                                                          
         B     INPUT                                                            
INPUT3A  CLI   LOGID,C'U'          TEST IF TIMER AUTO START RECORD              
         BNE   INPUT3B                                                          
         TM    INOPTS,X'44'        TEST IF WANT PRINTER RECORDS                 
         BNZ   INPUT3X                                                          
INPUT3B  TM    INOPTS,X'22'        TEST IF WANT SHUTTLE RECORDS                 
         BZ    INPUT                                                            
INPUT3X  EQU   *                                                                
*                                                                               
INPUT4   EQU   *                                                                
*                                                                               
INPUT9   GOTO1 =V(SORTER),P1,=C'PUT',REC                                        
         B     INPUT                                                            
*                                                                               
SORTIN   GOTO1 =V(SORTER),P1,=C'GET'                                            
         ICM   R3,15,P2                                                         
         BZ    ENDIN1                                                           
         USING LOGRECD,R3                                                       
         OC    LOGREC(4),LOGREC    IGNORE NULL RECORDS                          
         BZ    SORTIN                                                           
         EJECT                                                                  
* ROUTINES TO LIST INPUT                                                        
*                                                                               
DREC     CLC   LASTLUID,LOGLUID    NEW PAGE FOR A NEW LUID                      
         BE    DREC0                                                            
         MVC   LASTLUID,LOGLUID                                                 
         ZAP   LINE,=P'75'                                                      
*                                                                               
DREC0    MVC   LINLUID,LOGLUID     PRINT LUID                                   
*                                                                               
         MVC   WRK(10),=X'402020204B20204B2020'                                 
         ED    WRK(10),LOGTIME                                                  
         OC    WRK+2(8),=C'00.00.00'                                            
         MVC   LINTIME,WRK+2                                                    
*                                                                               
DREC1    MVC   DUB(1),LOGID+3      PRINT LOG DATA TYPE                          
         LA    RE,TYPETBL                                                       
DREC1A   CLC   DUB(1),0(RE)        SEARCH TYPE TABLE                            
         BE    DREC2                                                            
         LA    RE,L'TYPETBL(RE)                                                 
         CLI   0(RE),X'FF'                                                      
         BNE   DREC1A                                                           
         CLI   DUB,C'M'            SPECIAL MESSAGE TYPE?                        
         BNE   DREC2                                                            
         MVI   LINTYPE,C'M'                                                     
         MVC   LINTYPE+2(16),LOGTEXT                                            
         B     DRECX                                                            
*                                                                               
DREC2    MVC   LINTYPE(1),DUB                                                   
         MVC   LINTYPE+2(22),2(RE)                                              
         TM    1(RE),X'40'         TEST IF BUFFER NUMBER DEFINED                
         BZ    DREC3                                                            
         MVI   LINTYPE+18,C'#'                                                  
         MVC   LINTYPE+19(4),LOGNUM                                             
*                                                                               
DREC3    TM    1(RE),X'80'         TEST IF REPORT NAME DEFINED                  
         BO    DREC5                                                            
         MVC   LINUSER,NOTREL                                                   
         MVC   LINREPT,NOTREL                                                   
         B     DRECX                                                            
*                                                                               
DREC5    CLI   LOGREPU,C'A'        DISPLAY REPORT NAME                          
         BL    DREC6                                                            
         CLI   LOGREPU,C'Z'                                                     
         BH    DREC6                                                            
         MVC   LINUSER,LOGREPU     PRINT REPORT USER ID                         
         MVC   LINREPT(3),LOGREPI                                               
         MVI   LINREPT+3,C','                                                   
         MVC   LINREPT+4(5),LOGRENO                                             
*                                                                               
DREC6    CLI   LOGID+3,C'Q'        TRACE OF PRINTER ACTIVITY                    
         BNE   DRECX                                                            
         MVC   MID1(79),DISPPHDR   SET PRINTER TRACE MID LINES                  
         MVC   MID2(79),DISPPUND                                                
         MVC   LINTYPE+2(15),SPACES                                             
         MVI   LINTYPE+18,C' '                                                  
         GOTO1 VHEXOUT,DMCB,LOGFLAGS,LINTYPE+2,1,=C'MIX'                        
         GOTO1 VHEXOUT,DMCB,LOGSST,LINTYPE+5,2,=C'MIX'                          
DREC6A   CLC   LOGSST(2),LOGXST    TEST STATUS/STATUS1 CHANGE                   
         BE    DREC6B                                                           
         GOTO1 VHEXOUT,DMCB,LOGXST,LINTYPE+10,2,=C'MIX'                         
DREC6B   MVI   LINTYPE+15,C'.'     NORMAL STATUS                                
         TM    LOGXST,X'80'                                                     
         BZ    *+8                                                              
         MVI   LINTYPE+15,C'E'     ERROR STOPPED                                
         TM    LOGXST,X'08'                                                     
         BZ    *+8                                                              
         MVI   LINTYPE+15,C'A'     ACTIVE                                       
DREC6E   TM    LOGSNEX,X'01'       TEST EOR PENDING AT START                    
         BZ    DREC7                                                            
         TM    LOGXST,X'08'        TEST INACTIVE AT END                         
         BO    DREC7                                                            
         MVC   LINUSER,NOTREL      CLEAR REPORT STUFF                           
         MVC   LINREPT,NOTREL                                                   
         MVC   LINTYPE+19(4),NOTREL                                             
         B     DRECX                                                            
*                                                                               
DREC7    MVC   LINMISC,SPACES      MISCELLANEOUS STUFF                          
         LA    R7,LINMISC                                                       
*                                                                               
DREC7A   SR    RF,RF               LINES PRINTED                                
         ICM   RF,7,LOGXLNS                                                     
         BZ    DREC7AX                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R7),DUB                                                      
DREC7AX  LA    R7,5(R7)                                                         
*                                                                               
DREC7B   SR    RF,RF               PAGES PRINTED                                
         ICM   RF,3,LOGXPGS                                                     
         BZ    DREC7BX                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R7),DUB                                                      
DREC7BX  LA    R7,4(R7)                                                         
*                                                                               
DREC7C   GOTO1 VHEXOUT,DMCB,LOGXNEX,0(R7),1,=C'MIX'                             
         LA    R7,3(R7)                                                         
*                                                                               
DRECX    GOTO1 =V(PRINTER)                                                      
         B     SORTIN                                                           
         EJECT                                                                  
ENDIN    CLOSE (ADRIN)                                                          
         XC    LASTLUID,LASTLUID                                                
         B     SORTIN                                                           
*                                                                               
ENDIN1   GOTO1 =V(SORTER),P1,=C'END'                                            
*                                                                               
         XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DUB      DS    D                                                                
LASTLUID DC    CL8' '                                                           
REC      DC    XL64'00'                                                         
WORK     DS    CL24                                                             
WRK      DS    CL80                                                             
C        DS    CL80                                                             
DMCB     DS    6A                                                               
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
VHEXOUT  DC    V(HEXOUT)                                                        
NOTREL   DC    16C'.'                                                           
DEVICE   DC    CL2'S '             SHUTTLES BY DEFAULT                          
INOPTS   DC    X'00'                                                            
TRACE    DC    C'N'                                                             
RECLEN   DC    H'64'                                                            
BLKFAC   DC    H'100'                                                           
NXTREC   DC    H'100'              SET TO FORCE I/O FIRST TIME                  
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(5,8,A,13,4,A),FORMAT=BI,WORK=1'                
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=64'                                    
*                                                                               
REPORTID DC    CL32'Printer/Shuttle Activity Report'                            
DISPHDR  DC    CL79'Luid     hh.mm.ss T Log data message       Userid RX        
               eport-id'                                                        
DISPUND  DC    CL79'-------- -------- - ---------------------  ------ -X        
               --------'                                                        
DISPPHDR DC    CL79'Prt Luid hh.mm.ss T Fg Ssta Esta St  Buff  Userid RX        
               eport-id Lnes Pgs Px        '                                    
DISPPUND DC    CL79'-------- -------- - -- ---- ---- --  ----  ------ -X        
               -------- ---- --- --        '                                    
         EJECT                                                                  
TYPETBL  DS    0CL24               TABLE OF VALID LOG RECORD TYPES              
*                                                                               
         DC    C'A',X'00',CL22'start of session'                                
         DC    C'B',X'00',CL22'restarted session'                               
         DC    C'C',X'00',CL22'normal end of report'                            
         DC    C'D',X'00',CL22'abnormal end of report'                          
         DC    C'E',X'00',CL22'end of session'                                  
         DC    C'F',X'C0',CL22'data buff first'                                 
         DC    C'I',X'C0',CL22'data buff inter'                                 
         DC    C'L',X'C0',CL22'data buff last'                                  
*NOP     DC    C'M',X'01',CL22' '                                               
         DC    C'Q',X'C0',CL22'printer trace'                                   
         DC    C'S',X'00',CL22'timer pop first'                                 
         DC    C'T',X'00',CL22'timer pop'                                       
         DC    C'U',X'00',CL22'timer auto start'                                
*                                                                               
TYPETBLX DC    XL02'FF00',CL22'unknown'                                         
*                                                                               
ADRIN    DCB   DDNAME=ADRIN,DSORG=PS,MACRF=(GM),EODAD=ENDIN,           X        
               RECFM=F,BLKSIZE=6400                                             
*                                                                               
ANXTLUID DC    A(LUIDTAB)                                                       
LUIDTAB  DC    20CL8' '                                                         
         DC    8X'FF'                                                           
*                                                                               
         DS    0D                                                               
         DC    C'*ADRBUF*'                                                      
BUFF     DC    8000X'00'                                                        
         EJECT                                                                  
LOGRECD  DSECT                                                                  
LOGREC   DS    0CL64                                                            
LOGID    DS    CL4                                                              
LOGLUID  DS    CL8                                                              
LOGTIME  DS    PL4                                                              
LOGTEXT  DS    0CL16                                                            
LOGNUM   DS    CL4                                                              
         DS    CL4                                                              
LOGREPU  DS    CL6                                                              
LOGREPI  DS    CL3                                                              
LOGRENO  DS    CL5                                                              
LOGMISC  DS    0CL28               MISC INFO - DEPENDS ON REC TYPE              
LOGFLAGS DS    XL1                                                              
LOGSST   DS    XL1                                                              
LOGSST1  DS    XL1                                                              
LOGSST2  DS    XL1                                                              
LOGSSMOD DS    XL1                                                              
LOGSQNE  DS    XL1                                                              
LOGSST3  DS    XL1                                                              
LOGSNEX  DS    XL1                                                              
LOGXST   DS    XL1                                                              
LOGXST1  DS    XL1                                                              
LOGXST2  DS    XL1                                                              
LOGXSMOD DS    XL1                                                              
LOGXQNE  DS    XL1                                                              
LOGXST3  DS    XL1                                                              
LOGXNEX  DS    XL1                                                              
LOGXLNS  DS    XL3                                                              
LOGXPGS  DS    XL2                                                              
         DS    XL8                                                              
         EJECT                                                                  
* FAADRREC                                                                      
       ++INCLUDE FAADRREC                                                       
         EJECT                                                                  
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
*                                                                               
         ORG   P                                                                
*                                                                               
LINDATA  DS    0CL79                                                            
LINLUID  DS    CL8                                                              
         DS    CL1                                                              
LINTIME  DS    CL8                                                              
         DS    CL1                                                              
LINTYPE  DS    CL24                                                             
         DS    CL1                                                              
LINUSER  DS    CL6                                                              
         DS    CL1                                                              
LINREPT  DS    CL9                                                              
         DS    CL1                                                              
LINMISC  DS    CL19                                                             
         EJECT                                                                  
         PRINT NOGEN                                                            
* FAPGMLST                                                                      
       ++INCLUDE FAPGMLST                                                       
         SPACE 2                                                                
* FASELIST                                                                      
       ++INCLUDE FASELIST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005DDSTATSHT 07/24/08'                                      
         END                                                                    
