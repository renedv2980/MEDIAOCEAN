*          DATA SET CTREP9702  AT LEVEL 014 AS OF 05/01/02                      
*PHASE CT9702A                                                                  
*INCLUDE SCANNER                                                                
*INCLUDE SORTER                                                                 
         TITLE 'CT9702 - DISK ALLOCATION REPORT'                                
CT9702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CT9702                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING CT9702+4096,RC                                                   
         L     RA,0(R1)                                                         
         USING CTWORKD,RA                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    DA10                                                             
         CLI   MODE,REQFRST                                                     
         BE    DA20                                                             
         CLI   MODE,RUNLAST                                                     
         BE    DA50                                                             
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
DA10     LA    R1,VSCANNER                                                      
         LA    R0,2                                                             
*                                                                               
DA12     LA    RE,RELO                                                          
         S     RE,RELO                                                          
         A     RE,0(R1)                                                         
         ST    RE,0(R1)                                                         
*                                                                               
         LA    R1,4(R1)                                                         
         BCT   R0,DA12                                                          
*                                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD,0                                  
* ADD AN E-O-F REC TO SORT                                                      
         L     R8,ADIO                                                          
         MVI   0(R8),X'FF'                                                      
         GOTO1 VSORTER,DMCB,=C'PUT',(R8)                                        
         B     EXIT                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,18,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=38'                                    
*                                                                               
RELO     DC    A(*)                                                             
         EJECT                                                                  
* REQFRST - READ JCL BOOK AND BUILD SORT RECORDS                                
*                                                                               
DA20     DS    0H                                                               
         MVC   P(10),QSRTAREA                                                   
         GOTO1 REPORT                                                           
         L     R8,ADIO                                                          
         USING SRRECD,R8                                                        
         MVI   ISW,0                                                            
         XC    BUFF(25),BUFF                                                    
         MVI   BUFF,C'L'                                                        
         MVC   BUFF+14(10),QSRTAREA                                             
         BAS   RE,GETCARD                                                       
         MVI   ERRCD,NOBOOK                                                     
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         B     DA24                                                             
*                                                                               
DA22     BAS   RE,GETCARD                                                       
         TM    8(R1),X'80'         TEST EOF                                     
         BZ    DA24                                                             
         CLI   ISW,0               TEST IN 'INCLUDE' MODE                       
         BE    EXIT                                                             
         MVI   ISW,0               YES-RESET                                    
         B     DA22                 AND CONTINUE                                
*                                                                               
DA24     CLI   CARD,C'I'                                                        
         BNE   DA26                                                             
         CLI   ISW,C'I'            TEST 'I' MODE ALREADY                        
         BE    DA22                YES - IGNORE                                 
         MVI   ISW,C'I'                                                         
* READ INCLUDED BOOK                                                            
         XC    BUFF2(25),BUFF2                                                  
         MVI   BUFF2,C'L'                                                       
         MVC   BUFF2+14(10),CARD+2                                              
         BAS   RE,GETCARD                                                       
         MVI   ERRCD,NOINCL                                                     
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
         B     DA24                                                             
*                                                                               
DA26     CLI   CARD,C'D'                                                        
         BNE   DA30                                                             
* SAVE DLBL INFORMATION                                                         
         GOTO1 VSCANNER,DMCB,(C'C',CARD+2),(1,SCANBLK)                          
DA26X    MVC   SRFILE,SCANBLK+12                                                
         LH    RE,SEQ                                                           
         LA    RE,1(RE)                                                         
         STH   RE,SEQ                                                           
         STH   RE,SRSEQ                                                         
         XC    XTNT,XTNT                                                        
         B     DA22                                                             
         EJECT                                                                  
DA30     CLI   CARD,C'X'                                                        
         BNE   DA40                                                             
* PROCESS EXTENT INFORMATION                                                    
         GOTO1 VSCANNER,DMCB,(C'C',CARD+2),(8,SCANBLK)                          
* EDIT SYSNUM                                                                   
DA30X    DS    0H                                                               
         LA    R4,SCANBLK                                                       
         MVI   ERRCD,BADOVSYS                                                   
         CLI   0(R4),5                                                          
         BL    ERROR                                                            
         MVC   SRSYS,12(R4)                                                     
* EDIT VOLID                                                                    
         LA    R4,32(R4)                                                        
         MVI   ERRCD,BADVOLID                                                   
         CLI   0(R4),6                                                          
         BNE   ERROR                                                            
         MVC   SRVOL,12(R4)                                                     
* EDIT START TRACK                                                              
         LA    R4,32(R4)                                                        
         CLI   CARD,C'X'                                                        
         BE    *+8                                                              
         LA    R4,64(R4)           SKIP TYPE/EXTENT SEQ                         
         MVI   ERRCD,BADTRK                                                     
         OC    4(4,R4),4(R4)                                                    
         BZ    ERROR                                                            
         MVC   SRSTTRK,4(R4)                                                    
* EDIT NUMBER OI TRACKS                                                         
         LA    R4,32(R4)                                                        
         MVI   ERRCD,BADNTRKS                                                   
         OC    4(4,R4),4(R4)                                                    
         BZ    ERROR                                                            
         MVC   SRNTRKS,4(R4)                                                    
*                                                                               
         LH    RE,XTNT                                                          
         LA    RE,1(RE)                                                         
         STH   RE,XTNT                                                          
         STH   RE,SRXTNT                                                        
*                                                                               
         MVI   SRTYPE,C'1'                                                      
         GOTO1 VSORTER,DMCB,=C'PUT',(R8)                                        
*                                                                               
         CLI   RCTRACE,C'Y'                                                     
         BNE   DA32                                                             
         MVC   P+30(36),0(R8)                                                   
         GOTO1 REPORT                                                           
         EJECT                                                                  
DA32     LA    R9,100(R8)                                                       
         USING S3RECD,R9                                                        
         XC    0(100,R9),0(R9)                                                  
         MVI   S3TYPE,C'3'                                                      
         MVC   S3FILE,SRFILE                                                    
         MVC   S3XTNT,SRXTNT                                                    
         MVC   S3SEQ,SRSEQ                                                      
         MVC   S3VOL,SRVOL                                                      
         MVC   S3STTRK,SRSTTRK                                                  
         MVC   S3NTRKS,SRNTRKS                                                  
         MVC   S3SYS,SRSYS                                                      
         GOTO1 VSORTER,DMCB,=C'PUT',(R9)                                        
         DROP  R9                                                               
         SPACE 2                                                                
         USING S6RECD,R9                                                        
         XC    0(100,R9),0(R9)                                                  
         MVI   S6TYPE,C'6'                                                      
         MVC   S6SYS,SRSYS                                                      
         MVC   S6VOL,SRVOL                                                      
         GOTO1 VSORTER,DMCB,=C'PUT',(R9)                                        
         DROP  R9                                                               
         B     DA22                                                             
         EJECT                                                                  
DA40     CLC   =C'// DLBL',CARD                                                 
         BNE   DA42                                                             
         GOTO1 VSCANNER,DMCB,(C'C',CARD+8),(1,SCANBLK)                          
         B     DA26X                                                            
*                                                                               
DA42     CLC   =C'$/ EXTENT',CARD                                               
         BNE   *+8                                                              
         MVI   CARD,C'/'                                                        
*                                                                               
         CLC   =C'// EXTENT',CARD                                               
         BNE   DA44                                                             
         GOTO1 VSCANNER,DMCB,(C'C',CARD+10),(8,SCANBLK)                         
         B     DA30X                                                            
*                                                                               
DA44     B     DA22                SKIP OTHER CARDS                             
         SPACE 2                                                                
GETCARD  LR    R0,RE                                                            
         LA    RE,BUFF                                                          
         CLI   ISW,C'I'                                                         
         BNE   *+8                                                              
         LA    RE,BUFF2                                                         
         ST    RE,DMCB                                                          
         GOTO1 GETBOOK,DMCB,,CARD,DATAMGR                                       
         MVC   CARD+72(10),SPACES                                               
         LR    RE,R0                                                            
         CLC   CARD(80),SPACES                                                  
         BE    GETCARD             IGNORE BLANK CARDS                           
         BR    RE                                                               
         EJECT                                                                  
NOBOOK   EQU   1                                                                
NOXTNT   EQU   2                                                                
BADCARD  EQU   3                                                                
BADOVSYS EQU   4                                                                
BADSYS   EQU   5                                                                
BADVOLID EQU   6                                                                
DUPSYS   EQU   7                                                                
BADTRK   EQU   8                                                                
BADNTRKS EQU   9                                                                
NOINCL   EQU   10                                                               
         SPACE 2                                                                
ERROR    MVC   P+10(80),CARD                                                    
         MVC   PSECOND,SPACES                                                   
         MVC   P+60(11),=C'** ERROR **'                                         
         LA    R4,ERRTAB                                                        
ERROR2   CLC   0(1,R4),ERRCD                                                    
         BE    ERROR4                                                           
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'FF'                                                      
         BNE   ERROR2                                                           
         DC    H'0'                                                             
ERROR4   ZIC   R5,1(R4)            GET LEN                                      
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+72(0),2(R4)                                                    
         GOTO1 REPORT                                                           
         B     DA22                PROCESS NEXT CARD                            
         EJECT                                                                  
ERRTAB   DS    0C                                                               
ERR1     DC    AL1(NOBOOK),AL1(ERR2-ERR1)                                       
         DC    C'BOOK NOT FOUND'                                                
ERR2     DC    AL1(NOXTNT),AL1(ERR3-ERR2)                                       
         DC    C'MISSING EXTENT STATEMENT'                                      
ERR3     DC    AL1(BADCARD),AL1(ERR4-ERR3)                                      
         DC    C'INVALID CARD TYPE'                                             
ERR4     DC    AL1(BADOVSYS),AL1(ERR5-ERR4)                                     
         DC    C'INVALID SYSNUM'                                                
ERR5     DC    AL1(BADSYS),AL1(ERR6-ERR5)                                       
         DC    C'TOO MANY SYS NUMBERS GENERATED'                                
ERR6     DC    AL1(BADVOLID),AL1(ERR7-ERR6)                                     
         DC    C'VOLID NOT 6 CHARACTERS'                                        
ERR7     DC    AL1(DUPSYS),AL1(ERR8-ERR7)                                       
         DC    C'SYSNUM HAS TWO DIFFERENT VOL IDS'                              
ERR8     DC    AL1(BADTRK),AL1(ERR9-ERR8)                                       
         DC    C'START TRACK NOT VALID'                                         
ERR9     DC    AL1(BADNTRKS),AL1(ERR10-ERR9)                                    
         DC    C'NUMBER OF TRACKS NOT VALID'                                    
ERR10    DC    AL1(NOINCL),AL1(ERR11-ERR10)                                     
         DC    C'INCLUDE NOT FOUND'                                             
ERR11    EQU   *                                                                
*                                                                               
ERRX     DC    X'FF'                                                            
         EJECT                                                                  
* RUNLAST - PRINT SORTED RECORDS                                                
*                                                                               
DA50     DS    0H                                                               
         USING  SRRECD,R8                                                       
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R8,15,4(R1)                                                      
         CLI   SRTYPE,C'1'                                                      
         BNE   DA52X                                                            
*                                                                               
         CLI   RCTRACE,C'Y'                                                     
         BNE   DA52                                                             
         MVC   P+30(36),0(R8)                                                   
         GOTO1 REPORT                                                           
DA52     DS    0H                                                               
         CLC   SRVOL,SAVEVOL       TEST SAME VOLID                              
         BE    DA59                                                             
* CHANGE OF VOLID - SEE IF USED ALL OF PACK                                     
DA52X    OC    SAVEVOL,SAVEVOL     TEST FIRST TIME                              
         BZ    DA54                                                             
*                                                                               
         CLC   VOLEND,NEXTTRK      LAST FILE FINISH PACK                        
         BNH   DA54                YES                                          
*                                                                               
         MVC   STARTTRK,NEXTTRK    SET OPEN START                               
         L     R0,VOLEND                                                        
         S     R0,NEXTTRK                                                       
         ST    R0,NUMTRKS                                                       
         MVI   OPENSW,C'Y'         SET NO FILE SWITCH                           
         BAS   RE,DAPRT                                                         
*                                                                               
* SET FOR NEW VOLUME                                                            
*                                                                               
DA54     DS    0H                                                               
         CLI   SRTYPE,C'1'                                                      
         BNE   DA100                                                            
         LA    RE,D3340            ASSUME 3340                                  
         CLI   SRVOL,C'D'                                                       
         BNE   DA56                                                             
         CLI   SRVOL+2,C'5'                                                     
         BNE   DA56                 OR 3350                                     
         LA    RE,D3350                                                         
*                                                                               
DA56     MVC   VOLSTART(12),0(RE)  SET ST/END/NTRKS                             
         B     DA58                                                             
*                                                                               
D3340    DC    F'12',F'8352',F'12'                                              
D3350    DC    F'30',F'16650',F'30'                                             
*                                                                               
DA58     DS    0H                                                               
         MVC   NEXTTRK,VOLSTART    SET FIRST TRACK                              
         XC    PRVSTART,PRVSTART                                                
         MVC   SAVEVOL,SRVOL                                                    
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVC   P+4(7),=C'VOLID ='                                               
         MVC   P+13(6),SRVOL                                                    
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
*                                                                               
DA59     DS    0H                                                               
         CLC   NEXTTRK,SRSTTRK                                                  
         BNL   DA60                                                             
* HAVE OPEN SPACE                                                               
         L     R0,SRSTTRK                                                       
         S     R0,NEXTTRK                                                       
         ST    R0,NUMTRKS                                                       
         MVC   STARTTRK,NEXTTRK                                                 
         MVI   OPENSW,C'Y'                                                      
         BAS   RE,DAPRT                                                         
*                                                                               
DA60     CLC   PRVSTART,SRSTTRK    TEST DUPLICATE                               
         BE    DA64                                                             
         MVC   STARTTRK,SRSTTRK                                                 
         MVC   NUMTRKS,SRNTRKS                                                  
         BAS   RE,DAPRT                                                         
*                                                                               
         MVC   PRVSTART,SRSTTRK                                                 
         L     R0,PRVSTART                                                      
         A     R0,NUMTRKS                                                       
         ST    R0,NEXTTRK                                                       
*                                                                               
DA64     B     DA50                                                             
         EJECT                                                                  
DAPRT    NTR1                                                                   
*                                                                               
         L     R0,STARTTRK                                                      
         BAS   RE,CVD                                                           
         MVC   P+4(5),WORK+5                                                    
*                                                                               
         A     R0,NUMTRKS                                                       
         BCTR  R0,0                                                             
         BAS   RE,CVD                                                           
         MVC   P+13(5),WORK+5                                                   
*                                                                               
         L     R0,NUMTRKS                                                       
         EDIT  (R0),(5,P+22)                                                    
         CLI   OPENSW,C'Y'                                                      
         BNE   *+8                                                              
         MVI   P+27,C'*'                                                        
*                                                                               
         L     R0,STARTTRK                                                      
         LA    R4,P+30                                                          
         BAS   RE,DACCHH                                                        
         MVI   P+37,C'-'                                                        
*                                                                               
         A     R0,NUMTRKS                                                       
         BCTR  R0,0                                                             
         LA    R4,P+39                                                          
         BAS   RE,DACCHH                                                        
*                                                                               
         MVC   P+49(7),=7C'*'      PRINT *** FOR OPEN SPACE                     
         CLI   OPENSW,C'Y'                                                      
         BE    DAPRTX                                                           
*                                                                               
         MVC   P+49(7),SRFILE                                                   
         LH    R0,SRXTNT                                                        
         BAS   RE,CVD                                                           
         MVC   P+60(2),WORK+8                                                   
*                                                                               
         MVC   P+66(6),SRSYS                                                    
*                                                                               
DAPRTX   DS    0H                                                               
         GOTO1 REPORT                                                           
         MVI   OPENSW,0                                                         
         B     EXIT                                                             
         SPACE 2                                                                
CVD      CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         BR    RE                                                               
         EJECT                                                                  
* CONVERT REL TRK TO CCHH                                                       
*                                                                               
DACCHH   NTR1                                                                   
*                                                                               
         SRDA  R0,32                                                            
         D     R0,VOLTRKS                                                       
         BAS   RE,CVD              CONVERT TRACK FIRST                          
         MVC   4(2,R4),WORK+8                                                   
         LR    R0,R1                                                            
         BAS   RE,CVD                                                           
         MVC   0(3,R4),WORK+7                                                   
         MVI   3(R4),C'/'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* REPORT 3 - LIST FILES ALPHABETICALLY                                          
*                                                                               
DA100    DS    0H                                                               
         USING S3RECD,R8                                                        
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEREC,SAVEREC                                                  
         B     DA106                                                            
*                                                                               
DA102    MVI   SPACING,2                                                        
         MVC   SAVEREC(38),0(R8)                                                
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R8,15,4(R1)                                                      
         CLI   0(R8),C'3'                                                       
         BE    DA104                                                            
         GOTO1 REPORT                                                           
         B     DA200                                                            
DA104    LA    RE,SAVEREC+S3FILE-S3RECD                                         
         CLC   S3FILE(10),0(RE)    SAVE FILE/EXTENT                             
         BE    DA102                                                            
         CLC   S3FILE,0(RE)        SAME FILE                                    
         BE    DA108                                                            
         GOTO1 REPORT                                                           
DA106    MVC   P(8),S3FILE                                                      
         LA    R4,P+10                                                          
*                                                                               
DA108    MVC   0(6,R4),S3SYS                                                    
         MVC   132(8,R4),S3VOL                                                  
         LA    R4,10(R4)                                                        
         B     DA102                                                            
         EJECT                                                                  
* REPORT 6 - LIST VOLIDS BY SYSNUM                                              
*                                                                               
DA200    DS    0H                                                               
         USING S6RECD,R8                                                        
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEREC,SAVEREC                                                  
         B     DA206                                                            
*                                                                               
DA202    MVI   SPACING,2                                                        
         MVC   SAVEREC(38),0(R8)                                                
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R8,15,4(R1)                                                      
         CLI   0(R8),C'6'                                                       
         BE    DA204                                                            
         GOTO1 REPORT                                                           
         B     DA300                                                            
*                                                                               
DA204    DS    0H                                                               
         LA    RE,SAVEREC+S6SYS-S6RECD                                          
         CLC   S6SYS(14),0(RE)     SAME SYS/VOL                                 
         BE    DA202               YES - SKIP                                   
         CLC   S6SYS,0(RE)         SAME SYSNUM                                  
         BE    DA208                                                            
         GOTO1 REPORT              NO - PRINT                                   
DA206    MVC   P(6),S6SYS                                                       
         LA    R4,P+10                                                          
*                                                                               
DA208    MVC   0(8,R4),S6VOL                                                    
         LA    R4,10(R4)                                                        
         B     DA202                                                            
         SPACE 2                                                                
DA300    B     EXIT                                                             
         EJECT                                                                  
VSCANNER DC    V(SCANNER)                                                       
VSORTER  DC    V(SORTER)                                                        
VOLSTART DS    F                                                                
VOLEND   DS    F                                                                
VOLTRKS  DS    F                                                                
STARTTRK DC    F'0'                                                             
NUMTRKS  DC    F'0'                                                             
PRVSTART DS    F                                                                
SAVEVOL  DC    XL8'00'                                                          
NEXTTRK  DC    F'0'                                                             
OPENSW   DS    C                                                                
ISW      DS    C                                                                
ERRCD    DS    C                                                                
XTNT     DC    H'0'                                                             
SEQ      DC    H'0'                                                             
CARD     DC    CL160' '                                                         
SCANBLK  DS    8CL32                                                            
SAVEREC  DS    CL40                                                             
         DS    0D                                                               
BUFF     DS    1112C                                                            
BUFF2    DS    1112C                                                            
         EJECT                                                                  
SRRECD   DSECT                                                                  
*                                                                               
SRTYPE   DS    CL1                                                              
         DS    CL3                 SPARE                                        
SRVOL    DS    CL8                                                              
SRSTTRK  DS    XL4                                                              
SRSEQ    DS    XL2                                                              
SRNTRKS  DS    XL4                                                              
SRFILE   DS    CL8                                                              
SRXTNT   DS    XL2                                                              
SRSYS    DS    CL6                                                              
         SPACE 2                                                                
S3RECD   DSECT                                                                  
*                                                                               
S3TYPE   DS    CL1                                                              
         DS    CL3                 SPARE                                        
S3FILE   DS    CL8                                                              
S3XTNT   DS    XL2                                                              
S3SEQ    DS    XL2                                                              
S3VOL    DS    CL8                                                              
S3STTRK  DS    XL4                                                              
S3NTRKS  DS    XL4                                                              
S3SYS    DS    CL6                                                              
         SPACE 2                                                                
S6RECD   DSECT                                                                  
*                                                                               
S6TYPE   DS    CL1                                                              
         DS    CL3                 SPARE                                        
S6SYS    DS    CL6                                                              
S6VOL    DS    CL8                                                              
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE CTREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE CTREPWORKD                                                     
 END                                                                            
