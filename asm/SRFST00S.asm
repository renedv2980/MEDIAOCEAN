*          DATA SET SRFST00S   AT LEVEL 009 AS OF 05/01/02                      
*PHASE T14200A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE TIMEDT                                                                 
         TITLE '$FAST - FACPAK ADRBUFF STATISTICS'                              
         PRINT NOGEN                                                            
FAST     CSECT                                                                  
         NMOD1 WRKX-WRKD,*$FAST**,CLEAR=YES                                     
         USING WRKD,RC             RC=A(W/S)                                    
         USING SRPARMD,R1                                                       
         L     RA,SRPARM6                                                       
         USING SRFSTFFD,RA         RA=A(TWA)                                    
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R1,SRPARM4                                                       
         USING COMFACSD,R1                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VHEXIN,CHEXIN                                                    
         DROP  R1                                                               
         LH    R1,=Y(ADRIO-WRKD)                                                
         LA    R1,WRKD(R1)                                                      
         ST    R1,AIO                                                           
*                                                                               
         L     RE,VADRBUFF         ADRFILE DATA STORED AHEAD OF ADRBUFF         
         SH    RE,=H'16'                                                        
         LH    R0,0(RE)            GET RECORDS PER BUFFER                       
         ST    R0,BLKFAC                                                        
         MVC   RECLEN,2(RE)        GET RECORD LENGTH                            
         MH    R0,2(RE)                                                         
         ST    R0,BLKLEN           SET BLOCK LENGTH                             
*                                                                               
         GOTO1 VDADDS,P1,VDARPT,0,(R0),VADRFILE                                 
         LH    R0,P3+2                                                          
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,HIREC            SET HIREC ON TRACK                           
         EJECT                                                                  
*                                  VALIDATE BASIS (P1)                          
         LA    R2,SRVP1H                                                        
         CLI   5(R2),0             NOT INPUT                                    
         BNE   *+18                                                             
         MVC   8(3,R2),=C'SYS'     DEFAULT IS 'SYS'                             
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
         CLI   5(R2),5                                                          
         BH    ERROR2                                                           
         MVI   SYSFILT,1           SET LOW SYSTEM FILTER                        
         MVI   TSKFILT,1           SET LOW TASK FILTER                          
         MVI   BASIS,C'S'                                                       
         CLC   8(3,R2),=C'SYS'                                                  
         BNE   FAS0                                                             
         CLI   5(R2),3                                                          
         BE    FAS1                                                             
         GOTO1 VHEXIN,DMCB,11(R2),DUB,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERROR2                                                           
         BAS   RE,GETSYS                                                        
         BZ    ERROR2                                                           
         MVC   SYSFILT,DUB+1                                                    
         B     FAS1                                                             
*                                                                               
FAS0     MVI   BASIS,C'T'                                                       
         CLC   8(3,R2),=C'TSK'                                                  
         BNE   ERROR3              NOT VALID                                    
         CLI   5(R2),3                                                          
         BE    FAS1                                                             
         MVC   DUB(1),11(R2)                                                    
         BAS   RE,GETTSK                                                        
         BZ    ERROR2                                                           
         MVC   TSKFILT,DUB+1                                                    
*                                  VALIDATE NUMBER OF TRNS                      
FAS1     XC    NBLKS,NBLKS                                                      
         MVI   INPUT,C'B'                                                       
         LA    R2,SRVP2H                                                        
         CLI   5(R2),0                                                          
         BE    FAS2                                                             
         TM    4(R2),X'08'                                                      
         BZ    ERROR4              INPUT NOT NUMERIC                            
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CP    DUB,MAXTRNS                                                      
         BNH   *+14                                                             
         CLC   SRVP3(8),=C'OVERRIDE'                                            
         BNE   ERROR6                                                           
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,BLKFAC                                                        
         LTR   R1,R1                                                            
         BZ    FAS2                                                             
         BCTR  R1,0                                                             
         STH   R1,NBLKS            NUMBER OF ADRFILE BLOCKS TO READ             
         MVI   INPUT,C'F'                                                       
         XC    P1(24),P1                                                        
         MVC   P1,VFINDSYS         GET A(SYS1FLES)                              
         MVI   P2,1                                                             
         L     RF,VDMOD000                                                      
         LA    R1,P1                                                            
         LA    R2,DMCB                                                          
         BASR  RE,RF                                                            
         L     R1,P2                                                            
         LH    R2,2(R1)                                                         
         LA    R1,4(R1)                                                         
*                                                                               
         CLI   3(R1),ADRFEQU       FIND ADRFILE ENTRY                           
         BE    *+14                                                             
         LA    R1,8(R1)                                                         
         BCT   R2,*-12                                                          
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         L     R2,4(R1)            BUILD DADDS PLIST                            
         USING DTFPHD,R2                                                        
         XC    P1(24),P1                                                        
         MVC   P1,VRDID                                                         
         MVC   P2,AIO                                                           
         ST    R2,P4                                                            
         LA    R1,P6                                                            
         ST    R1,P5                                                            
         MVC   P6,DNEXT                                                         
*                                  VALIDATE LINE FILTER                         
FAS2     LA    R2,SRVP4H                                                        
         XC    LINFILT,LINFILT                                                  
         CLI   5(R2),0                                                          
         BE    FAS2A                                                            
         CLC   8(5,R2),=C'LINE='                                                
         BNE   ERROR2                                                           
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         SH    R1,=H'6'                                                         
         BM    ERROR1                                                           
         CH    R1,=H'7'                                                         
         BH    ERROR2                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINFILT+1(0),13(R2)                                              
         STC   R1,LINFILT                                                       
         CLI   INPUT,C'F'                                                       
         BE    FAS3                                                             
*                                                                               
FAS2A    L     R3,VADRBUFF         R3=A(ADRBUFF)                                
         USING ADRRECD,R3                                                       
         L     R4,BLKFAC           R4=NUMBER OF ENTRIES                         
         B     FAS4                                                             
*                                                                               
FAS3     L     RF,VDADDS           READ ADRFILE REC INTO ADRIO                  
         LA    R1,P1                                                            
         BASR  RE,RF                                                            
         LA    R2,SRVSREQH                                                      
         OC    P3(2),P3                                                         
         BZ    FAS3A                                                            
         TM    P3+1,X'08'          IF N/F GO GET NEXT BLOCK                     
         BO    FAS8B                                                            
         B     ERROR5              DISK ERROR ON ADRFILE                        
FAS3A    L     R3,AIO              R3=A(ADRREC)                                 
         L     R4,BLKFAC           R4=NUMBER OF ENTRIES                         
         CLI   0(R3),C'*'                                                       
         BE    FAS8A               IGNORE SPECIAL BLOCKS IN ADRFILE             
*                                                                               
FAS4     CLI   0(R3),C'A'          CHECK FOR FUNNY BUFFER ENTRY                 
         BL    FAS8                                                             
         TM    ADRSTTM,X'80'                                                    
         BO    FAS8                                                             
         TM    ADRNDTM,X'80'                                                    
         BO    FAS8                                                             
         TM    ADRCPUTM,X'80'                                                   
         BO    FAS8                                                             
         TM    ADRQTM,X'80'                                                     
         BO    FAS8                                                             
         TM    ADRIOCNT,X'80'                                                   
         BO    FAS8                                                             
         CLC   ADRSTTM,ADRNDTM                                                  
         BH    FAS8                                                             
         OC    ADRSTTM,ADRSTTM                                                  
         BZ    FAS8                                                             
*                                  CHECK LINE FILTER                            
         OC    LINFILT,LINFILT                                                  
         BZ    FAS5                                                             
         SR    R1,R1                                                            
         IC    R1,LINFILT                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ADRLINE(0),LINFILT+1                                             
         BNE   FAS8                                                             
*                                  BUILD A LINE TO POST                         
FAS5     XC    POSTLINE,POSTLINE                                                
         MVC   POSTLINE(4),=F'1'                                                
         MVC   POSTLINE+4(4),ADRCPUTM                                           
         MVC   POSTLINE+18(2),ADRIOCNT                                          
         L     R0,ADRNDTM          TOTAL I/O TIME                               
         S     R0,ADRSTTM                                                       
         S     R0,ADRCPUTM                                                      
         ST    R0,POSTLINE+20                                                   
         MVC   POSTLINE+34(2),ADRQTM                                            
         L     R0,ADRNDTM          TOTAL TRNS TIME                              
         S     R0,ADRSTTM                                                       
         ST    R0,POSTLINE+36                                                   
         MVC   POSTLINE+52(4),ADRSTTM                                           
         MVC   POSTLINE+56(4),ADRNDTM                                           
         CLI   BASIS,C'S'                                                       
         BE    FAS5A                                                            
*                                  TASK BASIS                                   
         MVC   DUB(1),ADRTASK                                                   
         BAS   RE,GETTSK                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,TSKFILT                                                       
         ZIC   R1,DUB+1                                                         
         B     FAS6                                                             
*                                  SYSTEM BASIS                                 
FAS5A    MVC   DUB(1),ADRSYSNO                                                  
         BAS   RE,GETSYS                                                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ZIC   R0,SYSFILT                                                       
         ZIC   R1,DUB+1                                                         
*                                                                               
FAS6     SR    R1,R0                                                            
         BM    FAS7                                                             
         LA    R1,1(R1)                                                         
         BAS   RE,POST                                                          
*                                                                               
FAS7     LA    R1,0                TOTALS IN LINE 0                             
         MVC   POSTLINE+52(4),ADRNDTM                                           
         BAS   RE,POST                                                          
*                                                                               
FAS8     LA    R3,L'ADRREC(R3)     BUMP TO NEXT BUFFER ENTRY                    
         BCT   R4,FAS4                                                          
*                                                                               
FAS8A    CLI   INPUT,C'B'          DONE IF LOOKING AT BUFFER ONLY               
         BE    FAS9                                                             
         OC    NBLKS,NBLKS         OR ALL BLOCKS READ                           
         BZ    FAS9                                                             
         LH    R1,NBLKS            DECREMENT BLOCK COUNT                        
         BCTR  R1,0                                                             
         STH   R1,NBLKS                                                         
*                                                                               
FAS8B    SR    R1,R1                                                            
         IC    R1,P6+2             DECREMENT RECORD                             
         SH    R1,=H'1'                                                         
         BZ    *+12                                                             
         STC   R1,P6+2                                                          
         B     FAS3                                                             
         LH    R1,P6               DECREMENT TRACK                              
         N     R1,=X'0000FFFF'                                                  
         SH    R1,=H'1'                                                         
         BZ    FAS9                DONE IF B-O-F                                
         STH   R1,P6                                                            
         MVC   P6+2(1),HIREC       SET HIGH REC                                 
         B     FAS3                                                             
         EJECT                                                                  
*                                                                               
FAS9     CLI   BASIS,C'T'          IF DISPLAYING BY TASK                        
         BNE   FAS9A               CALCULATE TOTAL QUEUE LENGTH                 
         L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         SR    R0,R0                                                            
         AH    R0,SEQLEN-SELISTD(R1)                                            
         BXLE  R1,RE,*-4                                                        
         ST    R0,ADRTOTS+76       POST QUEUE LENGTH TO TOTAL LINE              
*                                                                               
FAS9A    LH    R1,=Y(ADRTOTS2-WRKD)                                             
         LA    R1,WRKD(R1)                                                      
         MVC   0(ADRLENA,R1),ADRTOTS                                            
         LA    R2,SRVL1H           R2=A(FIRST SCREEN LINE)                      
         USING LINED,R2                                                         
         LA    R3,ADRACCS          R3=A(ACCUMS)                                 
         LA    R4,ADRMAXA+1        R4=NUMBER OF LINES (+1 FOR TOTAL)            
*                                                                               
FAS10    OC    0(ADRLENA,R3),0(R3)                                              
         BZ    FAS16                                                            
         LA    R5,12(R3)           R5=A(LINE)                                   
         L     R0,4(R5)            MEAN CPU TIME  (TOTAL CPU/TRNS)              
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         OC    0(4,R5),0(R5)                                                    
         BZ    FAS11                                                            
         D     R0,0(R5)                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,8(R5)                                                         
         L     R0,20(R5)           MEAN I/O TIME (TOTAL IO/TRNS)                
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,0(R5)                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,24(R5)                                                        
*                                                                               
         L     R0,16(R5)           IO/TRANS (TOTAL IO/TRANS)                    
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         M     R0,=F'10'                                                        
         D     R0,0(R5)                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,48(R5)                                                        
         L     R0,20(R5)           TIME/IO (TOTAL IO TIME/TOTAL IO)             
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         OC    16(4,R5),16(R5)                                                  
         BZ    FAS11                                                            
         D     R0,16(R5)                                                        
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,44(R5)                                                        
*                                                                               
FAS11    L     R0,4(R5)            PCT OF CPU TIME (CPU TIME/TOTAL CPU)         
         SRDA  R0,32                                                            
         M     R0,=F'10000'                                                     
         OC    ADRTOTS+16(4),ADRTOTS+16                                         
         BZ    FAS12                                                            
         D     R0,ADRTOTS+16                                                    
         SR    R0,R0                                                            
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         ST    R1,12(R5)                                                        
*                                                                               
FAS12    L     R0,20(R5)           PCT OF I/O TIME (I/O TIME/TOTAL I/O)         
         SRDA  R0,32                                                            
         M     R0,=F'10000'                                                     
         OC    ADRTOTS+32(4),ADRTOTS+32                                         
         BZ    FAS13                                                            
         D     R0,ADRTOTS+32                                                    
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         ST    R1,28(R5)                                                        
*                                                                               
FAS13    L     R0,36(R5)           PCT OF TRNS TIME (TTIME/TOTAL TIME)          
         SRDA  R0,32                                                            
         M     R0,=F'10000'                                                     
         OC    ADRTOTS+48(4),ADRTOTS+48                                         
         BZ    FAS14                                                            
         D     R0,ADRTOTS+48                                                    
         AH    R1,=H'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         ST    R1,40(R5)                                                        
*                                                                               
FAS14    L     R0,32(R5)           MEAN INP Q TIME (INPUT Q TIME/TRNS)          
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,0(R5)                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,32(R5)                                                        
*                                                                               
         L     R0,56(R5)           TRANSACTION RATE (TOTAL TIME/TRNS)           
         S     R0,52(R5)                                                        
         ST    R0,DUB                                                           
         L     R0,0(R5)                                                         
         SRDA  R0,32                                                            
         M     R0,=F'10000'                                                     
         D     R0,DUB                                                           
         ST    R1,60(R5)                                                        
*                                                                               
         L     R0,36(R5)           MEAN TRNS TIME (TRNS TIME/TRNS)              
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,0(R5)                                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,36(R5)                                                        
*                                                                               
         BAS   RE,FORMAT           FORMAT LINE INTO TWA                         
         CLI   LINNEXT,0           ALWAYS SHOW TOTAL LINE                       
         BE    *+8                                                              
         LA    R2,LINNEXT          BUMP TO NEXT TWA LINE                        
*                                                                               
FAS16    LA    R3,ADRLENA(R3)      BUMP TO NEXT ACCUM LINE                      
         BCT   R4,FAS10                                                         
         MVI   POSTLINE,C' '                                                    
         MVC   POSTLINE+1(L'POSTLINE-1),POSTLINE                                
         MVC   POSTLINE(22),=C'TOTAL ELAPSED TIME FOR'                          
         L     R1,ADRTOTS+12                                                    
         LA    R2,POSTLINE+23                                                   
         EDIT  (R1),(6,0(R2))                                                   
         MVC   POSTLINE+30(7),=C'TRNS. ='                                       
         L     R1,ADRTOTS+68                                                    
         S     R1,ADRTOTS+64                                                    
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         ST    R1,DMCB+8                                                        
         GOTO1 =V(TIMEDT),DMCB,DMCB+8,WORK,RR=RB                                
         MVC   POSTLINE+45(8),WORK+2                                            
         MVC   POSTLINE+53(7),=C'. SIN ='                                       
         L     R1,VSSB                                                          
         USING SSBD,R1                                                          
         L     R1,SSBSIN                                                        
         DROP  R1                                                               
         LA    R2,POSTLINE+69                                                   
         EDIT  (R1),(6,0(R2))                                                   
         MVI   POSTLINE+75,C'.'                                                 
         GOTO1 =V(SQUASHER),DMCB,POSTLINE,76,RR=RB                              
         MVC   SRVMSG,POSTLINE                                                  
         LA    R2,SRVP2H                                                        
         B     MEXIT                                                            
         EJECT                                                                  
* POST LINE OF ACCUMS TO BUFFER AND INITIALIZE BUFFER HEADER                    
* R4=BUFFER LINE NUMBER FOR POSTING                                             
*                                                                               
POST     NTR1  ,                                                                
         LA    RE,ADRLENA          RE=L'LINE                                    
         SR    R0,R0                                                            
         STC   R1,WORK             SAVE LINE NUMBER                             
         MR    R0,RE                                                            
         LA    R4,ADRTOTS(R1)                                                   
         OC    0(10,R4),0(R4)      IS LINE HEADER INITIALIZED                   
         BNZ   POST4               YES - GO POST                                
         CLI   WORK,0              TOTALS LINE ?                                
         BNE   *+14                                                             
         MVC   0(10,R4),=CL10'**TOTALS**'                                       
         B     POST4                                                            
         MVI   0(R4),C' '          CLEAR HEADER                                 
         MVC   1(9,R4),0(R4)                                                    
         CLI   BASIS,C'S'                                                       
         BNE   POST2                                                            
         L     R5,ASYS                                                          
         USING SELISTD,R5                                                       
         GOTO1 VHEXOUT,DMCB,SESYS,0(R4),1,=C'TOG'                               
         MVC   3(L'SENAME,R4),SENAME                                            
         MVC   DUB(2),SEQLEN                                                    
         LH    R0,DUB                                                           
         ST    R0,76(R4)           SAVE Q LENGTH                                
         A     R0,ADRTOTS+76       UPDATE TOTAL Q LENGTH                        
         ST    R0,ADRTOTS+76                                                    
         B     POST4                                                            
         DROP  R5                                                               
*                                                                               
POST2    MVC   0(5,R4),=C'TASK#'                                                
         MVC   5(1,R4),ADRTASK                                                  
*                                                                               
POST4    LA    R4,12(R4)           POST TO BUFFER LINE                          
         LA    R1,POSTLINE                                                      
*                                  POST START & END TIME                        
         OC    52(4,R4),52(R4)                                                  
         BNZ   *+10                                                             
         MVC   52(4,R4),52(R1)                                                  
         CLC   52(4,R1),52(R4)                                                  
         BH    *+10                                                             
         MVC   52(4,R4),52(R1)                                                  
         OC    56(4,R4),56(R4)                                                  
         BNZ   *+10                                                             
         MVC   56(4,R4),56(R1)                                                  
         CLC   56(4,R1),56(R4)                                                  
         BL    *+10                                                             
         MVC   56(4,R4),56(R1)                                                  
         LA    R0,10                                                            
*                                                                               
POST6    L     RE,0(R1)                                                         
         A     RE,0(R4)                                                         
         ST    RE,0(R4)                                                         
         LA    R1,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,POST6                                                         
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT A LINE OF ACCUMS INTO TWA - R3=A(BUFFER ENTRY)                         
*                                                                               
FORMAT   NTR1                                                                   
         MVC   LINDEFN,0(R3)                                                    
         LA    R3,12(R3)                                                        
         L     R1,0(R3)            TRANSACTIONS                                 
         EDIT  (R1),(6,LINTRNS),ZERO=BLANK                                      
         L     R1,16(R3)           TOTAL IOS                                    
         EDIT  (R1),(6,LINIOS),ZERO=BLANK                                       
         L     R1,48(R3)           IOS/TRAN                                     
         EDIT  (R1),(4,LINMIO),1,ZERO=BLANK                                     
         L     R0,8(R3)            MEAN CPU TIME                                
         BAS   RE,TUTOMS                                                        
         EDIT  (R1),(4,LINTCPU),ZERO=BLANK                                      
         L     R0,24(R3)           MEAN IO TIME                                 
         BAS   RE,TUTOMS                                                        
         EDIT  (R1),(4,LINTIO),ZERO=BLANK                                       
         L     R0,36(R3)           MEAN TRNS TIME                               
         BAS   RE,TUTOMS                                                        
         EDIT  (R1),(4,LINTTOT),ZERO=BLANK                                      
         L     R1,12(R3)           PCT OF CPU TIME                              
         EDIT  (R1),(3,LINPCPU),ZERO=BLANK                                      
         L     R1,28(R3)           PCT OF IO TIME                               
         EDIT  (R1),(3,LINPIO),ZERO=BLANK                                       
         L     R1,40(R3)           PCT OF TOTAL TRNS TIME                       
         EDIT  (R1),(3,LINPTOT),ZERO=BLANK                                      
         L     R0,44(R3)           TIME/IO                                      
         BAS   RE,TUTOMS                                                        
         EDIT  (R1),(3,LINTPIO),ZERO=BLANK                                      
         L     R0,32(R3)           MEAN INP Q TIME                              
         BAS   RE,TUTOMS                                                        
         EDIT  (R1),(5,LINQTM),ZERO=BLANK                                       
         L     R1,64(R3)           SE Q LENGTH                                  
         EDIT  (R1),(3,LINQLN),ZERO=BLANK                                       
         L     R1,60(R3)           TRANSACTION RATE                             
         EDIT  (R1),(5,LINTRAT),2,ZERO=BLANK                                    
         B     EXIT                                                             
         SPACE 1                                                                
TUTOMS   SR    R1,R1               CONVERT TUS TO MSECS                         
         SRDA  R0,32                                                            
         M     R0,=F'10'                                                        
         BR    RE                                                               
         EJECT                                                                  
* CONVERT TASK LETTER TO NUMBER                                                 
*                                                                               
GETTSK   NTR1  ,                                                                
         L     R1,VTCB                                                          
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         LA    R0,1                                                             
GETTSK2  CLC   TCBID+6-TCBD(1,R1),DUB                                           
         BE    GETTSKX                                                          
         AH    R0,=H'1'                                                         
         BXLE  R1,RE,GETTSK2                                                    
         SR    R0,R0                                                            
GETTSKX  ST    R1,ATSK                                                          
         STC   R0,DUB+1                                                         
         LTR   R0,R0                                                            
         B     EXIT                                                             
         SPACE 2                                                                
* CONVERT SE NUMBER TO RELATIVE SE NUMBER                                       
*                                                                               
GETSYS   NTR1  ,                                                                
         L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         LA    R0,1                                                             
GETSYS2  CLC   SESYS-SELISTD(1,R1),DUB                                          
         BE    GETSYSX                                                          
         AH    R0,=H'1'                                                         
         BXLE  R1,RE,GETSYS2                                                    
         SR    R0,R0                                                            
GETSYSX  ST    R1,ASYS                                                          
         STC   R0,DUB+1                                                         
         LTR   R0,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
* ERRORS AND EXIT                                                               
*                                                                               
ERROR1   MVC   SRVMSG,ERR1                                                      
         B     MEXIT                                                            
ERROR2   MVC   SRVMSG,ERR2                                                      
         B     MEXIT                                                            
ERROR3   MVC   SRVMSG,ERR3                                                      
         B     MEXIT                                                            
ERROR4   MVC   SRVMSG,ERR4                                                      
         B     MEXIT                                                            
ERROR5   MVC   SRVMSG,ERR5                                                      
         B     MEXIT                                                            
ERROR6   MVC   SRVMSG,ERR6                                                      
         B     MEXIT                                                            
         SPACE 1                                                                
MEXIT    NI    SRVP2H+6,X'BF'                                                   
         OI    6(R2),X'40'                                                      
         SPACE 1                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ADRFEQU  EQU   X'F3'               RECORDER FILE DATAMGR ID NUM                 
*                                                                               
TSKLIST  DC    C'123456789ABCDEFGHIJKLMNOPQ',X'FF'                              
MAXTRNS  DC    PL4'2000'                                                        
*                                                                               
ERR1     DC    CL60'** ERROR ** MISSING INPUT FIELD'                            
ERR2     DC    CL60'** ERROR ** INVALID INPUT FIELD'                            
ERR3     DC    CL60'** ERROR ** BASIS S/B SYS OR TSK'                           
ERR4     DC    CL60'** ERROR ** INPUT NOT NUMERIC'                              
ERR5     DC    CL60'** ERROR ** DISK ERROR ON ADRFILE'                          
ERR6     DC    CL60'** ERROR ** MAXIMUM OF 2000 ALLOWED'                        
         EJECT                                                                  
LINED    DSECT                     DISPLAY DATA LINE                            
         DS    CL8                                                              
LINDEFN  DS    CL10                                                             
         DS    C                                                                
LINTRNS  DS    CL6                                                              
         DS    C                                                                
LINIOS   DS    CL6                                                              
         DS    C                                                                
LINMIO   DS    CL4                                                              
         DS    C                                                                
LINTCPU  DS    CL4                                                              
         DS    C                                                                
LINTIO   DS    CL4                                                              
         DS    C                                                                
LINTTOT  DS    CL4                                                              
         DS    C                                                                
LINPCPU  DS    CL3                                                              
         DS    C                                                                
LINPIO   DS    CL3                                                              
         DS    C                                                                
LINPTOT  DS    CL3                                                              
         DS    CL2                                                              
LINTPIO  DS    CL3                                                              
         DS    C                                                                
LINQTM   DS    CL5                                                              
         DS    C                                                                
LINQLN   DS    CL3                                                              
         DS    CL2                                                              
LINTRAT  DS    CL5                                                              
LINNEXT  DS    0H                                                               
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
*                                                                               
P1       DS    F                                                                
P2       DS    F                                                                
P3       DS    F                                                                
P4       DS    F                                                                
P5       DS    F                                                                
P6       DS    F                                                                
*                                                                               
WORK     DS    CL20                                                             
VHEXOUT  DS    V                                                                
VHEXIN   DS    V                                                                
*                                                                               
AIO      DS    A                                                                
ASYS     DS    A                                                                
ATSK     DS    A                                                                
*                                                                               
BLKLEN   DS    F                                                                
BLKFAC   DS    F                                                                
RECLEN   DS    H                                                                
HIREC    DS    X                                                                
         DS    X                                                                
*                                                                               
SYSFILT  DS    X                                                                
TSKFILT  DS    X                                                                
LINFILT  DS    CL9                                                              
BASIS    DS    C                                                                
INPUT    DS    C                                                                
         DS    X                                                                
NBLKS    DS    H                                                                
*                                                                               
POSTLINE DS    CL76                                                             
ADRMAXA  EQU   200                 MAX NUMBER OF SE SYSTEMS                     
ADRLENA  EQU   80                                                               
ADRTOTS  DS    XL(ADRLENA)                                                      
ADRACCS  DS    (ADRMAXA)XL(ADRLENA)                                             
ADRTOTS2 DS    XL(ADRLENA)                                                      
ADRIO    DS    6400C                                                            
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFPH                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
SRFSTFFD DSECT                                                                  
         DS    CL64                                                             
* SRFSTFFD                                                                      
       ++INCLUDE SRFSTFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRFST00S  05/01/02'                                      
         END                                                                    
