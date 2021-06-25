*          DATA SET SRFST00    AT LEVEL 003 AS OF 05/15/14                      
*PHASE T14200A                                                                  
*INCLUDE SQUASHER                                                               
*INCLUDE TIMEDT                                                                 
         TITLE '$FAST - FACPAK ADRBUFF STATISTICS'                              
         PRINT NOGEN                                                            
FAST     CSECT                                                                  
         NMOD1 WRKX-WRKD,*$FAST**,RA,RR=RE,CLEAR=YES                            
         USING WRKD,RC             RC=A(W/S)                                    
         USING SRPARMD,R1                                                       
         ST    RE,RELO                                                          
         L     R8,SRPARM6                                                       
         USING SRFSTFFD,R8         R8=A(TWA)                                    
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R1,SRPARM4                                                       
         USING COMFACSD,R1                                                      
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VHEXIN,CHEXIN                                                    
*                                                                               
         L     R1,VSSB                                                          
         USING SSBD,R1                                                          
         MVC   SYSNAME(4),SSBSYSN4 ADV NAME                                     
         LLC   R0,SSBSYSIX         AORNUM/ADVNUM                                
         DROP  R1                                                               
         CLI   SYSNAME+3,C' '                                                   
         BNE   *+8                                                              
         MVI   SYSNAME+3,C'/'                                                   
         SRL   R0,4                                                             
         STC   R0,TORAOR                                                        
         LTR   R0,R0                                                            
         BNZ   *+12                                                             
         LHI   R0,X'A3'            SET TOR LETTER T                             
         B     *+8                                                              
         AHI   R0,X'80'            SET AOR LETTER A-H                           
         STC   R0,SYSNAME+4                                                     
*                                                                               
         LH    R1,=Y(ADRIO-WRKD)                                                
         LA    R1,WRKD(R1)                                                      
         ST    R1,AIO                                                           
*                                                                               
         L     RE,VADRBUFF         ADRFILE DATA STORED AHEAD OF ADRBUFF         
         AHI   RE,-16                                                           
         LH    R0,0(RE)            GET RECORDS PER BUFFER                       
         ST    R0,BLKFAC                                                        
         MVC   RECLEN,2(RE)        GET RECORD LENGTH                            
         MH    R0,2(RE)                                                         
         ST    R0,BLKLEN           SET BLOCK LENGTH                             
*                                                                               
         GOTO1 VDADDS,P1,VDARPT,0,(R0),VADRFILE                                 
         LH    R0,P3+2                                                          
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         STC   R0,HIREC            SET HIREC ON TRACK                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SERVICE REQUEST PARAMETER FIELDS                           *         
***********************************************************************         
FASP1    LA    R2,SRVP1H           VALIDATE BASIS (P1)                          
         CLI   5(R2),0             NOT INPUT                                    
         BNE   FASP1A                                                           
         MVC   8(3,R2),=C'SYS'     DEFAULT IS 'SYS'                             
         MVI   5(R2),3                                                          
         OI    6(R2),X'80'                                                      
FASP1A   CLI   5(R2),5                                                          
         BH    ERROR2                                                           
         MVI   SYSFILT,1           SET LOW SYSTEM FILTER                        
         MVI   TSKFILT,1           SET LOW TASK FILTER                          
         MVI   SRVFILT,1           SET LOW SRV FILTER                           
         MVI   BASIS,C'S'                                                       
         CLC   8(3,R2),=C'SYS'                                                  
         BNE   FASP1B                                                           
         CLI   5(R2),3                                                          
         BE    FASP2                                                            
         GOTO1 VHEXIN,DMCB,11(R2),DUB,2                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERROR2                                                           
         BAS   RE,GETSYS                                                        
         BZ    ERROR2                                                           
         MVC   SYSFILT,DUB+1                                                    
         B     FASP2                                                            
*                                                                               
FASP1B   MVI   BASIS,C'T'          TASK                                         
         CLC   8(3,R2),=C'TSK'                                                  
         BNE   FASP1C              NOT VALID                                    
         CLI   5(R2),3                                                          
         BE    FASP2                                                            
         MVC   DUB(1),11(R2)                                                    
         BAS   RE,GETTSK                                                        
         BZ    ERROR2                                                           
         MVC   TSKFILT,DUB+1                                                    
*                                                                               
FASP1C   MVI   BASIS,C'V'          SERVICE REQUESTS                             
         CLC   8(3,R2),=C'SRV'                                                  
         BNE   ERROR3              NOT VALID                                    
*                                                                               
FASP2    XC    NBLKS,NBLKS         VALIDATE NUM OF TRANS (P2)                   
         MVI   INPUT,C'B'                                                       
         LA    R2,SRVP2H                                                        
         CLI   5(R2),0                                                          
         BE    FASP4                                                            
         TM    4(R2),X'08'                                                      
         BZ    ERROR4              INPUT NOT NUMERIC                            
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CP    DUB,MAXTRNS                                                      
         BNH   *+14                                                             
         CLC   SRVP3(1),=C'OVERRIDE'                                            
         BNE   ERROR6                                                           
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,BLKFAC                                                        
         LTR   R1,R1                                                            
         BZ    FASP4                                                            
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
*                                                                               
FASP4    LA    R2,SRVP4H           VALIDATE FILTERS IN P4                       
         XC    LUIDFILT,LUIDFILT                                                
         XC    AGYFILT,AGYFILT                                                  
         XC    CTFILT,CTFILT                                                    
         CLI   5(R2),0                                                          
         BE    FAS3                                                             
*                                                                               
FASP4A   CLC   8(5,R2),=C'LUID='   LUID=FILTER                                  
         BNE   FASP4B                                                           
         LLC   R1,5(R2)                                                         
         AHI   R1,-6                                                            
         BM    ERROR1                                                           
         CHI   R1,7                                                             
         BH    ERROR2                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LUIDFILT+1(0),13(R2)                                             
         STC   R1,LUIDFILT                                                      
         CLI   INPUT,C'F'                                                       
         BE    FAS3A                                                            
         B     FAS3                                                             
*                                                                               
FASP4B   CLC   8(4,R2),=C'AGY='    AGY=FILT                                     
         BNE   FASP4C                                                           
         CLI   5(R2),6                                                          
         BNE   ERROR1                                                           
         MVC   AGYFILT,12(R2)                                                   
         CLI   INPUT,C'F'                                                       
         BE    FAS3A                                                            
         B     FAS3                                                             
*                                                                               
FASP4C   CLC   8(3,R2),=C'CT='     CT=FILT TO SHOW CONNECT TRANS                
         BNE   FAS3                                                             
         CLI   5(R2),4                                                          
         BNE   ERROR1                                                           
         MVC   CTFILT,11(R2)                                                    
         CLI   INPUT,C'F'                                                       
         BE    FAS3A                                                            
         EJECT                                                                  
***********************************************************************         
* READ NEXT ADRFILE RECORD                                            *         
***********************************************************************         
FAS3     L     R3,VADRBUFF         R3=A(ADRBUFF)                                
         USING ADRRECD,R3                                                       
         L     R4,BLKFAC           R4=NUMBER OF ENTRIES                         
         B     FAS4                                                             
*                                                                               
FAS3A    L     RF,VDADDS           READ ADRFILE REC INTO ADRIO                  
         LA    R1,P1                                                            
         BASR  RE,RF                                                            
         LA    R2,SRVSREQH                                                      
         OC    P3(2),P3                                                         
         BZ    FAS3B                                                            
         TM    P3+1,X'08'          IF N/F GO GET NEXT BLOCK                     
         BO    FAS8B                                                            
         B     ERROR5              DISK ERROR ON ADRFILE                        
FAS3B    L     R3,AIO              R3=A(ADRREC)                                 
         L     R4,BLKFAC           R4=NUMBER OF ENTRIES                         
         CLI   0(R3),C'*'                                                       
         BE    FAS8A               IGNORE SPECIAL BLOCKS IN ADRFILE             
*                                                                               
FAS4     CLI   0(R3),C'A'          CHECK FOR FUNNY BUFFER ENTRY                 
         BL    FAS8                                                             
         TM    ADRCPUTM,X'80'                                                   
         BO    FAS8                                                             
         TM    ADRIOCNT,X'80'                                                   
         BO    FAS8                                                             
         CLC   ADRSTTM,ADRNDTM                                                  
         BH    FAS8                                                             
         OC    ADRSTTM,ADRSTTM                                                  
         BZ    FAS8                                                             
*                                                                               
FAS4A    CLI   LUIDFILT,0          CHECK LUID FILTER                            
         BE    FAS4B                                                            
         LLC   R1,LUIDFILT                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ADRSYM(0),LUIDFILT+1                                             
         BNE   FAS8                                                             
         B     FAS5                                                             
*                                                                               
FAS4B    OC    AGYFILT,AGYFILT     CHECK AGENCY FILTER                          
         BZ    FAS4C                                                            
         CLC   ADRAGYID,AGYFILT                                                 
         BNE   FAS8                                                             
         B     FAS5                                                             
*                                                                               
FAS4C    CLI   CTFILT,C'C'         CHECK CONNECT ONLY                           
         BNE   FAS4D                                                            
         TM    ADRFLAG2,ADRFCTC                                                 
         BZ    FAS8                                                             
         B     FAS5                                                             
FAS4D    CLI   CTFILT,C'D'         CHECK DISCONNECT ONLY                        
         BNE   FAS4E                                                            
         TM    ADRFLAG2,ADRFCTD                                                 
         BZ    FAS8                                                             
         B     FAS5                                                             
FAS4E    CLI   CTFILT,C'B'         CHECK CONNECT AND DISCONNECT                 
         BNE   FAS5                                                             
         TM    ADRFLAG2,ADRFCTC+ADRFCTD                                         
         BZ    FAS8                                                             
*                                                                               
FAS5     XC    POSTLINE,POSTLINE   BUILD A LINE TO POST                         
         MVC   POSTLINE(4),=F'1'                                                
         MVC   POSTLINE+4(4),ADRCPUTM                                           
         MVC   POSTLINE+17(3),ADRIOCNT                                          
         L     R0,ADRNDTM                                                       
         SL    R0,ADRSTTM                                                       
         ST    R0,POSTLINE+36      TOTAL TRANSACTION TIME                       
         SL    R0,ADRCPUTM                                                      
         ST    R0,POSTLINE+20      TOTAL I/O TIME                               
         L     R0,ADRSTTM                                                       
         SL    R0,ADRINTM                                                       
         ST    R0,POSTLINE+32      INPUT QUEUE TIME                             
         MVC   POSTLINE+52(4),ADRSTTM                                           
         MVC   POSTLINE+56(4),ADRNDTM                                           
         CLI   BASIS,C'S'                                                       
         BE    FAS5B                                                            
         CLI   BASIS,C'V'                                                       
         BE    FAS5C                                                            
*                                                                               
FAS5A    MVC   DUB(1),ADRTASK      TASK BASIS                                   
         BAS   RE,GETTSK                                                        
         JZ    *+2                                                              
         LLC   R0,TSKFILT                                                       
         LLC   R1,DUB+1                                                         
         B     FAS6                                                             
*                                                                               
FAS5B    MVC   DUB(1),ADRSYSNO     SYSTEM BASIS                                 
         BAS   RE,GETSYS                                                        
         JZ    *+2                                                              
         LLC   R0,SYSFILT                                                       
         LLC   R1,DUB+1                                                         
         CLI   CTFILT,C'I'         CHECK CONNECT TRANS FILTER                   
         BNE   FAS6                                                             
         TM    ADRFLAG2,ADRFCTC                                                 
         BZ    FAS6                                                             
         LHI   R1,1                SET SYSTEM TO 1                              
         B     FAS6                                                             
*                                                                               
FAS5C    MVC   DUB(1),ADRPRGNO     SERVICE BASIS - PASS S/R PROGRAM             
         MVC   DUB+1(1),ADRFLAG2                                                
         TM    ADRFLAG2,ADRFCTC+ADRFCTD                                         
         BNZ   *+12                                                             
         CLI   ADRSYSNO,X'01'                                                   
         BNE   FAS8                IGNORE IF NOT A SERVICE REQUEST              
         BAS   RE,GETSRV                                                        
         JZ    *+2                                                              
         LLC   R0,SRVFILT                                                       
         LLC   R1,DUB+1                                                         
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
FAS8     AH    R3,RECLEN           BUMP TO NEXT BUFFER ENTRY                    
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
FAS8B    LLC   R1,P6+2             DECREMENT RECORD                             
         AHI   R1,-1                                                            
         BZ    *+12                                                             
         STC   R1,P6+2                                                          
         B     FAS3A                                                            
         LH    R1,P6               DECREMENT TRACK                              
         N     R1,=X'0000FFFF'                                                  
         AHI   R1,-1                                                            
         BZ    FAS9                DONE IF B-O-F                                
         STH   R1,P6                                                            
         MVC   P6+2(1),HIREC       SET HIGH REC                                 
         B     FAS3A                                                            
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
         ST    R0,ADRTOTD+64       POST QUEUE LENGTH TO TOTAL LINE              
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
*                                                                               
         L     R0,4(R5)            MEAN CPU TIME  (TOTAL CPU/TRNS)              
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         OC    0(4,R5),0(R5)                                                    
         BZ    FAS11                                                            
         D     R0,0(R5)                                                         
         AL    R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,8(R5)                                                         
*                                                                               
         L     R0,20(R5)           MEAN I/O TIME (TOTAL IO/TRNS)                
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         D     R0,0(R5)                                                         
         AL    R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,24(R5)                                                        
*                                                                               
         L     R0,16(R5)           IO/TRANS (TOTAL IO/TRANS)                    
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         D     R0,0(R5)                                                         
         AL    R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,48(R5)                                                        
*                                                                               
         L     R0,20(R5)           TIME/IO (TOTAL IO TIME/TOTAL IO)             
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         OC    16(4,R5),16(R5)                                                  
         BZ    FAS11                                                            
         D     R0,16(R5)                                                        
         AL    R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,44(R5)                                                        
*                                                                               
FAS11    L     R0,4(R5)            PCT OF CPU TIME (CPU TIME/TOTAL CPU)         
         SRDL  R0,32                                                            
         M     R0,=F'10000'                                                     
         OC    ADRTOTD+4(4),ADRTOTD+4                                           
         BZ    FAS12                                                            
         D     R0,ADRTOTD+4                                                     
         SR    R0,R0                                                            
         AL    R1,=F'50'                                                        
         D     R0,=F'100'                                                       
         ST    R1,12(R5)                                                        
*                                                                               
FAS12    L     R0,20(R5)           PCT OF I/O TIME (I/O TIME/TOTAL I/O)         
         SRDL  R0,32                                                            
         M     R0,=F'10000'                                                     
         OC    ADRTOTD+20(4),ADRTOTD+20                                         
         BZ    FAS13                                                            
         D     R0,ADRTOTD+20                                                    
         AL    R1,=F'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         ST    R1,28(R5)                                                        
*                                                                               
FAS13    L     R0,36(R5)           PCT OF TRNS TIME (TTIME/TOTAL TIME)          
         SRDL  R0,32                                                            
         M     R0,=F'10000'                                                     
         OC    ADRTOTD+36(4),ADRTOTD+36                                         
         BZ    FAS14                                                            
         D     R0,ADRTOTD+36                                                    
         AL    R1,=F'50'                                                        
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         ST    R1,40(R5)                                                        
*                                                                               
FAS14    L     R0,32(R5)           MEAN INP Q TIME (INPUT Q TIME/TRNS)          
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         D     R0,0(R5)                                                         
         AL    R1,=F'1'                                                         
         SRL   R1,1                                                             
         ST    R1,32(R5)                                                        
*                                                                               
FAS15    L     R1,56(R5)           TRANSACTION RATE (TOTAL TIME/TRNS)           
         SL    R1,52(R5)                                                        
         LTR   R1,R1                                                            
         BZ    FAS15X                                                           
         LR    RE,R1               RE=TIME OF LAST-TIME OF FIRST TRANS          
         L     R0,0(R5)                                                         
         SRDL  R0,32               R0/R1=#TRANS                                 
         L     RF,=F'38400'                                                     
         MH    RF,=H'100'                                                       
         MR    R0,RF               R0/R1=#TRANS*1SECINTUS*100                   
         DR    R0,RE                                                            
FAS15X   ST    R1,60(R5)                                                        
*                                                                               
         L     R0,36(R5)           MEAN TRNS TIME (TRNS TIME/TRNS)              
         SR    R1,R1                                                            
         SRDL  R0,31                                                            
         D     R0,0(R5)                                                         
         AL    R1,=F'1'                                                         
         SRL   R1,1                                                             
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
         MVC   POSTLINE(23),=C'XXXXX ELAPSED TIME FOR '                         
         MVC   POSTLINE(5),SYSNAME                                              
         L     R1,ADRTOTD                                                       
         LA    R2,POSTLINE+23                                                   
         EDIT  (R1),(6,0(R2))                                                   
         MVC   POSTLINE+30(7),=C'TRANS ='                                       
         L     R1,ADRTOTD+56                                                    
         SL    R1,ADRTOTD+52                                                    
         SR    R0,R0                                                            
         L     RF,=F'38400'                                                     
         DR    R0,RF                                                            
         ST    R1,DMCB+8                                                        
         GOTO1 =V(TIMEDT),DMCB,DMCB+8,WORK,RR=RB                                
         MVC   POSTLINE+37(8),WORK+2                                            
         MVC   POSTLINE+46(5),=C'SIN ='                                         
         L     R1,VSSB                                                          
         USING SSBD,R1                                                          
         L     R1,SSBSIN                                                        
         DROP  R1                                                               
         LA    R2,POSTLINE+51                                                   
         EDIT  (R1),(7,0(R2))                                                   
         GOTO1 =V(SQUASHER),DMCB,POSTLINE,76,RR=RB                              
         MVC   SRVMSG,POSTLINE                                                  
         LA    R2,SRVP2H                                                        
         B     MEXIT                                                            
         EJECT                                                                  
***********************************************************************         
* POST LINE OF ACCUMS TO BUFFER AND INITIALIZE BUFFER HEADER          *         
* R4=BUFFER LINE NUMBER FOR POSTING                                   *         
***********************************************************************         
POST     NTR1                                                                   
         LA    RE,ADRLENA          RE=L'LINE                                    
         SR    R0,R0                                                            
         STC   R1,BYTE             SAVE LINE NUMBER                             
         MR    R0,RE                                                            
         LA    R4,ADRTOTS(R1)                                                   
         OC    0(10,R4),0(R4)      IS LINE HEADER INITIALIZED                   
         BNZ   POST4               YES - GO POST                                
         CLI   BYTE,0              TOTALS LINE ?                                
         BNE   POST1                                                            
         MVC   0(5,R4),=C'TOTAL'                                                
         CLI   BASIS,C'T'                                                       
         BE    POST4                                                            
         MVC   0(3,R4),=C'** '                                                  
         MVC   3(5,R4),=C'TOTAL'                                                
         B     POST4                                                            
*                                                                               
POST1    MVC   0(10,R4),SPACES     CLEAR HEADER                                 
         CLI   BASIS,C'S'                                                       
         BNE   POST2                                                            
         L     R5,ASYS                                                          
         USING SELISTD,R5                                                       
         GOTO1 VHEXOUT,DMCB,SESYS,0(R4),1,X'24000000'                           
         MVC   DUB(7),SENAME                                                    
         MVI   DUB+7,C' '                                                       
         MVC   HALF,SEQLEN                                                      
*                                                                               
POST1A   CLC   DUB(4),=C'SERV'     SET SHORT SYSTEM NAME                        
         BNE   POST1B                                                           
         MVC   DUB(5),=C'SRV  '                                                 
         B     POST1N                                                           
POST1B   CLC   DUB(4),=C'CONT'                                                  
         BNE   POST1C                                                           
         MVC   DUB(5),=C'CON  '                                                 
         B     POST1N                                                           
POST1C   CLC   DUB(4),=C'PRNT'                                                  
         BNE   POST1D                                                           
         MVC   DUB(3),=C'PRT'                                                   
         MVC   DUB+3(2),DUB+4                                                   
         B     POST1N                                                           
POST1D   CLC   DUB(4),=C'SPOT'                                                  
         BNE   POST1N                                                           
         MVC   DUB(3),=C'SPT'                                                   
         MVC   DUB+3(2),DUB+4                                                   
*                                                                               
POST1N   MVC   3(5,R4),DUB         SHORT SE NAME                                
         LH    R0,HALF                                                          
         ST    R0,64(R4)           SAVE Q LENGTH                                
         A     R0,ADRTOTD+64       UPDATE TOTAL Q LENGTH                        
         ST    R0,ADRTOTD+64                                                    
         B     POST4                                                            
         DROP  R5                                                               
*                                                                               
POST2    CLI   BASIS,C'T'          TASK DISPLAY                                 
         BNE   POST3                                                            
         MVC   0(5,R4),=C'TASK#'                                                
         MVC   5(1,R4),ADRTASK                                                  
         B     POST4                                                            
*                                                                               
POST3    L     RF,ASRV             SERVICE DISPLAY                              
         MVC   BYTE1,0(RF)                                                      
         MVC   3(7,R4),1(RF)                                                    
         GOTO1 VHEXOUT,DMCB,BYTE1,0(R4),1,X'24000000'                           
*                                                                               
POST4    LA    R4,12(R4)           POST TO BUFFER LINE                          
         LA    R1,POSTLINE                                                      
*                                                                               
         OC    52(4,R4),52(R4)     POST START & END TIME                        
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
         AL    RE,0(R4)                                                         
         ST    RE,0(R4)                                                         
         LA    R1,4(R1)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,POST6                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT A LINE OF ACCUMS INTO TWA - R3=A(BUFFER ENTRY)               *         
***********************************************************************         
FORMAT   NTR1                                                                   
         MVC   LINDEFN,0(R3)                                                    
         LA    R3,12(R3)                                                        
         L     R1,0(R3)            TRANSACTIONS                                 
         EDIT  (R1),(8,LINTRNS-1),ZERO=BLANK                                    
         L     R1,16(R3)           TOTAL IOS                                    
         EDIT  (R1),(8,LINIOS-1),ZERO=BLANK                                     
         L     R1,48(R3)           IOS/TRAN                                     
         EDIT  (R1),(4,LINMIO),ZERO=BLANK                                       
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
*                                                                               
         L     R0,44(R3)           TIME PER I/O-ONE DECIMAL PLACE               
         SRDL  R0,32                                                            
         M     R0,=F'10000'                                                     
         D     R0,=F'38400'                                                     
         EDIT  (R1),(5,LINTPIO-1),1,ZERO=BLANK                                  
*                                                                               
         L     R0,32(R3)           MEAN INP Q TIME-ONE DECIMAL PLACE            
         SRDL  R0,32                                                            
         M     R0,=F'10000'                                                     
         D     R0,=F'38400'                                                     
         EDIT  (R1),(6,LINQTM-1),1,ZERO=BLANK                                   
*                                                                               
         L     R1,60(R3)           TRANSACTION RATE-TWO DECIMAL PLACES          
         EDIT  (R1),(5,LINTRAT),2,ZERO=BLANK                                    
*                                                                               
         CLI   TORAOR,0            QUEUE LENGTH NOW FOR TOR                     
         BNE   EXIT                                                             
         L     R1,64(R3)                                                        
         EDIT  (R1),(3,LINQLN),ZERO=BLANK                                       
         B     EXIT                                                             
*                                                                               
TUTOMS   SRDL  R0,32               1/38400 SECS TO MSECS                        
         M     R0,=F'1000'                                                      
         D     R0,=F'38400'                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT TASK LETTER TO NUMBER                                       *         
***********************************************************************         
GETTSK   NTR1                                                                   
         L     R1,VTCB                                                          
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         CLI   TORAOR,0                                                         
         BZ    *+6                                                              
         AR    RF,RE               ADD EXTRA TASK LEN FOR AOR                   
*                                                                               
         LA    R1,6(R1)                                                         
         LA    R0,1                                                             
GETTSK2  CLC   TCBID+6-TCBD(1,R1),DUB                                           
         BE    GETTSKX                                                          
         AHI   R0,1                                                             
         BXLE  R1,RE,GETTSK2                                                    
         SR    R0,R0                                                            
GETTSKX  ST    R1,ATSK                                                          
         STC   R0,DUB+1                                                         
         LTR   R0,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONVERT SE NUMBER TO RELATIVE SE NUMBER                             *         
***********************************************************************         
GETSYS   NTR1                                                                   
         L     R1,VSELIST                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         LA    R0,1                                                             
GETSYS2  CLC   SESYS-SELISTD(1,R1),DUB                                          
         BE    GETSYSX                                                          
         AHI   R0,1                                                             
         BXLE  R1,RE,GETSYS2                                                    
         SR    R0,R0                                                            
GETSYSX  ST    R1,ASYS                                                          
         STC   R0,DUB+1                                                         
         LTR   R0,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CONVERT SERVICE REQUEST TO RELATIVE SERVICE REQUEST NUMBER          *         
***********************************************************************         
GETSRV   NTR1                                                                   
         LA    R1,SRVTAB                                                        
         TM    DUB+1,ADRFCTC       TEST CT+ CONNECT TRANSACTION                 
         BZ    *+16                                                             
         LHI   R0,2                                                             
         LA    R1,SRVCTC                                                        
         B     GETSRVX                                                          
         TM    DUB+1,ADRFCTD       TEST CT- DISCONNECT TRASACTION               
         BZ    *+16                                                             
         LHI   R0,3                                                             
         LA    R1,SRVCTD                                                        
         B     GETSRVX                                                          
         LHI   R0,1                                                             
GETSRV1  CLC   0(1,R1),DUB         TEST PROGRAM NUMBER MATCHES                  
         BE    GETSRV3                                                          
GETSRV2  AHI   R0,1                                                             
         LA    R1,L'SRVTAB(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   GETSRV1                                                          
GETSRV3  CLI   0(R1),0             IGNORE PROGRAM ZERO                          
         NOP   GETSRV2             *NOP* BE                                     
GETSRVX  ST    R1,ASRV                                                          
         STC   R0,DUB+1                                                         
         LTR   R0,R0                                                            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERRORS AND EXIT                                                     *         
***********************************************************************         
ERROR1   LHI   RF,1                                                             
         B     EEXIT                                                            
ERROR2   LHI   RF,2                                                             
         B     EEXIT                                                            
ERROR3   LHI   RF,3                                                             
         B     EEXIT                                                            
ERROR4   LHI   RF,4                                                             
         B     EEXIT                                                            
ERROR5   LHI   RF,5                                                             
         B     EEXIT                                                            
ERROR6   LHI   RF,6                                                             
         B     EEXIT                                                            
*                                                                               
EEXIT    MVC   SRVMSG(14),=C'ED/9999 XXXXX '                                    
         MVC   SRVMSG+08(5),SYSNAME                                             
         L     RE,=A(ERRMSG)                                                    
         A     RE,RELO                                                          
         AHI   RF,-1                                                            
         MHI   RF,L'ERRMSG                                                      
         AR    RE,RF                                                            
         MVC   SRVMSG+14(45),0(RE)                                              
MEXIT    NI    SRVP2H+6,X'BF'                                                   
         OI    6(R2),X'40'                                                      
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
ADRFEQU  EQU   X'F3'               RECORDER FILE DATAMGR ID NUM                 
*                                                                               
         LTORG                                                                  
SPACES   DC    CL16' '                                                          
TSKLIST  DC    C'123456789ABCDEFGHIJKLMNOPQ',X'FF'                              
MAXTRNS  DC    PL4'10000'                                                       
*                                                                               
SRVTAB   DS    0CL8                                                             
         DC    X'00',C'       '                                                 
         DC    X'10',C'CT     '                                                 
SRVCTC   DC    X'10',C'CT+    '    MUST BE ENTRY#2 CONNECT DONE                 
SRVCTD   DC    X'10',C'CT-    '    MUST BE ENTRY#3 DISCONNECT DONE              
         DC    X'12',C'DONE   '                                                 
         DC    X'1E',C'PASS   '                                                 
         DC    X'04',C'TIM    '                                                 
         DC    X'20',C'RE     '                                                 
         DC    X'21',C'PC     '                                                 
         DC    X'28',C'WHOAMI '                                                 
         DC    X'31',C'PQ     '                                                 
         DC    X'49',C'SS     '                                                 
         DC    X'59',C'DPQ    '                                                 
         DC    X'05',C'TOP    '                                                 
         DC    X'42',C'FAST   '                                                 
SRVLST   DC    X'FF',C'OTHERS '    LAST ENTRY                                   
                                                                                
ERRMSG   DS    0CL45                                                            
ERR1     DC    CL45'MISSING INPUT FIELD'                                        
ERR2     DC    CL45'INVALID INPUT FIELD'                                        
ERR3     DC    CL45'BASIS S/B SYS OR TSK OR SRV'                                
ERR4     DC    CL45'INPUT NOT NUMERIC'                                          
ERR5     DC    CL45'DISK ERROR ON ADRFILE'                                      
ERR6     DC    CL45'MAXIMUM OF 10000 ALLOWED'                                   
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATA LINE DSECT                                             *         
***********************************************************************         
LINED    DSECT                                                                  
         DS    CL8                                                              
LINDEFN  DS    CL8                                                              
         DS    C                                                                
LINTRNS  DS    CL7                                                              
         DS    C                                                                
LINIOS   DS    CL7                                                              
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
         DS    C                                                                
LINTPIO  DS    CL4                                                              
         DS    C                                                                
LINQTM   DS    CL5                                                              
         DS    C                                                                
LINTRAT  DS    CL5                                                              
         DS    C                                                                
LINQLN   DS    CL3                                                              
         DS    C                                                                
LINNEXT  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
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
RELO     DS    A                                                                
*                                                                               
AIO      DS    A                                                                
ASYS     DS    A                                                                
ATSK     DS    A                                                                
ASRV     DS    A                                                                
*                                                                               
BLKLEN   DS    F                                                                
BLKFAC   DS    F                                                                
RECLEN   DS    H                                                                
HIREC    DS    X                                                                
*                                                                               
SYSFILT  DS    X                                                                
TSKFILT  DS    X                                                                
SRVFILT  DS    X                                                                
LUIDFILT DS    CL9                                                              
AGYFILT  DS    CL2                                                              
CTFILT   DS    CL1                                                              
BASIS    DS    C                                                                
INPUT    DS    C                                                                
TORAOR   DS    X                                                                
NBLKS    DS    H                                                                
SYSNAME  DS    CL5                                                              
*                                                                               
         DS    0F                                                               
POSTLINE DS    CL76                                                             
*                                                                               
ADRMAXA  EQU   200                 MAX NUMBER OF SE SYSTEMS                     
ADRLENA  EQU   80                  LEN OF EACH LINE OF ACCUMS                   
*                                                                               
ADRTOTS  DS    0XL80               FIRST LINE - GRAND TOTAL LINE                
ADRTOTH  DS    XL12                HEADER=NAME OF LINE FOR DISPLAY              
ADRTOTD  DS    XL68                DATA                                         
ADRACCS  DS    200XL80             INDIV LINES - ONE PER SE OR TASK             
ADRTOTS2 DS    XL80                                                             
*                                                                               
ADRIO    DS    6400C                                                            
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* SCREEN AND OTHER SYSTEM DSECTS                                      *         
***********************************************************************         
SRFSTFFD DSECT                                                                  
         DS    CL64                                                             
* SRFSTFFD                                                                      
       ++INCLUDE SRFSTFFD                                                       
                                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
                                                                                
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
                                                                                
* DMDTFPH                                                                       
       ++INCLUDE DMDTFPH                                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRFST00   05/15/14'                                      
         END                                                                    
