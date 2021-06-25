*          DATA SET CPREP4002  AT LEVEL 055 AS OF 05/01/02                      
*PHASE CP4002A                                                                  
*INCLUDE CPTAPEJW                                                               
         TITLE 'CP4002 - CUSTOMISED CPP REPORT CONTROLLER'                      
CP4002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CP40**                                                       
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         USING CP4002+4096,R8,R7                                                
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING CPWORKD,RA,RC                                                    
         STM   RA,RC,CP40RA                                                     
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         LA    RE,MYMID                                                         
         ST    RE,MIDHOOK                                                       
         LA    R2,RELOC                                                         
         S     R2,RELOC                                                         
         ST    R2,RELO                                                          
         L     R9,=A(USERAREA)                                                  
         A     R9,RELO                                                          
         USING USERD,R9                                                         
         L     R1,=A(BUFFALOC)                                                  
         A     R1,RELO                                                          
         ST    R1,ABUFF                                                         
         L     R1,=A(WORKERC)                                                   
         A     R1,RELO                                                          
         ST    R1,AWORK                                                         
         EJECT                                                                  
*              INITIALIZATION                                                   
         SPACE 3                                                                
INIT2    CLI   MODE,REQFRST                                                     
         BNE   MKT2                                                             
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(4),=C'D40 '                                                 
         MVI   ID+7,C'W'                                                        
         XC    LASTBUFF,LASTBUFF                                                
         MVI   OPTRDSEQ,C'M'                                                    
         MVC   DUB(2),=C'CP'       DO WE HAVE SELECTED PROGRAM                  
         MVC   DUB+2(2),QAGY       IN CORE YET                                  
         MVC   DUB+4(4),QPHASE                                                  
         CLC   DUB(7),=C'CPOMCP5'                                               
         BNE   *+10                                                             
         MVC   DUB+2(2),=C'JW'                                                  
         CLC   DUB(7),=C'CPH7CP5'                                               
         BNE   *+10                                                             
         MVC   DUB+2(2),=C'JW'                                                  
         CLC   DUB+4(4),ACTPHASE+4                                              
         BE    INIT4                                                            
         MVC   ACTPHASE,DUB                                                     
*                                  NO - SO TRY AND FIND CPUUPPPP                
         GOTO1 LOADER,PARAS,ACTPHASE,(R9),(C'M',(R9))                           
         OC    PARAS+4(4),PARAS+4                                               
         BNZ   INIT3                                                            
*                                  MISS NOW TRY FOR CPDDPPPP                    
         MVC   ACTPHASE+2(2),=C'DD'                                             
         GOTO1 LOADER,PARAS,ACTPHASE,(R9),(C'M',(R9))                           
         OC    PARAS+4(4),PARAS+4                                               
         BNZ   INIT3               MISSED AGAIN - GIVE UP                       
         MVC   P(22),=C'CANT FIND USER PROGRAM'                                 
         MVC   P+23(4),QPHASE                                                   
         GOTO1 REPORT                                                           
         XC    ACTPHASE,ACTPHASE                                                
         B     XIT                                                              
         SPACE 2                                                                
INIT3    LR    R2,R9                                                            
         LA    R9,24(R9)           FIGURE OUT RELO                              
         S     R9,0(R2)            USING PRESENT VALUE OF A(ROWDEF)             
         LA    R3,6                                                             
         SPACE 1                                                                
INIT3B   L     R1,0(R2)            RELOCATE ADDRESSES                           
         AR    R1,R9                                                            
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         BCT   R3,INIT3B                                                        
         L     R9,=A(USERAREA)                                                  
         A     R9,RELO                                                          
         EJECT                                                                  
*              ROUTINES TO OPTIMIZE BUFFALO                                     
         SPACE 3                                                                
INIT4    L     R2,AROW                                                          
         MVI   FCMONTH,C'N'                                                     
         SR    R3,R3                                                            
         SR    R4,R4               LOOK AT ROWS FOR HIGHEST LEVEL               
         SPACE 2                                                                
INIT6    CLI   0(R2),0                                                          
         BE    INIT8                                                            
         CLI   4(R2),10            (WAS MONTHLY ANALYSIS NEEDED)                
         BNE   *+8                                                              
         MVI   FCMONTH,C'Y'        SET CONTROL                                  
         CLI   4(R2),11            OR QUARTERLY                                 
         BNE   *+8                                                              
         MVI   FCMONTH,C'Q'                                                     
         ZIC   R5,3(R2)                                                         
         CR    R5,R4                                                            
         BL    *+6                                                              
         LR    R4,R5                                                            
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     INIT6                                                            
         SPACE 2                                                                
INIT8    STC   R4,NUMLEVEL                                                      
         L     R2,ACOL             LOOK AT COLUMNS FOR HIGHEST COLUMN           
         SR    R4,R4                                                            
         SPACE 2                                                                
INIT10   CLI   0(R2),0                                                          
         BE    INIT14                                                           
         CLC   0(2,R2),=X'2204'                                                 
         BNE   INIT12                                                           
         CLI   3(R2),C'M'          WAS SOFT MONTHLY ANALYSIS SPECIFIED          
         BNE   *+8                                                              
         MVI   FCMONTH,C'Y'                                                     
         CLI   3(R2),C'Q'          OR QUARTERLY                                 
         BNE   *+8                                                              
         MVI   FCMONTH,C'Q'                                                     
         SPACE 2                                                                
INIT12   ZIC   R5,2(R2)                                                         
         CR    R5,R4                                                            
         BL    *+6                                                              
         LR    R4,R5                                                            
         IC    R3,1(R2)                                                         
         AR    R2,R3                                                            
         B     INIT10                                                           
         SPACE 2                                                                
INIT14   STC   R4,NUMCOLS                                                       
         ZIC   R6,NUMLEVEL                                                      
         MH    R6,=H'4'                                                         
         LA    R6,3(R6)                                                         
         L     R5,ABUFF                                                         
         USING BUFFALOD,R5                                                      
         STC   R6,BUFFLIST                                                      
         ST    R6,BUFFLKEY                                                      
         MH    R4,=H'6'                                                         
         ST    R4,BUFFCOLS         N'COLUMNS (*6)                               
         SLL   R4,2                                                             
         ST    R4,BUFFLDTA                                                      
         AR    R4,R6                                                            
         ST    R4,BUFFLALL                                                      
         LA    RF,30+256           HOW MANY LINES FIT IN CORE                   
         M     RE,=F'400'                                                       
         DR    RE,R4                                                            
         ST    RF,BUFFCRMX                                                      
         L     R2,=A(WORKIO)                                                    
         A     R2,RELO                                                          
         ST    R2,AWORKIO                                                       
         LA    R4,4(R4)                                                         
         STH   R4,0(R2)                                                         
         LA    R2,4(R2)                                                         
         ST    R2,AKEY                                                          
         A     R2,BUFFLKEY                                                      
         ST    R2,ACOL1                                                         
         GOTO1 BUFFALO,DMCB,=C'SET',(R5)                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         XC    SOFTTAB,SOFTTAB                                                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              SELECT ROUTINE BASED ON MODE SETTING                             
         SPACE 3                                                                
MKT2     CLI   MODE,PROCDATA                                                    
         BNE   MKT4                                                             
         BAS   RE,COL2                                                          
         BAS   RE,ROW2                                                          
         B     XIT                                                              
         SPACE 2                                                                
MKT4     CLI   MODE,MKTLAST                                                     
         BNE   MKT6                                                             
         BAS   RE,SETIX                                                         
         BAS   RE,GET2                                                          
         BAS   RE,PUTGROUP                                                      
         MVC   LASTBUFF,=30X'FF'                                                
         L     R5,ABUFF                                                         
         GOTO1 BUFFALO,PARAS,=C'CLEAR',(X'01',(R5)),(X'80',1)                   
         B     XIT                                                              
         SPACE 2                                                                
MKT6     CLI   MODE,REQLAST                                                     
         BNE   MKT8                                                             
         MVI   FORCEHED,C'Y'                                                    
         BAS   RE,GET2                                                          
         B     XIT                                                              
         SPACE 2                                                                
MKT8     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         OC    VTAPE,VTAPE                                                      
         BZ    XIT                                                              
         GOTO1 VTAPE,DMCB,(X'FF',(RA)),0   CLOSE TAPE                           
         B     XIT                                                              
         EJECT                                                                  
*              INTERPRET THE COLUMN DEFINITION                                  
         SPACE 3                                                                
COL2     NTR1                                                                   
         BAS   RE,DIGX             GET SOME VALUES                              
         XC    COLTAB,COLTAB       PRECLEAR POSTING INSTRUCTIONS                
         L     R2,ACOL                                                          
         BAS   RE,NOBUMP                                                        
         SPACE 2                                                                
COL4     CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'22'                                                      
         BE    COL20                                                            
         ZIC   R5,2(R2)            COLUMN NUMBER                                
         CLI   3(R2),C'O'                                                       
         BE    COL8                                                             
         CLI   3(R2),C'A'                                                       
         BE    COL6                                                             
         MVI   COLSW,0             FOR IF, SET SW=0                             
         B     COL8                                                             
         SPACE 2                                                                
COL6     CLI   COLSW,0             FOR AND, IF A PREVIOUS TEST FAILED           
         BE    COL16                        IGNORE THIS                         
         SPACE 2                                                                
COL8     CLI   4(R2),10                                                         
         BE    COL10                                                            
         CLI   4(R2),11                                                         
         BE    COL10                                                            
         LA    R4,4(R2)                                                         
         BAS   RE,IF                                                            
         CLI   IFSW,C'Y'                                                        
         BNE   COL16                                                            
         CLI   COLSW,0                                                          
         BNE   COL12                                                            
         MVI   COLSW,X'FF'                                                      
         B     COL12                                                            
         SPACE 2                                                                
COL10    PACK  DUB,5(1,R2)         FOR MONTH AND QUARTER                        
         CLI   6(R2),C' '                                                       
         BE    *+10                                                             
         PACK  DUB,5(R2,R2)                                                     
         CVB   R1,DUB                                                           
         STC   R1,COLSW            FIND THE SELECTED VALUE                      
         SPACE 2                                                                
COL12    CLI   8(R2),X'20'         IS THE NEXT AN 'AND'                         
         BNE   COL14                                                            
         CLI   11(R2),C'A'                                                      
         BE    COL16                                                            
         SPACE 2                                                                
COL14    ZIC   R5,2(R2)            NOW PUT VALUE INTO COLTAB                    
         LA    R5,COLTAB-1(R5)                                                  
         MVC   0(1,R5),COLSW                                                    
         SPACE 2                                                                
COL16    BAS   RE,BUMP                                                          
         B     COL4                                                             
         EJECT                                                                  
*              HANDLE THE SOFT COLUMN DEFINITIONS                               
         SPACE 3                                                                
COL20    ZIC   R0,2(R2)            MAX COLS                                     
         CLI   3(R2),C'M'                                                       
         BE    COL22                                                            
         CLI   3(R2),C'Q'                                                       
         BNE   COL26                                                            
         SPACE 2                                                                
COL22    LA    R3,1                                                             
         LA    R4,COLTAB                                                        
         SPACE 2                                                                
COL24    STC   R3,0(R4)            FOR MONTHS AND QUARTERS, BUILD A             
         LA    R4,1(R4)                LIST IN COLTAB OF 1-N                    
         LA    R3,1(R3)                                                         
         BCT   R0,COL24                                                         
         B     COL16                                                            
         SPACE 2                                                                
COL26    L     R4,ADDATA                                                        
         USING CPKEYD,R4                                                        
         MVC   WORK(1),CPKDEMO                                                  
         CLI   3(R2),C'T'                                                       
         BNE   *+10                                                             
         MVC   WORK(1),CPKTARGT                                                 
         DROP  R4                                                               
         LA    R4,COLTAB                                                        
         LA    R5,SOFTTAB                                                       
         LA    R0,16                                                            
         SPACE 2                                                                
COL28    CLC   0(1,R5),WORK        FOR TARGET OR DEMO FIND COLUMN               
         BE    COL30               POSITION BASED ON ITS POSITION               
         CLI   0(R5),0             IN THE SOFT TABLE                            
         BE    COL30                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,COL28                                                         
         B     COL16                                                            
         SPACE 2                                                                
COL30    MVC   0(1,R5),WORK        POSSIBLE NEW ENTRY IN SOFT TABLE             
         MVI   0(R4),1                                                          
         B     COL16                                                            
         EJECT                                                                  
*              LOGIC TO HANDLE THE ROWS                                         
         SPACE 3                                                                
ROW2     NTR1                                                                   
         XC    LASTSPEC,LASTSPEC                                                
         XC    ROWAREA,ROWAREA                                                  
         L     R2,AROW                                                          
         BAS   RE,NOBUMP                                                        
         B     ROW6                                                             
         SPACE 2                                                                
ROW4     BAS   RE,BUMP                                                          
         SPACE 2                                                                
ROW6     CLI   0(R2),0                                                          
         BNE   ROW8                                                             
         BAS   RE,PUT                                                           
         B     XIT                                                              
         SPACE 2                                                                
ROW8     CLC   0(4,R2),LASTSPEC    IF REP/LEV DO NOT ASCEND,                    
         BH    *+8                                                              
         BAS   RE,PUT              ITS TIME TO PUT SOME BUFFALO RECS.           
         MVC   LASTSPEC,0(R2)                                                   
         MVC   ROWAREA+2(1),2(R2)  BUILD ROW - ALWAYS PUT IN REPORT             
         ZIC   R4,3(R2)            PICK UP LEVEL NUMBER                         
         BCTR  R4,0                                                             
         SLL   R4,2                                                             
         LA    R4,ROWAREA+3(R4)    POSITION TO THAT SPOT IN ROWAREA             
         XC    4(32,R4),4(R4)      CLEAR FROM LEVEL BELOW                       
         CLI   4(R2),12            IS THIS A TOTAL                              
         BNE   ROW10                                                            
         MVC   1(3,R4),=X'FFFFFF'  YES - FILL WITH HIGH VALUES                  
         B     ROW4                                                             
         SPACE 2                                                                
ROW10    MVC   0(1,R4),4(R2)       OTHERWISE PUT IN KEYWORD                     
         CLI   4(R2),10                                                         
         BE    ROW4                UNLESS ITS A MONTH                           
         CLI   4(R2),11                                                         
         BE    ROW4                           OR QUARTER                        
         CLI   4(R2),5                                                          
         BE    ROW4                           OR DAYPART                        
         ZIC   R5,4(R2)            DIG OUT VALUE                                
         BCTR  R5,0                                                             
         MH    R5,=H'3'                                                         
         LA    R5,XVALS(R5)                                                     
         MVC   1(3,R4),0(R5)                                                    
         B     ROW4                                                             
         EJECT                                                                  
*              ROUTINE TO PUT - LEVEL 0 - MARKETS/GROUPS                        
         SPACE 3                                                                
PUT      NTR1                                                                   
         MVI   ROWAREA,1           ONCE FOR MARKET                              
         MVI   ROWAREA+1,0                                                      
         BAS   RE,PUT10                                                         
         B     XIT                                                              
         SPACE 1                                                                
PUTGROUP NTR1                                                                   
         CLI   WORKSW,C'N'                                                      
         BE    XIT                                                              
         MVI   WORKSW,C'Y'                                                      
         GOTO1 WORKER,DMCB,=C'CLOSE',AWORK                                      
         GOTO1 WORKER,DMCB,=C'INDEX',AWORK,ID                                   
         SPACE 1                                                                
PG2      GOTO1 WORKER,DMCB,=C'READ',AWORK,,AWORKIO                              
         TM    DMCB+8,X'80'                                                     
         BO    PG3                                                              
         BAS   RE,PG4                                                           
         B     PG2                                                              
         SPACE 1                                                                
PG3      MVI   WORKSW,C'N'                                                      
         B     XIT                                                              
         SPACE 1                                                                
WORKSW   DC    C'N'                                                             
         EJECT                                                                  
*              ROUTINE TO CONVERT MARKET RECORDS TO GROUPS                      
         SPACE 3                                                                
PG4      NTR1                                                                   
         BAS   RE,COMPCPP                                                       
         L     R2,AMKT                                                          
         BAS   RE,NOBUMP                                                        
         B     PUT4                                                             
         SPACE 1                                                                
PUT2     BAS   RE,BUMP                                                          
         SPACE 1                                                                
PUT4     CLI   0(R2),0             LOOK ROUND MARKET GROUPS                     
         BE    XIT                                                              
         CLI   0(R2),X'60'                                                      
         BNE   PUT2                                                             
         L     R1,AKEY                                                          
         MVI   0(R1),2                                                          
         MVC   1(1,R1),2(R2)       PUT IN GROUP NUMBER                          
         CLI   3(R2),C'R'          AND SEE IF IT QUALIFIES                      
         BNE   PUT6                                                             
         CLC   MKTRANK,4(R2)       RANK - FROM                                  
         BL    PUT2                                                             
         CLI   1(R2),6                    TO NOT SPECIFIED                      
         BNE   *+12                                                             
         BAS   RE,PUT9                    SO ITS OK                             
         B     PUT2                                                             
         SPACE 1                                                                
         CLC   MKTRANK,6(R2)              TO SPECIFIED                          
         BH    PUT2                                                             
         BAS   RE,PUT9                    FITS                                  
         B     PUT2                                                             
         SPACE 1                                                                
PUT6     LA    R3,4(R2)            LIST - BROWSE ROUND LOOKING FOR HIT          
         ZIC   R4,1(R2)                                                         
         SH    R4,=H'4'                                                         
         SRL   R4,1                                                             
         SPACE 1                                                                
PUT8     CLC   0(2,R3),MARKET                                                   
         BNE   *+12                                                             
         BAS   RE,PUT9                                                          
         B     PUT2                                                             
         LA    R3,2(R3)                                                         
         BCT   R4,PUT8                                                          
         B     PUT2                                                             
         SPACE 1                                                                
PUT9     NTR1                                                                   
         L     R2,AKEY                                                          
         L     R5,ABUFF                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',(R5),(R2)                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO WORK OUT CPP AND CPS                                  
         SPACE 3                                                                
COMPCPP  NTR1                                                                   
         L     R2,ACOL1                                                         
         ZIC   R3,NUMCOLS                                                       
         SPACE 1                                                                
COMP2    L     R1,4(R2)            CPP                                          
         LTR   R1,R1                                                            
         BZ    COMP6                                                            
         OC    8(4,R2),8(R2)                                                    
         BZ    COMP4                                                            
         M     R0,=F'2000'                                                      
         D     R0,8(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,16(R2)                                                        
         SPACE 1                                                                
COMP4    OC    0(4,R2),0(R2)       CPS                                          
         BZ    COMP6                                                            
         L     R1,4(R2)                                                         
         M     R0,=F'2'                                                         
         D     R0,0(R2)                                                         
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ST    R1,20(R2)                                                        
         SPACE 1                                                                
COMP6    LA    R2,24(R2)                                                        
         BCT   R3,COMP2                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT - LEVEL 1 - MONTHS/QUARTERS                       
         SPACE 3                                                                
PUT10    NTR1                                                                   
         BAS   RE,ADDCOV                                                        
         LA    R4,ROWAREA+3                                                     
         ZIC   R5,NUMLEVEL                                                      
         LTR   R5,R5                                                            
         BZ    PUT14                                                            
         SPACE 1                                                                
PUT12    CLI   0(R4),10            SEE IF A MONTH                               
         BE    PUT16                                                            
         CLI   0(R4),11                   OR QUARTER IS SPECIFIED               
         BE    PUT16                                                            
         LA    R4,4(R4)                                                         
         BCT   R5,PUT12                                                         
         SPACE 1                                                                
PUT14    LA    R6,1                NO - PASS VALUE OF 1 TO NEXT LEVEL           
         BAS   RE,PUT20                                                         
         B     XIT                                                              
         SPACE 1                                                                
PUT16    LA    R6,1                                                             
         LA    R3,PERTABLE                                                      
         L     R5,NPERIODS         FOR EACH PERIOD                              
         SPACE 1                                                                
         USING CPERD,R3                                                         
PUT18    STC   R6,1(R4)            PUT IN PERIOD NUMBER                         
         MVC   2(2,R4),CPSTARTB           AND START YM                          
         BAS   RE,PUT20                   AND PASS TO NEXT LEVEL                
         LA    R6,1(R6)                                                         
         A     R3,WIDTHPER                                                      
         BCT   R5,PUT18                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO PUT - LEVEL 2 - DAYPARTS                              
         SPACE 3                                                                
PUT20    NTR1                                                                   
         LA    R4,ROWAREA+3        (NB R6=PERIOD NO)                            
         ZIC   R5,NUMLEVEL                                                      
         LTR   R5,R5                                                            
         BZ    PUT24                                                            
         SPACE 1                                                                
PUT22    CLI   0(R4),5             SEE IF DAYPART WAS SPECIFIED                 
         BE    PUT26                                                            
         LA    R4,4(R4)                                                         
         BCT   R5,PUT22                                                         
         SPACE 1                                                                
PUT24    BAS   RE,PUT30                                                         
         B     XIT                                                              
         SPACE 2                                                                
PUT26    L     R2,ADP              YES -                                        
         BAS   RE,NOBUMP                                                        
         B     *+8                                                              
         SPACE 1                                                                
PUT27    BAS   RE,BUMP                                                          
         CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),X'50'         LOOK FOR DAYPART LIST                        
         BNE   PUT27                                                            
         MVC   1(3,R4),2(R2)       PUT IN DAYPART CODE                          
         LA    R3,5(R2)            ADDRESS OF LIST                              
         IC    R5,1(R2)                                                         
         SH    R5,=H'5'                                                         
         SR    R0,R0                                                            
         LR    R1,R5                                                            
         D     R0,=F'3'                                                         
         LR    R5,R1               R5=NUMBER OF ITEMS IN LIST                   
         SPACE 1                                                                
PUT28    CLI   0(R3),C'*'          CHECK FOR MATCH ON TIME                      
         BE    PUT29                                                            
         CLI   1(R3),C'-'                                                       
         BE    PUT28B                                                           
         CLC   0(1,R3),XTIME                                                    
         BE    PUT29                                                            
         B     PUT29F                                                           
         SPACE 1                                                                
PUT28B   CLC   0(1,R3),XTIME                                                    
         BE    PUT29F                                                           
         SPACE 1                                                                
PUT29    CLI   1(R3),C'*'                          AND PROGRAM                  
         BE    PUT29D                                                           
         CLI   0(R3),C'-'                                                       
         BE    PUT29B                                                           
         CLC   1(1,R3),XPROGRAM                                                 
         BE    PUT29D                                                           
         B     PUT29F                                                           
         SPACE 1                                                                
PUT29B   CLC   1(1,R3),XPROGRAM                                                 
         BE    PUT29F                                                           
         SPACE 1                                                                
PUT29D   CLI   2(R3),C'*'                          AND AFFILIATION              
         BE    PUT29D2                                                          
         CLC   2(1,R3),XAFFIL                                                   
         BNE   PUT29F                                                           
         SPACE 1                                                                
PUT29D2  BAS   RE,PUT30            QUALIFIES                                    
         B     PUT27                                                            
         SPACE 1                                                                
PUT29F   LA    R3,3(R3)                                                         
         BCT   R5,PUT28                                                         
         B     PUT27                                                            
         EJECT                                                                  
*              ROUTINE TO PUT - LEVEL 3 - BUFFALO RECORDS                       
         SPACE 3                                                                
PUT30    NTR1                                                                   
*                                  (R6=PERIOD NO.)                              
         L     R2,AKEY                                                          
         MVC   0(32,R2),ROWAREA    MOVE IN KEY                                  
         L     R2,ACOL1                                                         
         LA    R3,COLTAB                                                        
         ZIC   R4,NUMCOLS                                                       
         MVI   SUMACT,C'N'                                                      
         SPACE 1                                                                
PUT32    XC    0(24,R2),0(R2)                                                   
         ZIC   R5,0(R3)            WAS POSTING SPECIFIED FOR COLUMN             
         LTR   R5,R5                                                            
         BZ    PUT34                                                            
         CH    R6,=H'1'            COLTAB SPECIFIES PERIOD NUMBER               
         BE    *+6                        UNLESS ITS PASSED AS VALUE            
         LR    R5,R6                                                            
         CH    R5,=H'255'          X'FF'=ADD ALL PERIODS TOGETHER               
         BNE   *+12                                                             
         BAS   RE,PUT40                                                         
         B     PUT34                                                            
         BCTR  R5,0                POSITION R5 INTO PERIOD TABLE                
         MH    R5,WIDTHPER+2                                                    
         LA    R5,PERTABLE(R5)                                                  
         USING CPERD,R5                                                         
         OC    CPSPOTS(16),CPSPOTS SET ACTIVE IF SIGNIFICANT VALUES             
         BZ    PUT34                                                            
         MVI   SUMACT,C'Y'                                                      
         MVC   0(16,R2),CPSPOTS    PUT IN NUMERIC VALUES                        
         L     R1,4(R2)            EQUIVALENCE DOLLARS                          
         M     R0,=F'1000'                                                      
         D     R0,EQVFACT                                                       
         ST    R1,4(R2)                                                         
         DROP  R5                                                               
         SPACE 1                                                                
PUT34    LA    R2,24(R2)                                                        
         LA    R3,1(R3)                                                         
         BCT   R4,PUT32                                                         
         CLI   SUMACT,C'Y'                                                      
         BNE   XIT                                                              
         L     R2,AKEY                                                          
         L     R5,ABUFF                                                         
         GOTO1 BUFFALO,DMCB,=C'PUT',(R5),(R2)                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD PERIODS TOGETHER                                  
         SPACE 3                                                                
PUT40    NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R5,PERTABLE                                                      
         L     R3,NPERIODS                                                      
         SPACE 1                                                                
         USING CPERD,R5                                                         
PUT42    L     R1,CPSPOTS                                                       
         A     R1,WORK                                                          
         ST    R1,WORK                                                          
         L     R1,CPCASH                                                        
         A     R1,WORK+4                                                        
         ST    R1,WORK+4                                                        
         L     R1,CPOINTS                                                       
         A     R1,WORK+8                                                        
         ST    R1,WORK+8                                                        
         L     R1,CPIMPS                                                        
         A     R1,WORK+12                                                       
         ST    R1,WORK+12                                                       
         A     R5,WIDTHPER                                                      
         BCT   R3,PUT42                                                         
         OC    WORK(16),WORK                                                    
         BZ    XIT                                                              
         MVI   SUMACT,C'Y'                                                      
         MVC   0(16,R2),WORK                                                    
         L     R1,4(R2)            EQUIVALENCE DOLLARS                          
         M     R0,=F'1000'                                                      
         D     R0,EQVFACT                                                       
         CLI   USERPROF+1,C'N'     UNLESS NOT REQUIRED                          
         BE    *+8                                                              
         ST    R1,4(R2)                                                         
         DROP  R5                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD MARKET COVER TO GROUPS                            
         SPACE 3                                                                
ADDCOV   NTR1                                                                   
         ZIC   R2,ROWAREA+1                                                     
         LTR   R2,R2                                                            
         BNP   XIT                                                              
         BCTR  R2,0                                                             
         CH    R2,=H'99'                                                        
         BH    XIT                                                              
         MH    R2,=H'36'           POSITION R2 TO CHUNK FOR GROUP               
         L     R1,=A(COVERC)                                                    
         A     R1,RELO                                                          
         AR    R2,R1                                                            
         MVC   DUB,MKTRANK         MARKET NO. 1-255                             
         LH    R1,DUB                                                           
         SR    R0,R0                                                            
         CH    R1,=H'255'                                                       
         BH    XIT                                                              
         D     R0,=F'8'            R0 NOW HAS 0-7                               
         AR    R1,R2               R1 NOW HAS 0-31                              
         LA    RF,1                                                             
         SPACE 1                                                                
ADDCOV2  CH    R0,=H'0'            POSITION BIT POSITION IN RF                  
         BE    ADDCOV4                                                          
         SLL   RF,1                                                             
         BCT   R0,ADDCOV2                                                       
         SPACE 1                                                                
ADDCOV4  STC   RF,DUB                                                           
         NC    DUB(1),0(R1)                                                     
         CLI   DUB,0               IF BIT IS STILL ON, COVER                    
         BNE   XIT                    HAS ALREADY BEEN POSTED                   
         STC   RF,DUB                                                           
         OC    0(1,R1),DUB         OTHERWISE TURN BIT ON                        
         MVC   DUB,MKTWT           AND ADD IN COVER                             
         LH    R1,DUB                                                           
         A     R1,32(R2)                                                        
         ST    R1,32(R2)                                                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL BUFFALO READING                               
         SPACE 3                                                                
GET2     NTR1                                                                   
         L     R2,AKEY                                                          
         XC    0(32,R2),0(R2)                                                   
         L     R5,ABUFF                                                         
         GOTO1 BUFFALO,PARAS,=C'HIGH',(R5),(R2),1                               
         B     GET6                                                             
         SPACE 1                                                                
GET4     L     R5,ABUFF                                                         
         GOTO1 BUFFALO,PARAS,=C'SEQ',(R5),(R2),1                                
         SPACE 1                                                                
GET6     TM    PARAS+8,X'80'                                                    
         BO    XIT                                                              
         MVC   ROWAREA,0(R2)                                                    
         CLI   MODE,MKTLAST                                                     
         BNE   GET8                                                             
         CLI   ROWAREA,1                                                        
         BNE   XIT                                                              
         MVI   WORKSW,C'Y'                                                      
         GOTO1 WORKER,DMCB,=C'ADD',AWORK,ID,AWORKIO                             
         SPACE 1                                                                
GET8     CLC   QPROG,=C'4T'                                                     
         BNE   GET9                                                             
         CLC   QAGY,=C'H7'                                                      
         BE    *+10                                                             
         CLC   QAGY,=C'OM'                                                      
         BE    *+10                                                             
         CLC   QAGY,=C'JW'                                                      
         BNE   GET9                                                             
         L     RE,=V(CPTAPEJW)                                                  
         ST    RE,VTAPE                                                         
         GOTO1 =V(CPTAPEJW),DMCB,(RA),(R2)                                      
GET9     BAS   RE,EXTOUT                                                        
         BAS   RE,CBSET                                                         
         CLI   CBSKIP,C'Y'                                                      
         BNE   GET10                                                            
         MVI   FORCEHED,C'Y'                                                    
         B     GET12                                                            
         SPACE 1                                                                
GET10    CLI   CBSPACE,0                                                        
         BE    GET12                                                            
         MVC   P,SPACES                                                         
         MVC   SPACING,CBSPACE                                                  
         GOTO1 REPORT                                                           
         MVI   SPACING,1                                                        
         SPACE 1                                                                
GET12    CLI   CBMID,C'Y'                                                       
         BNE   GET14                                                            
         MVI   FORCEMID,C'Y'                                                    
         MVI   CBMID,C'N'                                                       
         SPACE 1                                                                
GET14    MVI   EXTYPE,C'P'                                                      
         BAS   RE,EX2                                                           
         BAS   RE,ANYDUPE                                                       
         GOTO1 REPORT                                                           
         B     GET4                                                             
         EJECT                                                                  
*              ROUTINE TO GET VALUES FROM BUFFALO RECORD                        
         SPACE 3                                                                
EXTOUT   NTR1                                                                   
         L     R2,AKEY                                                          
         LA    R2,3(R2)                                                         
         ZIC   R3,NUMLEVEL                                                      
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         SPACE 1                                                                
XO2      CLI   0(R2),0                                                          
         BE    XO4                                                              
         ZIC   R4,0(R2)                                                         
         BCTR  R4,0                                                             
         MH    R4,=H'3'                                                         
         LA    R4,XVALS(R4)                                                     
         MVC   0(3,R4),1(R2)                                                    
         SPACE 1                                                                
XO4      LA    R2,4(R2)                                                         
         BCT   R3,XO2                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK BUFFALO CBS                                     
         SPACE 3                                                                
CBSET    NTR1                                                                   
         MVI   CBSKIP,C'N'         PRESET VALUES                                
         MVI   CBSPACE,0                                                        
         MVI   CBMID,C'N'                                                       
         L     R2,ACB                                                           
         BAS   RE,NOBUMP                                                        
         B     CBSET4                                                           
         SPACE 1                                                                
CBSET2   BAS   RE,BUMP                                                          
         SPACE 1                                                                
CBSET4   CLI   0(R2),0                                                          
         BNE   CBSET6                                                           
         MVC   LASTBUFF,ROWAREA                                                 
         B     XIT                                                              
         SPACE 1                                                                
CBSET6   CLI   0(R2),X'30'         BROWSE FOR CB ELEMENTS                       
         BNE   CBSET2                                                           
         OC    LASTBUFF,LASTBUFF   NO CB IF FIRST TIME                          
         BZ    CBSET2                                                           
         CLC   ROWAREA+2(1),2(R2)  CHECK REPORT NO. MATCH                       
         BNE   CBSET2                                                           
         ZIC   R3,3(R2)            LEVEL 0-8                                    
         MH    R3,=H'4'                                                         
         LA    R3,2(R3)                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   ROWAREA(0),LASTBUFF                                              
         BE    CBSET2                                                           
         CLI   4(R2),0             SKIP                                         
         BNE   CBSET8                                                           
         MVI   CBSKIP,C'Y'                                                      
         B     CBSET2                                                           
         SPACE 1                                                                
CBSET8   CLI   4(R2),10                                                         
         BNE   CBSET10                                                          
         MVI   CBMID,C'Y'                                                       
         B     CBSET2                                                           
         SPACE 1                                                                
CBSET10  CLI   CBSPACE,0                                                        
         BE    CBSET12                                                          
         CLC   CBSPACE,4(R2)       CHECK HIGHER SPACE HAS                       
         BH    CBSET2                    NOT BEEN SET YET                       
         SPACE 1                                                                
CBSET12  MVC   CBSPACE,4(R2)       SPACE                                        
         B     CBSET2                                                           
         EJECT                                                                  
*              ROUTINES TO CONTROL PRINT SPECS                                  
         SPACE 3                                                                
EX2      NTR1                                                                   
         L     R2,APRINT                                                        
         BAS   RE,NOBUMP                                                        
         B     EX6                                                              
         SPACE 1                                                                
EX4      L     R2,SAVER2                                                        
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
EX6      CLI   0(R2),0                                                          
         BE    XIT                                                              
         ST    R2,SAVER2                                                        
         CLI   0(R2),X'40'                                                      
         BL    EX4                                                              
         CLI   0(R2),X'42'                                                      
         BH    EX4                                                              
         CLC   EXTYPE,2(R2)        MATCH ON H,M,P                               
         BNE   EX4                                                              
         LA    R5,P                POSITION R5 TO SELECTED OUTPUT ADDR.         
         CLI   2(R2),C'P'                                                       
         BE    EX8                                                              
         LA    R5,H1                                                            
         CLI   2(R2),C'H'                                                       
         BE    EX8                                                              
         LA    R5,MID1                                                          
         SPACE 1                                                                
EX8      ZIC   R3,3(R2)            (LINE NO.)                                   
         BCTR  R3,0                                                             
         MH    R3,=H'132'                                                       
         ZIC   R4,4(R2)            (COLUMN NO.)                                 
         BCTR  R4,0                                                             
         AR    R5,R3                                                            
         AR    R5,R4                                                            
         CLI   0(R2),X'42'         HANDLE LITERALS NOW                          
         BNE   EX10                                                             
         ZIC   R3,1(R2)                                                         
         SH    R3,=H'6'                                                         
         EX    R3,*+8                                                           
         B     EX4                                                              
         MVC   0(0,R5),5(R2)                                                    
         EJECT                                                                  
*              BRANCH TO ROUTINES ACCORDING TO KEYWORD                          
         SPACE 3                                                                
EX10     SR    R3,R3               SET R3 WITH OPTIONAL PARAMETER               
         CLI   0(R2),X'40'         SET R4 WITH A(EXTRACT VALUE)                 
         BE    *+8                                                              
         IC    R3,6(R2)                                                         
         ZIC   R4,5(R2)                                                         
         BCTR  R4,0                                                             
         MH    R4,=H'3'                                                         
         LA    R4,XVALS(R4)                                                     
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     EX12(RF)                                                         
         SPACE 1                                                                
EX12     DS    0F                                                               
         B     EX20      1         CLIENT                                       
         B     EX20      2         OFFICE                                       
         B     EX20      3         TARGET                                       
         B     EX20      4         DEMO                                         
         B     EX20      5         DAYPART                                      
         B     EX20      6         TIME                                         
         B     EX20      7         PROGRAM                                      
         B     EX20      8         SERVICE                                      
         B     EX20      9         AFFIL                                        
         B     EX36      10        MONTH                                        
         B     EX36      11        QUARTER                                      
         B     EX4       12        TOTAL                                        
         B     EX4       13        LEVEL                                        
         B     EX20      14        LENGTH                                       
         B     EX4       15        REPORT                                       
         B     EX4       16        ALL                                          
         B     EX44      17        OPT1                                         
         B     EX46      18        OPT2                                         
         B     EX4       19        OPT3                                         
         B     EX48      20        CLINAME                                      
         B     EX4       21        OFFNAME                                      
         B     EX56      22        TARGNAME                                     
         B     EX56      23        DEMONAME                                     
         B     EX64      24        DPNAME                                       
         B     EX70      25        MKTNAME                                      
         B     EX86      26        MKTCODE                                      
         B     EX80      27        MKTRANK                                      
         B     EX82      28        MKTCOVER                                     
         B     EX82      29        MGRCOVER                                     
         B     EX90      30        DATA                                         
         B     EX90      31        STACK                                        
         B     EX98      32        SPACES                                       
         B     EX100     33        SPOTS                                        
         B     EX102     34        DOLLARS                                      
         B     EX104     35        IMPS                                         
         B     EX106     36        GRPS                                         
         B     EX108     37        CPP                                          
         B     EX110     38        CPM                                          
         B     EX112     39        PPS                                          
         B     EX114     40        IPS                                          
         B     EX116     41        CPS                                          
         B     EX118     42        RANGE                                        
         B     EX36      43        HQUARTER                                     
         B     EX4       44        GROUP                                        
         B     EX120     45        QDEMNAME                                     
         B     EX130     46        QTRGNAME                                     
         EJECT                                                                  
*              EXECUTING PRINT SPECS - BASIC KEYWORDS                           
         SPACE 3                                                                
EX20     CLI   0(R4),0                                                          
         BE    EX4                                                              
         CLI   0(R4),X'FF'                                                      
         BNE   EX22                                                             
         MVC   0(5,R5),=C'TOTAL'                                                
         B     EX4                                                              
         SPACE 1                                                                
EX22     MVC   0(1,R5),0(R4)                                                    
         CLI   1(R4),C' '                                                       
         BE    EX24                                                             
         MVC   0(2,R5),0(R4)                                                    
         CLI   2(R4),C' '                                                       
         BE    EX24                                                             
         MVC   0(3,R5),0(R4)                                                    
         SPACE 1                                                                
EX24     CLI   5(R2),6                                                          
         BE    EX26                                                             
         CLI   5(R2),7                                                          
         BE    EX30                                                             
         CLI   5(R2),8                                                          
         BE    EX34                                                             
         B     EX4                                                              
         SPACE 1                                                                
EX26     LA    R3,TIMETAB          EXPAND TIME                                  
         SPACE 1                                                                
EX28     CLI   0(R3),X'FF'                                                      
         BE    EX4                                                              
         CLC   0(1,R3),0(R5)                                                    
         BE    *+12                                                             
         LA    R3,4(R3)                                                         
         B     EX28                                                             
         MVC   0(3,R5),1(R3)                                                    
         B     EX4                                                              
         SPACE 1                                                                
EX30     LA    R3,PROGTAB          EXPAND PROGRAM                               
         SPACE 1                                                                
EX32     CLI   0(R3),X'FF'                                                      
         BE    EX4                                                              
         CLC   0(1,R3),0(R5)                                                    
         BE    *+12                                                             
         LA    R3,7(R3)                                                         
         B     EX32                                                             
         MVC   0(6,R5),1(R3)                                                    
         B     EX4                                                              
         SPACE 1                                                                
EX34     MVC   1(2,R5),=C'RB'      EXPAND SERVICE                               
         CLI   0(R5),C'A'                                                       
         BE    EX4                                                              
         MVC   1(2,R5),=C'SI'                                                   
         B     EX4                                                              
         EJECT                                                                  
*              TABLES FOR PRINT SPECS                                           
         SPACE 3                                                                
TIMETAB  DS    0F                                                               
         DC    C'AEAM'                                                          
         DC    C'CDAY'                                                          
         DC    C'EWEM'                                                          
         DC    C'GWEA'                                                          
         DC    C'JELY'                                                          
         DC    C'LPAC'                                                          
         DC    C'NPRI'                                                          
         DC    C'PLTE'                                                          
         DC    C'RLLT'                                                          
         DC    X'FF'                                                            
         SPACE 1                                                                
PROGTAB  DS    0F                                                               
         DC    C'NNEWS  '                                                       
         DC    C'SSPORTS'                                                       
         DC    C'KKIDS  '                                                       
         DC    C'MMOVIES'                                                       
         DC    C'FFAMILY'                                                       
         DC    C'OOTHERS'                                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
TYPETAB  DS    0F                                                               
         DC    C'SPTS'                                                          
         DC    C'$   '                                                          
         DC    C'IMPS'                                                          
         DC    C'GRPS'                                                          
         DC    C'CPP '                                                          
         DC    C'CPM '                                                          
         DC    C'PPS '                                                          
         DC    C'IPS '                                                          
         DC    C'CPS '                                                          
         SPACE 1                                                                
ROUTTAB  DS    0F                                                               
         DC    A(SPTS)                                                          
         DC    A(DOLS)                                                          
         DC    A(IMPS)                                                          
         DC    A(GRPS)                                                          
         DC    A(CPP)                                                           
         DC    A(CPM)                                                           
         DC    A(PPS)                                                           
         DC    A(IPS)                                                           
         DC    A(CPS)                                                           
         EJECT                                                                  
*              EXECUTING PRINT SPECS - MONTH & QUARTER                          
         SPACE 3                                                                
EX36     LTR   R3,R3                                                            
         BNZ   EX40                                                             
         CLI   0(R4),0                                                          
         BE    EX4                                                              
         CLI   0(R4),X'FF'                                                      
         BNE   EX38                                                             
         MVC   0(5,R5),=C'TOTAL'                                                
         B     EX4                                                              
         SPACE 1                                                                
EX38     ZIC   R1,1(R4)            MONTH/QUARTER FROM RECORDS                   
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R5),0(R1)                                                    
         EDIT  (1,0(R4)),(2,3(R5)),FILL=0                                       
         B     EX4                                                              
         SPACE 1                                                                
EX40     C     R3,NPERIODS         N'TH MONTH/QUARTER                           
         BH    EX4                                                              
         BCTR  R3,0                                                             
         MH    R3,WIDTHPER+2                                                    
         LA    R3,PERTABLE(R3)                                                  
         USING CPERD,R3                                                         
         MVC   0(5,R5),CPSTART     START                                        
         CLI   5(R2),43                                                         
         BE    EX42                                                             
         MVC   132(5,R5),CPEND     QUARTER - SHOW END UNDERNEATH                
         CLC   CPSTART,CPEND                                                    
         BNE   EX4                                                              
         MVC   132(5,R5),=C'-----'                                              
         B     EX4                                                              
         SPACE 1                                                                
EX42     MVI   5(R5),C'-'          HQUARTER- SHOW END ALONGSIDE                 
         MVC   6(5,R5),CPEND                                                    
         B     EX4                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              EXECUTING PRINT SPECS - OPTIONS                                  
         SPACE 3                                                                
EX44     MVC   0(1,R5),QOPT1       OPTION 1                                     
         B     EX4                                                              
         SPACE 1                                                                
EX46     MVC   0(1,R5),QOPT2       OPTION 2                                     
         B     EX4                                                              
         SPACE 1                                                                
EX48     CLI   XCLIENT,0           CLIENT NAME                                  
         BE    EX4                                                              
         CLI   XCLIENT,X'FF'                                                    
         BNE   EX50                                                             
         MVC   4(09,R5),=C'**TOTAL**'                                           
         B     EX4                                                              
         SPACE 1                                                                
EX50     L     R3,ADCLTBUF                                                      
         LA    R3,4(R3)                                                         
         L     R4,NCLTS                                                         
         SPACE 1                                                                
EX52     CLC   XCLIENT,0(R3)                                                    
         BE    EX54                                                             
         AH    R3,WIDTHCLT                                                      
         BCT   R4,EX52                                                          
         MVC   0(3,R5),XCLIENT                                                  
         B     EX4                                                              
         SPACE 1                                                                
EX54     MVC   0(20,R5),3(R3)                                                   
         B     EX4                                                              
         EJECT                                                                  
*              EXECUTING PRINT SPECS - DEMONAME AND TARGET NAME                 
         SPACE 3                                                                
EX56     LTR   R3,R3                                                            
         BNZ   EX58                                                             
         LA    R4,XTARGET          POSITION TO TARGET                           
         CLI   5(R2),22                                                         
         BE    *+8                                                              
         LA    R4,XDEMO                     OR DEMO                             
         CLI   0(R4),X'FF'                                                      
         BNE   *+14                                                             
         MVC   2(5,R5),=C'TOTAL'                                                
         B     EX4                                                              
         BAS   RE,EX60                                                          
         BAS   RE,EX62                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX58     LA    R3,SOFTTAB-1(R3)    SOFT TARGET/DEMO                             
         ZIC   R1,0(R3)                                                         
         LTR   R1,R1                                                            
         BZ    EX4                                                              
         BAS   RE,EX62                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX60     PACK  DUB,0(1,R4)         ROUTINE TO CVB LEFT-ALIGNED NUMS.            
         CLI   1(R4),C' '                                                       
         BE    *+10                                                             
         PACK  DUB,0(2,R4)                                                      
         CLI   2(R4),C' '                                                       
         BE    *+10                                                             
         PACK  DUB,0(3,R4)                                                      
         CVB   R1,DUB                                                           
         BR    RE                                                               
         SPACE 1                                                                
EX62     L     RF,ADDEMBUF         ROUTINE CONVERTS DEM NO TO X(7)              
         BCTR  R1,0                                                             
         MH    R1,=H'7'                                                         
         AR    R1,RF                                                            
         MVC   0(7,R5),0(R1)                                                    
         BR    RE                                                               
         EJECT                                                                  
*              EXECUTING PRINT SPECS - DPNAME & MARKET NAME (GROUP)             
         SPACE 3                                                                
*                                  DAYPART NAME                                 
EX64     MVC   0(3,R5),XDAYPART    PRESET TO X(3) IN CASE NOT FOUND             
         L     R2,ADP                                                           
         BAS   RE,NOBUMP                                                        
         B     EX68                                                             
         SPACE 1                                                                
EX66     BAS   RE,BUMP                                                          
         SPACE 1                                                                
EX68     CLI   0(R2),0                                                          
         BE    EX4                                                              
         CLI   0(R2),X'52'         BROWSE FOR A MATCHING DPNAME SPEC            
         BNE   EX66                                                             
         CLC   2(3,R2),XDAYPART                                                 
         BNE   EX66                                                             
         ZIC   R3,1(R2)                                                         
         SH    R3,=H'6'                                                         
         EX    R3,*+8              FOUND - OUTPUT IT                            
         B     EX4                                                              
         MVC   0(0,R5),5(R2)                                                    
         SPACE 1                                                                
EX70     CLI   ROWAREA+1,0         MARKET NAME                                  
         BNE   EX72                                                             
         MVC   0(24,R5),MKTNAME                                                 
         SPACE 1                                                                
EX70B    LR    R1,R5                                                            
         LA    R0,24                                                            
         SPACE 1                                                                
EX71     CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R1),0                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,EX71                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX72     L     R2,AMKT             OR MARKET GROUP NAME                         
         BAS   RE,NOBUMP                                                        
         B     EX76                                                             
         SPACE 1                                                                
EX74     BAS   RE,BUMP                                                          
         SPACE 1                                                                
EX76     CLI   0(R2),0                                                          
         BE    EX4                                                              
         CLC   ROWAREA+1(1),2(R2)  CHECK MATCH ON GROUP NUMBER                  
         BNE   EX74                                                             
         CLI   0(R2),X'62'                                                      
         BE    EX78                                                             
         CLI   3(R2),C'R'          IF MATCHING R IS FOUND                       
         BNE   EX74                   PRE-OUTPUT GROUP NAME                     
         CLI   0(R5),C' '                                                       
         BNE   EX74                                                             
         MVC   0(24,R5),=C'MARKETS RANKED NNN - NNN'                            
         EDIT  (2,4(R2)),(3,15(R5)),ALIGN=LEFT                                  
         EDIT  (2,6(R2)),(3,21(R5)),ALIGN=LEFT                                  
         GOTO1 SQUASHER,PARAS,(R5),24                                           
         B     EX70B                                                            
         SPACE 1                                                                
EX78     MVC   0(24,R5),SPACES                                                  
         ZIC   R3,1(R2)                                                         
         SH    R3,=H'4'                                                         
         EX    R3,*+8                                                           
         B     EX4                                                              
         MVC   0(0,R5),3(R2)                                                    
         EJECT                                                                  
*              EXECUTING PRINT SPECS - MARKET RANK & COVER                      
         SPACE 3                                                                
EX80     CLI   ROWAREA+1,0         MARKET RANK                                  
         BNE   EX4                                                              
         EDIT  (2,MKTRANK),(3,0(R5)),ALIGN=LEFT                                 
         B     EX4                                                              
         SPACE 1                                                                
EX82     CLI   ROWAREA+1,0         MARKET COVER                                 
         BNE   EX84                                                             
         MVC   DUB,MKTWT                                                        
         LH    R0,DUB                                                           
         SPACE 1                                                                
EX83     SRDA  R0,32                                                            
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
         EDIT  (R1),(5,0(R5)),2,ALIGN=LEFT                                      
         B     EX4                                                              
         SPACE 1                                                                
EX84     ZIC   R0,ROWAREA+1        MARKET GROUP COVER                           
         BCTR  R0,0                                                             
         MH    R0,=H'36'                                                        
         L     R1,=A(COVERC)                                                    
         A     R1,RELO                                                          
         AR    R1,R0                                                            
         L     R0,32(R1)                                                        
         B     EX83                                                             
         SPACE 1                                                                
EX86     CLI   ROWAREA+1,0         MKTCODE                                      
         BNE   EX4                                                              
         EDIT  (2,MARKET),(3,0(R5)),ALIGN=LEFT                                  
         B     EX4                                                              
         EJECT                                                                  
*              EXECUTING PRINT SPECS - DATA & STACK                             
         SPACE 3                                                                
EX90     LA    R6,QDATATYP                                                      
         LA    R4,9                                                             
         SPACE 1                                                                
EX92     CLI   0(R6),C' '                                                       
         BE    EX4                                                              
         PACK  DUB,0(1,R6)                                                      
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         CLI   5(R2),31                                                         
         BE    EX94                                                             
         LA    R1,TYPETAB(R1)                                                   
         MVC   0(4,R5),0(R1)                                                    
         B     EX96                                                             
         SPACE 1                                                                
EX94     L     RF,ROUTTAB(R1)                                                   
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         SPACE 1                                                                
EX96     CLI   INDEXOPT,0          ENSURE PRINTING OF INDEX LINES               
         BE    EX97                                                             
         CLI   P2,C' '                                                          
         BNE   EX97                                                             
         MVI   P2,0                                                             
         SPACE 1                                                                
EX97     LA    R5,132(R5)          BUMP TO NEXT LINE                            
         CLI   INDEXOPT,0                                                       
         BE    *+8                                                              
         LA    R5,132(R5)          OR 1 MORE IF INDEXING IS ACTIVE              
         LA    R6,1(R6)                                                         
         BCT   R4,EX92                                                          
         B     EX4                                                              
         EJECT                                                                  
*              EXECUTING PRINT SPECS - SPACES AND NUMBERS AND RANGE             
         SPACE 3                                                                
EX98     LTR   R3,R3               SPACES                                       
         BNZ   *+8                                                              
         LA    R3,132                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     EX4                                                              
         MVC   0(0,R5),SPACES                                                   
         SPACE 1                                                                
EX100    BAS   RE,SPTS                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX102    BAS   RE,DOLS                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX104    BAS   RE,IMPS                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX106    BAS   RE,GRPS                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX108    BAS   RE,CPP                                                           
         B     EX4                                                              
         SPACE 2                                                                
EX110    BAS   RE,CPM                                                           
         B     EX4                                                              
         SPACE 1                                                                
EX112    BAS   RE,PPS                                                           
         B     EX4                                                              
         SPACE 1                                                                
EX114    BAS   RE,IPS                                                           
         B     EX4                                                              
         SPACE 1                                                                
EX116    BAS   RE,CPS                                                           
         B     EX4                                                              
         SPACE 1                                                                
EX118    MVC   0(9,R5),=C'BANK DATA'                                            
         CLI   QRANGE,C'B'                                                      
         BE    EX4                                                              
         MVC   0(11,R5),=C'AGENCY DATA'                                         
         CLI   QRANGE,C'A'                                                      
         BE    EX4                                                              
         MVC   0(20,R5),CLTNM                                                   
         B     EX4                                                              
         SPACE 1                                                                
EX120    MVC   0(23,R5),=C'REPORTING TARGET DEMOS.'                             
         CLI   QOPT3,C'Y'                                                       
         BE    EX4                                                              
         MVC   0(25,R5),=C'REQUESTED DEMO. - ALL  '                             
         CLC   QSELECT,SPACES                                                   
         BE    EX4                                                              
         PACK  DUB,QSELECT                                                      
         CVB   R1,DUB                                                           
         LA    R5,18(R5)                                                        
         BAS   RE,EX62                                                          
         B     EX4                                                              
         SPACE 1                                                                
EX130    MVC   0(22,R5),=C'REQUESTED TARGET - ALL'                              
         CLC   QTARGET,SPACES                                                   
         BE    EX4                                                              
         PACK  DUB,QTARGET                                                      
         CVB   R1,DUB                                                           
         LA    R5,19(R5)                                                        
         BAS   RE,EX62                                                          
         B     EX4                                                              
         EJECT                                                                  
*              ROUTINES FOR FORMATTING NUMERIC FIELDS                           
         SPACE 1                                                                
SPTS     NTR1                                                                   
         BAS   RE,LSPTS            SPOTS                                        
         EDIT  (R1),(5,0(R5))                                                   
         CLI   5(R2),31                                                         
         BNE   SPTS2                                                            
         EDIT  (R1),(7,0(R5))                                                   
         SPACE 1                                                                
SPTS2    LA    R1,1                                                             
         B     INDX                                                             
         SPACE 1                                                                
DOLS     NTR1                                                                   
         BAS   RE,LDOLS            DOLLARS                                      
         EDIT  (R1),(9,DMCB)                                                    
         MVC   0(7,R5),DMCB+2                                                   
         CLI   DMCB+1,C' '                                                      
         BE    DOLS2                                                            
         SH    R5,=H'2'                                                         
         MVC   0(2,R5),DMCB                                                     
         LA    R5,2(R5)                                                         
         SPACE 1                                                                
DOLS2    LA    R1,2                                                             
         B     INDX                                                             
         SPACE 2                                                                
IMPS     NTR1                                                                   
         BAS   RE,LIMPS            IMPRESSIONS                                  
         EDIT  (R1),(7,0(R5))                                                   
         LA    R1,3                                                             
         B     INDX                                                             
         SPACE 2                                                                
GRPS     NTR1                                                                   
         BAS   RE,LGRPS            GRPS                                         
         EDIT  (R1),(5,0(R5))                                                   
         CLI   5(R2),31                                                         
         BNE   GRPS2                                                            
         EDIT  (R1),(7,0(R5))                                                   
         SPACE 2                                                                
GRPS2    LA    R1,4                                                             
         B     INDX                                                             
         SPACE 2                                                                
CPP      NTR1                                                                   
         BAS   RE,LCPP             SEE IF CPP IS THERE ALREADY                  
         BNZ   CPP1                                                             
         BAS   RE,LDOLS            CPP                                          
         BZ    CPP4                                                             
         LR    RF,R1                                                            
         BAS   RE,LGRPS2                                                        
         BZ    CPP4                                                             
         M     RE,=F'2000'                                                      
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         SPACE 1                                                                
CPP1     CLI   5(R2),31                                                         
         BNE   CPP2                                                             
         EDIT  (RF),(7,0(R5)),2                                                 
         SH    R5,=H'2'                                                         
         CLC   0(2,R5),SPACES                                                   
         LA    R5,2(R5)                                                         
         BNE   CPP4                                                             
         SH    R5,=H'2'                                                         
         EDIT  (RF),(9,0(R5)),2                                                 
         LA    R5,2(R5)                                                         
         B     CPP4                                                             
         SPACE 2                                                                
CPP2     EDIT  (RF),(5,0(R5)),2                                                 
         CH    RF,=H'9999'                                                      
         BNH   CPP4                                                             
         EDIT  (RF),(7,PARAS)                                                   
         MVC   0(5,R5),PARAS                                                    
         SPACE 2                                                                
CPP4     LA    R1,5                                                             
         B     INDX                                                             
         SPACE 2                                                                
CPM      NTR1                                                                   
         BAS   RE,LDOLS            CPM                                          
         BZ    CPM4                                                             
         LR    RF,R1                                                            
         BAS   RE,LIMPS                                                         
         BZ    CPM4                                                             
         M     RE,=F'200'                                                       
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         CLI   5(R2),31                                                         
         BNE   CPM2                                                             
         EDIT  (RF),(7,0(R5)),2                                                 
         B     CPM4                                                             
         SPACE 2                                                                
CPM2     EDIT  (RF),(5,0(R5)),2                                                 
         CH    RF,=H'9999'                                                      
         BNH   CPM4                                                             
         EDIT  (RF),(7,PARAS)                                                   
         MVC   0(5,R5),PARAS                                                    
         SPACE 2                                                                
CPM4     LA    R1,6                                                             
         B     INDX                                                             
         SPACE 2                                                                
PPS      NTR1                                                                   
         BAS   RE,LGRPS            PPS                                          
         BZ    PPS2                                                             
         LR    RF,R1                                                            
         BAS   RE,LSPTS                                                         
         BZ    PPS2                                                             
         M     RE,=F'20'                                                        
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(4,0(R5)),1                                                 
         CLI   5(R2),31                                                         
         BNE   PPS2                                                             
         EDIT  (RF),(7,0(R5)),1                                                 
         SPACE 2                                                                
PPS2     LA    R1,7                                                             
         B     INDX                                                             
         SPACE 2                                                                
IPS      NTR1                                                                   
         BAS   RE,LIMPS            IPS                                          
         BZ    IPS2                                                             
         LR    RF,R1                                                            
         BAS   RE,LSPTS                                                         
         BZ    IPS2                                                             
         M     RE,=F'2'                                                         
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         EDIT  (RF),(5,0(R5))                                                   
         CLI   5(R2),31                                                         
         BNE   IPS2                                                             
         EDIT  (RF),(7,0(R5))                                                   
         SPACE 2                                                                
IPS2     LA    R1,8                                                             
         B     INDX                                                             
         SPACE 2                                                                
CPS      NTR1                                                                   
         BAS   RE,LCPS                                                          
         BNZ   CPS1                                                             
         BAS   RE,LDOLS            CPS                                          
         BZ    CPS2                                                             
         LR    RF,R1                                                            
         BAS   RE,LSPTS                                                         
         BZ    CPS2                                                             
         M     RE,=F'2'                                                         
         DR    RE,R1                                                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         SPACE 1                                                                
CPS1     EDIT  (RF),(5,0(R5))                                                   
         CLI   5(R2),31                                                         
         BNE   CPS2                                                             
         EDIT  (RF),(7,0(R5))                                                   
         SPACE 2                                                                
CPS2     LA    R1,9                                                             
         B     INDX                                                             
         EJECT                                                                  
*              SUBSIDIARY ROUTINES FOR NUMBERIC SPECS                           
         SPACE 3                                                                
LSPTS    LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     R1,0(R1)                                                         
         LTR   R1,R1                                                            
         BR    RE                                                               
         SPACE 2                                                                
LDOLS    LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     R1,4(R1)                                                         
         LTR   R1,R1                                                            
         BR    RE                                                               
         SPACE 2                                                                
LGRPS    LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     R1,8(R1)                                                         
         SR    R0,R0                                                            
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
         LTR   R1,R1                                                            
         BR    RE                                                               
         SPACE 2                                                                
LGRPS2   LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     R1,8(R1)                                                         
         LTR   R1,R1                                                            
         BR    RE                                                               
         SPACE 2                                                                
LIMPS    LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     R1,12(R1)                                                        
         LTR   R1,R1                                                            
         BR    RE                                                               
         SPACE 1                                                                
LCPP     LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     RF,16(R1)                                                        
         CLI   PROGPROF,C'Y'       ONLY VALID IF PROFILE IS SET                 
         BE    *+6                                                              
         SR    RF,RF                                                            
         LTR   RF,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
LCPS     LR    R1,R3                                                            
         BCTR  R1,0                                                             
         MH    R1,=H'24'                                                        
         A     R1,ACOL1                                                         
         L     RF,20(R1)                                                        
         CLI   PROGPROF,C'Y'       ONLY VALID IF PROFILE IS SET                 
         BE    *+6                                                              
         SR    RF,RF                                                            
         LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
*              INDEX ROUTINES  - SET UP COLUMN NUMBER OF BASE                   
         SPACE 3                                                                
SETIX    NTR1                                                                   
         MVI   INDEXOPT,0          SEE IF INDEX IS ACTIVE                       
         CLC   QINDEX,SPACES                                                    
         BE    XIT                                                              
         CLI   QPROJECT,C' '                                                    
         BNE   XIT                                                              
         PACK  DUB,QINDEX(2)                                                    
         CVB   R1,DUB                                                           
         PACK  DUB,QINDEX+2(2)                                                  
         CVB   R0,DUB                                                           
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         LA    R2,PERTABLE                                                      
         L     R3,NPERIODS                                                      
         LA    R4,1                                                             
         SPACE 2                                                                
         USING CPERD,R2                                                         
SETIX2   CLC   CPSTARTB,DUB                                                     
         BE    SETIX4                                                           
         A     R2,WIDTHPER                                                      
         LA    R4,1(R4)                                                         
         BCT   R3,SETIX2                                                        
         B     XIT                                                              
         SPACE 2                                                                
SETIX4   STC   R4,INDEXOPT                                                      
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              INDEX ROUTINES - COMPUTE AND EDIT INDEX                          
         SPACE 3                                                                
INDX     BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,INDXTAB(R1)                                                   
         ZIC   R4,INDEXOPT                                                      
         LTR   R4,R4               R3=DATA MONTH                                
         BZ    XIT                 R4=INDEX MONTH                               
         ST    R5,SAVER5                                                        
         ZIC   R5,0(R1)            R5=MULTIPLIER                                
         ZIC   R6,1(R1)            R6=DIVIDER                                   
         MVC   IXDISP,2(R1)                                                     
         LR    R0,R3               GET DATA MULTIPLIER                          
         LR    R1,R5                                                            
         BAS   RE,INDX2                                                         
         ST    R1,WORK                                                          
         LR    R0,R4               GET INDEX MULTPLIER                          
         LR    R1,R5                                                            
         BAS   RE,INDX2                                                         
         ST    R1,WORK+4                                                        
         XC    WORK+8(8),WORK+8                                                 
         LTR   R6,R6                                                            
         BZ    INDXA                                                            
         LR    R0,R3               GET DATA DIVIDER                             
         LR    R1,R6                                                            
         BAS   RE,INDX2                                                         
         ST    R1,WORK+8                                                        
         LR    R0,R4               GET INDEX DIVIDER                            
         LR    R1,R6                                                            
         BAS   RE,INDX2                                                         
         ST    R1,WORK+12                                                       
         SPACE 2                                                                
INDXA    L     R1,WORK             NOW WORK OUT PERCENTAGE                      
         M     R0,=F'200'                                                       
         OC    WORK+4(4),WORK+4                                                 
         BZ    XIT                                                              
         D     R0,WORK+4           DM/IM                                        
         OC    WORK+12(4),WORK+12  AND IF A DIVIDER IS ACTIVE                   
         BZ    INDXB                                                            
         M     R0,WORK+12          X ID                                         
         OC    WORK+8(4),WORK+8                                                 
         BZ    XIT                                                              
         D     R0,WORK+8           / DD                                         
         SPACE 2                                                                
INDXB    AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
         ZIC   R3,IXDISP           GET DISPLACEMENT                             
         CLI   5(R2),31                                                         
         BNE   *+8                                                              
         LA    R3,3                                                             
         L     R5,SAVER5                                                        
         LA    R3,132(R3,R5)       POSITION TO LINE BELOW                       
         EDIT  (R1),(4,(R3))                                                    
         CH    R1,=H'9999'                                                      
         BNH   XIT                                                              
         MVC   0(4,R3),=C'HIGH'                                                 
         B     XIT                                                              
         SPACE 2                                                                
INDX2    L     RF,ACOL1            GET DATA FROM                                
         BCTR  R0,0                                                             
         MH    R0,=H'24'               MONTH (R0)                               
         AR    RF,R0                                                            
         BCTR  R1,0                                                             
         SLL   R1,2                    DATA (R1)                                
         AR    RF,R1                                                            
         L     R1,0(RF)                                                         
         BR    RE                                                               
         SPACE 2                                                                
INDXTAB  DC    AL1(1,0,1)          (MULTIPLIER,DIVIDER,DISPLACEMENT)            
         DC    AL1(2,0,3)                                                       
         DC    AL1(3,0,3)                                                       
         DC    AL1(4,0,1)                                                       
         DC    AL1(2,3,1)                                                       
         DC    AL1(2,4,1)                                                       
         DC    AL1(3,1,0)                                                       
         DC    AL1(4,1,1)                                                       
         DC    AL1(2,1,1)                                                       
         EJECT                                                                  
*              ROUTINE TO CHECK FOR DUPLICATES IN PRINT LINES                   
         SPACE 3                                                                
ANYDUPE  NTR1                                                                   
         MVC   THISP,P                                                          
         LA    R2,P                                                             
         LA    R3,132                                                           
         SPACE 2                                                                
ANYDUPE2 CLI   0(R2),C' '                                                       
         BNE   ANYDUPE4                                                         
         LR    R4,R2                                                            
         LA    R5,P                                                             
         SR    R4,R5                                                            
         BZ    ANYDUPE4                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   THISP(0),LASTP                                                   
         BNE   ANYDUPE6                                                         
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   P(0),SPACES                                                      
         SPACE 2                                                                
ANYDUPE4 LA    R2,1(R2)                                                         
         BCT   R3,ANYDUPE2                                                      
         SPACE 2                                                                
ANYDUPE6 MVC   LASTP,THISP                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DIG OUT XTRACT VALUES                                 
         SPACE 3                                                                
DIGX     NTR1                                                                   
         MVC   XVALS,SPACES                                                     
         MVC   XCLIENT,CLIENT                                                   
         MVC   XOFFICE(1),CLTOFF                                                
         L     R4,ADDATA                                                        
         USING CPKEYD,R4                                                        
         EDIT  (1,CPKTARGT),(3,XTARGET),ALIGN=LEFT                              
         EDIT  (1,CPKDEMO),(3,XDEMO),ALIGN=LEFT                                 
         MVC   XDAYPART(1),CPKDAYPT                                             
         MVC   XTIME(1),CPKDAYPT                                                
         MVC   XPROGRAM(1),CPKPROG                                              
         MVC   XSERVICE(1),CPKSERV                                              
         MVC   XAFFIL(1),CPKAFFIL                                               
         MVC   XOPT1(1),QOPT1                                                   
         MVC   XOPT2(2),QOPT2                                                   
         MVC   XOPT3(3),QOPT3                                                   
         EDIT  (1,CPKSPTLN),(3,XLENGTH),ALIGN=LEFT                              
         L     R2,AKEY                                                          
         EDIT  (1,0(R2)),(3,XREPORT),ALIGN=LEFT                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO BUMP TO NEXT ELEMENT AND HANDLE                       
*              ANY APPLY ELEMENTS FOUND                                         
         SPACE 3                                                                
BUMP     NTR1                                                                   
         SPACE 2                                                                
BUMP2    ZIC   R3,1(R2)                                                         
         AR    R2,R3                                                            
         SPACE 2                                                                
BUMP3    CLI   0(R2),0             END OF LIST                                  
         BNE   BUMP4                                                            
         MVI   APPSW,C'Y'          RELEASE APPLY                                
         B     BUMPEX                                                           
         SPACE 2                                                                
BUMP4    CLI   0(R2),X'1F'                                                      
         BE    BUMP6                                                            
         CLI   APPSW,C'N'          DONT PASS BACK ELEMENTS                      
         BE    BUMP2               IF APPLY IS OFF                              
         SPACE 2                                                                
BUMPEX   XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
BUMP6    CLI   2(R2),C'E'          IS THIS AN APPLY END                         
         BNE   BUMP8                                                            
         MVI   APPSW,C'Y'          YES - RELEASE APPLY                          
         B     BUMP2                                                            
         SPACE 2                                                                
BUMP8    CLI   2(R2),C'O'                                                       
         BE    BUMP12                                                           
         CLI   2(R2),C'I'          IF STATEMENTS TURN APPLY OFF                 
         BE    BUMP9                                                            
         CLI   2(R2),C'N'                                                       
         BNE   BUMP10                                                           
         SPACE 2                                                                
BUMP9    MVI   APPSW,C'N'                                                       
         B     BUMP12                                                           
         SPACE 2                                                                
BUMP10   CLI   APPSW,C'N'          AND STATEMENTS - IGNORE IF                   
         BE    BUMP2               PREVIOUS CONDITION FAILED                    
         SPACE 2                                                                
BUMP12   LA    R4,3(R2)                                                         
         BAS   RE,IF               TEST CONDITION                               
         CLI   2(R2),C'N'                                                       
         BE    BUMP14                                                           
         CLI   IFSW,C'Y'           IF OK,                                       
         BNE   BUMP2                                                            
         MVI   APPSW,C'Y'          RELEASE APPLY                                
         B     BUMP2                                                            
         SPACE 2                                                                
BUMP14   CLI   IFSW,C'Y'           NOT EXPRESSIONS CHECK THE OPPOSITE           
         BE    BUMP2                                                            
         MVI   APPSW,C'Y'          RELEASE APPLY                                
         B     BUMP2                                                            
         SPACE 2                                                                
         SPACE 2                                                                
NOBUMP   NTR1                                                                   
         B     BUMP3                                                            
         EJECT                                                                  
*              HANDLE IF CONDITION                                              
         SPACE 3                                                                
IF       NTR1                                                                   
*                                  R4=A(KVVV) K=KEYWORD, VVV=VALUE              
         SPACE 1                                                                
         CLI   0(R4),5             DAYPART IS TRICKY                            
         BE    IF2                                                              
         CLI   0(R4),44            GROUP HAS SPECIAL SIGNIFICANCE               
         BE    IF20                                                             
         CLI   0(R4),16                                                         
         BE    IFYES                                                            
         ZIC   R3,0(R4)            ROUTINE FOR REST IS TO BUMP INTO             
         BCTR  R3,0                EXTRACT TABLE AND CHECK FOR MATCH            
         MH    R3,=H'3'                                                         
         LA    R3,XVALS(R3)                                                     
         CLC   0(3,R3),1(R4)                                                    
         BE    IFYES                                                            
         B     IFNO                                                             
         SPACE 2                                                                
IF2      L     R2,ADP              SPECIAL DAYPART TESTS                        
         B     IF6                                                              
         SPACE 2                                                                
IF4      BAS   RE,BUMP                                                          
         SPACE 2                                                                
IF6      CLI   0(R2),0                                                          
         BE    IFNO                                                             
         CLI   0(R2),X'50'         FIRST FIND DAYPART LIST FOR                  
         BNE   IF4                       SPECIFIED CODE                         
         CLC   2(3,R2),1(R4)                                                    
         BNE   IF4                                                              
         ZIC   R3,1(R2)            R3=N'ITEMS IN LIST                           
         SH    R3,=H'5'                                                         
         LR    R1,R3                                                            
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         LR    R3,R1                                                            
         LA    R5,5(R2)            R5=A(LIST ITEM)                              
         SPACE 2                                                                
IF8      CLI   0(R5),C'*'          CHECK FOR MATCH ON TIME CODE                 
         BE    IF12                *=OK                                         
         CLI   1(R5),C'-'          A- = ALL BUT A                               
         BE    IF10                                                             
         CLC   0(1,R5),XTIME       CHECK FOR MATCH                              
         BE    IF12                                                             
         B     IF16                                                             
         SPACE 2                                                                
IF10     CLC   0(1,R5),XTIME       CHECK FOR ALL BUT                            
         BE    IF16                                                             
         SPACE 2                                                                
IF12     CLI   1(R5),C'*'          CHECK FOR MATCH ON PROGRAM                   
         BE    IFYES                                                            
         CLI   0(R5),C'-'          -B = ALL BUT B                               
         BE    IF14                                                             
         CLC   1(1,R5),XPROGRAM    CHECK FOR MATCH                              
         BE    IF18                                                             
         B     IF16                                                             
         SPACE 2                                                                
IF14     CLC   1(1,R5),XPROGRAM    CHECK FOR ALL BUT                            
         BNE   IF18                                                             
         SPACE 2                                                                
IF16     LA    R5,3(R5)                                                         
         BCT   R3,IF8                                                           
         B     IFNO                                                             
         SPACE 2                                                                
IF18     CLI   2(R5),C'*'          CHECK FOR MATCH ON AFFIL                     
         BE    IFYES                                                            
         CLC   2(1,R5),XAFFIL                                                   
         BE    IFYES                                                            
         B     IFNO                                                             
         SPACE 2                                                                
IF20     CLC   1(3,R4),=C'ALL'                                                  
         BE    IF22                                                             
         PACK  DUB,1(1,R4)                                                      
         CLI   2(R4),C' '                                                       
         BE    *+10                                                             
         PACK  DUB,1(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,DUB                                                           
         CLC   ROWAREA+1(1),DUB    MATCH ON GROUP NO.                           
         BE    IFYES                                                            
         B     IFNO                                                             
         SPACE 2                                                                
IF22     CLI   ROWAREA+1,0                                                      
         BE    IFNO                                                             
         SPACE 2                                                                
IFYES    MVI   IFSW,C'Y'                                                        
         B     XIT                                                              
         SPACE 2                                                                
IFNO     MVI   IFSW,C'N'                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         USING *,RF                                                             
HOOK     NTR1                                                                   
         MVI   EXTYPE,C'H'                                                      
         L     RB,CP40RB                                                        
         B     ALLHOOK                                                          
         SPACE 2                                                                
         USING *,RF                                                             
MYMID    NTR1                                                                   
         MVI   EXTYPE,C'M'                                                      
         L     RB,CP40RB                                                        
         SPACE 2                                                                
ALLHOOK  DS    0H                                                               
         DROP  RF                                                               
         USING CP4002,RB,R8,R7                                                  
         LA    R8,2048(RB)                                                      
         LA    R8,2048(R8)                                                      
         LA    R7,2048(R8)                                                      
         LA    R7,2048(R7)                                                      
         LM    RA,RC,CP40RA                                                     
         L     R9,=A(USERAREA)                                                  
         A     R9,RELO                                                          
         BAS   RE,BOXSET                                                        
         BAS   RE,EX2                                                           
         CLC   P,SPACES                                                         
         BE    ALLHOOK2                                                         
         MVC   P,THISP                                                          
         B     XIT                                                              
         SPACE 2                                                                
ALLHOOK2 MVC   LASTP,SPACES                                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP BOXES IF APPROPRIATE                           
         SPACE 3                                                                
BOXSET   NTR1                                                                   
         USING BOXD,R6                                                          
         L     R6,ABOX                                                          
         CLI   ANYBOX,0                                                         
         BNE   XIT                                                              
         MVI   ANYBOX,C'N'                                                      
         L     R2,APRINT                                                        
         BAS   RE,NOBUMP                                                        
         B     BS4                                                              
         SPACE 1                                                                
BS2      L     R2,SAVER2                                                        
         BAS   RE,BUMP                                                          
         SPACE 1                                                                
BS4      CLI   0(R2),0                                                          
         BE    XIT                                                              
         ST    R2,SAVER2                                                        
         CLI   2(R2),C'T'                                                       
         BE    BS6                                                              
         CLI   2(R2),C'B'                                                       
         BE    BS8                                                              
         B     BS2                                                              
         SPACE 1                                                                
BS6      ZIC   R1,4(R2)            TOP BOX SPEC (COL NO)                        
         BCTR  R1,0                                                             
         LA    R1,BOXCOLS(R1)                                                   
         ZIC   R3,1(R2)                                                         
         SH    R3,=H'6'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),5(R2)                                                    
         ZIC   R1,3(R2)                                                         
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'T'                                                       
         B     BS2                                                              
         SPACE 1                                                                
BS8      ZIC   R1,3(R2)            BOTTOM BOX SPEC                              
         LA    R1,BOXROWS-1(R1)                                                 
         MVI   0(R1),C'M'          ACTUALLY MID                                 
         MVI   BOXROWS+58,C'B'     BOTTOM IS FIXED                              
         MVI   ANYBOX,C'Y'                                                      
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         B     XIT                                                              
         SPACE 1                                                                
ANYBOX   DC    X'00'                                                            
         EJECT                                                                  
*              ASSORTED WORK SPACE, CONSTANTS                                   
         SPACE 3                                                                
PARAS    DS    6F                                                               
CP40RA   DS    F                                                                
CP40RB   DS    F                                                                
CP40RC   DS    F                                                                
SAVER2   DS    F                                                                
SAVER5   DS    F                                                                
ABUFF    DS    F                                                                
VTAPE    DC    F'0'                                                             
ACTPHASE DC    CL8' '                                                           
IFSW     DC    C'N'                                                             
NUMLEVEL DC    X'00'                                                            
NUMCOLS  DC    X'00'                                                            
AKEY     DS    A                                                                
ACOL1    DS    A                                                                
APPSW    DC    C'Y'                                                             
COLSW    DC    X'00'                                                            
COLTAB   DS    CL16                                                             
SOFTTAB  DS    CL16                                                             
EXTYPE   DS    CL1                                                              
CBSKIP   DS    CL1                                                              
CBSPACE  DS    CL1                                                              
CBMID    DS    CL1                                                              
LASTSPEC DC    XL5'00'                                                          
THISP    DC    CL132' '                                                         
LASTP    DC    CL132' '                                                         
LASTBUFF DC    XL30'00'                                                         
INDEXOPT DC    AL1(0)                                                           
IXDISP   DC    AL1(0)                                                           
ROWAREA  DS    CL60                                                             
SUMACT   DS    CL1                                                              
XVALS    DS    0CL57                                                            
XCLIENT  DS    CL3                                                              
XOFFICE  DS    CL3                                                              
XTARGET  DS    CL3                                                              
XDEMO    DS    CL3                                                              
XDAYPART DS    CL3                                                              
XTIME    DS    CL3                                                              
XPROGRAM DS    CL3                                                              
XSERVICE DS    CL3                                                              
XAFFIL   DS    CL3                                                              
XMONTH   DS    CL3                                                              
XQUARTER DS    CL3                                                              
XTOTAL   DS    CL3                                                              
XLEVEL   DS    CL3                                                              
XLENGTH  DS    CL3                                                              
XREPORT  DS    CL3                                                              
XALL     DS    CL3                                                              
XOPT1    DS    CL3                                                              
XOPT2    DS    CL3                                                              
XOPT3    DS    CL3                                                              
AWORK    DS    A                                                                
AWORKIO  DS    A                                                                
ID       DC    XL16'00'                                                         
RELOC    DC    A(*)                                                             
RELO     DS    A                                                                
         EJECT                                                                  
*              LTORG                                                            
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER TABLE AREA                                        
         SPACE 3                                                                
USERD    DSECT                                                                  
AROW     DS    A                                                                
ACOL     DS    A                                                                
ACB      DS    A                                                                
APRINT   DS    A                                                                
ADP      DS    A                                                                
AMKT     DS    A                                                                
         PRINT OFF                                                              
       ++INCLUDE CPGENFILE                                                      
       ++INCLUDE CPREPMODES                                                     
       ++INCLUDE CPREPWORKD                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
         SPACE 3                                                                
CP4002B  CSECT                                                                  
         ENTRY WORKIO                                                           
         ENTRY BUFFREC                                                          
         ENTRY WORKERC                                                          
         ENTRY COVERC                                                           
         ENTRY USERAREA                                                         
         SPACE 1                                                                
USERAREA DC    20000X'00'                                                       
WORKIO   DS    F                                                                
BUFFREC  DS    360C                                                             
         DS    CL288                                                            
         BUFF  LINES=400,                                              X        
               ROWS=1,                                                 X        
               COLUMNS=80,                                             X        
               FLAVOR=BINARY,                                          X        
               KEYLIST=(30,1)                                                   
         SPACE 2                                                                
COVERC   DC    3600X'00'                                                        
WORKERC  DC    4500X'00'                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055CPREP4002 05/01/02'                                      
         END                                                                    
