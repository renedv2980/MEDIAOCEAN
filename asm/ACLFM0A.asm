*          DATA SET ACLFM0A    AT LEVEL 006 AS OF 05/01/02                      
*PHASE T6030AA,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
         TITLE 'MODULE TO HANDLE JOB OPENING/CLOSING'                           
T6030A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LFMA**,R7,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         LA    R9,SAVEHEIR                                                      
         USING ACHEIRD,R9                                                       
         ST    R5,PRELOC                                                        
*                                                                               
         CLI   COMPANY,X'7E'       SPECIAL CODE FOR FCB                         
         BE    JB0                                                              
         CLI   COMPANY,X'79'                                                    
         BNE   JB1                                                              
*                                                                               
JB0      CLI   LOGREC,C'J'                                                      
         BNE   JB1                                                              
         MVI   ERROR,INVALID                                                    
         LA    R2,LOGACTH                                                       
         CLI   LOGACT,C'N'                                                      
         BE    XIT                                                              
         MVI   ERROR,X'FF'                                                      
         EJECT                                                                  
*              BUILD KEY FOR JOB                                                
         SPACE 3                                                                
JB1      CLI   MODE,BUILDKEY                                                    
         BNE   JB20                                                             
         BAS   RE,ANYCOMP                                                       
         MVC   SAVEBUDG,=2PL6'0'                                                
         FOUT  LOGCNAMH,SPACES,36                                               
         FOUT  LOGPNAMH,SPACES,36                                               
         LA    RF,SAVECOMP                                                      
         USING ACCOMPD,RF                                                       
         MVC   KEY,COMPANY         CLIENT                                       
         MVC   KEY+1(2),ACMPJOB                                                 
         LA    R2,LOGCLIH                                                       
         DROP  RF                                                               
         GOTO1 ANY                                                              
         GOTO1 MOVE                                                             
         CLC   WORK(6),SPACES                                                   
         BE    MISSCODE                                                         
         MVC   KEY+3(12),WORK                                                   
         CLC   5(1,R2),ACHRLEVA                                                 
         BNH   JB2                                                              
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
JB2      GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         LA    R4,IO                                                            
         LA    R6,CLIUNIT                                                       
         MVI   PROUNIT,0                                                        
         MVI   JOBUNIT,0                                                        
         BAS   RE,ANYUNIT                                                       
*                                  WITH UNAPPROVED ESTIMATE STATUS              
         TM    4(R2),X'20'                                                      
         BO    JB4                                                              
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         SPACE 2                                                                
JB4      LA    R2,LOGPRODH         PRODUCT                                      
         GOTO1 ANY                                                              
         SR    R3,R3                                                            
         IC    R3,ACHRLEVA                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVB                                                      
         LA    R5,KEY+3(R3)                                                     
         SR    R1,R3                                                            
         LR    R3,R1                                                            
         STC   R3,WORK                                                          
         CLC   5(1,R2),WORK                                                     
         BNH   JB6                                                              
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         SPACE 2                                                                
JB6      GOTO1 MOVE                                                             
         CLC   WORK(6),SPACES                                                   
         BE    MISSCODE                                                         
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
         LA    R4,IO                                                            
         MVI   PROUNIT,0                                                        
         MVI   JOBUNIT,0                                                        
         LA    R6,PROUNIT                                                       
         BAS   RE,ANYUNIT                                                       
         TM    4(R2),X'20'                                                      
         BO    JB8                                                              
         MVI   ANYKEY,C'Y'                                                      
         OI    4(R2),X'20'                                                      
         SPACE 2                                                                
JB8      LA    R2,LOGJOBH          JOB                                          
         GOTO1 ANY                                                              
         CLI   8(R2),C' '          DOES JOB START WITH A BLANK ?                
         BNE   JB8A                NO                                           
         MVI   ERROR,INVALID       YES, ERROR                                   
         B     XIT                                                              
         SPACE 2                                                                
JB8A     TM    4(R2),X'20'         (CHECK MEDIA)                                
         BO    JB9                                                              
         MVC   PRODKEY,KEY                                                      
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),LOGJOB                                                  
         GOTO1 READ                                                             
         MVC   KEY,PRODKEY                                                      
         SPACE 2                                                                
JB9      SR    R3,R3                                                            
         IC    R3,ACHRLEVB                                                      
         SR    R1,R1                                                            
         IC    R1,ACHRLEVC                                                      
         LA    R5,KEY+3(R3)                                                     
         SR    R1,R3                                                            
         LR    R3,R1                                                            
         STC   R3,WORK                                                          
         CLC   5(1,R2),WORK                                                     
         BNH   JB10                                                             
         MVI   ERROR,ACTOOLNG                                                   
         B     XIT                                                              
         DROP  R9                                                               
         SPACE 2                                                                
JB10     GOTO1 MOVE                                                             
         BCTR  R3,R0                                                            
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK                                                     
         SPACE 1                                                                
         CLI   LOGACT,C'N'         TEST ACTION=NEW                              
         BNE   JB12                NO                                           
         BAS   RE,RDOPT            READ THE JOB'S OPTIONS                       
         CLI   GOPROD,C'N'         TEST JOB LOCKOUT                             
         BNE   JB12                NO                                           
         MVI   ERROR,NOTVLREC                                                   
         LA    R2,LOGACTH                                                       
         B     XIT                                                              
         SPACE 2                                                                
JB12     TM    4(R2),X'20'                                                      
         BO    XIT                                                              
         OI    4(R2),X'20'                                                      
         MVI   ANYKEY,C'Y'                                                      
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY JOB RECORD FOR AMENDMENT/ENQUIRY                         
         SPACE 3                                                                
JB20     CLI   MODE,DSPLYREC                                                    
         BNE   JB38                                                             
         LA    R2,LOGJOBH                                                       
         MVI   LABELSW,C'N'                                                     
         SR    R3,R3                                                            
         IC    R3,0(R2)                                                         
         AR    R2,R3                                                            
         GOTO1 NAMOUT                                                           
         BAS   RE,HANDLPRF                                                      
         BAS   RE,HANDLEX                                                       
         BAS   RE,HANDLSTA                                                      
         FOUT  LOGCOM1H,SPACES,50                                               
         FOUT  LOGCOM2H,SPACES,50                                               
         FOUT  LOGCOM3H,SPACES,50                                               
         LA    R6,LOGCOM1H                                                      
         LA    R9,8(R6)                                                         
         MVI   COMMSW,C'N'         SHOWS NATURE OF EACH OUTPUT LINE             
         MVI   BYTE,1              FOR FIRST OUTPUT FIELD ON EACH LNE           
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         LA    R0,3                MAXIMUM NUMBER OF COMMENT LINES              
         SPACE 2                                                                
JB22     LA    R2,LOGJNAMH                                                      
         OI    4(R2),X'20'                                                      
         CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'26'                                                      
         BE    JB26                                                             
         CLI   0(R4),X'3E'                                                      
         BE    JB28                                                             
         SPACE 2                                                                
JB24     IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     JB22                                                             
         SPACE 2                                                                
JB26     DS    0H                                                               
         USING ACJOBD,R4                                                        
         MVC   LOGCLOS,SPACES                                                   
         OI    LOGCLOSH+6,X'80'                                                 
         LA    R3,LOGCLOS                                                       
         CLI   ACJBLEN,ACJBLNQ2                                                 
         BL    JB26A               OLD ELEMENT                                  
         OC    ACJBOPND,ACJBOPND                                                
         BZ    JB26A               NO OPEN DATE                                 
         GOTO1 DATCON,DMCB,(1,ACJBOPND),(8,0(R3))                               
         CLI   0(R3),C' '                                                       
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(R3)                                                         
JB26A    GOTO1 DATCON,DMCB,(1,ACJBCLOS),(8,0(R3))                               
         B     JB24                                                             
         SPACE 2                                                                
JB28     DS    0H                                                               
         USING ACOMMD,R4           DISPLAY A COMMENT                            
         CLI   ACOMTYPE,C'M'       OLD STYLE COMMENTS                           
         BE    JB29                                                             
         CLI   ACOMTYPE,0                                                       
         BNE   JB32                                                             
JB29     CLI   COMMSW,C'Y'         DOES THIS LINE CONTAIN STANDARD              
         BNE   JB30                COMMENT DATA                                 
         BAS   RE,BUMPFT                                                        
         MVI   COMMSW,C'N'                                                      
         MVI   BYTE,1                                                           
JB30     DS    0H                                                               
         IC    R5,ACOMLEN                                                       
         SH    R5,=H'5'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R6),ACOMMENT                                                 
         FOUT  (R6)                                                             
         BAS   RE,BUMPFT                                                        
         LA    R9,8(R6)                                                         
         B     JB24                                                             
         SPACE 2                                                                
JB32     MVI   COMMSW,C'Y'                                                      
         LR    RF,R9                                                            
         AH    RF,=H'11'           ROOM ON THIS LINE FOR THIS ENTRY             
         LA    RE,ACOMMENT                                                      
JB33     CLI   0(RE),X'40'         SUBTRACT THE NO OF BLANKS                    
         BNE   JB33A               AT START OF STD COMMENT NO.                  
         BCTR  RF,R0                                                            
         LA    RE,1(RE)                                                         
         B     JB33                                                             
JB33A    SR    RF,R6                                                            
         IC    R5,0(R6)                                                         
         CR    RF,R5                                                            
         BL    JB34                YES - BRANCH                                 
         BAS   RE,BUMPFT                                                        
         LA    R9,8(R6)                                                         
         MVI   BYTE,1                                                           
         SPACE 1                                                                
JB34     CLI   BYTE,1              NO COMMA BEFORE FIRST STD COMMENT            
         BE    JB35                                                             
         MVI   0(R9),C','                                                       
         LA    R9,1(R9)                                                         
JB35     MVI   BYTE,2                                                           
         MVC   0(4,R9),=C'EST='    ALLOW COMMENTS ON ESTIMATES & BILLS          
         TM    ACOMTYPE,X'40'      AND AFTER OPTIONS FOR NOW                    
         BO    *+10                                                             
         MVC   0(3,R9),=C'BIL'                                                  
         TM    ACOMTYPE,X'C0'                                                   
         BM    *+10                                                             
         MVC   0(3,R9),=C'B+E'     PLUS PRINT-ON-BOTH                           
         LA    RF,5                                                             
         MVC   WORK(6),ACOMMENT                                                 
JB36     CLI   WORK,C' '                                                        
         BNE   JB37                                                             
         MVC   WORK+6(5),WORK+1                                                 
         MVC   WORK(6),SPACES                                                   
         MVC   WORK(5),WORK+6                                                   
         BCTR  RF,R0                                                            
         B     JB36                                                             
         SPACE 1                                                                
JB37     EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R9),WORK                                                     
         FOUT  (R6)                                                             
         LA    R9,5(RF,R9)                                                      
         B     JB24                                                             
         SPACE 1                                                                
BUMPFT   IC    R5,0(R6)                                                         
         AR    R6,R5                                                            
         BCTR  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              BUILD JOB RECORD                                                 
         SPACE 3                                                                
JB38     CLI   MODE,BUILDREC                                                    
         BNE   JB40                                                             
         CLI   LOGACT,C'N'         TEST ACTION=NEW                              
         BE    *+8                 YES-ALREADY HAVE THE OPTIONS                 
         BAS   RE,RDOPT            READ OPTIONS                                 
         TM    LOGCLOSH+4,X'80'    TEST IF ANY LABEL DATA HAS CHANGED           
         BZ    *+8                                                              
         MVI   LABELSW,C'Y'                                                     
         TM    LOGJNAMH+4,X'80'                                                 
         BZ    *+8                                                              
         MVI   LABELSW,C'Y'                                                     
         TM    LOGUNITH+4,X'80'                                                 
         BZ    *+8                                                              
         MVI   LABELSW,C'Y'                                                     
         TM    LOGTYPEH+4,X'80'                                                 
         BZ    *+8                                                              
         MVI   LABELSW,C'Y'                                                     
         TM    LOGNONBH+4,X'80'                                                 
         BZ    *+8                                                              
         MVI   LABELSW,C'Y'                                                     
         LA    R2,LOGJNAMH                                                      
         GOTO1 ANY                                                              
         CLI   LOGACT,C'A'         FOR ACTION AMEND, SAVE NAME                  
         BNE   JB38_0                                                           
         GOTO1 CHKNAM,DMCB,(C'B',IO),NAMESAVE                                   
         LA    RF,NAMESAVE-LOCALS  SAVE DISP TO SAVED NAME                      
         STCM  RF,3,DSAVNAM        FOR SEARCH                                   
*                                                                               
JB38_0   GOTO1 NAMIN                                                            
**T                                                                             
         CLI   ACCEMU,C'Y'         IS IT THE NEW FILE                           
         BNE   JB38A                                                            
         CLI   LOGACT,C'A'         MUST BE AMEND                                
         BNE   JB38A                                                            
         TM    4(R2),X'20'         HAS NAME CHANGED                             
         BO    JB38A                                                            
         GOTO1 CHKNAM,DMCB,(C'A',IO2),NAMESAVE                                  
JB38A    DS    0H                                                               
**T                                                                             
         LA    R2,LOGCLOSH          MAY BE MMMDD/YY,MMMDD/YY                    
         GOTO1 ANY                  FOR OPENED,CLOSED                           
         LA    R4,ELEMENT           OR MMMDD/YY                                 
         XC    ELEMENT,ELEMENT      FOR CLOSED                                  
         USING ACJOBD,R4                                                        
         MVI   ACJBEL,X'26'                                                     
         MVI   ACJBLEN,ACJBLNQ3                                                 
         GOTO1 DATCON,DMCB,(5,0),(1,ACJBSTRT)                                   
         MVI   ERROR,INVALID                                                    
         L     RF,COMFACS                                                       
         USING COMFACSD,RF                                                      
         GOTO1 CSCANNER,DMCB,(R2),(2,BLOCK),0                                   
         ZIC   R5,DMCB+4                                                        
         LTR   R5,R5                                                            
         BZ    XIT                 INVALID INPUT                                
         MVI   ERROR,DATERR                                                     
         L     RF,COMFACS                                                       
         GOTO1 CDATVAL,DMCB,(0,BLOCK+12),WORK                                   
         OC    DMCB(4),DMCB                                                     
         BZ    XIT                 DATE ERROR                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACJBCLOS)                                
         CLI   BLOCK+32,0          NO SECOND DATE                               
         BE    JB38B                                                            
         MVC   ACJBOPND,ACJBCLOS   IF 2 DATES FIRST WAS OPENED                  
         MVI   ERROR,DATERR                                                     
         L     RF,COMFACS                                                       
         GOTO1 CDATVAL,DMCB,(0,BLOCK+32+12),WORK                                
         OC    DMCB(4),DMCB                                                     
         BZ    XIT                 DATE ERROR                                   
         DROP  RF                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACJBCLOS)                                
         CLC   ACJBOPND,ACJBCLOS                                                
         BNL   XIT                 OPEN MUST BE BEFORE CLOSE                    
JB38B    LA    R5,IO2                                                           
         AH    R5,DATADISP                                                      
JB38C    CLI   0(R5),0                                                          
         BE    JB38G                                                            
         CLI   0(R5),X'26'                                                      
         BE    JB38D                                                            
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     JB38C                                                            
JB38D    MVC   ACJBSTRT,ACJBSTRT-ACJOBD(R5)                                     
         CLI   ACJBLEN-ACJOBD(R5),ACJBLNQ2                                      
         BL    JB38G               OLD ELEMENT - NO REVISION                    
         MVC   ACJBREV,ACJBREV-ACJOBD(R5)      SAVE REVISION NUMBER             
         MVC   ACJBREVD,ACJBREVD-ACJOBD(R5)    AND DATE                         
JB38F    CLI   ACJBLEN-ACJOBD(R5),ACJBLNQ3                                      
         BL    JB38G               OLD ELEMENT - NO SAVED FILTER                
         MVC   ACJBFLT2(ACJBLNQ3-ACJBLNQ2),ACJBFLT2-ACJOBD(R5)                  
         SPACE 1                                                                
JB38G    MVI   ERROR,X'FF'                                                      
         GOTO1 REMANEL,DMCB,(X'26',0)                                           
         CLI   GONEWJUN,C'Y'       TEST NEW JOBS UNAPPROVED                     
         BNE   *+16                                                             
         CLI   LOGACT,C'N'         TEST NEW JOB                                 
         BNE   *+8                                                              
         OI    ACJBSTAT,X'80'      INITIALIZE ESTIMATE=UNAPPROVED               
         GOTO1 ADDANEL                                                          
         BAS   RE,HANDLPRF                                                      
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         BAS   RE,HANDLEX                                                       
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         CLI   GONEWJUN,C'Y'       TEST NEW JOBS UNAPPROVED                     
         BNE   JB39                                                             
         CLI   LOGACT,C'N'         TEST NEW JOB                                 
         BNE   JB39                                                             
         BAS   RE,UNAPPADD         FORCE OPTION TO BE DISPLAYED                 
         SPACE 1                                                                
JB39     GOTO1 REMANEL,DMCB,(X'3E',0)                                           
         LA    R2,LOGCOM1H                                                      
         MVI   COMMBYTE,0                                                       
         BAS   RE,ANYCOM                                                        
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         LA    R2,LOGCOM2H                                                      
         BAS   RE,ANYCOM                                                        
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         LA    R2,LOGCOM3H                                                      
         BAS   RE,ANYCOM                                                        
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         GOTO1 STATIN                                                           
         CLI   LOGACT,C'N'                                                      
         BNE   XIT                                                              
         GOTO1 BALIN                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS COMMENTS                                      
         SPACE 3                                                                
ANYCOM   NTR1                                                                   
         MVI   LEVEL,3                                                          
         LA    R4,ELEMENT                                                       
         CLI   5(R2),0                                                          
         BE    ACEXT                                                            
         USING ACOMMD,R4                                                        
         MVI   ACOMEL,X'3E'                                                     
         SR    R3,R3                                                            
         IC    R3,5(R2)                                                         
         LA    R5,8(R2,R3)                                                      
         MVC   BORROW,0(R5)        SCANNER PROBLEM WITH LAST BYTE               
         SPACE 1                                                                
         GOTO1 =V(SCANNER),DMCB,(R2),BLOCK,RR=RB                                
         MVC   0(1,R5),BORROW                                                   
         IC    R3,DMCB+4           NO OF VALID ENTRIES                          
         LTR   R3,R3                                                            
         BZ    AC2                                                              
         CLI   BLOCK+1,0           RH SIDE OF SCANNER TABLE                     
         BNE   AC4                                                              
         SPACE 1                                                                
*                                  MUST BE STRAIGHT DATA IF INVALID             
AC2      DS    0H                                                               
         IC    R3,5(R2)                                                         
         LA    R3,4(R3)                                                         
         STC   R3,ACOMLEN                                                       
         MVC   ACOMSEQ,COMMBYTE                                                 
         SR    RE,RE                                                            
         IC    RE,COMMBYTE                                                      
         LA    RE,1(RE)                                                         
         STC   RE,COMMBYTE                                                      
         MVI   ACOMTYPE,0          DATA                                         
         SH    R3,=H'5'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ACOMMENT(0),8(R2)                                                
         GOTO1 ADDANEL                                                          
         B     ACEXT                                                            
         SPACE 2                                                                
AC4      LA    R6,BLOCK            HANDLE STANDARD COMMENT NUMBERS              
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+1(1),COMPANY                                                 
AC6      MVI   ACOMLEN,10          STANDARD LENGTH                              
         MVI   ACOMTYPE,X'44'                                                   
         CLC   12(3,R6),=C'EST'                                                 
         BE    AC8                                                              
         MVI   ACOMTYPE,X'84'                                                   
         CLC   12(3,R6),=C'BIL'                                                 
         BE    AC8                                                              
         OI    ACOMTYPE,X'40'                                                   
         CLC   12(3,R6),=C'B+E'                                                 
         BE    AC8                                                              
         MVI   ERROR,INVALID                                                    
         B     ACEXT                                                            
         SPACE 1                                                                
AC8      MVC   ACOMSEQ,COMMBYTE                                                 
         SR    RE,RE                                                            
         IC    RE,COMMBYTE                                                      
         LA    RE,1(RE)            BUMP SEQUENCE FOR NEXT ELEMENT               
         STC   RE,COMMBYTE                                                      
         MVC   ACOMMENT(6),SPACES                                               
         SR    R1,R1                                                            
         IC    R1,1(R6)            LENGTH OF COMMENT NO                         
         CH    R1,=H'6'            CAN'T INPUT MORE THAN 6 CHARS                
         BNH   AC10                                                             
         MVI   ERROR,INVALID                                                    
         B     ACEXT                                                            
AC10     LTR   R1,R1                                                            
         BNZ   AC12                OR NONE                                      
         MVI   ERROR,INVALID                                                    
         B     ACEXT                                                            
AC12     LA    RE,6                                                             
         SR    RE,R1                                                            
         LA    RE,ACOMMENT(RE)                                                  
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(R6)                                                   
         MVC   KEY+2(6),ACOMMENT   SEE IF STD COMMENT EXISTS                    
         GOTO1 READ                                                             
         LA    R6,32(R6)                                                        
         GOTO1 ADDANEL                                                          
         BCT   R3,AC6                                                           
         SPACE 2                                                                
ACEXT    MVI   LEVEL,2                                                          
         XIT1                                                                   
         EJECT                                                                  
*              WRITE A REQUEST FOR JOB LABELS                                   
         SPACE 3                                                                
JB40     CLI   MODE,ADDAREQ                                                     
         BNE   JB50                                                             
         LA    RF,SAVECOMP         COMP INTO RF                                 
         USING ACCOMPD,RF                                                       
         TM    ACMPSTAT,X'02'                                                   
         BO    XIT                 NO LABELS                                    
         DROP  RF                                                               
         CLI   LOGACT,C'C'         ALWAYS DO A LABEL FOR CLOSING                
         BE    JB41                                                             
         CLI   LOGACT,C'N'         AND FOR A NEW JOB                            
         BE    JB41                                                             
         CLI   LABELSW,C'N'                                                     
         BE    XIT                                                              
JB41     MVC   WORK,SPACES                                                      
         MVC   WORK(2),=C'12'                                                   
         LA    R4,IO2                                                           
         MVI   JOBUNIT,0                                                        
         LA    R6,JOBUNIT                                                       
         BAS   RE,ANYUNIT                                                       
         MVC   WORK+3(1),CLIUNIT                                                
         CLI   PROUNIT,0                                                        
         BE    *+10                                                             
         MVC   WORK+3(1),PROUNIT                                                
         CLI   JOBUNIT,0                                                        
         BE    *+10                                                             
         MVC   WORK+3(1),JOBUNIT                                                
         MVC   WORK+2(1),COMPANY                                                
         MVC   WORK+9(15),IO2                                                   
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+10,12                                                    
         MVC   ELEMENT+26(80),WORK                                              
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'ACCREQS',ELEMENT,ELEMENT,     X        
               (TERMINAL,0)                                                     
         CLI   LOGACT,C'N'         FOR NEW JOBS WRITE OUT                       
         BNE   XIT                 ESTIMATE REQUEST AS WELL                     
         B     XIT                 REMOVED FOR NOW                              
         SPACE 2                                                                
         MVI   ELEMENT+10,14                                                    
         MVC   ELEMENT+26(2),=C'14'                                             
         MVC   ELEMENT+68(6),SPACES                                             
         BASR  RE,RF                                                            
         B     XIT                                                              
         EJECT                                                                  
*              CHECK FOR UNIT FOR ANALYSIS                                      
         SPACE 3                                                                
ANYUNIT  NTR1                                                                   
         AH    R4,DATADISP                                                      
         USING ACPROFD,R4                                                       
         SPACE 2                                                                
JB42     CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'24'                                                      
         BNE   JB44                                                             
         MVC   0(1,R6),ACPRUNIT                                                 
         B     XIT                                                              
         SPACE 2                                                                
JB44     SR    R5,R5                                                            
         IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     JB42                                                             
         EJECT                                                                  
* SUB-ROUTINE TO READ OPTIONS THROUGH GETOPT                                    
*                                                                               
RDOPT    NTR1                                                                   
         GOTO1 CALLOV,DMCB,0,X'D9000A84'                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETOPT,0(R1)                                                     
         SPACE 1                                                                
RDOPT2   LA    RE,GOBLOCK          CLEAR GOBLOCK                                
         LA    RF,L'GOBLOCK                                                     
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         SPACE 1                                                                
RDOPT4   MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL(1),COMPANY                                              
         LA    RE,SAVECOMP                                                      
         USING ACCOMPD,RE                                                       
         MVC   GOSELCUL+1(2),ACMPJOB GET UNIT/LEDGER                            
*                                                                               
         LA    R2,LOGCLIH                                                       
         GOTO1 MOVE                                                             
         MVC   GOSELCLI,WORK                                                    
*                                                                               
         LA    R2,LOGPRODH                                                      
         GOTO1 MOVE                                                             
         MVC   GOSELPRO,WORK                                                    
*                                                                               
         LA    R2,LOGJOBH                                                       
         GOTO1 MOVE                                                             
         MVC   GOSELJOB,WORK                                                    
*                                                                               
RDOPT6   GOTO1 GETOPT,DMCB,GOBLOCK                                              
         B     XIT                                                              
         EJECT                                                                  
UNAPPADD NTR1                                                                   
*        CLI   LOGPRFH+5,0         EXTRA PROFILE EL PRESENT                     
*        BE    UNAP10              NO - BRANCH                                  
*        LA    R4,IO2                                                           
*        AH    R4,DATADISP                                                      
*        SR    R5,R5                                                            
*UNAP2   CLI   0(R4),0                                                          
*        BE    XIT                                                              
*        CLI   0(R4),X'3C'         FIND EXTRA PROFILE EL                        
*        BE    UNAP4                                                            
*        IC    R5,1(R4)                                                         
*        AR    R4,R5                                                            
*        B     UNAP2                                                            
         SPACE 1                                                                
*        USING ACXPROFD,R4                                                      
*UNAP4   OI    ACXPST1,X'04'       AND TURN ON UNAPPROVED BIT                   
*        B     UNAPX                                                            
         SPACE 1                                                                
*UNAP10  LA    R4,ELEMENT          ADD A STANDARD EL                            
*        USING ACXPROFD,R4                                                      
*        MVC   ELEMENT(L'STANDX),STANDX                                         
*        OI    ACXPST1,X'04'                                                    
*        GOTO1 ADDANEL                                                          
         SPACE 1                                                                
UNAPX    LA    R4,LOGPR2H          FIELD HEADER                                 
         ZIC   RF,0(R4)            FIELD LENGTH                                 
         ZIC   R1,5(R4)            WHAT'S ALREADY IN FIELD                      
         LA    R4,8(R4)            START OF UNPROTECTED FIELD                   
         CH    R1,=H'0'            ANYTHING ELSE ON LINE                        
         BE    UNAPX2                                                           
         AR    R4,R1               FIRST AVAILABLE SLOT IN FIELD                
         AH    R1,=H'18'           HOW MUCH IS NEEDED (HDR+EST=...)             
         CR    R1,RF               DO WE HAVE ENOUGH ROOM                       
         BH    UNAPX3              NO -- WILL IT FIT ON LINE 3                  
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
UNAPX2   MVC   0(9,R4),=C'EST=UNAPP'                                            
         OI    LOGPR2H+6,X'80'     TRANSMIT FIELD                               
         B     XIT                                                              
UNAPX3   LA    R4,LOGPR3H          FIELD HEADER PROF LINE 3                     
         ZIC   RF,0(R4)            FIELD LENGTH                                 
         ZIC   R1,5(R4)            WHAT'S ALREADY IN FIELD                      
         LA    R4,8(R4)            START OF UNPROTECTED FIELD                   
         CH    R1,=H'0'            ANYTHING ELSE ON LINE                        
         BE    UNAPX4                                                           
         AR    R4,R1               FIRST AVAILABLE SLOT IN FIELD                
         AH    R1,=H'18'           HOW MUCH IS NEEDED (HDR+EST=...)             
         CR    R1,RF               DO WE HAVE ENOUGH ROOM                       
         BH    XIT                 NO                                           
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
UNAPX4   MVC   0(9,R4),=C'EST=UNAPP'                                            
         OI    LOGPR3H+6,X'80'     TRANSMIT FIELD                               
         B     XIT                                                              
         EJECT                                                                  
*              CHANGE STATUS OF JOB TO CLOSE                                    
         SPACE 3                                                                
JB50     BAS   RE,RDOPT                                                         
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
         SR    R5,R5                                                            
         SPACE 2                                                                
JB52     CLI   0(R4),X'30'                                                      
         BE    JB54                                                             
         CLI   0(R4),X'32'                                                      
         BE    JB56                                                             
         CLI   0(R4),X'26'                                                      
         BE    JB58                                                             
         CLI   0(R4),ASTELQ                                                     
         BE    JB59                                                             
         CLI   0(R4),0                                                          
         BE    JB60                                                             
         SPACE 2                                                                
JB53     IC    R5,1(R4)                                                         
         AR    R4,R5                                                            
         B     JB52                                                             
         SPACE 2                                                                
JB54     DS    0H                                                               
         USING ACSTATD,R4                                                       
         OI    ACSTSTAT,X'40'                                                   
         B     JB53                                                             
         SPACE 2                                                                
JB56     DS    0H                                                               
         USING ACBALD,R4                                                        
         CP    ACBLDR,ACBLCR       DEBITS MUST EQUAL CREDITS                    
         BE    JB53                                                             
         MVI   ERROR,156                                                        
         B     XIT                                                              
         SPACE 2                                                                
         USING ACJOBD,R4                                                        
JB58     DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,ACJBCLOS)                                   
         B     JB53                                                             
         SPACE 2                                                                
         USING ASTELD,R4                                                        
JB59     OC    ASTDRAFT,ASTDRAFT                                                
         BZ    JB53                                                             
         MVI   ERROR,CLODRFT                                                    
         B     XIT                 CANNOT CLOSE IF DRAFT ITEMS                  
         SPACE 2                                                                
JB60     LA    R4,IO               READ TO SEE IF ANY UNMATCHED ORDERS          
         USING ACKEYACC,R4                                                      
         GOTO1 HIGH                                                             
JB62     GOTO1 SEQ                                                              
         CLC   ACKEYACC,KEYSAVE                                                 
         BNE   XIT                                                              
         CLI   ACRECORD,X'44'                                                   
         BNE   JB62                                                             
         CLC   ACKEYWRK,=C'99'                                                  
         BE    JB62                                                             
         CLC   ACKEYWRK,=C'**'                                                  
         BNE   JB63                                                             
*                                                                               
         BAS   RE,FIRMPO                                                        
         B     JB62                                                             
*                                                                               
         USING TRANSD,R6                                                        
JB63     LA    R6,ACRECORD                                                      
         MVI   ERROR,HOLDIT                                                     
         TM    TRNSSTAT,X'04'      IS RECORD ON HOLD ?                          
         BO    XIT                 YES, CAN'T CLOSE                             
*                                                                               
         CLI   GOBILTYP,C'C'       IS THIS CLIENT BILLING ?                     
         BE    JB64                YES, USE DIFFERENT LOGIC                     
         OC    ACDTUSED,ACDTUSED     NO, IS IT BILLED ?                         
         BNZ   JB62                  YES                                        
*                                                                               
         TM    TRNSSTAT,X'20'        NO, IS IT A REVERSAL ?                     
         BO    JB62                  YES, SKIP IT                               
*                                                                               
         MVI   ERROR,OPENSKS                                                    
         CLC   ACKEYCON+1(2),=C'SK'  NO, IS THE CONTRA SK                       
         BE    XIT                   YES, CAN'T CLOSE                           
         MVI   ELCODE,X'4C'          NO, LOOK AT SUBSIDIARY POSTING             
         BAS   RE,GETELIO                                                       
         BNE   JB62                                                             
         USING TRSDESCD,R6                                                      
         CLC   TRSDACCS(2),=C'SK'                                               
         BE    XIT                                                              
         B     JB62                                                             
*                                                                               
         USING TRBDETD,R6                                                       
JB64     MVI   ERROR,UNBILLED                                                   
         LR    R5,R6                                                            
         ZAP   AMOUNT,=P'0'                                                     
         MVI   ELCODE,TRBDELQ                                                   
*                                                                               
JB66     BAS   RE,NEXTEL                                                        
         BNE   JB68                                                             
         CLC   TRBDNO,SPACES       IS THIS BILLED ?                             
         BNH   JB66                NO, GET NEXT                                 
         ICM   RF,15,TRBDAMNT      YES, GET AMOUNT                              
         CVD   RF,DUB                                                           
         AP    AMOUNT,DUB                                                       
         B     JB66                                                             
*                                                                               
JB68     CP    AMOUNT,TRNSAMNT-TRANSD(L'TRNSAMNT,R5)                            
         BNE   XIT                                                              
         B     JB62                                                             
*                                                                               
FIRMPO   NTR1  WORK=(R4,250)                                                    
         MVC   SAVEKEY,ACKEYACC    CONFIRM THAT PO IS ON FILE                   
         XC    0(42,R4),0(R4)                                                   
         MVI   0(R4),X'1A'                                                      
         MVC   1(1,R4),COMPANY                                                  
         MVC   ACOKNUM-ACKEYD(6,R1),ACKEYREF                                    
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BE    FIRMPON                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SAVEKEY,IO                   
         B     XIT                                                              
*                                                                               
FIRMPON  MVI   ERROR,NOTVLREC                                                   
         B     XIT                                                              
*                                                                               
GETELIO  LA    R6,IO                                                            
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
XIT      XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
MISSCODE MVI   ERROR,MISSING                                                    
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY SOME STATUS INFORMATION                                  
         SPACE 2                                                                
HANDLSTA NTR1                                                                   
         FOUT  LOGSTATH,SPACES,36                                               
         LA    R4,IO2                                                           
         AH    R4,DATADISP                                                      
*                                                                               
HSTA2    CLI   0(R4),0                                                          
         BE    XIT                                                              
         CLI   0(R4),X'30'         STATUS ELEMENT                               
         BE    HSTA4                                                            
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     HSTA2                                                            
*                                                                               
         USING ACSTATD,R4                                                       
HSTA4    DS    0H                                                               
         LA    R6,LOGSTAT                                                       
         TM    ACSTSTAT,X'60'                                                   
         BZ    HSTA20                                                           
         MVC   0(3,R6),=C'ST='                                                  
         TM    ACSTSTAT,X'40'                                                   
         BZ    HSTA6                                                            
         MVC   3(2,R6),=C'C,'                                                   
HSTA6    TM    ACSTSTAT,X'20'                                                   
         BO    HSTA8                                                            
         MVI   4(R6),C' '                                                       
         B     HSTA20                                                           
HSTA8    MVI   5(R6),C'L'                                                       
*                                                                               
*              DISPLAY FILTER INFORMATION                                       
*                                                                               
HSTA20   LA    R6,8(R6)            FILTER #1                                    
         CLI   ACSTFILT,C' '                                                    
         BNH   HSTA22                                                           
         MVC   0(3,R6),=C'F1='                                                  
         MVC   3(1,R6),ACSTFILT                                                 
         LA    R6,5(R6)                                                         
*                                                                               
HSTA22   CLI   ACSTFILT+1,C' '     FILTER #2                                    
         BNH   HSTA24                                                           
         MVC   0(3,R6),=C'F2='                                                  
         MVC   3(1,R6),ACSTFILT+1                                               
         LA    R6,5(R6)                                                         
*                                                                               
HSTA24   CLI   ACSTSUB,C' '        FILTER #4                                    
         BNH   HSTA26                                                           
         MVC   0(3,R6),=C'F4='                                                  
         MVC   3(1,R6),ACSTSUB                                                  
         LA    R6,5(R6)                                                         
*                                                                               
HSTA26   CLI   ACSTFLT5,C' '       FILTER #5                                    
         BNH   HSTA30                                                           
         MVC   0(3,R6),=C'F5='                                                  
         MVC   3(1,R6),ACSTFLT5                                                 
*                                                                               
HSTA30   DS    0H                                                               
         CLC   LOGSTAT(36),SPACES                                               
         BE    XIT                                                              
         MVC   DMCB+4,=X'D9000A0D' SQUASHER                                     
         GOTO1 CALLOV,DMCB,0                                                    
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,LOGSTAT,36                                             
         FOUT  LOGSTATH                                                         
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RE                                                               
       ++INCLUDE ACLFM089A                                                      
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMF5D                                                       
SAVECOMP DS    CL64                                                             
SAVEHEIR DS    CL66                                                             
SAVEBUDG DS    CL12                                                             
SAVESTRT DS    CL3                                                              
SAVEUNIT DS    CL1                                                              
CLIUNIT  DS    CL1                                                              
PROUNIT  DS    CL1                                                              
JOBUNIT  DS    CL1                                                              
THISPROF DS    CL255                                                            
THISADD  DS    CL105                                                            
LABELSW  DS    CL1                                                              
DELETIT  DS    CL1                                                              
SAVEKEY  DS    CL42                                                             
GETOPT   DS    V                                                                
       ++INCLUDE ACGOBLOCK                                                      
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
SAVEADDR DS    CL107                                                            
SAVEPROF DS    CL255                                                            
SAVELEM  DS    CL255                                                            
LOGGRUPH DS    CL8                 DUMMY                                        
LOGADD1H DS    0C                  (DUMMY TAG - ADDRESSES NOT USED)             
LOGADD2H DS    0C                                                               
LOGADD3H DS    0C                                                               
LOGADD4H DS    0C                                                               
COMMSW   DS    CL1                                                              
COMMBYTE DS    CL1                                                              
BYTE     DS    CL1                                                              
BORROW   DS    CL1                                                              
PRELOC   DS    F                                                                
PRODKEY  DS    CL32                                                             
ELCODE   DS    C                                                                
AMOUNT   DS    PL8                                                              
TEMPIO   DS    CL1000                                                           
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACLFM0A   05/01/02'                                      
         END                                                                    
