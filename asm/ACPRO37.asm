*          DATA SET ACPRO37    AT LEVEL 058 AS OF 09/12/02                      
*PHASE T60B37A,*                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T60B37 - JOB DETAIL'                                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* NOTE: IF YOU MAKE ANY CHANGES TO THIS PROGRAM, PLEASE CHECK IF THEY           
*       SHOULD GO IN PRESTO'S VERSION (ACPRF01)                                 
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
T60B37   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T60B37**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK AREA                    
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
*                                                                               
         CLI   MODE,VALKEY                                                      
         BE    KEYLOGIC                                                         
         CLI   MODE,VALREC                                                      
         BE    RECLOGIC                                                         
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALKEY  LOGIC                                                                 
*                                                                               
KEYLOGIC LA    RE,LOCAL            CLEAR LOCAL STORAGE                          
         LA    RF,LOCALLN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALHED           VALIDATE SCREEN                              
         MVI   INTMODE,DISMODE     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,FSTMODE     YES - SET FIRST TIME LIST                    
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,FSTMODE     YES - SET FIRST TIME LIST                    
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
* VALREC  LOGIC - DISPLAY OR CHANGE                                             
*                                                                               
RECLOGIC LA    R2,JOBDATAH         SET SCREEN LINE ADDRESSES                    
         ST    R2,AFSTLNE                                                       
         LA    R2,JOBPFH                                                        
         ST    R2,APFFLD                                                        
         LA    R2,JOBLAST                                                       
         ST    R2,AENDSCR                                                       
*                                                                               
         CLI   INTMODE,FSTMODE     TEST FOR FIRST TIME LIST                     
         BE    DISLOGIC            YES                                          
*                                                                               
PROCPF   L     R2,AFSTLNE                                                       
         SR    R3,R3                                                            
         ICM   R3,1,NUMLINES                                                    
         BZ    PROCPFX             NOTHING ON SCREEN TO PROCESS                 
*                                                                               
PROCPF10 CLI   PFKEY,PF10                                                       
         BNE   PROCPF8                                                          
         BAS   RE,PRINT            PRINT THE REPORT                             
         B     XIT                                                              
*                                                                               
PROCPF8  CLI   PFKEY,PF8           SCROLL FORWARDS                              
         BNE   PROCPF7                                                          
         SR    R3,R3                                                            
         IC    R3,NXTSTORE         IF NEXT LINE IS GREATER THAN                 
         SR    R1,R1                                                            
         IC    R1,LSTSTORE          LAST LINE, START OVER                       
         CR    R3,R1                                                            
         BNH   *+8                                                              
         MVI   NXTSTORE,X'00'                                                   
*                                                                               
PROCPF7  CLI   PFKEY,PF7           SCROLL BACKWARDS                             
         BNE   PROCPFX                                                          
         MVI   NXTSTORE,X'00'      CLEAR NEXT # TO BE PRINTED                   
         SR    R3,R3                                                            
         IC    R3,LINEINDX                                                      
         BCTR  R3,0                BACKUP TO CURRENT INDEX                      
         LTR   R3,R3               IF INDEX IS ZERO,                            
         BZ    PROCPF7A             START OVER                                  
         BCTR  R3,0                BACKUP TO PREVIOUS NOW                       
         LA    R1,LINETBL(R3)       ADDRESS TABLE AND GET PREVIOUS              
         MVC   NXTSTORE,0(R1)       STARTING #                                  
*                                                                               
PROCPF7A STC   R3,LINEINDX                                                      
         B     PROCPFX                                                          
*                                                                               
PROCPFX  MVI   INTMODE,DISMODE                                                  
         B     DISLOGIC                                                         
         EJECT                                                                  
***********************************************************************         
* CLEAR SCREEN AND TABLE, BRANCH TO DISPLAY ROUTINE AND               *         
* DETERMINE APPROPRIATE MESSAGE                                       *         
***********************************************************************         
*                                                                               
DISLOGIC GOTO1 VCLEARF,DMCB,AFSTLNE,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTLNE),APFFLD                                     
         LA    R2,JOBCLNTH         POSITION CURSOR AT FIRST KEY FIELD           
         CLI   LSTSTORE,X'00'                                                   
         BE    NONEMESS            NO DATA TO DISPLAY                           
         MVI   NUMLINES,0          INITIALIZE LINES                             
         BAS   RE,DISPLAY                                                       
         CLI   NUMLINES,MAXLINE    SEE IF SCREEN IS FULL                        
         BNL   FULLMESS            YES                                          
         MVI   MYMSGNO1,IENDDIS                                                 
         B     INFEXIT                                                          
*                                                                               
FULLMESS MVI   MYMSGNO1,IDATADIS                                                
         B     INFEXIT                                                          
*                                                                               
NONEMESS MVI   MYMSGNO1,INODIS                                                  
         B     INFEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE HEADING FIELD(S)                                           *         
***********************************************************************         
*                                                                               
VALHED   NTR1                                                                   
         LA    R2,CONRECH                                                       
         GOTO1 SETHEIR                                                          
         MVI   KEYCHG,C'N'                                                      
         MVI   OPTION,0                                                         
         LA    R2,JOBCLNTH         CLIENT IS REQUIRED                           
         BAS   RE,TSTKEY                                                        
         GOTO1 VALCLI                                                           
         MVC   QCLNT,CLICODE                                                    
*                                                                               
         LA    R2,JOBPRODH         PRODUCT IS REQUIRED                          
         BAS   RE,TSTKEY                                                        
         GOTO1 VALPROD                                                          
         MVC   QPROD,PRODCODE                                                   
*                                                                               
         LA    R2,JOBJOBH          JOB IS REQUIRED                              
         BAS   RE,TSTKEY                                                        
         GOTO1 VALJOB                                                           
         MVC   QJOB,JOBNUM                                                      
*                                                                               
         LA    R2,JOBWRKH          WORKCODE IS REQUIRED                         
         BAS   RE,TSTKEY                                                        
         GOTO1 VALWORK                                                          
         MVC   QWORK,WORKCODE                                                   
         MVC   QSUFFIX,WORK+2                                                   
*                                                                               
         CLI   QSUFFIX,C' '        VALIDATE SUFFIX FOR BLANK, C OR N            
         BE    VALH020                                                          
         CLI   QSUFFIX,C'C'                                                     
         BE    VALH020                                                          
         CLI   QSUFFIX,C'N'                                                     
         BNE   INVSUFF                                                          
*                                                                               
VALH020  LA    R2,JOBOPTH          OPTION IS OPTIONAL                           
         BAS   RE,TSTKEY                                                        
         XC    QDRAFT,QDRAFT                                                    
         CLI   5(R2),0                                                          
         BE    VALH060                                                          
         MVI   ERROR,INVALID                                                    
         CLI   8(R2),C'Y'          ONLY VALID OPTIONS ARE Y,N AND O.            
         BE    VALH040                                                          
         CLI   8(R2),C'N'                                                       
         BE    VALH040                                                          
         CLI   8(R2),C'O'                                                       
         BNE   ERREXIT                                                          
*                                                                               
VALH040  MVC   QDRAFT,8(R2)                                                     
*                                                                               
VALH060  LA    R2,JOBWNAMH                                                      
         MVC   8(L'ACANDESC,R2),WORKNAME                                        
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN XJOB                              
         BZ    VALHEDX             NO                                           
         MVC   JOBEXP,JDXJOB       (EXP)                                        
         LA    R2,JOBEXPH                                                       
         OI    4(R2),X'20'         INDICATE VALIDATED                           
         OI    6(R2),X'80'         TRANSMIT FIELD                               
*                                                                               
*                                                                               
VALHEDX  CLI   KEYCHG,C'Y'         DID SOMETHING CHANGE ?                       
         BNE   XIT                 NO, EXIT                                     
         ZAP   POTOTAL,=P'0'       YES, CLEAR FIELDS                            
         ZAP   TRTOTAL,=P'0'                                                    
         ZAP   M1TOTAL,=P'0'                                                    
         ZAP   M2TOTAL,=P'0'                                                    
         ZAP   M3TOTAL,=P'0'                                                    
         ZAP   POCTR,=P'0'                                                      
         ZAP   TRCTR,=P'0'                                                      
         MVI   FSTSTORE,X'00'                                                   
         MVI   NXTSTORE,X'00'                                                   
         MVI   LINEINDX,X'00'                                                   
         XC    LINETBL,LINETBL                                                  
*                                                                               
         BAS   RE,LOAD             LOAD UP BUFFER TO WRITE TO                   
         L     RE,ASTORAGE         CLEAR IT                                     
         LH    RF,=Y(L'BUFFER)                                                  
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BAS   RE,READALL          NOW READ ALL REQUIRED RECORDS                
         BAS   RE,PUTTAB           SAVE BUFFER AWAY                             
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
TSTKEY   TM    4(R2),X'20'         HAS FIELD CHANGED ?                          
         BOR   RE                  NO, EXIT                                     
         OI    4(R2),X'20'         YES, INDICATE VALIDATED                      
         MVI   KEYCHG,C'Y'          AND SET SWITCH                              
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO DISPLAY DATA FROM STORAGE                             *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                                                                   
         BAS   RE,GETTAB           GET BUFFER BACK                              
         MVI   TRDONE,0            CLEAR TRANSACTION TITLE INDICATOR            
         MVI   FNDONE,0            CLEAR FINAL TOTAL INDICATOR                  
         L     R2,AFSTLNE          GET FIRST LINE OF SCREEN                     
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,1,NXTSTORE       GET # OF NEXT RECORD TO BE PRINTED           
         BZ    DISP020                                                          
         SR    R3,R3                                                            
         IC    R3,LSTSTORE                                                      
         CR    R5,R3               IF HIGHER THAN LAST RECORD,                  
         BL    DISP040                                                          
         LA    R5,0                 START OVER                                  
*                                                                               
DISP020  XC    LINETBL,LINETBL     FIRST TIME OR STARING OVER                   
         MVI   LINEINDX,X'00'       CLEAR INDEX AND TABLE                       
*                                                                               
DISP040  SR    R3,R3               ADJUST INDEX                                 
         IC    R3,LINEINDX                                                      
*                                                                               
DISP060  LA    R1,LINETBL(R3)      ADDRESS THE TABLE AND SAVE                   
         LA    R3,1(R3)            ADD 1 TO INDEX                               
         STC   R3,LINEINDX                                                      
         STC   R5,0(R1)             STARTING #                                  
*                                                                               
         STC   R5,FSTSTORE         SAVE IT ALSO AS 1ST RECORD ON SCREEN         
         MH    R5,=Y(SLENGTH)                                                   
         L     R4,ASTORAGE                                                      
         LA    R4,0(R4,R5)         ADDRESS DATA IN STORAGE                      
         SR    R5,R5                                                            
         IC    R5,FSTSTORE         GET STARTING RECORD # AGAIN                  
*                                                                               
         USING STORED,R4                                                        
         CLI   STYPE,C'S'                                                       
         BE    POHEAD                                                           
*                                                                               
         CLI   STYPE,C'U'                                                       
         BE    TRHEAD                                                           
*                                                                               
DISP080  CLI   NUMLINES,MAXLINE    IS SCREEN FULL ?                             
         BNL   DISPX               YES, SAVE POINTER                            
         NI    1(R2),X'F7'         TURN OFF HIGH INTENSITY, IF ON               
         CLI   STYPE,C'S'          LINE 1 OF P.O. ?                             
         BE    DISPPOS             YES                                          
         CLI   STYPE,C'T'          NO, LINE 2 OF P.O.                           
         BE    DISPOS2             YES                                          
         CLI   STYPE,C'U'          NO, LINE 1 OF TRANSACTION ?                  
         BE    DISPTRS             YES                                          
         CLI   STYPE,C'V'          NO, LINE 2 OF TRANSACTION ?                  
         BE    DISPTRS2            YES                                          
         CLI   STYPE,C'Y'          NO, TOTAL LINE ?                             
         BE    DISP100             YES                                          
         CLI   STYPE,C'Z'          NO, IS THIS A FINAL TOTAL ?                  
         BNE   DISP120             NO, MUST BE A BLANK LINE                     
         CLI   FNDONE,X'01'        YES, WAS ONE ALREADY ENCOUNTERED ?           
         BE    DISP100             YES                                          
         MVI   NEEDED,X'03'        NO, MAKE ROOM FOR THEM                       
         BAS   RE,CHKFIT                                                        
         OI    FNDONE,X'01'        SET FOR NEXT TIME THROUGH                    
         B     DISP080             NOW CHECK LINE COUNT                         
*                                                                               
         USING TOTALD,R2                                                        
DISP100  MVC   TOTNAME,STOTAL      YES                                          
         EDIT  SAMOUNT,(11,TOTAMT),2,MINUS=YES                                  
         OI    1(R2),X'08'         MAKE TOTAL HIGH INTENSITY                    
*                                                                               
DISP120  BAS   RE,BUMP2            GET NEXT LINE                                
         LA    R5,1(R5)            INCREMENT STORAGE INDEX                      
         LA    R4,SLENGTH(R4)       AND GET ENTRY FROM STORAGE                  
         CLI   0(R4),X'FF'         END OF DATA ?                                
         BNE   DISP080             NO, PRINT NEXT LINE                          
*                                                                               
DISPX    STC   R5,NXTSTORE         YES, SAVE POINTER TO STORAGE                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINES TO PUT OUT PURCHASE ORDER AND TRANSACTION TITLES AND TO    *         
* DETERMINE IF THERE IS ENOUGH ROOM TO PRINT ALL LINES OF TR TITLE    *         
***********************************************************************         
*                                                                               
POHEAD   MVC   8(L'JOBDATA,R2),POTIT1                                           
         OI    1(R2),X'08'                                                      
         BAS   RE,BUMP2                                                         
         MVC   8(L'JOBDATA,R2),POTIT2                                           
         OI    1(R2),X'08'                                                      
         BAS   RE,BUMP2                                                         
         B     DISP080                                                          
         SPACE 3                                                                
TRHEAD   MVI   NEEDED,X'04'        GET NUMBER OF LINES NEEDED                   
         BAS   RE,CHKFIT                                                        
         MVC   8(L'JOBDATA,R2),TRTIT1                                           
         OI    1(R2),X'08'                                                      
         BAS   RE,BUMP2                                                         
         MVC   8(L'JOBDATA,R2),TRTIT2                                           
         OI    1(R2),X'08'                                                      
         BAS   RE,BUMP2                                                         
         OI    TRDONE,X'01'                                                     
         B     DISP080                                                          
         SPACE 3                                                                
CHKFIT   ST    RE,SAVERE                                                        
         SR    RE,RE               GET NUMBER OF LINES USED                     
         IC    RE,NUMLINES                                                      
         SR    RF,RF                                                            
         IC    RF,NEEDED           GET MINUMUM NUMBER OF LINES NEEDED           
         AR    RE,RF               ADD THEM TOGETHER                            
         CH    RE,=YL2(MAXLINE)    IF NOT OVER MAX, PUT IT IN STORAGE           
         BNH   CHKFX                                                            
*                                                                               
CHKF020  SH    RE,=YL2(MAXLINE)    IF OVER MAX, SUBTRACT MAX UNTIL              
         BNM   CHKF020              NEGATIVE                                    
         AH    RE,=YL2(MAXLINE)    ONCE NEGATIVE, ADD BACK MAXLINE              
         SR    RF,RE               SUBTRACT RESULT FROM LINES ADDED             
         BNP   CHKFX               IF RESULT NEGATIVE, NO BLANKS NEEDED         
*                                                                               
CHKF040  BAS   RE,BUMP2            IF POSITIVE, PUT OUT BLANKS                  
         BCT   RF,CHKF040                                                       
*                                                                               
CHKFX    L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PURCHASE ORDER DATA                              *         
***********************************************************************         
*                                                                               
         USING PODSECT,R2                                                       
DISPPOS  MVC   POSUPP(14),SACCT+1                                               
         GOTO1 DATCON,DMCB,(1,SDATE),(10,PODATE)                                
         MVC   PONUMBER,SNUMB                                                   
         MVC   POAUTHOR,SAUTHOR                                                 
         EDIT  SAMOUNT,(11,POAMOUNT),2,MINUS=YES                                
         B     DISP120                                                          
*                                                                               
DISPOS2  MVC   POSUPP,SACCTNAM                                                  
         B     DISP120                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY TRANSACTION DATA                                 *         
***********************************************************************         
*                                                                               
         USING TRDSECT,R2                                                       
DISPTRS  CLI   TRDONE,X'01'        DO WE HAVE A HEADING ?                       
         BNE   TRHEAD                                                           
         MVC   TRCONTRA(14),SACCT+1                                             
         GOTO1 DATCON,DMCB,(1,SDATE),(10,TRDATE)                                
         MVC   TRREF,SNUMB                                                      
         MVC   TRBATCH,SBATCH                                                   
         MVC   TRDESC,SDESC                                                     
         MVC   TRBILL,SB                                                        
         EDIT  SAMOUNT,(11,TRAMOUNT),2,MINUS=YES                                
         B     DISP120                                                          
*                                                                               
DISPTRS2 MVC   TRCONTRA,SACCTNAM                                                
         B     DISP120                                                          
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LOAD THE BUFFER WHERE SCREEN DATA IS TO BE STORED                   *         
***********************************************************************         
*                                                                               
LOAD     NTR1                                                                   
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,DMCB             SET LOAD POINT FOR BUFFER                    
         MVC   DMCB+4(3),SYSPHASE                                               
         MVI   DMCB+7,X'50'        SET BUFFER OVERLAY NUMBER                    
         GOTO1 CALLOV,DMCB                                                      
         CLI   DMCB+4,X'FF'        TEST LOAD OK                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ASTORAGE,DMCB       RF=A(BUFFER PHASE)                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SUB-ROUTINE TO PUT STORAGE DATA FROM BUFF TO TWA 2                  *         
***********************************************************************         
*                                                                               
PUTTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2            PAGE=TWA2                                    
         MVC   DMCB+10(2),TERM     TERMINAL NUMBER                              
         GOTO1 DATAMGR,DMCB,=CL8'DMWRT',=C'TEMPSTR',,ASTORAGE                   
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 3                                                                
***********************************************************************         
* SUB-ROUTINE TO GET STORAGE DATA FROM TWA2                           *         
***********************************************************************         
*                                                                               
GETTAB   ST    RE,SAVERE                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVI   DMCB+8,2                                                         
         MVC   DMCB+10(2),TERM                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'4800'                                              
         GOTO1 DATAMGR,DMCB,=CL8'DMREAD',=C'TEMPSTR',,BUFFER                    
         LA    RE,BUFFER                                                        
         ST    RE,ASTORAGE                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* READ RECORDS AND DETERMINE WHETHER PURCHASE ORDER OR TRANSACTION    *         
* AND BRANCH TO APPROPRIATE ROUTINE TO READ ELEMENTS                  *         
***********************************************************************         
*                                                                               
READALL  NTR1                                                                   
         L     R4,ASTORAGE         ADDRESS STORAGE AREA                         
         USING STORED,R4                                                        
         LA    R3,0                GET STARTING DISPLACEMENT                    
*                                                                               
         LA    R6,KEY                                                           
         USING ACKEYD,R6                                                        
         XC    KEY,KEY             READ FIRST RECORD                            
         MVC   ACKEYACC(3),CUL                                                  
         MVC   ACKEYACC+3(3),QCLNT                                              
         MVC   ACKEYACC+6(3),QPROD                                              
         MVC   ACKEYACC+9(6),QJOB                                               
         GOTO1 HIGH                                                             
         B     REALL060                                                         
*                                                                               
REALL020 MVC   KEY,SEQKEY          GET KEY OF LAST RECORD                       
         GOTO1 HIGH                RE-READ IT                                   
*                                                                               
REALL040 GOTO1 SEQ                 GET NEXT RECORD                              
         L     R6,AIO                                                           
*                                                                               
REALL060 CLC   KEYSAVE(L'ACKEYACC),KEY    ACCOUNT CHANGED - EXIT                
         BNE   REALL100                                                         
         MVC   SEQKEY,KEY          SAVE KEY FOR RE-READ                         
         CLC   ACKEYWRK,=C'99'     DON'T WANT BILLING                           
         BE    REALL040                                                         
*                                                                               
         CLC   ACKEYWRK,=C'**'     IF NOT AN ORDER, SEE IF WE                   
         BE    READPOS              WANT THIS WORK CODE                         
         CLC   ACKEYWRK,QWORK                                                   
         BNE   REALL040                                                         
*                                                                               
         MVI   SB,C' '             DETERMINE WHETHER BILLED OR NOT              
         OC    ACDTUSED,ACDTUSED                                                
         BZ    *+8                                                              
         MVI   SB,C'*'                                                          
*                                                                               
         CP    POCTR,=P'0'         IF P.O. TOTAL ZERO, TOTALS ALREADY           
         BE    READTRNS             DONE OR THERE WEREN'T ANY                   
         LA    RF,READTRNS         LOAD RETURN POINT                            
         B     DOPOTOTS             AND BRANCH TO PUT OUT TOTALS                
*                                                                               
REALL080 MVC   AIO,AIO1            SWAP BUFFERS BACK                            
         BAS   RE,BUMP4            GET NEXT STORAGE SLOT                        
         B     REALL020            RE-READ THIS ONE                             
*                                                                               
REALL100 CP    TRCTR,=P'0'         IF TRANSACTION COUNTER ZERO,                 
         BE    REALL120             DO TOTALS FOR P.O.'S                        
         BAS   RE,BUMP4            LEAVE A BLANK SPOT                           
         MVI   STYPE,C'Y'          INDICATE THIS IS A TOTAL                     
         MVC   STOTAL,TRTIT3       MOVE IN HEADING                              
         MVC   SAMOUNT,TRTOTAL      AND TOTAL AMOUNT                            
         ZAP   TRCTR,=P'0'         CLEAR THE COUNTER                            
         BAS   RE,BUMP4                                                         
         B     REALL140                                                         
*                                                                               
REALL120 CP    POCTR,=P'0'         SHOULD WE DO P.O. TOTALS ?                   
         BE    REALL140            NO                                           
         LA    RF,REALL140         YES, LOAD RETURN ADDRESS                     
         B     DOPOTOTS            DO P.O. TOTALS                               
*                                                                               
REALL140 LTR   R3,R3               DO WE HAVE ANY DATA ?                        
         BZ    REALLX              NO, EXIT                                     
*                                                                               
         ZAP   M1TOTAL,POTOTAL                                                  
         AP    M1TOTAL,TRTOTAL                                                  
         MVI   STYPE,C'Z'                                                       
         MVC   STOTAL,MISCT1                                                    
         MVC   SAMOUNT,M1TOTAL                                                  
         BAS   RE,BUMP4                                                         
*                                                                               
         BAS   RE,CALCEST          CALCULATE TOTAL ESTIMATE                     
         MVI   STYPE,C'Z'          INDICATE THIS IS A TOTAL                     
         MVC   STOTAL,MISCT2                                                    
         MVC   SAMOUNT,M2TOTAL                                                  
         BAS   RE,BUMP4                                                         
*                                                                               
         MVI   STYPE,C'Z'                                                       
         MVC   STOTAL,MISCT3                                                    
         ZAP   M3TOTAL,M2TOTAL                                                  
         SP    M3TOTAL,M1TOTAL                                                  
         MVC   SAMOUNT,M3TOTAL                                                  
         BAS   RE,BUMP4                                                         
*                                                                               
REALLX   MVI   STYPE,X'FF'         INDICATE END OF DATA                         
         STC   R3,LSTSTORE         SAVE DISPLACEMENT OF LAST LINE               
         B     XIT                                                              
         SPACE 3                                                                
DOPOTOTS BAS   RE,BUMP4            LEAVE A BLANK SPOT                           
         MVI   STYPE,C'Y'          INDICATE THIS IS A TOTAL                     
         MVC   STOTAL,POTIT3       MOVE IN HEADING                              
         MVC   SAMOUNT,POTOTAL      AND TOTAL AMOUNT                            
         ZAP   POCTR,=P'0'         CLEAR THE COUNTER                            
         BAS   RE,BUMP4                                                         
         BR    RF                  RETURN                                       
         EJECT                                                                  
***********************************************************************         
* READ P.O.'S FOR A PARTICULAR JOB AND SAVE IN STORAGE FOR DISPLAY.   *         
***********************************************************************         
*                                                                               
READPOS  MVI   ELCODE,X'44'        FIND THE TRANSACTION ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   REALL040            GET NEXT RECORD IF NONE                      
*                                                                               
         L     RE,AIO                                                           
         USING TRNRECD,RE                                                       
         CLI   QDRAFT,C'Y'         INCLUDE DRAFT & LIVE TRANSACTIONS ?          
         BE    REPOS010            YES                                          
         TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT TRANSACTION ?                
         BZ    REPOS005            NO                                           
         CLI   QDRAFT,C'O'         YES, DO WE WANT DRAFTS ONLY ?                
         BE    REPOS010            YES                                          
         B     REALL040            NO, SKIP IT                                  
*                                                                               
REPOS005 CLI   QDRAFT,C'O'         THIS IS LIVE, DO WE WANT IT ?                
         BE    REALL040            NO, ONLY WANT DRAFTS                         
         DROP  RE                                                               
*                                                                               
         USING TRANSD,R6                                                        
REPOS010 MVC   SDATE,TRNSDATE      MOVE DATE AND REFERENCE                      
         MVC   SNUMB,TRNSREF                                                    
         ZAP   SAMOUNT,=P'0'       CLEAR AMOUNT FIELD                           
         MVI   SWT68,C' '          AND 68 INDICATOR                             
*                                                                               
         MVI   ELCODE,X'68'        GET ORDER AMOUNT ELEMENT                     
         BAS   RE,GETELIO                                                       
         BNE   REALL020            NO ORDER AMOUNT ELEMENT, GET NEXT            
*                                                                               
         USING ACOAMTD,R6                                                       
REPOS020 CLC   ACOAWC,QWORK        LOOK FOR WORKCODE                            
         BNE   REPOS070                                                         
         MVI   SWT68,C'Y'          INDICATE WE HAVE AN ELEMENT                  
         CLI   QSUFFIX,C' '                                                     
         BE    REPOS060                                                         
         TM    ACOASTAT,X'80'      IS ITEM NON-COMMISSIONABLE ?                 
         BO    REPOS080            YES                                          
*                                                                               
REPOS040 CLI   QSUFFIX,C'C'        NO, DO WE WANT COMMISSIONABLE ?              
         BNE   REPOS070            NO, GET NEXT ELEMENT                         
         B     REPOS060            YES, ADD TO BUCKET                           
*                                                                               
REPOS080 CLI   QSUFFIX,C'N'        NON-COMMISSIONABLE- DO WE WANT IT ?          
         BNE   REPOS070            NO                                           
*                                                                               
REPOS060 AP    SAMOUNT,ACOAMT      YES, ADD TO BUCKET                           
         SP    SAMOUNT,ACOAIVAL    OPEN AMOUNT = ORDER AMT - INVOICED           
*                                                                               
REPOS070 BAS   RE,NEXTEL           AND GET THE NEXT ELEMENT                     
         BE    REPOS020                                                         
         CLI   SWT68,C'Y'                                                       
         BNE   REALL020                                                         
*                                                                               
REPOS100 MVC   AIO,AIO2            SWAP BUFFERS FOR THIS                        
         LA    R6,KEY                                                           
         USING ACKEYD,R6                                                        
         MVC   ACKEYDTE(ACRECORD-ACKEYDTE),SPACES                               
         CLI   EMULATE,C'Y'        IS THIS AN EMULATED FILE ?                   
         BNE   *+10                NO, SPACES OK. YES, BINARY ZEROS             
         XC    ACKEYD+36(6),ACKEYD+36                                           
         GOTO1 HIGH                READ FOR SUB-ACCOUNT                         
         CLC   KEYSAVE(ACKEYREF-ACKEYD),KEY                                     
         BE    *+6                                                              
         DC    H'0'                WE MUST FIND IT                              
*                                                                               
         MVI   ELCODE,X'43'        GET THE HEADER ELEMENT NOW                   
         BAS   RE,GETELIO2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSUBHD,R6                                                       
REPOS120 AP    POCTR,=P'1'         ADD 1 TO COUNTER                             
         MVC   SACCT,TRSBACNT      GET THE SUB-ACCOUNT NUMBER                   
         MVC   SVACCTNM,SPACES                                                  
         CLC   TRSBLEN,=YL1(TRSBNAME-TRSBEL)                                    
         BE    REPOS130                                                         
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=YL2(TRSBNAME-TRSBEL)                                         
         CH    R1,=H'15'                                                        
         BNH   *+8                                                              
         LH    R1,=H'15'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVACCTNM(0),TRSBNAME   SAVE NAME FOR NEXT ENTRY                  
*                                                                               
REPOS130 AP    POTOTAL,SAMOUNT     AND ADD AMOUNT TO TOTAL                      
*                                                                               
         LA    R6,KEY                                                           
         USING ACOKEY,R6                                                        
         XC    ACOKEY,ACOKEY       NOW READ ORDER RECORD                        
         MVI   ACOKCODE,ACOKCODQ                                                
         MVC   ACOKCOMP,CUL                                                     
         MVC   ACOKNUM,SNUMB                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(ACOKNUM-ACOKEY+6),KEY                                    
         BNE   REPOS140                                                         
*                                                                               
         MVI   ELCODE,X'67'        GET THE ORDER ELEMENT                        
         BAS   RE,GETELIO2                                                      
         BNE   REPOS140                                                         
         USING ACORDRD,R6                                                       
         MVC   SAUTHOR(L'ACORAUTH),ACORAUTH  SAVE AUTHORIZATION                 
*                                                                               
REPOS140 MVI   STYPE,C'S'          INDICATE LINE 1 OF P.O.                      
         BAS   RE,BUMP4                                                         
         MVC   SACCTNAM,SVACCTNM   PUT OUT NAME NOW                             
         MVI   STYPE,C'T'          INDICATE LINE 2 OF P.O.                      
         B     REALL080                                                         
         EJECT                                                                  
***********************************************************************         
* READ TRANSACTIONS FOR A PARTICULAR JOB AND SAVE IN STORAGE          *         
* FOR DISPLAY                                                         *         
***********************************************************************         
*                                                                               
READTRNS MVI   ELCODE,X'44'        FIND THE TRANSACTION ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   REALL040            GET NEXT RECORD IF NONE                      
*                                                                               
* CONVERT OLD ELEMENTS TO NEW STYLE ELEMENTS                                    
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'80',AIO),0,ACOMFACS,0,PROBLOCK                  
*                                                                               
         L     RE,AIO                                                           
         USING TRNRECD,RE                                                       
         CLI   QDRAFT,C'Y'         INCLUDE DRAFT & LIVE TRANSACTIONS ?          
         BE    RETRN020            YES                                          
         TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT TRANSACTION ?                
         BZ    RETRN010            NO                                           
         CLI   QDRAFT,C'O'         YES, DO WE WANT DRAFTS ONLY ?                
         BE    RETRN020            YES                                          
         B     REALL040            NO, SKIP IT                                  
*                                                                               
RETRN010 CLI   QDRAFT,C'O'         THIS IS LIVE, DO WE WANT IT ?                
         BE    REALL040            NO, ONLY WANT DRAFTS                         
         DROP  RE                                                               
*                                                                               
         USING TRANSD,R6                                                        
RETRN020 MVC   SDATE,TRNSDATE      SAVE DATA                                    
         MVC   SNUMB,TRNSREF                                                    
         MVC   SBATCH,TRNSBTCH                                                  
         MVC   SAMOUNT,TRNSAMNT                                                 
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN X-JOB ?                           
         BZ    *+8                 NO                                           
         BAS   RE,GETXAMT          YES, GET AMOUNT FROM X'50' ELEMENT           
*                                                                               
         MVC   SDESC,SPACES                                                     
         SR    RF,RF                                                            
         IC    RF,TRNSLEN                                                       
         SH    RF,=AL2(TRNSNARR-TRNSEL)                                         
         BZ    RETRN040                                                         
         CH    RF,=H'20'                                                        
         BNH   *+8                                                              
         LA    RF,20                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     RETRN040                                                         
         MVC   SDESC(0),TRNSNARR                                                
*                                                                               
RETRN040 CP    PA$NETBL,=P'0'      ANY CREATIVE OR PARTIAL BILLING ?            
         BE    RETRN120                                                         
         MVI   SB,C'*'                                                          
         CP    SAMOUNT,PA$NETBL                                                 
         BE    RETRN120                                                         
         MVI   SB,C'P'                                                          
*                                                                               
RETRN120 MVC   AIO,AIO2            SWAP BUFFERS FOR THIS                        
         LA    R6,KEY                                                           
         USING ACKEYD,R6                                                        
         MVC   ACKEYDTE(ACRECORD-ACKEYDTE),SPACES                               
         CLI   EMULATE,C'Y'        IS THIS AN EMULATED FILE ?                   
         BNE   *+10                NO, SPACES OK. YES, BINARY ZEROS             
         XC    ACKEYD+36(6),ACKEYD+36                                           
         GOTO1 HIGH                READ FOR SUB-ACCOUNT                         
         CLC   KEYSAVE(ACKEYREF-ACKEYD),KEY                                     
         BE    *+6                                                              
         DC    H'0'                WE MUST FIND IT                              
*                                                                               
         MVI   ELCODE,X'43'        GET THE HEADER ELEMENT NOW                   
         BAS   RE,GETELIO2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSUBHD,R6                                                       
RETRN140 AP    TRCTR,=P'1'         ADD 1 TO COUNTER                             
         MVC   SACCT,TRSBACNT      GET THE SUB-ACCOUNT NUMBER                   
         AP    TRTOTAL,SAMOUNT                                                  
*                                                                               
         MVI   STYPE,C'U'          INDICATE LINE 1 OF TRANSACTION               
         BAS   RE,BUMP4                                                         
         MVC   SACCTNAM,SPACES                                                  
         CLC   TRSBLEN,=YL1(TRSBNAME-TRSBEL)                                    
         BE    RETRN160                                                         
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=YL2(TRSBNAME-TRSBEL)                                         
         CH    R1,=H'15'                                                        
         BNH   *+8                                                              
         LH    R1,=H'15'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SACCTNAM(0),TRSBNAME   PUT OUT NAME NOW                          
*                                                                               
RETRN160 MVI   STYPE,C'V'          INDICATE LINE 2 OF TRANSACTION               
         B     REALL080                                                         
         DROP  R6                                                               
*                                                                               
* SUB-ROUTINE TO GET AMOUNT FROM X'50' ELEMENT FOR X-JOBS                       
*                                                                               
         USING TRCASHD,R6                                                       
GETXAMT  NTR1  ,                                                                
         MVI   ELCODE,TRCSELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    GETXAMT4                                                         
         B     GETXAMTX            USE TRANSACTION AMOUNT IF NO X'50'           
*                                                                               
GETXAMT2 SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),0             END OF RECORD ?                              
         BE    GETXAMTX            YES                                          
         CLI   0(R6),TRCSELQ       NO, X'50' ELEMENT ?                          
         BNE   GETXAMT2            NO, KEEP LOOKING                             
*                                                                               
GETXAMT4 CLI   TRCSTYPE,C'S'       IS THIS THE EXPENSE AMOUNT ?                 
         BNE   GETXAMT2            NO                                           
         MVC   SAMOUNT,TRCSAMNT    YES, SAVE IT                                 
*                                                                               
GETXAMTX B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* READ NEW ESTIMATE RECORDS AND CALCULATE TOTAL ESTIMATE              *         
***********************************************************************         
*                                                                               
CALCEST  NTR1                                                                   
         LA    R4,KEY                                                           
         USING ACEVKEY,R4                                                       
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,QCLNT                                                    
         MVC   ACEVPROD,QPROD                                                   
         MVC   ACEVJOB,QJOB                                                     
         MVI   ACEVTYPE,ACEVREV    READ REVISIONS                               
*                                                                               
CALC020  GOTO1 HIGH                                                             
         B     CALC060                                                          
*                                                                               
CALC040  LA    R4,KEY                                                           
         GOTO1 SEQ                                                              
*                                                                               
CALC060  CLC   ACEVKEY(ACEVERS-ACEVKEY),KEYSAVE                                 
         BNE   CALC080                                                          
         LA    RF,ACEVKEY+(EVEKWC-EVEKEY)                                       
         OC    0(L'EVEKWC,RF),0(RF)  IS IT A TIME EST RECORD?                   
         BNZ   CALC040               YES - SO READ NEXT RECORD                  
*                                                                               
         L     R4,AIO                                                           
         MVC   CESTTYP,ACEVTYPE                                                 
         MVC   CESTVER,ACEVERS                                                  
*                                                                               
         MVI   ELCODE,ACEAELQ      SEE IF APPROVED ELEMENT EXISTS               
         BAS   RE,GETELIO                                                       
         BNE   CALC040                                                          
*                                                                               
         MVC   HIATYPE,ACEVTYPE                                                 
         MVC   HIAVERS,ACEVERS                                                  
         B     CALC040                                                          
*                                                                               
CALC080  CLI   CESTTYP,0                                                        
         BE    CALCX                                                            
         BAS   RE,RDOPT            READ JOB OPTIONS                             
         CLI   GONEEDAE,C'Y'       TEST NEED APPROVED ESTIMATE                  
         BNE   CALC090                                                          
         CLI   HIATYPE,0           TEST FOR ANY APPROVED ESTIMATE               
         BE    CALC090                                                          
         MVC   CESTTYP(2),HIATYPE                                               
*                                                                               
CALC090  LA    R4,KEY                                                           
         XC    ACEVKEY,ACEVKEY                                                  
         MVI   ACEVRTYP,ACEVEQU                                                 
         MVI   ACEVSREC,ACEVSEQU                                                
         MVC   ACEVCUL,CUL                                                      
         MVC   ACEVCLI,QCLNT                                                    
         MVC   ACEVPROD,QPROD                                                   
         MVC   ACEVJOB,QJOB                                                     
         MVC   ACEVTYPE,CESTTYP                                                 
         MVC   ACEVERS,CESTVER                                                  
         GOTO1 READ                                                             
*                                                                               
         MVI   ELCODE,ACEDELQ                                                   
         BAS   RE,GETELIO                                                       
         B     CALC120                                                          
         USING ACEDD,R6                                                         
*                                                                               
CALC100  MVI   ELCODE,ACEDELQ                                                   
         BAS   RE,NEXTEL                                                        
*                                                                               
CALC120  BNE   CALCX                                                            
         CLI   ACEDTYPE,1                                                       
         BNE   CALC100                                                          
         CLC   ACEDWORK,QWORK                                                   
         BNE   CALC100                                                          
         CLI   QSUFFIX,C'N'                                                     
         BE    CALC140                                                          
         AP    M2TOTAL,ACEDCOMM                                                 
         CLI   QSUFFIX,C' '                                                     
         BNE   CALCX                                                            
*                                                                               
CALC140  CLI   ACEDLEN,ACEDLNQ1                                                 
         BE    CALCX                                                            
         AP    M2TOTAL,ACEDNCOM                                                 
*                                                                               
CALCX    B     XIT                                                              
         DROP  R4,R6                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO READ THE JOB'S OPTIONS                                         
*                                                                               
RDOPT    ST    RE,SAVERE                                                        
         MVC   GOADM,DATAMGR                                                    
         MVC   GOSELCUL,CUL                                                     
         MVC   GOSELCLI,QCLNT                                                   
         MVC   GOSELPRO,QPROD                                                   
         MVC   GOSELJOB,QJOB                                                    
         MVI   GOWHICH,C'N'        NEW OPTIONS ONLY                             
         MVI   GOANYWC,C'N'                                                     
         GOTO1 GETOPT,DMCB,GOBLOCK                                              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT A REPORT FROM STORAGE DATA                         *         
***********************************************************************         
*                                                                               
PRINT    NTR1                                                                   
         BAS   RE,GETTAB                                                        
         L     R4,ASTORAGE                                                      
         USING STORED,R4                                                        
*                                                                               
         MVC   REMUSER,TWAALIAS                                                 
         GOTO1 OPENPQ                                                           
*                                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         MVI   RCSUBPRG,0                                                       
         MVI   TRDONE,X'00'                                                     
*                                                                               
         MVC   P,SPACES                                                         
         LA    R6,P-7                                                           
         USING TRDSECT,R6                                                       
*                                                                               
PRIN020  CLI   STYPE,X'FF'                                                      
         BE    PRIN140                                                          
*                                                                               
         CLI   STYPE,X'00'         BLANK LINE ?                                 
         BE    PRIN100                                                          
*                                                                               
         CLI   STYPE,C'Y'          HANDLE TOTALS DIFFERENTLY                    
         BE    PRIN120                                                          
         CLI   STYPE,C'Z'                                                       
         BE    PRIN120                                                          
*                                                                               
         MVC   RCSUBPRG,STYPE                                                   
         NI    RCSUBPRG,X'0F'      TURN OFF HIGH ORDER BITS                     
*                                                                               
         CLI   STYPE,C'U'          IS THIS A TRANSACTION ?                      
         BNE   PRIN040             NO, IGNORE THIS                              
         CLI   TRDONE,X'01'        YES, DID WE DO TITLE ALREADY?                
         MVI   TRDONE,X'01'                                                     
         BE    *+8                 YES                                          
         MVI   LINE,99             FORCE NEW PAGE                               
*                                                                               
PRIN040  MVC   TRCONTRA,SACCT      MOVE IN ACCOUNT/NAME                         
*                                                                               
         CLI   STYPE,C'T'          SECOND LINE OF P.O. ?                        
         BE    PRIN080             YES, ALL DONE                                
         CLI   STYPE,C'V'          NO, SECOND LINE OF TRANSACTION ?             
         BE    PRIN080             YES, ALL DONE                                
*                                                                               
         EDIT  SAMOUNT,(11,TRAMOUNT),2,MINUS=YES                                
*                                                                               
         GOTO1 DATCON,DMCB,(1,SDATE),(10,TRDATE)                                
*                                                                               
         MVC   TRREF,SNUMB                                                      
*                                                                               
         CLI   STYPE,C'S'          PURCHASE ORDER ?                             
         BNE   PRIN060             NO                                           
*                                                                               
         MVC   TRBATCH(L'SAUTHOR),SAUTHOR                                       
         B     PRIN080                                                          
*                                                                               
PRIN060  MVC   TRBATCH,SBATCH                                                   
         MVC   TRDESC,SDESC                                                     
         MVC   TRBILL,SB                                                        
*                                                                               
PRIN080  GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
PRIN100  LA    R4,SLENGTH(R4)                                                   
         B     PRIN020                                                          
*                                                                               
PRIN120  MVC   P,SPACES            SKIP A LINE                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   TRCONTRA(L'STOTAL),STOTAL                                        
         EDIT  SAMOUNT,(11,TRAMOUNT),2,MINUS=YES                                
         B     PRIN080                                                          
*                                                                               
PRIN140  MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R4,CONHEAD                                                       
         MVC   0(2,R4),=C'**'                                                   
         MVC   2(3,R4),SPOOLID                                                  
         MVI   5(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,6(R4)),ALIGN=LEFT                                        
         AR    R4,R0                                                            
         LA    R4,7(R4)                                                         
         MVC   0(7,R4),=C'SPOOLED'                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
BUMP2    ZIC   R0,0(R2)            BUMP TO NEXT SCREEN LINE                     
         AR    R2,R0                                                            
*                                                                               
         SR    R1,R1               INCREMENT LINE COUNT                         
         IC    R1,NUMLINES                                                      
         LA    R1,1(R1)                                                         
         STC   R1,NUMLINES                                                      
         BR    RE                  RETURN                                       
         SPACE 3                                                                
BUMP4    LA    R4,SLENGTH(R4)      BUMP TO NEXT STORAGE LOCATION                
         LA    R3,1(R3)                                                         
         CH    R3,=Y(MAXBUF)       MAKE SURE NOT OVER MAX LINES                 
         BNHR  RE                                                               
         MVI   ERROR,TOOMANY                                                    
         LA    R2,JOBCLNTH         POSITION CURSOR AT FIRST KEY FIELD           
         B     ERREXIT                                                          
         SPACE 3                                                                
MYSPECS  DS    0F                  SPECS FOR OFFLINE REPORT                     
         SPROG 2,3,4,5                                                          
         SSPEC H1,2,CREATED                                                     
         SSPEC H2,2,REQUESTOR                                                   
         SSPEC H1,45,C'JOB DETAIL REPORT'                                       
         SSPEC H2,45,C'-----------------'                                       
         SSPEC H1,85,AGYNAME                                                    
         SSPEC H2,85,AGYADD                                                     
         SSPEC H4,2,C'CLIENT          PRODUCT         JOB         W/C'          
         SSPEC H4,85,REPORT                                                     
         SSPEC H4,98,PAGE                                                       
*                                                                               
         SPROG 2,3                                                              
         SSPEC H6,26,C'*** OPEN PURCHASE ORDERS ***'                            
         SSPEC H8,2,C'SUPPLIER         __DATE__   NUMBER'                       
         SSPEC H8,40,C'__AUTHORIZED__               __AMOUNT__'                 
*                                                                               
         SPROG 4,5                                                              
         SSPEC H6,27,C'*** TRANSACTION DETAIL ***'                              
         SSPEC H8,2,C'CONTRA-ACCOUNT    __DATE__   _REF#_  BATCH'               
         SSPEC H8,48,C'DESCRIPTION          __AMOUNT__'                         
*                                                                               
         DC    H'0'                                                             
         EJECT                                                                  
HOOK     NTR1                                                                   
         MVC   H4+9(L'QCLNT),QCLNT                                              
         MVC   H4+25(L'QPROD),QPROD                                             
         MVC   H4+37(L'QJOB),QJOB                                               
         MVC   H4+49(L'QWORK),QWORK                                             
         MVC   H4+51(L'QSUFFIX),QSUFFIX                                         
         B     XIT                                                              
         EJECT                                                                  
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 3                                                                
GETELIO2 L     R6,AIO2                                                          
         GETEL2 (R6),DATADISP,ELCODE                                            
         SPACE 3                                                                
INVSUFF  MVI   ERROR,SUFFIX                                                     
         MVI   ERRNDX,X'02'                                                     
ERREXIT  GOTO1 VERRCUR                                                          
*                                                                               
INFEXIT  ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USGETTXT                                                
         GOTO1 INFOXIT                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
JDXJOB   DC    C'(EXP)'                                                         
         SPACE 3                                                                
POTIT1   DC    CL(L'JOBDATA)' '                                                 
         ORG   POTIT1+25                                                        
         DC    CL28'*** OPEN PURCHASE ORDERS ***'                               
         ORG                                                                    
         SPACE 3                                                                
POTIT2   DC    CL(L'JOBDATA)' '                                                 
         ORG   POTIT2                                                           
         DC    CL38'SUPPLIER          __DATE__   NUMBER'                        
         DC    CL39'__AUTHORIZED___              __AMOUNT__'                    
         ORG                                                                    
*                                                                               
POTIT3   DC    CL20'TOTAL OPEN ORDERS'                                          
*                                                                               
TRTIT1   DC    CL(L'JOBDATA)' '                                                 
         ORG   TRTIT1+26                                                        
         DC    CL26'*** TRANSACTION DETAIL ***'                                 
         ORG                                                                    
*                                                                               
TRTIT2   DC    CL(L'JOBDATA)' '                                                 
         ORG   TRTIT2                                                           
         DC    CL46'CONTRA-ACCOUNT    __DATE__   _REF#_  BATCH  B'              
         DC    CL32'DESCRIPTION           __AMOUNT__'                           
         ORG                                                                    
*                                                                               
TRTIT3   DC    CL20'TOTAL ACTUAL CHARGES'                                       
*                                                                               
MISCT1   DC    CL20'TOTAL COMMITTED'     OPEN P.O.'S + ACTUAL CHANGES           
*                                                                               
MISCT2   DC    CL20'TOTAL ESTIMATE'      CURRENT ESTIMATE FOR WORKCODE          
*                                                                               
MISCT3   DC    CL20'TOTAL UNCOMMITTED'   ESTIMATE - COMMITTED                   
         SPACE 3                                                                
RELO     DC    A(0)                                                             
         EJECT                                                                  
PROBLOCK DS    0C                                                               
       ++INCLUDE ACPRORATAD                                                     
         EJECT                                                                  
         LTORG                                                                  
BUFFER   DS    CL18432                                                          
MAXBUF   EQU   (L'BUFFER/SLENGTH)                                               
         EJECT                                                                  
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROC7D                                                       
         DS    0F                                                               
NUMLINES DS    X                   N'LINES ON CURRENT SCREEN                    
*                                                                               
QCLNT    DS    CL(L'ACEVCLI)                                                    
QPROD    DS    CL(L'ACEVPROD)                                                   
QJOB     DS    CL(L'ACEVJOB)                                                    
QWORK    DS    CL(L'ACEDWORK)                                                   
QSUFFIX  DS    C                                                                
QDRAFT   DS    C                                                                
*                                                                               
FSTSTORE DS    X                   # OF 1ST RECORD ON CURRENT SCREEN            
NXTSTORE DS    X                   # OF NEXT RECORD TO BE PRINTED               
LSTSTORE DS    X                   # OF LAST RECORD IN STORAGE                  
LINETBL  DS    XL15                TABLE OF NXTSTORES                           
LINEINDX DS    X                   INDEX INTO LINETBL                           
*                                                                               
ASTORAGE DS    A                   ADDRESS OF STORAGE BUFFER                    
*                                                                               
*                                                                               
MAXLINE  EQU   16                  MAXIMUM N'LINES -1 PER SCREEN                
FSTMODE  EQU   1                   FIRST TIME INDICATOR                         
DISMODE  EQU   2                   DISPLAY INDICATOR                            
         EJECT                                                                  
*                                                                               
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
SUBSYSD  DSECT                                                                  
         ORG   DRGEN               USE THE DRONEBLK AREA                        
LOCAL    DS    0C                                                               
KEYCHG   DS    C                                                                
INTMODE  DS    X                   INTERNAL MODE                                
SWT68    DS    C                                                                
NEEDED   DS    X                   NUMBER OF PRINT LINES NEEDED                 
*                                                                               
SAVERE   DS    A                   SAVE RE FOR SUB-ROUTINES                     
SEQKEY   DS    CL48                SAVE SEQUENTIAL KEY WHILE READING            
CESTTYP  DS    C                   SAVE ESTIMATE TYPE WHILE READING             
CESTVER  DS    X                   SAVE ESTIMATE VERSION WHILE READING          
HIATYPE  DS    C                   HIGHEST APPROVED ESTIMATE TYPE               
HIAVERS  DS    X                   HIGHEST APPROVED ESTIMATE VERSION            
*                                                                               
SVACCTNM DS    CL15                SAVE ACCOUNT NAME FOR SECOND LINE            
*                                                                               
POTOTAL  DS    PL6                 TOTAL OF P.O.'S                              
TRTOTAL  DS    PL6                 TOTAL OF TRANSACTIONS                        
M1TOTAL  DS    PL6                 TOTAL COMMITTED (POTOTAL+TRTOTAL)            
M2TOTAL  DS    PL6                 TOTAL ESTIMATE                               
M3TOTAL  DS    PL6                 TOTAL UNCOMMITTED (M2TOTAL-M1TOTAL)          
*                                                                               
POCTR    DS    PL2                 TOTAL # OF P.O.'S                            
TRCTR    DS    PL2                 TOTAL # OF TRANSACTIONS                      
FNDONE   DS    X                   FINAL TOTAL INDICATOR                        
TRDONE   DS    X                   TITLE INDICATOR                              
*                                                                               
AFSTLNE  DS    A                   A(FIRST LINE OF DATA)                        
APFFLD   DS    A                   A(PF LINE)                                   
AENDSCR  DS    A                   A(LAST LINE)                                 
*                                                                               
       ++INCLUDE ACGOBLOCK                                                      
*                                                                               
LOCALLN  EQU   *-LOCAL                                                          
         EJECT                                                                  
PODSECT  DSECT                                                                  
         DS    CL8                 HEADER                                       
POSUPP   DS    CL15                                                             
         DS    CL3                                                              
PODATE   DS    CL8                                                              
         DS    CL3                                                              
PONUMBER DS    CL6                                                              
         DS    CL3                                                              
POAUTHOR DS    CL15                                                             
         DS    CL14                                                             
POAMOUNT DS    CL11                                                             
         EJECT                                                                  
TRDSECT  DSECT                                                                  
         DS    CL8                 HEADER                                       
TRCONTRA DS    CL15                                                             
         DS    CL3                                                              
TRDATE   DS    CL8                                                              
         DS    CL3                                                              
TRREF    DS    CL6                                                              
         DS    CL2                                                              
TRBATCH  DS    CL6                                                              
         DS    CL1                                                              
TRBILL   DS    CL1                                                              
         DS    CL1                                                              
TRDESC   DS    CL20                                                             
         DS    CL1                                                              
TRAMOUNT DS    CL11                                                             
         EJECT                                                                  
TOTALD   DSECT                                                                  
         DS    CL8                 HEADER                                       
TOTNAME  DS    CL20                                                             
         DS    CL47                                                             
TOTAMT   DS    CL11                                                             
         EJECT                                                                  
STORED   DSECT                                                                  
STYPE    DS    C                   S = P.O. LINE 1; T = P.O. LINE2              
*                                  U = TRANS LINE 1; V = TRANS LINE 2           
*                                  Y = TOTALS; Z = FINAL TOTALS                 
SACCT    DS    CL15                SUB OR CONTRA ACCOUNT NUMBER                 
*                                                                               
         ORG   SACCT                                                            
SACCTNAM DS    CL15                SUB OR CONTRA ACCOUNT NAME                   
*                                                                               
         ORG   SACCT                                                            
STOTAL   DS    CL20                                                             
*                                                                               
         ORG                                                                    
SDATE    DS    PL3                 DATE                                         
SNUMB    DS    CL6                 INVOICE OR REFERENCE NUMBER                  
SBATCH   DS    CL6                 BATCH NUMBER                                 
SAUTHOR  DS    CL20                AUTHORIZER                                   
*                                                                               
         ORG   SAUTHOR                                                          
SDESC    DS    CL20                DESCRIPTION                                  
SB       DS    C                                                                
SAMOUNT  DS    PL6                 AMOUNT                                       
*                                                                               
         ORG   SACCT                                                            
STITLE   DS    CL(L'JOBDATA)                                                    
SLENGTH  EQU   *-STYPE                                                          
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACPRO37   09/12/02'                                      
         END                                                                    
