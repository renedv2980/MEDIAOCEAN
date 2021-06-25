*          DATA SET SRISG00    AT LEVEL 002 AS OF 03/02/12                      
*PHASE T18800A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$ISGQ - DISPLAY/DEQUEUE CURRENT ENQUEUES'                       
         PRINT NOGEN                                                            
ISGQ     RSECT                                                                  
         NMOD1 WRKX-WRKD,*$ISGQ**,R9,RA,CLEAR=YES,RR=R4                         
         USING WRKD,RC                                                          
         ST    RD,BASERD                                                        
         ST    R4,RELO                                                          
         USING SRPARMD,R1          R1=A(S/R PARAM LIST)                         
         L     R8,SRQASYSF                                                      
         USING SYSFACD,R8          R8=A(SYS FAC LIST)                           
         L     R3,SRQATWA                                                       
         USING SRISGFFD,R3         R3=A(TWA)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ASAVE            USE TIA AS SAVE AREA                         
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         DROP  RF                                                               
*                                                                               
INIT010  L     RF,SRQACOMF         EXTRACT COMFACS ADDRESSES                    
         USING COMFACSD,RF         RF=A(COM FAC LIST)                           
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETDAY,CGETDAY                                                  
         DROP  R1,RF                                                            
*                                                                               
         L     RF,=V(SQUASHER)     SAVE OTHER ROUTINES                          
         A     RF,RELO                                                          
         ST    RF,ASQUASH                                                       
*                                                                               
         L     RF,VSSB             EXTRACT SSB DATA                             
         MVC   RECLEN,SSBTWAL-SSBD(RF)                                          
*                                                                               
         LA    R4,DMCB             EXTRACT CURRENT JOB NAME/DATASPACE           
         EXTRACT (4),'S',FIELDS=TIOT                                            
         L     R4,DMCB                                                          
         MVC   THISJOB,0(R4)                                                    
         EJECT                                                                  
*************************************************************                   
*        MAIN CONTROL                                       *                   
*************************************************************                   
         SPACE 1                                                                
         BAS   RE,READSTR                                                       
*                                                                               
         CLI   SRVMAJH+5,0         IF NO MAJOR NAME,                            
         BNE   MAIN10                                                           
         MVC   SRVMAJ(6),=C'DDSENQ' DEFAULT TO DDDSENQ                          
         MVI   SRVMAJH+5,6                                                      
         BAS   RE,CLRSAVE          AND START OVER                               
*                                                                               
MAIN10   BAS   RE,PARMVAL          VALIDATE PARAMETERS                          
*                                                                               
         BAS   RE,COMPPARM         COMPARE PARAMETERS EXCEPT PASSWORD           
         BE    MAIN15                                                           
         BAS   RE,CLRSAVE          NEW REQUEST. CLEAR SAVED AREA                
         B     MAIN20                                                           
*                                                                               
MAIN15   CLI   SRVPSWH+5,0         CK IF ALL THAT CHANGED WAS THE USER          
         BE    MAIN20              ADDING OR CHANGING A PASSWORD                
         BAS   RE,COMPPSW                                                       
         BE    MAIN20                                                           
         MVI   CHGFLAG,PSWONLY                                                  
*                                                                               
MAIN20   BAS   RE,SAVEPARM         SAVE NEW PARAMETERS                          
*                                                                               
         MVC   SRVHDR,ENQHEADR     DISPLAY HEADER LINE                          
         MVC   SRVHDR1,ENQUNDER    UNDERLINE                                    
*                                                                               
         BAS   RE,VALACT           VALIDATE ACTION                              
         CLI   ACTFLAG,HAVEACT                                                  
         BNE   MAIN30                                                           
         BAS   RE,LASTSCRN         REDISPLAY LAST SCREEN                        
         BAS   RE,ACTION           PERFORM ACTION(S)                            
         B     EXIT                                                             
*                                                                               
MAIN30   CLI   CHGFLAG,PSWONLY     IF ALL THAT CHANGED WAS THE USER             
         BNE   MAIN40              ADDING OR CHANGING A PASSWORD,               
         BAS   RE,LASTSCRN         REDISPLAY LAST SCREEN INSTEAD OF             
         BAS   RE,VALPASW          GOING TO NEXT PAGE.                          
         BE    EXIT                                                             
         LA    R1,SRVPSWH                                                       
         ST    R1,CURSOR                                                        
         B     ERR7                                                             
*                                                                               
MAIN40   BAS   RE,QUERY            DISPLAY CURRENT ENQUEUES                     
*                                                                               
EXIT     BAS   RE,WRITESTR         WRITE SAVED STORAGE                          
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
*                                                                               
EXIT1    XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
*        VALIDATE PARMS                                     *                   
*************************************************************                   
         SPACE 1                                                                
PARMVAL  NTR1                                                                   
*                                                                               
MAJVAL   LA    R7,SRVMAJH          *MAJOR NAME*                                 
         ST    R7,CURSOR                                                        
         ZIC   R1,5(R7)                                                         
         LTR   R1,R1                                                            
         BZ    ERR2                MISSING MAJOR NAME                           
         BCTR  R1,0                                                             
         MVC   MAJNAME,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MAJNAME(0),8(R7) SAVE MAJOR NAME PADDED WITH SPACES              
MAJVALX  EQU   *                                                                
*                                                                               
MINVAL   LA    R7,SRVMINH          *MINOR NAME*                                 
         ST    R7,CURSOR                                                        
         XC    MINNAME,MINNAME                                                  
         ZIC   R1,5(R7)                                                         
         LTR   R1,R1                                                            
         BZ    MINVALX             MINOR NAME IS NOT REQUIRED                   
         STC   R1,MINNAML          TRUE LENGTH OF MINOR NAME                    
         BCTR  R1,0                                                             
         MVC   MINNAME,SPACES                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MINNAME(0),8(R7)    SAVE MINOR NAME PADDED WITH SPACES           
MINVALX  EQU   *                                                                
*                                                                               
THISVAL  LA    R7,SRVTHSH          *FILTER ON CURRENT JOB/DATASPACE*            
         ST    R7,CURSOR                                                        
         ZIC   R1,5(R7)                                                         
         LTR   R1,R1                                                            
         BNZ   THISVAL5                                                         
         MVI   5(R7),1             DEFAULT: NO FILTER                           
         MVI   8(R7),C'N'                                                       
         B     THISVALX                                                         
THISVAL5 CLI   8(R7),C'N'                                                       
         BE    THISVALX                                                         
         CLI   8(R7),C'Y'                                                       
         BNE   ERR1                                                             
THISVALX EQU   *                                                                
*                                                                               
         LA    R7,SRVACTH          ALL PARAMETERS VALIDATED OK                  
         ST    R7,CURSOR           PUT CURSOR ON FIRST ACTION FIELD             
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        READ IN SAVED STORAGE                              *                   
*************************************************************                   
         SPACE 1                                                                
READSTR  NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),ISGQID  TEST FOR MY ID                               
         BNE   READX                                                            
         LA    R0,SAVEDSTR                                                      
         LHI   R1,SAVEDL                                                        
         LA    RE,SRCOMWRK+4                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
READX    B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        WRITE OUT SAVED STORAGE                            *                   
*************************************************************                   
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R0,SAVEDSTR                                                      
         LHI   R1,SAVEDL                                                        
         LA    RE,SRCOMWRK+4                                                    
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   0(4,R1),ISGQID                                                   
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
XIT1     XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        PERFORM QUERY ON CURRENT ENQUEUES                  *                   
* NOTES: THE ISGQUERY MACRO RETURNS AS MANY ENQS AS IT CAN  *                   
* FIT IN THE ANSAREA. AFTER PROCESSING THESE ENQS, WE WILL  *                   
* RESUME THE QUERY, WHICH WILL BRING IN THE NEXT BATCH OF   *                   
* ENQS. IF WE ARE IN THE MIDDLE OF THE SCREEN, A MESSAGE TO *                   
* PRESS ENTER WILL COME UP, AND THE NEXT BATCH WILL BE      *                   
* DISPLAYED STARTING WITH A FRESH SCREEN.                   *                   
*************************************************************                   
         SPACE 1                                                                
QUERY    NTR1                                                                   
*                                                                               
         OC    ENQSCNT,ENQSCNT  UPDATE START COUNT                              
         BNZ   QUE10                                                            
         MVC   ENQSCNT,=A(1)   NEW QUERY, START FROM 1                          
         B     QUE20                                                            
QUE10    L     R0,ENQECNT                                                       
         AHI   R0,1                                                             
         ST    R0,ENQSCNT                                                       
*                                                                               
QUE20    TM    FLAG,MOREQ      DISPLAY NEXT PAGE FROM ANSAREA                   
         BNO   QUE30                                                            
         ZIC   RE,PAGE#                                                         
         AHI   RE,1                                                             
         STC   RE,PAGE#                                                         
         B     QSHOW                                                            
*                                                                               
QUE30    MVI   PAGE#,1                                                          
         TM    FLAG,RESUMEQ    RESUME A PREVIOUS QUERY                          
         BNO   QUE40                                                            
         NI    FLAG,X'FF'-RESUMEQ                                               
         B     QUE140                                                           
*                                                                               
QUE40    CLI   SRVTHS,C'Y'         FILTER ON CURRENT JOB/DATASPACE              
         BE    QUE120                                                           
         OC    MINNAME,MINNAME     FILTER ON MINOR NAME                         
         BNZ   QUE110                                                           
*NO FILTER ON MINOR NAME                                                        
QUE100   ISGQUERY REQINFO=QSCAN,SCANACTION=START,SEARCH=BY_FILTER,     +        
               ANSAREA=ANSAREA,ANSLEN=AREALEN,ANSDETAIL=FULL2,         +        
               QNAMEMATCH=SPECIFIC,QNAME=MAJNAME,RNAMEMATCH=ANY,       +        
               RESUMETOKEN=RESTOKEN,RETCODE=RETCODE,RSNCODE=RSNCODE,   +        
               MF=(E,QLIST)                                                     
         B     QUE200                                                           
*FILTER ON MINOR NAME (COULD BE A PATTERN)                                      
QUE110   ISGQUERY REQINFO=QSCAN,SCANACTION=START,SEARCH=BY_FILTER,     +        
               ANSAREA=ANSAREA,ANSLEN=AREALEN,ANSDETAIL=FULL2,         +        
               QNAMEMATCH=SPECIFIC,QNAME=MAJNAME,                      +        
               RNAMEMATCH=PATTERN,RNAME=MINNAME,RNAMELEN=MINNAML,      +        
               RESUMETOKEN=RESTOKEN,RETCODE=RETCODE,RSNCODE=RSNCODE,   +        
               MF=(E,QLIST)                                                     
         B     QUE200                                                           
QUE120   OC    MINNAME,MINNAME                                                  
         BNZ   QUE130                                                           
*FILTER ON JOB/DATASPACE, NO FILTER ON MINOR NAME                               
         ISGQUERY REQINFO=QSCAN,SCANACTION=START,SEARCH=BY_FILTER,     +        
               ANSAREA=ANSAREA,ANSLEN=AREALEN,ANSDETAIL=FULL2,         +        
               QNAMEMATCH=SPECIFIC,QNAME=MAJNAME,RNAMEMATCH=ANY,       +        
               JOBNAME=THISJOB,                                        +        
               RESUMETOKEN=RESTOKEN,RETCODE=RETCODE,RSNCODE=RSNCODE,   +        
               MF=(E,QLIST)                                                     
         B     QUE200                                                           
*FILTER ON JOB/DATASPACE AND MINOR NAME (COULD BE A PATTERN)                    
QUE130   ISGQUERY REQINFO=QSCAN,SCANACTION=START,SEARCH=BY_FILTER,     +        
               ANSAREA=ANSAREA,ANSLEN=AREALEN,ANSDETAIL=FULL2,         +        
               QNAMEMATCH=SPECIFIC,QNAME=MAJNAME,                      +        
               RNAMEMATCH=PATTERN,RNAME=MINNAME,RNAMELEN=MINNAML,      +        
               JOBNAME=THISJOB,                                        +        
               RESUMETOKEN=RESTOKEN,RETCODE=RETCODE,RSNCODE=RSNCODE,   +        
               MF=(E,QLIST)                                                     
         B     QUE200                                                           
*RESUME A PREVIOUS QUERY                                                        
QUE140   ISGQUERY REQINFO=QSCAN,SCANACTION=RESUME,                     +        
               ANSAREA=ANSAREA,ANSLEN=AREALEN,                         +        
               RESUMETOKEN=RESTOKEN,RETCODE=RETCODE,RSNCODE=RSNCODE,   +        
               MF=(E,QLIST)                                                     
         B     QUE200                                                           
*                                                                               
QUE200   CLC   RETCODE,=A(ISGQUERYRC_OK)    SUCCESSFUL RETURN?                  
         BE    QUE300              YES, DIG OUT THE ENQUEUES                    
         CLC   RETCODE,=A(ISGQUERYRC_WARN)                                      
         BNE   ERR4                ISGQ ERROR                                   
         MVC   FULL,RSNCODE        ISGQ WARNINGS                                
         NC    FULL,=X'0000FFFF'                                                
         CLC   FULL,=A(ISGQUERYRSN_ANSWERAREAFULL)                              
         BNE   QUE300              ANSAREA IS FULL. WILL NEED TO RESUME         
         OI    FLAG,RESUMEQ        QUERY AFTER DISPLAYING THESE ENQS            
*                                                                               
QUE300   BAS   RE,ANSDISP          REPLACE ADDRESSES WITH DISPLACEMENTS         
*                                  IN ANSAREA                                   
QSHOW    DS    0H                                                               
         LA    R7,SRVACTH                                                       
         SHI   R7,ENQLINEL                                                      
         MVI   COUNT,0                                                          
*                                                                               
         LA    R4,ANSAREA          A(ANSWER AREA FROM ISGQUERY)                 
         USING ISGYQUAAHDR,R4                                                   
         ICM   R2,15,ISGYQUAAHDRNUMRECORDS   NUMBER OF RESOURCES                
         BZ    QUERYX                                                           
         L     R4,ISGYQUAAHDRFIRSTRECORD31 DISPLACEMENT TO 1ST RESOURCE         
         LA    RE,ANSAREA                                                       
         AR    R4,RE                   A(FIRST RESOURCE DATA RECORD)            
         USING ISGYQUAARS,R4                                                    
*                                                                               
QSHOW10  ICM   R1,15,ISGYQUAARSNUMRQ   NUMBER OF REQUESTERS                     
         BZ    QNXTRES                                                          
         L     R5,ISGYQUAARSFIRSTRQ31  DISPLACEMENT TO 1ST REQUESTER            
         LA    RE,ANSAREA                                                       
         AR    R5,RE                   A(FIRST REQUESTER DATA RECORD)           
         USING ISGYQUAARQ,R5                                                    
*                                                                               
QSHOW20  ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         ZIC   RF,PAGE#                GET PAST LINES DISPLAYED IN              
         SHI   RF,1                    PREVIOUS PAGES                           
         MHI   RF,MAXLINES                                                      
         CR    RE,RF                                                            
         BNH   QNXTREQ                                                          
*                                                                               
         LA    R7,ENQLINEL(R7)         POINT TO NEXT LINE ON SCREEN             
         LA    RE,SRVXACTH                                                      
         CR    R7,RE                                                            
         BNH   QSHOW30                                                          
         OI    FLAG,MOREQ              END OF SCREEN. MORE ENTRIES IN           
         B     QUERYX                  ANSAREA                                  
*                                                                               
         USING ENQLINED,R7             DISPLAY INFO FOR THIS ENQUEUE            
QSHOW30  MVC   ENQMINOR,SPACES                                                  
         L     RE,ISGYQUAARSRNAME31    DISPLACEMENT TO MINOR NAME               
         LA    RF,ANSAREA                                                       
         AR    RE,RF                   A(MINOR NAME)                            
         SR    RF,RF                                                            
         ICM   RF,3,ISGYQUAARSRNAMELEN   MINOR NAME LENGTH                      
         CHI   RF,L'ENQMINOR                                                    
         BNH   *+8                                                              
         LHI   RF,L'ENQMINOR           FIT AS MUCH AS I CAN                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ENQMINOR(0),0(RE)                                                
*                                                                               
         L     R6,ISGYQUAARQRQX31   DISPLACEMENT TO REQUESTER EXTENSION         
         LA    RE,ANSAREA                                                       
         AR    R6,RE                A(REQUESTER DATA RECORD EXTENSION)          
         USING ISGYQUAARQX,R6                                                   
         MVC   ENQJOB,ISGYQUAARQXJOBNAME    JOB NAME                            
         TM    ISGYQUAARQFLAGS1,ISGYQUAARQMISC64VALID                           
         BNO   *+8                                                              
         BAS   RE,TIMEOUT              TIME OF ENQUEUE                          
*                                                                               
         TM    ISGYQUAARQFLAGS1,ISGYQUAARQCONTROL                               
         BO    *+8                                                              
         MVI   ENQEXCL,C'E'            EXCLUSIVE CONTROL                        
*                                                                               
         MVI   ENQOWNER,C'W'           WAITER                                   
         TM    ISGYQUAARQFLAGS1,ISGYQUAARQOWNER                                 
         BNO   *+8                                                              
         MVI   ENQOWNER,C'O'           OWNER                                    
*                                                                               
         CLC   =C'EO',ENQEXCL          HIGHLIGHT EXCL OWNERS W/WAITERS          
         BNE   QSHOW50                                                          
         ICM   RE,15,ISGYQUAARSNUMEWAITERS   WAITERS FOR EXCLUSIVE CTRL         
         BNZ   QSHOW40                                                          
         ICM   RF,15,ISGYQUAARSNUMEWAITERS   WAITERS FOR SHARED CONTROL         
         BZ    QSHOW50                                                          
QSHOW40  OI    ENQLINEH+FLDATB-FLDHDRD,FATBHIGH   H+1 HIGH INTENSITY            
*                                                                               
QSHOW50  DS    0X                                                               
         DROP  R7                                                               
         L     R0,ENQECNT              UPDATE ENQUEUE COUNT                     
         AHI   R0,1                                                             
         ST    R0,ENQECNT                                                       
*                                                                               
QNXTREQ  L     R5,ISGYQUAARQNEXT31     DISPLACEMENT TO NEXT REQUESTER           
         LA    RE,ANSAREA                                                       
         AR    R5,RE                   A(NEXT REQUESTER DATA RECORD)            
         BCT   R1,QSHOW20                                                       
         DROP  R5,R6                                                            
*                                                                               
QNXTRES  L     R4,ISGYQUAARSNEXT31     DISPLACEMENT TO NEXT RESOURCE            
         LA    RE,ANSAREA                                                       
         AR    R4,RE                   A(NEXT RESOURCE DATA RECORD)             
         BCT   R2,QSHOW10                                                       
         NI    FLAG,X'FF'-MOREQ        NO MORE ENTRIES IN ANSAREA               
         B     QUERYX                                                           
         DROP  R4                                                               
*                                                                               
QUERYX   TM    FLAG,MOREQ                                                       
         BO    INF1                'PRESS ENTER FOR MORE'                       
         TM    FLAG,RESUMEQ                                                     
         BNO   QDONE                                                            
         LA    R7,ENQLINEL(R7)     WHEN RESUMING A QUERY I MIGHT BE             
         LA    RE,SRVXACTH         IN THE MIDDLE OF THE SCREEN. IN              
         CR    R7,RE               THAT CASE PUT A MESSAGE THAT'S               
         BH    INF1                MORE VISIBLE MID-SCREEN                      
         MVC   20(22,R7),=C'*PRESS ENTER FOR MORE*'                             
         B     INF1                'PRESS ENTER FOR MORE'                       
*                                                                               
QDONE    OC    ENQECNT,ENQECNT                                                  
         BZ    INF2                'NO MATCHING ENTRIES'                        
         CLC   RETCODE,=A(ISGQUERYRC_WARN)                                      
         BNE   INF3                'DONE'                                       
         B     INF4                DONE, WITH WARNING                           
         SPACE 2                                                                
*************************************************************                   
*        REPLACE ADDRESSES WITH DISPLACEMENTS IN ANSAREA.   *                   
*        THE ISGQUERY MACRO RETURNS A LINKED LIST WITH      *                   
*        ABSOLUTE ADDRESSES WHICH CAN'T BE SAVED BETWEEN    *                   
*        TRANSACTIONS.                                      *                   
*************************************************************                   
         SPACE 1                                                                
ANSDISP  NTR1                                                                   
         LA    R7,ANSAREA          A(ANSWER AREA FROM ISGQUERY)                 
         USING ISGYQUAAHDR,R7                                                   
*                                                                               
         ICM   R2,15,ISGYQUAAHDRNUMRECORDS  NUMBER OF RESOURCES                 
         BZ    ANSDISPX                                                         
         L     R4,ISGYQUAAHDRFIRSTRECORD31  A(FIRST RESOURCE)                   
         LR    RE,R4                                                            
         SR    RE,R7                                                            
         ST    RE,ISGYQUAAHDRFIRSTRECORD31  REPLACE WITH DISPLACEMENT           
*                                           FROM BEGINNING OF ANSAREA           
         USING ISGYQUAARS,R4                                                    
*                                                                               
ANSDS10  ICM   R1,15,ISGYQUAARSNUMRQ   NUMBER OF REQUESTERS                     
         BZ    ANSDISPX                                                         
         L     R5,ISGYQUAARSFIRSTRQ31  A(FIRST REQUESTER DATA RECORD)           
         LR    RE,R5                                                            
         SR    RE,R7                                                            
         ST    RE,ISGYQUAARSFIRSTRQ31  REPLACE WITH DISPLACEMENT                
         USING ISGYQUAARQ,R5                                                    
*                                                                               
ANSDS20  L     R6,ISGYQUAARQRQX31      A(REQUESTER EXTENSION)                   
         LR    RE,R6                                                            
         SR    RE,R7                                                            
         ST    RE,ISGYQUAARQRQX31      REPLACE WITH DISPLACEMENT                
         USING ISGYQUAARQX,R6                                                   
*                                                                               
         L     RE,ISGYQUAARSRNAME31    A(MINOR NAME)                            
         CHI   RE,AREALNQ              THIS COULD ALREADY BE A DISPL IF         
         BL    *+6                     MINOR NAME IS SHARED                     
         SR    RE,R7                   REPLACE WITH DISPLACEMENT                
*                                                                               
         ST    RE,ISGYQUAARSRNAME31    REPLACE WITH DISPLACEMENT                
*                                                                               
ANXTREQ  SHI   R1,1                                                             
         LTR   R1,R1                                                            
         BZ    ANXTRES                                                          
         L     RF,ISGYQUAARQNEXT31     A(NEXT REQUESTER DATA RECORD)            
         LR    RE,RF                                                            
         SR    RE,R7                                                            
         ST    RE,ISGYQUAARQNEXT31     REPLACE WITH DISPLACEMENT                
         LR    R5,RF                                                            
         B     ANSDS20                                                          
         DROP  R5,R6                                                            
*                                                                               
ANXTRES  SHI   R2,1                                                             
         LTR   R2,R2                                                            
         BZ    ANSDISPX                                                         
         L     RF,ISGYQUAARSNEXT31     A(NEXT RESOURCE DATA RECORD)             
         LR    RE,RF                                                            
         SR    RE,R7                                                            
         ST    RE,ISGYQUAARSNEXT31     REPLACE WITH DISPLACEMENT                
*                                                                               
         LR    R4,RF                                                            
         B     ANSDS10                                                          
         DROP  R4,R7                                                            
*                                                                               
ANSDISPX B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        DISPLAY TIME OF ENQUEUE                            *                   
*        INPUT  : R6=A(REQUESTER DATA RECORD EXTENSION)     *                   
*                 R7=A(PRINT LINE)                          *                   
*************************************************************                   
         SPACE 1                                                                
TIMEOUT  NTR1                                                                   
         USING ISGYQUAARQX,R6                                                   
         USING ENQLINED,R7                                                      
*                                                                               
         MVC   ENQTIME,SPACES                                                   
*                                                                               
* CURRENT UTC TIME                                                              
         XC    NOWUTCE,NOWUTCE                                                  
         TIME  STCKE,NOWUTCE,LINKAGE=SYSTEM,MF=(E,TLIST)                        
         LTR   RF,RF                                                            
         BNZ   TIMEOUTX                ERROR                                    
         STCKCONV STCKEVAL=NOWUTCE,CONVVAL=NOWUTC_BIN,TIMETYPE=BIN,    +        
               MF=(E,SLIST)                                                     
         LTR   RF,RF                                                            
         BNZ   TIMEOUTX                ERROR                                    
* CURRENT TOD TIME                                                              
         XC    NOWTOD_BIN,NOWTOD_BIN                                            
         TIME  BIN,NOWTOD_BIN,LINKAGE=SYSTEM,MF=(E,TLIST)                       
         LTR   RF,RF                                                            
         BNZ   TIMEOUTX                ERROR                                    
* CALCULATE TIME ADJUSTMENT BETWEEN UTC AND TOD                                 
         L     R1,NOWUTC_BIN                                                    
         L     R0,NOWTOD_BIN                                                    
         A     R0,=A(6*60*60*100)      ADD 6 HOURS TO TOD TIME                  
         C     R0,=A(24*60*60*100)                                              
         BNH   *+8                                                              
         S     R0,=A(24*60*60*100)     MINUS 24 HOURS                           
         SR    R1,R0                                                            
         LTR   R1,R1                                                            
         BM    TIMEOUTX                ERROR                                    
         ST    R1,TIMEADJ                                                       
*                                                                               
* ENQUEUE UTC TIME                                                              
         STCKCONV STCKEVAL=ISGYQUAARQXENQTIME,CONVVAL=ENQUTC_BIN,      +        
               TIMETYPE=BIN,DATETYPE=YYYYMMDD,MF=(E,SLIST)                      
         LTR   RF,RF                                                            
         BNZ   TIMEOUTX                ERROR CONVERTING TIME                    
         MVC   ENQTIME,=C'MMMDD HH:MM:SS'                                       
         GOTO1 VDATCON,DMCB,(1,ENQUTC_BIN+9),(4,ENQTIME)    MMMDD               
*                                                                               
* CALCULATE TOD ENQUEUE TIME USING THE TIME ADJUSTMENT                          
         L     R3,ENQUTC_BIN                                                    
         L     R2,TIMEADJ                                                       
         CR    R2,R3                                                            
         BNH   *+8                                                              
         A     R3,=A(12*60*60*100)     ADD 12 HOURS                             
         SR    R3,R2                                                            
         C     R3,=A(24*60*60*100)                                              
         BNH   *+8                                                              
         S     R3,=A(24*60*60*100)     MINUS 24 HOURS                           
*                                                                               
         SR    R2,R2                                                            
         D     R2,=A(100)                                                       
         SR    R2,R2                                                            
         D     R2,=A(60)                                                        
         EDIT  (R2),(2,ENQTIME+12),FILL=0,ZERO=NOBLANK    SS                    
         SR    R2,R2                                                            
         D     R2,=A(60)                                                        
         EDIT  (R2),(2,ENQTIME+9),FILL=0,ZERO=NOBLANK     MM                    
         EDIT  (R3),(2,ENQTIME+6),FILL=0,ZERO=NOBLANK     HH                    
*                                                                               
TIMEOUTX B     XIT1                                                             
         DROP  R6,R7                                                            
         EJECT                                                                  
*************************************************************                   
*        VALIDATE ACTION FIELDS                             *                   
*************************************************************                   
         SPACE 1                                                                
VALACT   NTR1                                                                   
         XC    ACTCODES(ACTCODL),ACTCODES  CLEAR SAVE AREA FOR ACTIONS          
*                                                                               
         LHI   R4,1                LINE#                                        
         LA    R7,SRVACTH                                                       
VALA010  CLI   5(R7),0                                                          
         BNE   VALA020                                                          
VALA011  LA    R7,ENQLINEL(R7)                                                  
         AHI   R4,1                                                             
         CHI   R4,MAXLINES                                                      
         BH    VALACTX                                                          
         B     VALA010                                                          
*                                                                               
VALA020  MVC   THISACT,8(R7)                                                    
         ZIC   R1,5(R7)                                                         
         CHI   R1,3                REQUIRE 3-CHAR ACTION TO AVOID               
         BNE   VALAERR             CONFUSION                                    
         LA    R6,ACTTAB                                                        
         USING ACTTABD,R6                                                       
VALA023  CLC   ACTNAME,8(R7)                                                    
         BE    VALA030                                                          
VALA024  LA    R6,ACTTABL(R6)                                                   
         CLI   0(R6),0                                                          
         BNE   VALA023                                                          
         B     VALAERR             INVALID ACTION                               
*                                                                               
VALA030  CLC   =C'DEQ',8(R7)       VERIFY IF ACTION 'DEQ' IS ALLOWED            
         BNE   VALA040                                                          
         CLC   =CL8'DDSENQ',MAJNAME                                             
         BE    VALA032                                                          
         CLC   =CL8'ENQDEQ',MAJNAME                                             
         BE    VALA032                                                          
         MVI   ACTFLAG,NODEQMAJ    ACTION DEQ NOT ALLOWED FOR THIS              
         B     VALAERR             MAJOR NAME                                   
VALA032  CLI   SRVPSWH+5,0                                                      
         BNE   VALA034                                                          
         MVI   ACTFLAG,MISSPASW    MISSING PASSWORD FOR DEQ                     
         B     VALAERR                                                          
VALA034  BAS   RE,VALPASW                                                       
         BE    VALA040                                                          
         MVI   ACTFLAG,BADPASW     INCORRECT PASSWORD FOR DEQ                   
         B     VALAERR                                                          
*                                                                               
VALA040  MVI   ACTFLAG,HAVEACT                                                  
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         MHI   RF,L'ACTCODES                                                    
         LA    RF,ACTCODES(RF)                                                  
         MVC   0(1,RF),ACTNUM      SAVE ACTION CODE                             
         B     VALA011                                                          
         DROP  R6                                                               
*                                                                               
VALACTX  B     XIT1                                                             
*                                                                               
VALAERR  BAS   RE,LASTSCRN         REDISPLAY LAST SCREEN                        
*                                                                               
         LA    R1,SRVPSWH          PASSWORD ISSUES:                             
         ST    R1,CURSOR                                                        
         CLI   ACTFLAG,MISSPASW    MISSING PASSWORD                             
         BE    ERR6                                                             
         CLI   ACTFLAG,BADPASW     INCORRECT PASSWORD                           
         BE    ERR7                                                             
*                                  OTHER ERRORS:                                
         MVC   8(3,R7),THISACT                                                  
         ST    R7,CURSOR                                                        
         CLI   ACTFLAG,NODEQMAJ    DEQ NOT ALLOWED FOR THIS MAJOR NAME          
         BE    ERR5                                                             
         B     ERR3                INVALID ACTION                               
         SPACE 2                                                                
**************************************************************                  
*        VALIDATE PASSWORD FOR ACTION DEQ                    *                  
*        FORMAT: 8-CHAR PID, TODAY'S DATE(MMMDD),DAY OF WEEK *                  
*        EXAMPLE: AHYDDDNY JAN01MON                          *                  
**************************************************************                  
         SPACE 1                                                                
VALPASW  NTR1                                                                   
         CLI   SRVPSWH+5,17     LENGTH SHOLD BE EXACTLY 17                      
         BNE   VALPWBAD                                                         
*                                                                               
         CLC   =C'YYUNDDNY',SRVPSW                                              
         BE    VALPW10                                                          
         CLC   =C'AWILDDNY',SRVPSW                                              
         BE    VALPW10                                                          
         CLC   =C'RCRIDDNY',SRVPSW                                              
         BE    VALPW10                                                          
         CLC   =C'AHYDDDNY',SRVPSW                                              
         BE    VALPW10                                                          
         CLC   =C'OPUPDDNY',SRVPSW                                              
         BE    VALPW10                                                          
         B     VALPWBAD                                                         
*                                                                               
VALPW10  GOTO1 VDATCON,DMCB,(5,0),(4,DUB),0    TODAY'S DATE (MMMDD)             
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK),0   TODAY'S DATE (YYMMDD)            
         GOTO1 VGETDAY,DMCB,(0,WORK),DUB+5     DAY OF WEEK                      
*                                                                               
         CLC   DUB,SRVPSW+9                                                     
         BE    VALPWOK                                                          
*                                                                               
VALPWBAD CHI   RB,0                                                             
         B     VALPASWX                                                         
VALPWOK  CR    RB,RB                                                            
VALPASWX B     XIT1                                                             
         EJECT                                                                  
******************************************************                          
*        EXECUTE ACTION(S)                           *                          
******************************************************                          
         SPACE 1                                                                
ACTION   NTR1                                                                   
         LHI   R4,1                LINE#                                        
         LA    R7,SRVACTH                                                       
         USING ENQLINED,R7                                                      
ACT010   LR    RF,R4                                                            
         BCTR  RF,0                                                             
         MHI   RF,L'ACTCODES                                                    
         LA    RF,ACTCODES(RF)                                                  
         CLI   0(RF),0                                                          
         BE    ACT050              NO ACTION                                    
*                                                                               
         LA    R2,ACTTAB           SCAN ACTTAB FOR ACTION                       
         USING ACTTABD,R2                                                       
ACT020   CLC   ACTNUM,0(RF)                                                     
         BE    ACT030                                                           
         LA    R2,ACTTABL(R2)                                                   
         CLI   0(R2),0                                                          
         BNE   ACT020                                                           
         DC    H'0'                ACTION CODE NOT FOUND                        
*                                                                               
ACT030   ZIC   RE,PAGE#                                                         
         SHI   RE,1                                                             
         MHI   RE,MAXLINES                                                      
         AR    RE,R4                                                            
         STC   RE,ENQINDX          INDEX OF ENQ WE'RE INTERESTED IN             
         ICM   RF,15,ACTROUTN                                                   
         A     RF,RELO                                                          
         BASR  RE,RF               EXECUTE ACTION ROUTINE,R1=RESULT CD          
         DROP  R2                                                               
*                                                                               
         LTR   R1,R1               DISPLAY ACTION RESULT                        
         BZ    *+8                                                              
         BAS   RE,ACTRES                                                        
*                                                                               
         ST    R7,CURSOR                                                        
*                                                                               
ACT050   LA    R7,ENQLINEL(R7)     NEXT LINE                                    
         AHI   R4,1                                                             
         CHI   R4,MAXLINES                                                      
         BNH   ACT010                                                           
*                                                                               
         BAS   RE,SAVESCRN         SAVE CURRENT SCREEN                          
*                                                                               
         B     XIT1                                                             
         DROP  R7                                                               
         SPACE 2                                                                
*************************************************************                   
*        DISPLAY ACTION RESULT                              *                   
*        INPUT: R1 HAS THE ACTION RESULT CODE               *                   
*               R7 POINTS TO LINE ON SCREEN                 *                   
*************************************************************                   
ACTRES   NTR1                                                                   
         USING ENQLINED,R7                                                      
*                                                                               
         LA    RF,RESTAB                                                        
ACTR10   CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                RESULT CODE NOT IN TABLE                     
         ZIC   RE,0(RF)                                                         
         CR    R1,RE                                                            
         BNE   ACTR20                                                           
         MVC   ENQJOB(10),1(RF)                                                 
         B     ACTRESX                                                          
ACTR20   LA    RF,L'RESTAB(RF)                                                  
         B     ACTR10                                                           
*                                                                               
ACTRESX  B     XIT1                                                             
         DROP  R7                                                               
         EJECT                                                                  
*************************************************************                   
*        ACTION DEQUEUE                                     *                   
*        ON EXIT, R1 WILL HOLD THE ACTION RESULT CODE       *                   
*************************************************************                   
         SPACE 1                                                                
DEQUEUE  NTR1                                                                   
         MVI   COUNT,0                                                          
*                                                                               
         LA    R4,ANSAREA          A(SAVED ANSWER AREA FROM ISGQUERY)           
         USING ISGYQUAAHDR,R4                                                   
         ICM   R2,15,ISGYQUAAHDRNUMRECORDS   NUMBER OF RESOURCES                
         BZ    DEQUEUX                                                          
*                                                                               
         L     R4,ISGYQUAAHDRFIRSTRECORD31 DISPLACEMENT TO 1ST RESOURCE         
         LA    RE,ANSAREA                                                       
         AR    R4,RE               A(FIRST RESOURCE DATA RECORD)                
         USING ISGYQUAARS,R4                                                    
*                                                                               
DEQ010   ICM   R1,15,ISGYQUAARSNUMRQ        NUMBER OF REQUESTERS                
         BZ    DEQNXRS                                                          
*                                                                               
         L     R5,ISGYQUAARSFIRSTRQ31  DISPLACEMENT TO 1ST REQUESTER            
         LA    RE,ANSAREA                                                       
         AR    R5,RE               A(FIRST REQUESTER DATA RECORD)               
         USING ISGYQUAARQ,R5                                                    
*                                                                               
DEQ020   ZIC   RE,COUNT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,COUNT                                                         
         ZIC   RF,ENQINDX                                                       
         CR    RE,RF                                                            
         BNE   DEQNXRQ                                                          
*                                  FOUND THE ENQ WE'RE LOOKING FOR              
         TM    ISGYQUAARQFLAGS1,ISGYQUAARQMISC64VALID                           
         BNO   DEQERR              THE ENQ TOKEN IS NOT AVAILABLE               
         ISGENQ REQUEST=RELEASE,ENQTOKEN=ISGYQUAARQENQTOKEN,           +        
               COND=YES,RETCODE=15,RSNCODE=0,MF=(E,DLIST)                       
*                                                                               
         CHI   RF,ISGENQRC_OK                                                   
         BE    DEQOK                                                            
         B     DEQERR                                                           
*                                                                               
DEQNXRQ  L     R5,ISGYQUAARQNEXT31     DISPLACEMENT TO NEXT REQUESTER           
         LA    RE,ANSAREA                                                       
         AR    R5,RE                   A(NEXT REQUESTER DATA RECORD)            
         BCT   R1,DEQ020                                                        
         DROP  R5                                                               
*                                                                               
DEQNXRS  L     R4,ISGYQUAARSNEXT31     DISPLACEMENT TO NEXT RESOURCE            
         LA    RE,ANSAREA                                                       
         AR    R4,RE                   A(NEXT RESOURCE DATA RECORD)             
         BCT   R2,DEQ010                                                        
         B     DEQUEUX                                                          
         DROP  R4                                                               
*                                                                               
DEQERR   LHI   R1,ACTERR               RETURN ACTION RESULT: ERROR              
         B     DEQUEUX                                                          
*                                                                               
DEQOK    BAS   RE,WTO                  WRITE TO CONSOLE                         
         LHI   R1,ACTDEQ               RETURN ACTION RESULT: DEQUEUED           
         B     DEQUEUX                                                          
*                                                                               
DEQUEUX  XIT1  REGS=(R1)                                                        
         SPACE 2                                                                
*************************************************************                   
*        WRITE DEQUEUE INFORMATION TO CONSOLE               *                   
*        'PID DEQ - MAJORNAME.MINORNAME'                    *                   
*        INPUT: R4 = A(RESOURCE DATA RECORD)                *                   
*************************************************************                   
WTO      NTR1                                                                   
         SPACE 1                                                                
         USING ISGYQUAARS,R4                                                    
*                                      BUILD CONSOLE MESSAGE                    
         LA    R2,WORK+2                                                        
         MVC   0(8,R2),SRVPSW          PID                                      
         LA    R2,8(R2)                                                         
         MVC   0(7,R2),=C' DEQ - '     DEQ -                                    
         LA    R2,7(R2)                                                         
         MVC   0(L'MAJNAME,R2),MAJNAME                                          
*                                                                               
         LA    R2,L'MAJNAME-1(R2)      MAJOR NAME                               
WTO10    CLI   0(R2),C' '                                                       
         BNE   WTO20                                                            
         SHI   R2,1                                                             
         B     WTO10                                                            
WTO20    LA    R2,1(R2)                                                         
         MVI   0(R2),C'.'              .                                        
         LA    R2,1(R2)                                                         
*                                                                               
         L     RF,ISGYQUAARSRNAME31    DISPLACEMENT TO MINOR NAME               
         LA    RE,ANSAREA                                                       
         AR    RF,RE                   A(MINOR NAME)                            
         SR    R1,R1                                                            
         ICM   R1,3,ISGYQUAARSRNAMELEN                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         ICM   R1,3,ISGYQUAARSRNAMELEN                                          
         AR    R2,R1                                                            
*                                                                               
         LA    RF,WORK+2                                                        
         SR    R2,RF                                                            
         STCM  R2,3,WORK               MESSAGE LENGTH                           
*                                                                               
         WTO   TEXT=WORK                                                        
*                                                                               
         B     XIT1                                                             
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        SAVE CURRENT SCREEN                                *                   
*************************************************************                   
         SPACE 1                                                                
SAVESCRN NTR1                                                                   
         MVC   SVMSG,SRVMSG                                                     
         LA    RE,SRVACTH                                                       
         LHI   RF,SCREENL                                                       
         LA    R4,SVLSTSCR                                                      
         LR    R5,RF                                                            
         MVCL  R4,RE                                                            
         B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        DISPLAY LAST SCREEN                                *                   
*************************************************************                   
         SPACE 1                                                                
LASTSCRN NTR1                                                                   
         MVC   SRVMSG,SVMSG                                                     
         LA    RE,SRVACTH                                                       
         LHI   RF,SCREENL                                                       
         LA    R4,SVLSTSCR                                                      
         LR    R5,RF                                                            
         MVCL  RE,R4                                                            
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLEAR SAVE AREA                                    *                   
*************************************************************                   
         SPACE 1                                                                
CLRSAVE  NTR1                                                                   
         LA    RE,SAVEDSTR                                                      
         LHI   RF,SAVEDL                                                        
         XCEF                                                                   
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        COMPARE REQUEST PARAMETERS TO PREVIOUS REQUEST     *                   
*************************************************************                   
         SPACE 1                                                                
COMPPARM DS    0H                                                               
COMPMAJ  CLC   SVPMAJL,SRVMAJH+5                                                
         BNE   COMPPARX                                                         
         ZIC   R1,SVPMAJL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVPMAJ(0),SRVMAJ                                                 
         BNE   COMPPARX                                                         
COMPMIN  CLC   SVPMINL,SRVMINH+5                                                
         BNE   COMPPARX                                                         
         ZIC   R1,SVPMINL                                                       
         LTR   R1,R1                                                            
         BZ    COMPTHIS                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVPMIN(0),SRVMIN                                                 
         BNE   COMPPARX                                                         
COMPTHIS CLC   SVPTHSL,SRVTHSH+5                                                
         BNE   COMPPARX                                                         
         ZIC   R1,SVPTHSL                                                       
         CHI   R1,0                                                             
         BE    COMPPARX                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVPTHS(0),SRVTHS                                                 
COMPPARX BR    RE                                                               
         SPACE 2                                                                
*************************************************************                   
*        COMPARE PASSWORD TO PREVIOUS REQUEST               *                   
*************************************************************                   
         SPACE 1                                                                
COMPPSW  CLC   SVPPSWL,SRVPSWH+5                                                
         BNE   COMPPSWX                                                         
         ZIC   R1,SVPPSWL                                                       
         CHI   R1,0                                                             
         BE    COMPPSWX                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVPPSW(0),SRVPSW                                                 
COMPPSWX BR    RE                                                               
         SPACE 2                                                                
*************************************************************                   
*        SAVE CURRENT REQUEST PARAMETERS                    *                   
*************************************************************                   
         SPACE 1                                                                
SAVEPARM MVC   SVPMAJL,SRVMAJH+5                                                
         MVC   SVPMAJ,SRVMAJ                                                    
         MVC   SVPMINL,SRVMINH+5                                                
         MVC   SVPMIN,SRVMIN                                                    
         MVC   SVPTHSL,SRVTHSH+5                                                
         MVC   SVPTHS,SRVTHS                                                    
         MVC   SVPPSWL,SRVPSWH+5                                                
         MVC   SVPPSW,SRVPSW                                                    
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*************************************************************                   
*        ERROR AND INFORMATION MESSAGES                     *                   
*************************************************************                   
ERR1     LA    R0,2                INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,1                MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR3     LA    R0,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR4     XC    MSG,MSG             ISGQ ERROR: RC AND REASON CODE               
         MVC   MSG(38),=C'*ISGQUERY ERROR: RC=XX, REASON=XXXX*'                 
         EDIT  RETCODE,(2,MSG+20),FILL=0                                        
         NC    RSNCODE,=X'0000FFFF'     REASON CODE                             
         GOTO1 VHEXOUT,DMCB,RSNCODE+2,WORK,2,0                                  
         MVC   MSG+31(4),WORK                                                   
         OI    DFLAG,DFLGMSG                                                    
         B     ERRX                                                             
ERR5     XC    MSG,MSG                                                          
         MVC   MSG(44),=C'*ACTION DEQ NOT ALLOWED FOR THIS MAJOR NAME*'         
         OI    DFLAG,DFLGMSG                                                    
         B     ERRX                                                             
ERR6     XC    MSG,MSG                                                          
         MVC   MSG(18),=C'*MISSING PASSWORD*'                                   
         OI    DFLAG,DFLGMSG                                                    
         B     ERRX                                                             
ERR7     XC    MSG,MSG                                                          
         MVC   MSG(20),=C'*INCORRECT PASSWORD*'                                 
         OI    DFLAG,DFLGMSG                                                    
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     MVC   SRVMSG,MSG                                                       
         L     RD,BASERD                                                        
         TM    DFLAG,DFLGMSG                                                    
         BO    EXIT                                                             
         XC    SRVMSG,SRVMSG                                                    
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),(4,FACID)                           
         B     EXIT                                                             
         SPACE 2                                                                
INF1     XC    MSG,MSG                                                          
         BAS   RE,INFCNTS          DISPLAY ENQUEUE COUNTS                       
         MVC   MSG+25(20),=C'PRESS ENTER FOR MORE'                              
         GOTO1 ASQUASH,DMCB,MSG,L'MSG                                           
         OI    DFLAG,DFLGMSG                                                    
         B     INFX                                                             
INF2     XC    MSG,MSG                                                          
         MVC   MSG(21),=C'*NO MATCHING ENTRIES*'                                
         OI    DFLAG,DFLGMSG                                                    
         B     INFX                                                             
INF3     XC    MSG,MSG                                                          
         BAS   RE,INFCNTS          DISPLAY ENQUEUE COUNTS                       
         MVC   MSG+25(4),=C'DONE'                                               
         GOTO1 ASQUASH,DMCB,MSG,L'MSG                                           
         OI    DFLAG,DFLGMSG                                                    
         XC    ENQSCNT,ENQSCNT     CLEAR ENQUEUE COUNTS                         
         XC    ENQECNT,ENQECNT                                                  
         B     INFX                                                             
INF4     XC    MSG,MSG            ISGQ WARNING: RC AND REASON CODE              
         MVC   MSG(5),=C'DONE '                                                 
         MVC   MSG+5(38),=C'*ISGQUERY WARNING: RC=XX, REASON=XXXX*'             
         EDIT  RETCODE,(2,MSG+27),FILL=0                                        
         NC    RSNCODE,=X'0000FFFF'   REASON CODE                               
         GOTO1 VHEXOUT,DMCB,RSNCODE+2,WORK,2,0                                  
         MVC   MSG+38(4),WORK                                                   
         OI    DFLAG,DFLGMSG                                                    
         B     INFX                                                             
         SPACE 2                                                                
INFX     MVC   SRVMSG,MSG                                                       
         BAS   RE,SAVESCRN         SAVE CURRENT SCREEN                          
         B     EXIT                                                             
         SPACE 2                                                                
INFCNTS  DS    0X                  ADD START-END COUNT TO MESSG                 
         EDIT  ENQSCNT,(10,MSG+1),ALIGN=RIGHT                                   
         MVI   MSG+11,C'-'                                                      
         EDIT  ENQECNT,(10,MSG+12),ALIGN=LEFT                                   
         BR    RE                                                               
         EJECT                                                                  
******************************************************                          
*        CONSTANTS                                   *                          
******************************************************                          
         SPACE 1                                                                
         CNOP  0,8                                                              
SPACES   DC    80C' '                                                           
ISGQID   DC    C'$ISGQ'                                                         
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
TEMPSTR  DC    CL7'TEMPSTR'                                                     
*             ***....+....1....+....2....+....3....+....4***                    
ENQHEADR DC    C'Act Minor Name                         '                       
         DC    C'         Date  hh:mm:ss Job            '                       
ENQUNDER DC    C'--- -----------------------------------'                       
         DC    C'----- -- -------------- --------       '                       
*                                                                               
AREALEN  DC    A(AREALNQ)       L(ANSWER AREA)                                  
*                                                                               
ACTTAB   DS    0X                                                               
         DC    CL3'DEQ',X'01',AL4(DEQUEUE)                                      
         DC    XL4'00'                                                          
*                                                                               
ACTERR   EQU   1                                                                
ACTDEQ   EQU   2                                                                
*                                                                               
RESTAB   DS    0CL11                                                            
         DC    AL1(ACTERR),CL10'**ERROR'                                        
         DC    AL1(ACTDEQ),CL10'**DEQUEUED'                                     
         DC    X'FF'                                                            
         EJECT                                                                  
******************************************************                          
*        WORK AREA                                   *                          
******************************************************                          
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
DMCB     DS    6F                                                               
WORK     DS    CL80                                                             
MSG      DS    CL60                                                             
*                                                                               
CURSOR   DS    A                                                                
SAVERE   DS    A                                                                
TRM      DS    H                                                                
RECLEN   DS    H                                                                
FACID    DS    CL4                                                              
THISJOB  DS    CL8                                                              
THISACT  DS    CL3                                                              
*                                                                               
         DS    0F                                                               
NOWUTC_BIN DS  CL16                UTC CURRENT TIME IN .01 SECS                 
NOWTOD_BIN DS  CL16                TOD CURRENT TIME IN .01 SECS                 
ENQUTC_BIN DS  CL16                UTC ENQUEUE TIME IN .01 SECS                 
NOWUTCE  DS    CL16                UTC CURRENT TIME IN ETOD FORMAT              
TIMEADJ  DS    F                   ADJUSTMENT BTW UTC AND TOD TIME              
*                                                                               
DFLAG    DS    X                                                                
DFLGMSG  EQU   X'10'               MESSAGE FIELD ALREADY SET                    
*                                                                               
ACTFLAG  DS    X                                                                
HAVEACT  EQU   1                                                                
NODEQMAJ EQU   2                                                                
MISSPASW EQU   3                                                                
BADPASW  EQU   4                                                                
*                                                                               
CHGFLAG  DS    X                                                                
PSWONLY  EQU   1                                                                
*                                                                               
COUNT    DS    X                                                                
ENQINDX  DS    X                                                                
*                                                                               
MAJNAME  DS    CL8                 MAJOR NAME                                   
MINNAME  DS    CL40                MINOR NAME (COULD BE A PATTERN)              
MINNAML  DS    AL1                 TRUE LENGTH OF MINOR NAME                    
QTOKEN   DS    XL32                                                             
*                                                                               
ACTCODES DS    (MAXLINES)XL1       SAVED ACTIONS                                
ACTCODL  EQU   *-ACTCODES                                                       
*                                                                               
QLIST    ISGQUERY MF=(L,QPARMS)                                                 
DLIST    ISGENQ MF=(L,DPARMS)                                                   
SLIST    STCKCONV MF=L                                                          
TLIST    TIME LINKAGE=SYSTEM,MF=L                                               
*                                                                               
VHEXOUT  DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VGETDAY  DS    A                                                                
ASQUASH  DS    A                                                                
ASAVE    DS    A                                                                
BASERD   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
SAVEDSTR EQU   *                   SAVED PART OF W/S                            
*                                                                               
SVPARS   DS    0X                  PARAMETERS                                   
SVPMAJL  DS    X                                                                
SVPMAJ   DS    CL(L'SRVMAJ)                                                     
SVPMINL  DS    X                                                                
SVPMIN   DS    CL(L'SRVMIN)                                                     
SVPTHSL  DS    X                                                                
SVPTHS   DS    CL(L'SRVTHS)                                                     
SVPPSWL  DS    X                                                                
SVPPSW   DS    CL(L'SRVPSW)                                                     
SVPARSL  EQU    *-SVPARS                                                        
*                                                                               
PAGE#    DS    XL1                 PAGE#                                        
FLAG     DS    XL1                                                              
MOREQ    EQU   X'80'               MORE ENTRIES IN ANSAREA                      
RESUMEQ  EQU   X'40'               RESUME QUERY                                 
RETCODE  DS    F                   RETURN CODE                                  
RSNCODE  DS    F                   REASON CODE                                  
RESTOKEN DS    CL16                RESUME TOKEN                                 
ENQSCNT  DS    F                   START COUNT                                  
ENQECNT  DS    F                   END COUNT                                    
*                                                                               
MAXLINES EQU   16                  NUMBER OF LINES OF DATA ON SCREEN            
*                                                                               
SVLSTSCR DS    CL(SCREENL)         SAVED LAST SCREEN                            
SCREENL  EQU   MAXLINES*ENQLINEL                                                
SVMSG    DS    CL(L'SRVMSG)                                                     
*                                                                               
ANSAREA  DS    5XL1024                                                          
AREALNQ  EQU   *-ANSAREA                                                        
*                                                                               
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
WRKX     EQU   *                                                                
*                                                                               
         DS    (SRSMAX-SAVEDL)X    FORCE ASSEMBLY ERROR IF THE SAVE             
*                                  AREA GOES ABOVE THE MAXIMUM 8K               
         EJECT                                                                  
ACTTABD  DSECT                                                                  
ACTNAME  DS    CL3                 ACTION NAME                                  
ACTNUM   DS    XL1                 ACTION NUMBER                                
ACTROUTN DS    AL4                 ACTION ROUTINE TO BE PERFORMED               
ACTTABL  EQU   *-ACTTABD                                                        
*                                                                               
ENQLINED DSECT                                                                  
ENQACTH  DS    CL8                                                              
ENQACT   DS    CL3                                                              
ENQLINEH DS    CL8                                                              
ENQLINE  DS    CL75                                                             
         ORG   ENQLINE                                                          
ENQMINOR DS    CL(L'SRVMIN)                                                     
         DS    CL1                                                              
ENQEXCL  DS    CL1                                                              
ENQOWNER DS    CL1                                                              
         DS    CL1                                                              
ENQTIME  DS    CL14                                                             
         DS    CL1                                                              
ENQJOB   DS    CL8                                                              
         DS    CL1                                                              
         ORG                                                                    
ENQLINEL EQU   *-ENQLINED                                                       
         EJECT                                                                  
*                                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FADSECTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         EJECT                                                                  
SRISGFFD DSECT                                                                  
         DS    CL64                                                             
*SRISGFFD                                                                       
       ++INCLUDE SRISGFFD                                                       
         SPACE 2                                                                
         EJECT                                                                  
*ISG MACROS                                                                     
         PRINT GEN                                                              
         ISGYQUAA                                                               
         ISGYCON                                                                
         PRINT NOGEN                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRISG00   03/02/12'                                      
         END                                                                    
