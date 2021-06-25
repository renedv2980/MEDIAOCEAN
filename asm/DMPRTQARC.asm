*          DATA SET DMPRTQARC  AT LEVEL 008 AS OF 08/05/20                      
*PHASE PRTQARCB                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DDWTO                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PQSCAN                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE XSORT                                                                  
         TITLE 'DMPRTQARC - TASK TO SPOOL PQ TO ARCHIVE'                        
         PRINT NOGEN                                                            
PRTQARC  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,*PRTQARC,=A(WORKAREA),RA,R9,CLEAR=YES                
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         L     R1,=A(ERRBLK-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AERRBLK                                                       
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'*ERRBLK*'                                             
*                                                                               
         L     R1,=A(SCNBLK-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,ASCNBLK                                                       
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'*SCNBLK*'                                             
*                                                                               
         L     R1,=A(PQLNEL-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,APQLNEL                                                       
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'*PQLINE*'                                             
         L     R1,=A(PQLNE-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,APQLNE                                                        
*                                                                               
         L     R1,=A(CIREC-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ACIREC                                                        
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'*PQBUFF*'                                             
*                                                                               
         L     R1,=A(CTREC-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ACTREC                                                        
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'*CTREC**'                                             
*                                                                               
         L     R1,=A(USRBUF-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AUSRBUF                                                       
         AHI   R1,-8                                                            
         MVC   0(8,R1),=C'*USRBUF*'                                             
         L     R1,=A(USRBUFX-WORKD)                                             
         AR    R1,RC                                                            
         MVC   0(2,R1),=X'FFFF'                                                 
*                                                                               
         BAS   RE,INIT             READ CARDS ECT                               
*                                                                               
         BAS   RE,PRINTI           INIT PRINTING                                
*                                                                               
         CLI   EASTEA,C'Y'         TEST IF WE WANT ESTAE                        
         BNE   *+8                                                              
         BAS   RE,SETESTAE         INIT ESTAE                                   
*                                                                               
         BAS   RE,SETEMAIL         SET UP EMAIL ADDRESSEE LIST                  
*                                                                               
         BAS   RE,PRINTX           CLOSE PRINTING IF IT WAS OPEN                
*                                                                               
         BAS   RE,SETOPS           SET UP OPERATOR COMMUNICATIONS               
*                                                                               
LOOP     LA    R1,RESTART          MAIN OUTER LOOP                              
         STM   R0,RF,MYREGS                                                     
         B     LOOP01                                                           
*                                                                               
RESTART  TM    ERREXITF,X'01'      DID ERREXIT BUILD PRINT DATA                 
         BZ    RESTART2                                                         
*NOP*    LA    R0,PLINE            DONT PRINT ERROR LINE IN REPORT              
*NOP*    BAS   RE,PRINT                                                         
*                                                                               
RESTART2 TM    ERREXITF,X'02'      DID ERREXIT BUILD EMAIL DATA                 
         BZ    RESTART4                                                         
         ICM   RF,15,AAUTOMSG                                                   
         BZ    RESTART4                                                         
         LA    R0,ERRMSGLQ(RF)                                                  
         LA    RF,AUTONOTE                                                      
         SR    R0,RF                                                            
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',((R0),(RF))                           
*                                                                               
RESTART4 CLI   ENDTASK,C'Y'        DO WE WANT TO END                            
         BE    ENDOFJOB                                                         
         CLI   PRINTOPN,C'Y'       CLOSE PRINTING IF IT WAS OPEN                
         BNE   LOOP01                                                           
         BAS   RE,PRINTX                                                        
*                                                                               
LOOP01   L     RE,ASCNBLK          CLEAR START OF SCAN BLOCK                    
         XC    0(256,RE),0(RE)                                                  
         BAS   RE,MAIN             MAIN ARCHIVE LOOP                            
         CLI   ONCE,C'Y'                                                        
         BE    ENDOFJOB            EXIT IF ONCE=Y                               
*                                                                               
         GOTO1 =V(DMISGENQ),DMCB,C'TASK'  RELEASE PQ ENQS                       
*                                                                               
         BAS   RE,SETWAIT                                                       
*                                                                               
         CLI   ENDTASK,C'Y'        DO WE WANT TO END                            
         BNE   LOOP                                                             
*                                                                               
ENDOFJOB CLOSE LOGFILE             CLOSE LOGFILE                                
         CLI   PRINTOPN,C'Y'       CLOSE PRINTING IF IT WAS OPEN                
         BNE   XBASE                                                            
         BAS   RE,PRINTX                                                        
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE RC=COND,RL=1                                                     
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        READ REPORTS AND SPOOL TO ARCHIVE                  *                   
*************************************************************                   
MAIN     NTR1                                                                   
         LA    R2,PQBLK            DO PQ SCAN ARCHIVABLE+NOT ARCHIVED           
         USING PQBLOCKD,R2                                                      
         XC    PQBLOCK,PQBLOCK                                                  
*                                                                               
         MVI   PQFILE,X'FF'        ALL PQS                                      
         MVI   PQSORT,2            SORT=CREATE                                  
         OI    PQFLAGS,PQFXUSER                                                 
         OI    PQFLAGS,PQFEXPD                                                  
         OI    PQFLAGS,PQFREFNO    RETURN REPORT NUMBER                         
*                                                                               
         OI    PQSTATN,QLSTTE      IGNORE TEMPORARY STATUS                      
         OI    PQSTATN,QLSTIN      IGNORE INVISIBLE STATUS                      
         OI    PQATTBN,QLATERR     IGNORE REPORTS IN ERROR                      
         OI    PQTYPEN,QLTYDL      IGNORE DOWNLOADABLE                          
*                                                                               
         OI    PQTYP1I,QLTYAR      FIND ARCHIVABLE                              
         OI    PQTYP1N,QLTYAD      IGNORE ARCHIVED                              
*                                                                               
         GOTO1 =V(PQSCAN),DMCB,PQBLK,PQCNT,ASCNBLK                              
*                                                                               
         L     R2,ASCNBLK          POINT R2 TO START OF ENTRIES                 
         USING PQEDATAD,R2                                                      
MAIN010  CLC   0(2,R2),=X'FFFF'    TEST IF LAST ENTRY                           
         BE    MAINX                                                            
         CLC   PQEUSER,NULLS       IGNORE REPORT IF ZERO USERID                 
         BE    MAIN990                                                          
*                                                                               
         L     RF,AERRBLK          IGNORE REPORT IF IN ERROR TABLE              
         USING ERPQRD,RF                                                        
         LA    R0,ERRBLK#                                                       
MAIN020  CLC   ERSRCID,NULLS       TEST END OF TABLE                            
         BE    MAIN030                                                          
         CLC   ERPRTQ,PQEPRTQ      CHECK SAME PRTQ                              
         BNE   *+14                                                             
         CLC   ERREPNO,PQEREPNO    CHECK SAME REPORT NUMBER                     
         BE    MAIN990                                                          
         LA    RF,ERLNQ(RF)                                                     
         BCT   R0,MAIN020                                                       
         DROP  RF                                                               
*                                                                               
MAIN030  MVC   USERN,PQEUSER       GET USER ID ALPHA                            
         BAS   RE,GETUSER                                                       
         LR    R1,R2               POINT TO SCAN BLOCK ENTRY                    
         BAS   RE,PRINTREP         SPOOL REPORT TO SYSPRINT                     
*                                                                               
MAIN032  CLI   PRINTOPN,C'Y'       CLOSE PRINTING IF IT WAS OPEN                
         BNE   MAIN034                                                          
         BAS   RE,PRINTX                                                        
*                                                                               
MAIN034  L     RF,=A(OKMSG)        BUILD OK MESSAGE                             
         MVC   00(8,RF),USERA                                                   
         MVC   09(3,RF),NXSUBID                                                 
         SR    R0,R0                                                            
         ICM   R0,3,NXREPNO                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  13(5,RF),DUB                                                     
*                                                                               
         CLI   RESULT,0            TEST IF ERROR                                
         BE    *+8                                                              
         L     RF,=A(ERRMSG)       ERROR MESSAGE BUILT BY BADBOY                
*                                                                               
         LR    R6,RF               POINT TO L'MESSAGE                           
         AHI   R6,-2                                                            
         WTO   TEXT=(R6),MCSFLAG=HRDCPY                                         
*                                                                               
         CLI   RESULT,0            DONT LOG IF ERROR                            
         BNE   MAIN990                                                          
*                                                                               
MAIN040  GOTO1 =V(DATCON),DMCB,(5,0),(26,WORK)                                  
         MVC   LOGREC,SPACES                                                    
         MVC   LOGDATE,WORK        YYYYMMDD                                     
         MVC   LOGTIME,WORK+8      HHMMSS                                       
         MVC   LOGSRCID,USERA                                                   
         MVC   LOGSRCIX,ARCLINE+9                                               
         MVC   LOGSUBID,NXSUBID                                                 
         EDIT  (B2,NXREPNO),(5,LOGREPNO),ALIGN=LEFT                             
*                                                                               
         MVC   HALF2,NXAGELD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(20,LOGCDATE)                         
         ORG   *-2                                                              
         TM    NXTYP1,QLTYNCD      SET HALF2 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
*                                                                               
         SR    R1,R1               R1=CREATE TIME IN (SECS*3)/4                 
         ICM   R1,3,NXAGELT                                                     
         SLL   R1,2                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'            R1=CREATE TIME IN SECONDS                    
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=CREATE TIME IN MINUTES                    
         STC   R0,FULL+2                                                        
         SR    R0,R0                                                            
         D     R0,=F'60'           R1=CREATE TIME IN HOURS                      
         STC   R0,FULL+1                                                        
         STC   R1,FULL             FULL=X'HHMMSS'                               
*                                                                               
         LA    RE,LOGCTIME         CONVERT TO C'HHMMSS'                         
         LA    RF,FULL                                                          
         LA    R1,3                                                             
         SR    R0,R0                                                            
MAIN042  IC    R0,0(RF)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,RE),DUB+6(2)                                                 
         LA    RE,2(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,MAIN042                                                       
*                                                                               
         MVC   LOGRSLT,RESULT      RESULT IS C'0' IF ALL OK                     
         OI    LOGRSLT,C'0'                                                     
*                                                                               
         GOTO1 =V(SQUASHER),DMCB,LOGREC,(C';',80)                               
*                                                                               
         LA    R1,LOGSRCID         CHANGE USERID,SPP TO USERID-SPP              
         LA    R0,12                                                            
MAIN044  CLI   0(R1),C';'                                                       
         BE    MAIN046                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,MAIN044                                                       
MAIN046  MVI   0(R1),C'-'                                                       
         PUT   LOGFILE,LOGREC                                                   
*                                                                               
MAIN990  LA    R2,PQELNQ(R2)       BUMP TO NEXT SCAN TABLE ENTRY                
         B     MAIN010                                                          
*                                                                               
MAINX    B     EXITEQ                                                           
***********************************************************************         
*        SPOOL REPORT TO SYSPRINT                                     *         
***********************************************************************         
PRINTREP NTR1                                                                   
         LR    R2,R1               R2=A(SCAN TABLE ENTRY)                       
         MVI   RESULT,0                                                         
         MVC   PRTQID,=CL8'PRTQU'                                               
         MVC   PRTQID+4(1),PQEPRTQ SET PRTQUE NAME                              
*                                                                               
         L     R3,APQLNE           R3=A(PQ HEADER LINE)                         
         USING PQPLD,R3                                                         
         XC    0(128,R3),0(R3)                                                  
*                                                                               
PREP001  XC    INDEX,INDEX         LOCATE REPORT AND READ FIRST CI              
         MVC   NXSRCID,PQEUSER                                                  
         MVC   NXSUBID,PQESUBID                                                 
         MVC   NXREPNO,PQEREPNO                                                 
         MVI   NXFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                   
         MVC   QLINDEX,NXNDX       MOVE BASIC DATA TO PQ HEADER LINE            
*                                                                               
         GOTO1 =V(DATAMGR),DMCB,(X'00',NDX),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   PREPERR1                                                         
         CLC   PQEUSER,NXSRCID     CHECK REPORT DATA MATCHES SCANTBL            
         BNE   PREPERR1                                                         
         CLC   PQESUBID,NXSUBID                                                 
         BNE   PREPERR1                                                         
         CLC   PQEREPNO,NXREPNO                                                 
         BNE   PREPERR1                                                         
         MVC   QLINDEX,NXNDX       MOVE INDEX DATA TO PQ HEADER LINE            
*                                                                               
PREP002  CLI   PENDING,C'Y'        TEST TO SET ARCHIVE PENDING                  
         BNE   PREP003                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'00',ADP),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   PREPERR2            ERROR IF WE CANT SET ARCHIVE PENDING         
*                                                                               
PREP003  MVI   NXFLAG,UKFLDAT      SET WANT DATA                                
         MVI   NXFLAG1,UKFLLOCI    SET HAVE LOCATED REPORT                      
         GOTO1 =V(DATAMGR),DMCB,(X'00',SEQ),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   PREPERR3            ERROR IF WE CANT READ FIRST LINE             
*                                                                               
PREP004  L     R3,APQLNE           R3=A(PQ REPORT LINE)                         
         USING PQPLD,R3                                                         
         XC    WIDTH,WIDTH         EXTRACT REPORT LINE WIDTH                    
         MVC   WIDTH+1(1),QLLINEW                                               
         MVC   REPDATE,QLAGELD                                                  
         TM    QLTYP1,QLTYNCD      CONVERT TO NEW CMPRSD DATE                   
         BO    PREP004A                                                         
         GOTO1 =V(DATCON),DMCB,(2,REPDATE),(30,REPDATE)                         
PREP004A MVC   REPDESC,QLDESC                                                   
         MVC   REPMAKER,QLMAKER                                                 
*                                                                               
PREP005  XC    ARCLEN,ARCLEN       SET NO ARC=CARD PRESENT                      
         MVC   ARCLINE,ARCDFLT                                                  
         SR    R1,R1                                                            
         ICM   R1,3,NXUSRINF+6     GET DISPLACEMENT TO ARC=CARD RECORD          
         BZ    PREP005B                                                         
         A     R1,ACIREC           R1 POINTS TO LLC<ARC=XX,......>              
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)          RF=L'RECORD IN PQBUFF                        
         AHI   RF,2                                                             
         STCM  RF,3,ARCLEN         RF=L'DATA+4                                  
         AHI   RF,-5                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ARCLINE(0),2(R1)    #<ARC=AA,SPP,FFFF  ,NNNNN,NNNNN,T,C          
*                                                                               
PREP005A CLC   ARCLINE+9(3),SPACES SET DEFAULTS FOR NON-SET VALUES              
         BH    *+10                                                             
         MVC   ARCLINE+9(3),ARCDFLT+9                                           
         CLC   ARCLINE+13(4),SPACES                                             
         BH    *+10                                                             
         MVC   ARCLINE+13(4),ARCDFLT+13                                         
         CLI   ARCLINE+34,C' '                                                  
         BH    *+10                                                             
         MVC   ARCLINE+34(1),ARCDFLT+34                                         
*                                                                               
PREP005B MVC   ARCCLASS,ARCLINE+34                                              
*                                                                               
PREP006  BAS   RE,PRINTI           ALLOCATE AND OPEN SYSPRINT                   
*                                                                               
PREP007  MVC   PLINE,SPACES        OUTPUT CONTROL CARDS HEADER                  
         MVI   PLINE,X'09'                                                      
         MVC   PLINE+1(13),=C'CONTROL CARDS'                                    
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
         MVC   PLINE,SPACES                                                     
         MVI   PLINE,X'09'                                                      
         MVC   PLINE+1(13),=C'-------------'                                    
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   PLINE,SPACES        OUTPUT DATE=CARD                             
         MVI   PLINE,X'09'                                                      
         MVC   PLINE+1(5),=C'DATE='                                             
         GOTO1 =V(DATCON),DMCB,(14,REPDATE),(10,PLINE+6)                        
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   PLINE,SPACES        OUTPUT LOGO=CARD                             
         MVI   PLINE,X'09'                                                      
         MVC   PLINE+1(5),=C'LOGO='                                             
         MVC   PLINE+6(7),USERA       ALPHA ID                                  
         MVC   PLINE+13(4),ARCLINE+13 FORMS CODE                                
         MVC   PLINE+20(4),USERP      POWER CODE                                
*&&UK*&& MVC   PLINE+24(4),REPDESC    DESCRIPTION                               
*&&US*&& MVC   PLINE+24(3),REPMAKER   MAKER                                     
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   PLINE,SPACES        OUTPUT ARC=CARD............                  
         MVC   PLINE(1),ARCLINE                                                 
         MVC   PLINE+1(L'ARCLINE-3),ARCLINE+2                                   
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   PLINE,SPACES                                                     
         MVI   PLINE,X'09'                                                      
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
*                                                                               
PREP010  MVI   NXFLAG,UKFLDAT      SET WANT DATA                                
         MVI   NXFLAG1,UKFLLOCI    SET HAVE LOCATED REPORT                      
         GOTO1 =V(DATAMGR),DMCB,(X'00',SEQ),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   PREPERR4            ERROR IF WE CANT READ NEXT LINE              
         TM    NXUSRINF+5,X'80'                                                 
         BO    PREP030             END OF REPORT RETURNED IN NXUSRINF           
*                                                                               
PREP012  MVC   PLINE,SPACES        PRINT REPORT LINE                            
         L     R3,APQLNE                                                        
         L     R1,APQLNEL                                                       
         LH    R1,0(R1)            GET L'LINE+4                                 
         AHI   R1,-5                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLINE(0),0(R3)                                                   
         BRAS  RE,FILTER           FILTER OUT UNWANTED LINES                    
         BNE   PREP010                                                          
         LA    R0,PLINE                                                         
         BAS   RE,PRINT                                                         
         B     PREP010                                                          
*                                                                               
PREP030  GOTO1 =V(DATAMGR),DMCB,(X'00',ADS),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   PREPERR5            ERROR ON CHANGING STATUS TO ARCHIVED         
         B     PREPX                                                            
*                                                                               
PREPERR1 MVI   RESULT,1            REPORT NO LONGER EXISTS                      
         B     PREPBAD2                                                         
PREPERR2 MVI   RESULT,2            CANT SET ARCHIVE PENDING IN INDEX            
         B     PREPBAD2                                                         
PREPERR3 MVI   RESULT,3            CANT READ FIRST LINE                         
         B     PREPBAD1                                                         
PREPERR4 MVI   RESULT,4            CANT READ NEXT LINE                          
         B     PREPBAD1                                                         
PREPERR5 MVI   RESULT,5            CANT CHANGE STATUS TO ARCHIVED               
         B     PREPBAD2                                                         
*                                                                               
PREPBAD1 CLI   PENDING,C'Y'        DID WE SET ARCHIVE PENDING                   
         BNE   PREPBAD2                                                         
PREPBAD2 LR    R1,R2               PUT BAD REPORT IN ERROR TABLE                
         BAS   RE,BADBOY                                                        
*                                                                               
PREPX    CLI   RESULT,0            EXIT WITH CC EQL IF ARCHIVED OK              
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        FILTER OUT PRINT QUEUE INDEXING                    *                   
*************************************************************                   
FILTER   ST    RE,SAVERE                                                        
         CLC   =C'<DECL',PLINE+1                                                
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<REQNAME ',PLINE+1                                            
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<IH ',PLINE+1                                                 
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<IM ',PLINE+1                                                 
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<IC ',PLINE+1                                                 
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<< ',PLINE+1                                                  
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<HL ',PLINE+1                                                 
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'</DECL',PLINE+1                                               
         JE    FILTERNO            DON'T PASS DATA                              
         CLC   =C'<DATA ',PLINE+1                                               
         JE    FILTERNO            DON'T PASS DATA                              
         J     FILTEROK                                                         
*                                                                               
FILTERNO MVC   PLINE,SPACES                                                     
         SR    RE,RE                                                            
*                                                                               
FILTEROK C     RE,SAVERE                                                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
*************************************************************                   
*        EDIT TIME FROM FULL TO WORK1                       *                   
*************************************************************                   
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
EXITNEQ  LTR   RB,RB               SET CC NEQ                                   
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GET USERID ALPHA FROM NUMBER                       *                   
*************************************************************                   
GETUSER  NTR1                      GET USER ALPHA FROM USER NUMBER              
         MVI   DMCB1+8,0                                                        
         MVC   USERA,SPACES                                                     
         L     R5,AUSRBUF                                                       
*                                                                               
GUSER1   CLC   0(2,R5),NULLS       SEARCH USER ID TABLE                         
         BE    GUSER2                                                           
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    GUSER2              END OF TABLE                                 
         CLC   0(2,R5),USERN                                                    
         BE    *+12                                                             
         LA    R5,20(R5)                                                        
         B     GUSER1                                                           
         MVC   USERA,2(R5)         MOVE OUT ALPHA FROM TABLE                    
         MVC   USERP,12(R5)        MOVE OUT POWER CODE                          
         MVC   USERG,16(R5)        MOVE OUT AGENCY                              
         MVC   USERL,18(R5)        MOVE OUT AGENCY                              
         B     GUSERX                                                           
*                                                                               
GUSER2   LR    R0,R5               SAVE A(NEXT AVAIL TABLE ENTRY)               
         L     R5,ACTREC                                                        
         XC    0(25,R5),0(R5)      READ CONTROL FILE FOR NUMBER                 
         MVI   0(R5),C'I'                                                       
         MVC   23(2,R5),USERN                                                   
         GOTO1 =V(DATAMGR),DMCB1,DMREAD,=C'CTFILE',(R5),(R5)                    
         CLI   8(R1),0                                                          
         BNE   GUSER4                                                           
         LA    R5,28(R5)           SEARCH CONTROL RECORD FOR ALPHA              
         SR    R6,R6                                                            
GUSER3   CLI   0(R5),0                                                          
         BNE   *+12                                                             
         MVI   DMCB1+8,1                                                        
         B     GUSER5                                                           
         CLI   0(R5),X'02'                                                      
         BE    GUSER3B                                                          
         CLI   0(R5),CTDSTELQ                                                   
         BE    GUSER3C                                                          
         CLI   0(R5),CTAGYELQ                                                   
         BE    GUSER3D                                                          
GUSER3A  SR    R6,R6                                                            
         ICM   R6,1,1(R5)                                                       
         BZ    GUSER5                                                           
         AR    R5,R6                                                            
         B     GUSER3                                                           
*                                                                               
GUSER3B  MVC   USERA,2(R5)         EXTRACT ALPHA                                
         B     GUSER3A                                                          
*                                                                               
GUSER3C  MVC   USERP,CTDSTPOW-CTDSTD(R5) SET POWER CODE                         
         B     GUSER3A                                                          
*                                                                               
GUSER3D  MVC   USERG,CTAGYID-CTAGYD(R5)  SET AGENCY                             
         MVC   USERL,CTAGYLNG-CTAGYD(R5) SET AGENCY LANG                        
         B     GUSER3A                                                          
*                                                                               
GUSER4   MVC   USERA(2),=C'U='     ERROR IN FINDING USER ID NUM                 
         SR    R6,R6                                                            
         ICM   R6,3,USERN                                                       
         CVD   R6,DUB                                                           
         UNPK  USERA+2(6),DUB                                                   
         OI    USERA+7,X'F0'       SET ALPHA TO U=NNNNNN                        
         MVC   USERP(8),DOTS                                                    
*                                                                               
GUSER5   LR    R5,R0               SAVE NUM/ALPHA IN TABLE                      
         CLC   0(2,R5),=X'FFFF'                                                 
         BE    GUSERX              NO MORE ROOM IN TABLE                        
         MVC   0(2,R5),USERN                                                    
         MVC   2(10,R5),USERA                                                   
         MVC   12(4,R5),USERP                                                   
         MVC   16(2,R5),USERG                                                   
         MVC   18(1,R5),USERL                                                   
*                                                                               
GUSERX   CLI   DMCB1+8,0           SET CC=EQL IF USER FOUND OK                  
         XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        SET UP OPERATOR COMMS                              *                   
*************************************************************                   
SETOPS   ST    RE,SAVERE                                                        
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT N SECONDS                              *         
* RETURN WHEN AN INTERUPT IS DETECTED                                 *         
***********************************************************************         
SETWAIT  NTR1                                                                   
         WTO   'WAITING - ',MCSFLAG=HRDCPY                                      
*                                                                               
         SR    R0,R0                                                            
         LH    R1,WAIT                                                          
         MHI   R1,100                                                           
         ST    R1,TIME                                                          
         XC    TIMRECB2,TIMRECB2                                                
         STIMERM SET,ID=STIMERID,BINTVL=TIME,EXIT=TIMRXIT2                      
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  1,ECBLIST=ECBLST2   WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    WB15                NO                                           
*                                                                               
         STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
         WTO   'OPER INTERRUPT',MCSFLAG=HRDCPY                                  
*                                                                               
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
         B     SETWAITX                                                         
*                                                                               
WB15     TM    TIMRECB2,X'40'      DID THE TIMER POP?                           
         BO    *+6                 YES                                          
         DC    H'0'                HOW DID WE GET OUT OF THE WAIT?              
         WTO   'TIMER INTERRUPT',MCSFLAG=HRDCPY                                 
*        B     EXITEQ                                                           
*                                                                               
SETWAITX MVC   TIMEOLD,TIMENOW     SET THE PREVIOUS TIME                        
         MVC   DATEOLD,DATENOW                                                  
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R1,DATENOW                                                       
         ST    R0,TIMENOW                                                       
*                                                                               
         CLC   DATEOLD,DATENOW     HAS DATE CHANGED                             
         BE    EXITEQ                                                           
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         GOTO1 =V(GETDAY),DMCB,(0,DUB),FULL                                     
         MVC   DAYNOW,0(R1)                                                     
         GOTO1 =V(DATCON),DMCB,(5,0),(1,DUB)                                    
         MVC   DATNOW(1),DUB+2                                                  
         MVC   MONNOW(1),DUB+1                                                  
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        CHECK OPER INTERRUPT                               *                   
*************************************************************                   
CHKOPER  NTR1                                                                   
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         USING CIBNEXT,R2                                                       
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CHEK010                                                          
         B     ENDOFJOB                                                         
*                                                                               
CHEK010  CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
         CLC   CARD(3),=C'EOJ'     EOJ SO ACT IMMEDIATELY                       
         BNE   *+12                                                             
         MVI   EOJ,C'Y'                                                         
         B     CHEK020                                                          
*                                                                               
         LA    R1,CARD                                                          
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         BE    CHEK020             NEQ SO TRY GROUP COMMAND                     
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         B     CHEKRSET                                                         
*                                                                               
CHEK020  CLI   EOJ,C'Y'                                                         
         BNE   CHEKRSET                                                         
         MVI   ENDTASK,C'Y'                                                     
*                                                                               
CHEKRSET L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    EXITEQ                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        INITIALISE                                         *                   
*************************************************************                   
INIT     NTR1                                                                   
         LA    R3,CARD                                                          
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
         L     RF,=V(DDSIO)        SET UP DDSIO FIRST                           
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
*NOP*    MVC   PLINE+1(80),0(R3)                                                
*NOP*    BAS   RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT010                                                          
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT020  GOTO1 =V(DATAMGR),DMCB,DMOPEN,CONTROL,=C'NCTFILE X',ACTREC,0           
         OPEN  (LOGFILE,OUTPUT)                                                 
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        R0=A(PRINT LINE) PUT TO SYSPRINT AND SYSPRIN2      *                   
*************************************************************                   
PRINT    ST    RE,SAVPRE                                                        
*                                                                               
PRINT1   L     R1,=A(SYSPRINT)     PRINT LINE TO SYSPRINT                       
         PUT   (1),(0)                                                          
*                                                                               
PRINT2   CLI   SYSOU2,C' '         TEST IF SYSPRIN2 REQUESTED                   
         BNH   PRINT3                                                           
         L     R1,=A(SYSPRIN2)     PRINT LINE TO SYSPRIN2                       
         PUT   (1),(0)                                                          
*                                                                               
PRINT3   L     RE,SAVPRE                                                        
         BR    RE                                                               
*************************************************************                   
*        OPEN AND INITIALIZE SYSPRINT AND SYSPRIN2          *                   
*************************************************************                   
PRINTI   ST    RE,SAVERE                                                        
*                                                                               
PRINTI1  CLI   SYSOUT,C' '         SYSOUT=CARD INPUT                            
         BNH   PRINTI2                                                          
         LA    R1,TXTSYSOU+6                                                    
         MVC   0(1,R1),SYSOUT      SET DEFAULT                                  
         CLI   ARCCLASS,C'A'                                                    
         BNH   PRINTI2             A IS ASSUMED TO BE DEFAULT                   
         MVC   0(1,R1),ARCCLASS    GET IT FROM <ARC=CARD                        
*                                                                               
PRINTI2  CLI   SYSOU2,C' '         SYSOU2=CARD INPUT                            
         BNH   PRINTI3                                                          
         LA    R1,TXTSYSO2+6                                                    
         MVC   0(1,R1),SYSOU2      SET DEFAULT                                  
         CLI   ARCCLASS,C'A'                                                    
         BNH   PRINTI3             A IS ASSUMED TO BE DEFAULT                   
         MVC   0(1,R1),ARCCLASS    GET IT FROM <ARC=CARD                        
*                                                                               
PRINTI3  CLI   TEST,C'Y'           TEST=Y INPUT                                 
         BE    PRINTIT                                                          
         CLI   TEST,C'R'           TEST=R INPUT                                 
         BE    PRINTIR                                                          
         CLI   USERL,X'03'         GERMAN USES CLASS V                          
         BE    PRINTIG                                                          
*                                                                               
PRINTIN  LA    R1,ARBLK            ALLOCATE SYSPRINT                            
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   SYSOU2,C' '                                                      
         BNH   PRINTIX                                                          
         LA    R1,ARBLK2           ALLOCATE SYSPRIN2                            
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     PRINTIX                                                          
*                                                                               
PRINTIG  LA    R1,ARBLKG           ALLOCATE SYSPRINT GERMANY                    
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     PRINTIX                                                          
*                                                                               
PRINTIT  LA    R1,ARBLKT           ALLOCATE SYSPRINT TEST=Y                     
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     PRINTIX                                                          
*                                                                               
PRINTIR  LA    R1,ARBLKR           ALLOCATE SYSPRINT TEST=R                     
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     PRINTIX                                                          
*                                                                               
PRINTIX  OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         MVI   PRINTOPN,C'Y'       INDICATE PRINT IS OPEN                       
         CLI   SYSOU2,C' '                                                      
         BNH   PRINTIXX                                                         
         OPEN  (SYSPRIN2,OUTPUT)   OPEN EXTRA COPY OF PRINTED OUTPUT            
*                                                                               
PRINTIXX ZAP   LINE,=P'0'          SET LINE AND PAGE COUNTERS                   
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         LA    R0,TITLE                                                         
         BAS   RE,PRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTL   ST    RE,SAVERE           PRINT LINE                                   
         AP    LINE,=P'1'          BUMP LINECOUNT                               
         CP    LINE,MAXLINE        TEST FOR MAX LINES                           
         BL    PRINTL2                                                          
*                                                                               
PRINTL1  ZAP   LINE,=P'3'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         LA    R0,T1               PRINT TITLE                                  
         BAS   RE,PRINT                                                         
         LA    R0,T2                                                            
         BAS   RE,PRINT                                                         
         LA    R0,T3                                                            
         BAS   RE,PRINT                                                         
*                                                                               
PRINTL2  LA    R0,PLINE            PRINT LINE                                   
         BAS   RE,PRINT                                                         
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*************************************************************                   
*        CLOSE SYSPRINT AND SYSPRIN2 IF SPECIFIED           *                   
*************************************************************                   
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         MVI   PRINTOPN,C'N'       INDICATE PRINT IS CLOSED                     
         CLI   SYSOU2,C' '                                                      
         BNH   PRINTXX                                                          
         CLOSE SYSPRIN2            CLOSE SECOND COPY OF OUTPUT                  
PRINTXX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        INITIALISE ERROR HANDLING                          *                   
*************************************************************                   
SETESTAE NTR1                                                                   
         ESTAEX ERREXIT,CT,ASYNCH=YES,TERM=NO                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        INITIALISE EMAIL ADDRESSEE LIST                    *                   
*************************************************************                   
SETEMAIL NTR1                                                                   
         MVC   AAUTOMSG,NULLS      CLEAR A(START OF EMAIL TEXT)                 
         LA    RE,EMAIL+L'EMAIL-1                                               
         LA    R0,L'EMAIL                                                       
SETE002  CLI   0(RE),C' '          FIND END OF EMAIL LIST                       
         BH    SETE004                                                          
         BCTR  RE,0                BUMP BACK ONE                                
         BCT   R0,SETE002                                                       
         B     EXITEQ                                                           
SETE004  LA    RE,1(RE)            SET COLON AT END OF LIST                     
         MVI   0(RE),C':'                                                       
         LA    RE,1(RE)                                                         
         ST    RE,AAUTOMSG         RE=A(START OF EMAIL TEXT)                    
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
                                                                                
*CL7'KEYWORD',AL1(L'KEYWORD-1,L'DATA-1),XL2'FLAGS',AL3(OUTPUT)                  
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*        X'0080'                   STRING COMMA DELIMITED LIST                  
                                                                                
         DS    0F                                                               
CARDTAB  DS    0CL14                                                            
         DC    C'DDSIO  ',AL1(4,07),X'0000',AL3(DDSIO)                          
         DC    C'DSPACE ',AL1(5,00),X'0000',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    C'ESTAE  ',AL1(4,00),X'0000',AL3(EASTEA)                         
         DC    C'EMAIL  ',AL1(4,39),X'0080',AL3(EMAIL)                          
         DC    C'ONCE   ',AL1(3,00),X'0000',AL3(ONCE)                           
         DC    C'PENDING',AL1(6,00),X'0000',AL3(PENDING)                        
         DC    C'SYSOUT ',AL1(5,00),X'0000',AL3(SYSOUT)                         
         DC    C'SYSOU2 ',AL1(5,00),X'0000',AL3(SYSOU2)                         
         DC    C'WAIT   ',AL1(3,02),X'0800',AL3(WAIT)                           
         DC    C'TEST   ',AL1(3,00),X'0000',AL3(TEST)                           
         DC    X'0000'                                                          
                                                                                
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
DDSIO    DC    CL8'DDSIO '                                                      
WAIT     DC    AL2(60)                                                          
SYSOUT   DC    C' '                                                             
SYSOU2   DC    C' '                                                             
EOJ      DC    C'N'                                                             
EASTEA   DC    C'Y'                                                             
ONCE     DC    C'N'                                                             
PENDING  DC    C'Y'                                                             
TEST     DC    C' '                                                             
         EJECT                                                                  
***********************************************************************         
* CARD VALIDATION                                                     *         
***********************************************************************         
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITEQU                                                          
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC021                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(,RF)           GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         B     VALC025                                                          
*                                                                               
VALCDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALC025  LR    R1,R2               GET LEN FOR MOVE                             
VALC026  CLI   0(R1),C','                                                       
         BE    VALC030                                                          
         CLI   0(R1),C' '                                                       
         BE    VALC030                                                          
         CLI   0(R1),0                                                          
         BE    VALC030                                                          
         LA    R1,1(R1)                                                         
         B     VALC026                                                          
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    VALC032                                                          
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
VALC032  TM    10(R4),X'80'        COMMA DELIMITED LIST?                        
         BZ    VALC038             NO                                           
         L     R1,CARDEND                                                       
         BCTR  R1,0                                                             
VALC033  CLI   0(R1),C' '                                                       
         BH    *+8                 FIND END OF DATA                             
         BRCT  R1,VALC033                                                       
         SR    R1,R2               R1=L'DATA-1                                  
         BNP   CERRKEY                                                          
         B     VALC400                                                          
                                                                                
VALC038  TM    9(R4),X'04'         IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    9(R4),X'60'         IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    9(R4),X'08'         DEC INPUT                                    
         BZ    VALC070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         BAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    9(R4),X'01'         DATE INPUT                                   
         BZ    VALC400                                                          
         LA    R0,1(R1)            SET R0 INPUT LEN                             
         ST    RF,FULL                                                          
         GOTO1 =V(PERVAL),DMCB,((R0),(R2)),(X'60',WORK)                         
         L     RF,FULL                                                          
         CLI   4(R1),X'04'                                                      
         BNE   CERRDAT                                                          
         MVC   0(2,RF),WORK+PVALNSTA-PERVALD                                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            GET MAXLEN-1 FROM TABLE                      
         EX    RE,*+8                                                           
         B     VALC410                                                          
         MVC   0(0,RF),SPACES      PAD WITH SPACES                              
                                                                                
VALC410  EX    R1,*+8                                                           
         B     VALC420                                                          
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC420  TM    10(R4),X'80'        COMMA DELIMITED LIST?                        
         BZ    VALC500                                                          
         AR    R2,R1               POINT TO NEW LOCATION                        
                                                                                
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
*                                                                               
EXITEQU  CR    RB,RB               SET CC EQU                                   
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         B     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         B     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         B     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         B     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         B     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         B     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         B     CERRX                                                            
CERRDAT  LA    R1,=C'INVALID DATE    '                                          
         B     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SYSTEM  '                                          
         B     CERRX                                                            
*                                                                               
CERRX    MVC   PLINE,SPACES                                                     
         L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
         LA    RF,PLINE+1                                                       
CERRX1   MVC   0(1,RF),0(R2)                                                    
         CLI   0(RF),C' '                                                       
         BE    CERRX2                                                           
         CLI   0(RF),C','                                                       
         BE    CERRX2                                                           
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         B     CERRX1                                                           
*                                                                               
CERRX2   LA    RF,1(RF)                                                         
         MVC   0(13,RF),=C'*** ERROR ***'                                       
         LA    RF,14(RF)                                                        
         MVC   0(16,RF),0(R1)                                                   
         MVC   16(2,RF),=C'//'                                                  
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU  *                   
*************************************************************                   
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         BNE   VALT010                                                          
*                                                                               
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         B     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         BAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BAS   RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BAS   RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BAS   RE,VALTADD                                                       
         B     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
*                                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*        ROUTINE TO PUT BAD REPORT IN TABLE                 *                   
*************************************************************                   
BADBOY   NTR1                                                                   
         LR    R2,R1               R2=A(SCAN TABLE ENTRY)                       
         USING PQEDATAD,R2                                                      
         CLC   PQEUSER,NULLS       IGNORE IF ZERO USERID                        
         BE    BAD006                                                           
         L     RF,AERRBLK          RF=A(NEXT AVAILABLE SLOT IN TABLE)           
         USING ERPQRD,RF                                                        
         LA    R0,ERRBLK#                                                       
BAD002   CLC   ERSRCID,NULLS       TEST EMPTY SLOT                              
         BE    BAD004                                                           
         CLC   ERPRTQ,PQEPRTQ      CHECK SAME PRTQ                              
         BNE   *+14                                                             
         CLC   ERREPNO,PQEREPNO    CHECK SAME REPORT NUMBER                     
         BE    BAD004                                                           
         LA    RF,ERLNQ(RF)                                                     
         BCT   R0,BAD002                                                        
         MVI   RESULT,6            ERROR TABLE IS FULL                          
         B     BAD006                                                           
*                                                                               
BAD004   MVC   ERSRCID,PQEUSER     USERID                                       
         MVC   ERSUBID,PQESUBID    SUBID                                        
         MVC   ERREPNO,PQEREPNO    REPORT NUM                                   
         MVC   ERPRTQ,PQEPRTQ      PRTQ CHR                                     
         MVC   ERNUM,RESULT        ERROR NUM                                    
         MVC   ERDMCB,DMCB+8       DMCB+8                                       
         DROP  RF                                                               
*                                                                               
BAD006   OI    COND,X'04'          SET BAD REPORT ENCOUNTERED                   
         L     R6,=A(ERRMSG)       BUILD ERROR MESSAGE                          
         MVC   00(8,R6),USERA      USERID                                       
         MVC   09(3,R6),PQESUBID   SUBID                                        
         SR    R0,R0                                                            
         ICM   R0,3,PQEREPNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  13(5,R6),DUB        REPORT NUMBER                                
         MVC   23(1,R6),PQEPRTQ    PRTQ ID                                      
         SR    R0,R0                                                            
         IC    R0,RESULT                                                        
         STC   R0,34(R6)           ERROR NUMBER                                 
         OI    34(R6),X'F0'                                                     
         AHI   R0,-1                                                            
         MHI   R0,32                                                            
         L     RE,=A(ERRMSG1)      INDEX INTO ERROR MESSAGE TABLE               
         AR    RE,R0                                                            
         MVC   36(32,R6),0(RE)     ERROR TEXT                                   
*                                                                               
BAD008   EQU   *                                                                
*                                                                               
BAD010   EQU   *                                                                
*                                                                               
BAD012   ICM   RF,15,AAUTOMSG      TEST IF EMAIL IS REQUIRED                    
         BZ    BAD020                                                           
         MVC   0(ERRMSGLQ,RF),ERRMSG                                            
         LA    R0,ERRMSGLQ(RF)                                                  
         LA    RF,AUTONOTE                                                      
         SR    R0,RF                                                            
         GOTO1 =V(DATAMGR),DMCB,=C'OPMSG',((R0),(RF))                           
*                                                                               
BAD020   CLI   RESULT,6            TEST IF ERROR TABLE IS FULL                  
         BNE   BADBOYX                                                          
         DC    H'0'                DIE IF ERROR TABLE IS FULL                   
*                                                                               
BADBOYX  XIT1                                                                   
*                                                                               
AAUTOMSG DC    A(0)                A(FIRST CHR OF EMAIL TEXT)                   
AUTONOTE DC    C'AUTONOTE*'                                                     
EMAIL    DC    CL40' '             SET BY EMAIL=CARD                            
EMAILTXT DC    CL80' '                                                          
                                                                                
*************************************************************                   
*        CONSTANTS & LTORG                                  *                   
*************************************************************                   
         LTORG                                                                  
COND     DC    XL1'00'             RETURNED CONDITION CODE                      
         DS    0D                                                               
ADP      DC    CL8'ADPSET'                                                      
ADS      DC    CL8'ADSET'                                                       
NDX      DC    CL8'INDEX'                                                       
SEQ      DC    CL8'SEQ '                                                        
*                                                                               
DMREAD   DC    CL8'DMREAD '                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
CONTROL  DC    CL8'CONTROL'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
DOTS     DC    16C'.'                                                           
STARS    DC    16C'*'                                                           
FFS      DC    16X'FF'                                                          
NULLS    DS    16X'00'                                                          
*                                                                               
MAXLINE  DC    PL3'60'                                                          
*                                                                               
SPACES   DC    CL198' '                                                         
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
*                                                                               
ARCDFLT  DS    0CL75                                                            
*&&UK*&& DC    X'09',C'<',CL72'ARC=D1,ERR,FORM  ,00038,00038, ,A',C'>'          
*&&US*&& DC    X'09',C'<',CL72'ARC=SJ,ERR,FORM  ,00017,00017, ,A',C'>'          
         DS    0F                                                               
ARBLK    DC    X'80',AL3(RBLK)     R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLK     DC    X'1401000000000000',A(ATXT),X'0000000018000000'                  
*                                                                               
ATXT     DC    X'00',AL3(TXTDD)    DDNAME                                       
         DC    X'00',AL3(TXTSYSOU) SYSOUT                                       
         DC    X'00',AL3(TXTSYSFN) FORMS                                        
*&&US*&& DC    X'00',AL3(TXTSYSDS) DEST                                         
         DC    X'80',AL3(TXTCLOSE) FREE=CLOSE                                   
*                                                                               
TXTDD    DC    AL2(DALDDNAM),X'00010008',C'SYSPRINT'                            
TXTSYSOU DC    AL2(DALSYSOU),X'00010001',C'S'                                   
TXTSYSFN DC    AL2(DALSFMNO),X'00010004',C'BARR'                                
*&&US                                                                           
TXTSYSDS DC    AL2(DALSUSER),X'00010004',C'BARR'                                
*&&                                                                             
TXTCLOSE DC    AL2(DALCLOSE),X'00000000'                                        
*                                                                               
         DS    0F                                                               
ARBLK2   DC    X'80',AL3(RBLK2)    R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLK2    DC    X'1401000000000000',A(ATXT2),X'0000000018000000'                 
*                                                                               
ATXT2    DC    X'00',AL3(TXTDD2)   DDNAME                                       
         DC    X'00',AL3(TXTSYSO2) SYSOUT                                       
         DC    X'00',AL3(TXTSYSF2) FORMS                                        
*&&US*&& DC    X'00',AL3(TXTSYSD2) DEST                                         
         DC    X'80',AL3(TXTCLOS2) FREE=CLOSE                                   
*                                                                               
TXTDD2   DC    AL2(DALDDNAM),X'00010008',C'SYSPRIN2'                            
TXTSYSO2 DC    AL2(DALSYSOU),X'00010001',C'S'                                   
TXTSYSF2 DC    AL2(DALSFMNO),X'00010004',C'ICON'                                
*&&US                                                                           
TXTSYSD2 DC    AL2(DALSUSER),X'00010004',C'ICON'                                
*&&                                                                             
TXTCLOS2 DC    AL2(DALCLOSE),X'00000000'                                        
*                                                                               
         DS    0F                                                               
ARBLKG   DC    X'80',AL3(RBLKG)    R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKG    DC    X'1401000000000000',A(ATXTG),X'0000000018000000'                 
*                                                                               
ATXTG    DC    X'00',AL3(TXTDDG)   DDNAME                                       
         DC    X'00',AL3(TXTSYSOG) SYSOUT                                       
         DC    X'00',AL3(TXTSYSFG) FORMS                                        
         DC    X'80',AL3(TXTCLOSG) FREE=CLOSE                                   
*                                                                               
TXTDDG   DC    AL2(DALDDNAM),X'00010008',C'SYSPRINT'                            
TXTSYSOG DC    AL2(DALSYSOU),X'00010001',C'V'                                   
TXTSYSFG DC    AL2(DALSFMNO),X'00010004',C'BARR'                                
TXTCLOSG DC    AL2(DALCLOSE),X'00000000'                                        
*                                                                               
         DS    0F                                                               
ARBLKT   DC    X'80',AL3(RBLKT)    R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKT    DC    X'1401000000000000',A(ATXTT),X'0000000018000000'                 
*                                                                               
ATXTT    DC    X'00',AL3(TXTDDT)   DDNAME                                       
         DC    X'00',AL3(TXTSYSOT) SYSOUT                                       
         DC    X'80',AL3(TXTCLOST) FREE=CLOSE                                   
*                                                                               
TXTDDT   DC    AL2(DALDDNAM),X'00010008',C'SYSPRINT'                            
TXTSYSOT DC    AL2(DALSYSOU),X'00010001',C'X'                                   
TXTCLOST DC    AL2(DALCLOSE),X'00000000'                                        
*                                                                               
         DS    0F                                                               
ARBLKR   DC    X'80',AL3(RBLKR)    R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLKR    DC    X'1401000000000000',A(ATXTR),X'0000000018000000'                 
*                                                                               
ATXTR    DC    X'00',AL3(TXTDDR)   DDNAME                                       
         DC    X'00',AL3(TXTSYSOR) SYSOUT                                       
         DC    X'80',AL3(TXTCLOSR) FREE=CLOSE                                   
*                                                                               
TXTDDR   DC    AL2(DALDDNAM),X'00010008',C'SYSPRINT'                            
TXTSYSOR DC    AL2(DALSYSOU),X'00010001',C'R'                                   
TXTCLOSR DC    AL2(DALCLOSE),X'00000000'                                        
*                                                                               
         LTORG                                                                  
                                                                                
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBM,LRECL=(198)          
*                                                                               
SYSPRIN2 DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRIN2,RECFM=FBM,LRECL=(198)          
*                                                                               
LOGFILE  DCB   DSORG=PS,MACRF=PM,DDNAME=LOGFILE,RECFM=FBM,LRECL=(80)            
*                                                                               
ADATAMGR DC    V(DATAMGR)                                                       
*                                                                               
ACOMM    DC    A(0)                                                             
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
*                                                                               
SSB      DC    H'0',X'FF',X'14',XL252'00'                                       
         EJECT                                                                  
*************************************************************                   
*        ECBS AND TIMER FIELDS                              *                   
*************************************************************                   
         DS    0F                                                               
ECBLST2  EQU   *                                                                
ADSPECB  DC    A(DSPACECB)                                                      
AOPERECB DC    A(0)                                                             
         DC    X'80',AL3(TIMRECB2) A(TIMER ECB)                                 
*                                                                               
STIMERID DS    F                                                                
TIME     DS    F                                                                
DSPACECB DS    F                                                                
                                                                                
OKMSGL   DC    AL2(OKMSGLQ)                                                     
OKMSG    DC    CL24'UUUUUUUU,XXX,NNNNN Arcd '                                   
OKMSGLQ  EQU   *-OKMSG                                                          
                                                                                
ERRMSGL  DC    AL2(ERRMSGLQ)                                                    
ERRMSG   DC    CL36'UUUUUUUU,XXX,NNNNN PrtqX ARCH Err#N ',CL32' '               
ERRMSGLQ EQU   *-ERRMSG                                                         
                                                                                
ERRMSG1  DC    CL32'Report no longer exists         '                           
ERRMSG2  DC    CL32'Cant set ARCHIV pending in Index'                           
ERRMSG3  DC    CL32'Cant read first line            '                           
ERRMSG4  DC    CL32'Cant read next line             '                           
ERRMSG5  DC    CL32'Cant change status to ARCHIVED  '                           
*                                                                               
ERRMSG6  DC    CL32'Error PRTQARC table is full     '                           
ERRMSG7  DC    CL32'Died in **????** at 00000000    '                           
         EJECT                                                                  
*************************************************************                   
* THE TIMER EXIT ROUTINE. IT POSTS AN ECB.                  *                   
*************************************************************                   
TIMRXIT2 SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMRXIT2,RB                                                      
         POST  TIMRECB2                                                         
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
                                                                                
TIMRECB2 DC    F'0'                ECB OF ATTACHED TASK TIMER                   
         EJECT                                                                  
*************************************************************                   
*        ERROR HANDLER                                      *                   
*************************************************************                   
         DROP  RB,RA,R9                                                         
ERREXIT  DS    0D                                                               
         USING *,RB                                                             
         LR    RB,RF                                                            
         CHI   R0,12               TEST SDWA ACQUIRED                           
         BNE   *+8                 NO,JUST LET MVS PERC                         
         SR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
         STM   RE,RC,12(RD)        SAVE CALLING REGS                            
         ST    RD,ESTAERD                                                       
         ST    R1,ESTAER1          AND POINTER TO SDWA                          
         MVC   MYSDWA,0(R1)                                                     
*                                                                               
         LA    R2,MYSDWA           COPY START OF SDWA                           
         USING SDWA,R2                                                          
         L     RC,MYRC             RELOAD RC FROM SAVED COPY                    
         USING WORKD,RC                                                         
         L     R6,=A(ERRMSG7)      R6=A(ABEND ERROR MESSAGE)                    
         CLI   RESULT,6                                                         
         BNE   *+8                                                              
         MVI   ENDTASK,C'Y'        END JOB IF DIED ON FULL ERROR TABLE          
*                                                                               
ERRX002  ICM   R1,15,SDWAGR13      FIND ABENDING RD                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         C     R1,MYRD             MUST BE BEYOND MAIN                          
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ERRX004  ICM   R1,15,SDWAGR11      FIND ABENDING RB                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   22(8,R1),=C'*PRTQARC'                                            
         BNE   *+8                                                              
         MVI   ENDTASK,C'Y'        END JOB IF WE DIE IN PRTQARC                 
         MVC   8(8,R6),22(R1)      MOVE TO ABEND ERROR MESSAGE                  
*                                                                               
ERRX006  SR    RF,RF               SET RF=DISPLACEMENT                          
         ICM   RF,7,SDWANXTP                                                    
         S     RF,SDWAGR11                                                      
         ST    RF,FULL                                                          
         LA    R4,20(R6)           R4=A(HEX STRING IN ERRMSG7)                  
         LA    R0,8                                                             
ERRX007  SR    RE,RE               CONVERT DISPLACEMENT TO HEX STRING           
         SLDL  RE,4                                                             
         IC    RE,HEXTAB(RE)                                                    
         STC   RE,0(R4)                                                         
         LA    R4,1(R4)                                                         
         BCT   R0,ERRX007                                                       
*                                                                               
         DROP  R2                                                               
*                                                                               
ERRX008  L     R6,=A(ERRMSG)       BUILD ERROR MESSAGE                          
         MVC   00(8,R6),USERA      USERID                                       
         MVC   09(3,R6),NXSUBID    SUBID                                        
         SR    R0,R0                                                            
         ICM   R0,3,NXREPNO                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  13(5,R6),DUB        REPORT NUMBER                                
         MVC   23(1,R6),PRTQID+4   PRTQ ID                                      
         SR    R0,R0                                                            
         IC    R0,RESULT                                                        
         STC   R0,34(R6)           ERROR NUMBER                                 
         OI    34(R6),X'F0'                                                     
         L     RF,=A(ERRMSG7)                                                   
         MVC   36(32,R6),0(RF)     ABEND ERROR TEXT                             
*                                                                               
         AHI   R6,-2               POINT TO L'ERROR MESSAGE                     
         WTO   TEXT=(R6),MCSFLAG=HRDCPY                                         
*                                                                               
         AHI   R6,2                POINT TO ERROR MESSAGE TEXT                  
         L     R5,=A(PLINE-WORKD)  POINT TO PLINE                               
         AR    R5,RC                                                            
         MVI   0(R5),X'09'                                                      
         LA    R5,1(R5)                                                         
         MVI   0(R5),C' '                                                       
         MVC   1(L'PLINE-2,R5),0(R5)                                            
         MVC   00(24,R5),0(R6)     MOVE ERROR TEXT TO PRINT LINE                
         MVC   25(38,R5),30(R6)                                                 
         MVI   08(R5),C'*'         SET EYECATCHER FOR ERROR                     
         OI    ERREXITF,X'01'      SET ERROR PLINE BUILT                        
*                                                                               
ERRX010  L     RF,=A(AAUTOMSG)     TEST IF EMAIL IS REQUIRED                    
         ICM   RF,15,0(RF)                                                      
         BZ    ERRX020                                                          
         L     RE,=A(ERRMSG)       MOVE ERROR MESSAGE TO EMAIL TEXT             
         MVC   0(ERRMSGLQ,RF),0(RE)                                             
         OI    ERREXITF,X'02'      SET EMAIL ERROR TEXT BUILT                   
*                                                                               
ERRX020  L     RF,AERRBLK          LOCATE NEXT SLOT IN ERROR TABLE              
         USING ERPQRD,RF                                                        
         LA    R0,ERRBLK#                                                       
ERRX022  OC    ERSRCID,ERSRCID                                                  
         BZ    ERRX024                                                          
         CLC   ERPRTQ,PRTQID+4     CHECK SAME PRTQ                              
         BNE   *+14                                                             
         CLC   ERREPNO,NXREPNO     CHECK SAME REPORT NUMBER                     
         BE    ERRX024                                                          
         LA    RF,ERLNQ(RF)                                                     
         BCT   R0,ERRX022                                                       
         MVI   ENDTASK,C'Y'        END JOB IF ERROR TABLE IS FULL               
         B     ERRX030                                                          
*                                                                               
ERRX024  MVC   ERKEY,NXKEY         USERID/SUBID/REPNUM                          
         MVC   ERPRTQ,PRTQID+4     PQ CHR                                       
         MVC   ERNUM,RESULT        ERROR NUMBER                                 
         MVI   ERDMCB,X'FF'        SET ABENDED                                  
         DROP  RF                                                               
*                                                                               
ERRX030  CLI   ENDTASK,C'Y'        SET CONDITION CODE FOR ENDTASK=Y             
         BNE   ERRX040                                                          
         L     RF,=A(COND)                                                      
         OI    0(RF),X'08'                                                      
*                                                                               
ERRX040  L     R1,ESTAER1          R1=POINTER TO SDWA                           
         USING SDWA,R1                                                          
         L     RD,ESTAERD                                                       
         LA    R2,ERRRETN          SET 'RETRY' ADDRESS                          
         SETRP DUMP=YES,RC=4,RETADDR=(R2),FRESDWA=YES                           
         LM    RE,RC,12(RD)                                                     
         BR    RE                  CONTROL SHOULD RETURN TO ERRRETN             
*                                                                               
         USING *,RF                                                             
ERRRETN  LM    R0,RF,MYREGS        LOAD REGS SAVED BEFORE RESTART               
         BR    R1                  BRANCH TO RESTART                            
*                                                                               
         DROP  R1,RF                                                            
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF'                                              
*                                                                               
         DS    0D                                                               
         DC    C'**SDWA**'                                                      
MYSDWA   DS    CL256                                                            
*                                                                               
ESTAERD  DS    F                                                                
ESTAER1  DS    F                                                                
*                                                                               
         DS    0D                                                               
         DC    C'**REGS**'                                                      
MYREGS   DS    15F                 SAVED REGS FROM RESTART POINT                
         ORG   MYREGS                                                           
MYR0     DS    F                                                                
MYR1     DS    F                                                                
MYR2     DS    F                                                                
MYR3     DS    F                                                                
MYR4     DS    F                                                                
MYR5     DS    F                                                                
MYR6     DS    F                                                                
MYR7     DS    F                                                                
MYR8     DS    F                                                                
MYR9     DS    F                                                                
MYRA     DS    F                                                                
MYRB     DS    F                                                                
MYRC     DS    F                                                                
MYRD     DS    F                                                                
MYRE     DS    F                                                                
MYRF     DS    F                                                                
*                                                                               
         LTORG                                                                  
                                                                                
WORKAREA DC    70000D'0'                                                        
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
SAVPRE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
HALF     DS    H                                                                
HALF2    DS    H                                                                
BYTE     DS    X                                                                
DAYNOW   DS    XL1                                                              
DATNOW   DS    XL1                                                              
MONNOW   DS    XL1                                                              
*                                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
*                                                                               
TIMENOW  DS    A                   BINARY TIME AT END OF WAIT                   
TIMEOLD  DS    A                   PREVIOUS POP TIME                            
DATENOW  DS    PL4                 BINARY DATE                                  
DATEOLD  DS    PL4                 BINARY DATE LAST TIME                        
*                                                                               
RESULT   DS    X                                                                
ERREXITF DS    X                   01=PLINE BUILT,02=EMAIL BUILT                
PRINTOPN DS    C                   SET TO Y IS SYSPRINT IS OPEN                 
ENDTASK  DS    C                   SET TO Y IF ABEND HAS HAD ENOUGH             
ARCCLASS DS    C                   ARCHIVE CLASS FROM <ARC=                     
*                                                                               
         DS    0D                                                               
INDEX    DS    0XL56                                                            
*                                                                               
NXNDX    DS    0XL24               PRTQ INDEX ENTRY                             
NXKEY    DS    0CL7                REPORT KEY                                   
NXSRCID  DS    XL2                 USER ID NUMBER                               
NXSUBID  DS    CL3                 REPORT ID                                    
NXREPNO  DS    XL2                 FILE REPORT NUMBER WITHIN USERID             
NXCLASS  DS    XL1                 CLASS                                        
NXTYPE   DS    XL1                 TYPE FLAGS                                   
NXATTB   DS    XL1                 REPORT ATTRIBUTE                             
NXSTAT   DS    XL1                 REPORT STATUS                                
NXSEQ    DS    XL1                 CI SEQUENCE NUMBER                           
NXAGES   DS    XL1                 NUM OF CONTROL INTERVALS                     
NXAGELD  DS    XL2                 REPORT LIVE DATE                             
NXAGEDD  DS    XL2                 DEAD DATE                                    
NXAGERD  DS    XL2                 RETN DATE                                    
NXAGERT  DS    XL1                 RETN TIME (BINARY 10MIN)                     
NXAGELT  DS    XL2                 LIVE TIME (SECS*3)/4                         
NXREPTY  DS    CL1                 REPORT TYPE                                  
NXTYP1   DS    XL1                 REPORT FLAGS#1                               
*                                                                               
NXINFO   DS    XL2                 INFO PASSING FIELD                           
         DS    XL1                                                              
NXCIADDL DS    XL1                 0T   FIRST CI FIRST BYTE                     
NXCIADDR DS    XL2                 TTTT FIRST CI LAST TWO BYTES                 
NXFLAG   DS    XL1                 FLAG VALUES                                  
NXFLAG1  DS    XL1                 FLAG VALUES                                  
NXUSRINF DS    XL8                 USER INFO                                    
*                                                                               
NXXTNSN  DS    XL16                EXTENSION AREA FOR 20-BIT FILES              
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
REPDATE  DS    H                   DATCON TYPE 14 CMPRSD                        
REPDESC  DS    CL11                                                             
REPMAKER DS    CL5                                                              
*                                                                               
WIDTH    DS    H                                                                
*                                                                               
USERN    DS    H                                                                
USERA    DS    CL10                                                             
USERP    DS    CL4                                                              
USERG    DS    CL2                                                              
USERL    DS    CL1                                                              
USERSPAR DS    CL1                                                              
*                                                                               
ARCLEN   DS    XL4                                                              
ARCLINE  DS    CL75                #<ARC=AA,SPP,FFFF  ,NNNNN,NNNNN,T,C          
         DS    CL1                                                              
*                                                                               
PLINE    DS    CL198                                                            
TITLE    DS    CL198                                                            
*                                                                               
MSGL     DS    AL2                                                              
MSG      DS    CL80                                                             
*                                                                               
LOGREC   DS    0CL80                                                            
LOGDATE  DS    CL8                                                              
         DS    C                                                                
LOGTIME  DS    CL6                                                              
         DS    C                                                                
LOGSRCID DS    CL8                                                              
         DS    C                                                                
LOGSRCIX DS    CL3                                                              
         DS    C                                                                
LOGSUBID DS    CL3                                                              
         DS    C                                                                
LOGREPNO DS    CL5                                                              
         DS    C                                                                
LOGCDATE DS    CL8                                                              
         DS    C                                                                
LOGCTIME DS    CL6                                                              
         DS    C                                                                
LOGRSLT  DS    CL1                                                              
         DS    CL24                                                             
*                                                                               
CARD     DS    CL80                                                             
*                                                                               
AERRBLK  DS    A                                                                
ASCNBLK  DS    A                                                                
APQLNEL  DS    A                                                                
APQLNE   DS    A                                                                
ACIREC   DS    A                                                                
ACTREC   DS    A                                                                
AUSRBUF  DS    A                                                                
*                                                                               
PLIST    DS    6F                                                               
*                                                                               
PRTQID   DS    CL8                                                              
*                                                                               
PQBLK    DS    CL80                                                             
PQCNT    DS    17CL48              COUNTERS FOR UP TO TO 17 PQS                 
*                                                                               
KEY      DS    CL40                                                             
*                                                                               
         DS    D                                                                
ERRBLK   DS    (ERRBLK#)XL10       ROOM FOR 256 ERROR REPORTS                   
ERRBLK#  EQU   256                                                              
         DS    D                                                                
SCNBLK   DS    14336C              ROOM FOR 800 PRTQUE REPORT ENTRIES           
         DS    D                                                                
PQLNEL   DS    XL4                 PRTQ LINE LENGTH                             
PQLNE    DS    4096C               PRTQ LINE RECORD                             
         DS    D                                                                
CIREC    DS    14336C              PRTQ BUFFER FOR CI REC                       
         DS    D                                                                
CTREC    DS    4096C               I/O AREA FOR CTFILE READ                     
         DS    D                                                                
USRBUF   DS    800CL20             BUFFER FOR USERID NUMBERS AND NAMES          
USRBUFX  DS    CL20                                                             
*                                                                               
WORKX    EQU   *                                                                
                                                                                
         IEFZB4D0                                                               
*                                                                               
         IEFZB4D2                                                               
*                                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
                                                                                
*************************************************************                   
* DSECT TO COVER PQ ERROR TABLE                             *                   
*************************************************************                   
ERPQRD   DSECT                                                                  
ERKEY    DS    0CL7                ERROR REPORT KEY                             
ERSRCID  DS    XL2                 USER ID NUMBER                               
ERSUBID  DS    CL3                 SUB ID                                       
ERREPNO  DS    XL2                 REPORT ID                                    
ERPRTQ   DS    CL1                 PQ FILE ID CHARACTER                         
ERNUM    DS    XL1                 ERROR NUMBER                                 
ERDMCB   DS    XL1                 ERROR VALUE IN DMCB+8                        
ERLNQ    EQU   *-ERPQRD                                                         
                                                                                
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
*IHASDWA MACRO                                                                  
         IHASDWA GR32=YES                                                       
                                                                                
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
* DDPQSCAND                                                                     
       ++INCLUDE DDPQSCAND                                                      
* DMPRTQK                                                                       
       ++INCLUDE DMPRTQK                                                        
* DMPRTQL                                                                       
       ++INCLUDE DMPRTQL                                                        
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008DMPRTQARC 08/05/20'                                      
         END                                                                    
