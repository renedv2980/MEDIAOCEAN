*          DATA SET DMPRTQBAC  AT LEVEL 010 AS OF 07/27/20                      
*PROCESS USING(WARN(15))                                                        
*PHASE PRTQBACA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DDWTO                                                                  
*INCLUDE GETDAY                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE PQSCAN                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE XSORT                                                                  
*&&NOP   SET   N                                                                
         TITLE 'DMPRTQBAC - TASK TO SPOOL PQ TO BACKUP'                         
         PRINT NOGEN                                                            
PRTQBAC  CSECT                                                                  
*                                                                               
         ENTRY UTL                                                              
         ENTRY SSB                                                              
*                                                                               
         NBASE WORKX-WORKD,*PRTQBAC,=A(WORKAREA),RA,R9,CLEAR=YES                
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
         MVC   REPNAME,REPACTV     INIT PRINTING OF ACTIVITY REPORT             
         BAS   RE,PRINTI                                                        
*                                                                               
         CLI   ESTAE,C'Y'          TEST IF WANT ESTAE                           
         BNE   *+8                                                              
         BAS   RE,SETESTAE         INIT ESTAE                                   
*                                                                               
         BAS   RE,SETEMAIL         SET UP EMAIL ADDRESSEE LIST                  
*                                                                               
         BAS   RE,SETOPS           SET UP OPERATOR COMMUNICATIONS               
*                                                                               
LOOP     LA    R1,RESTART          MAIN OUTER LOOP                              
         STM   R0,RF,MYREGS        SAVE THESE REGS FOR ESTAE RESTART            
         B     LOOP01                                                           
*                                                                               
RESTART  SAM24                     Make sure we go back into 24 bit             
         TM    ERREXITF,X'01'      DID ERREXIT BUILD PRINT DATA                 
         BZ    RESTART2                                                         
         PUT   SYSPRINT,PLINEA                                                  
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
*                                                                               
LOOP01   L     RE,ASCNBLK          CLEAR START OF SCAN BLOCK                    
         XC    0(256,RE),0(RE)                                                  
         BAS   RE,MAIN             MAIN BACKUP LOOP                             
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
ENDOFJOB BRAS  RE,SUMMARY          PRINT ACTIVITY SUMMARY REPOTY                
         BRAS  RE,PRINTX           CLOSE SYSPRINT                               
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE RC=COND,RL=1                                                     
*                                                                               
EXITEQ   CR    RB,RB                                                            
         B     EXIT                                                             
EXITNE   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        PRINT SUMMARY FROM SORTER                          *                   
*************************************************************                   
SUMMARY  NTR1                                                                   
         MVC   REPNAME,REPSUMM     INIT PRINTING OF SUMMARY REPORT              
         BRAS  RE,PRINTI                                                        
*                                                                               
         L     R3,APQLNE           R3=A(PQ HEADER RECORD)                       
         USING PQPLD,R3                                                         
         XC    QLSRCID,QLSRCID                                                  
*                                                                               
SUMLOOP  MVC   OLDUSER,QLSRCID     SAVE LAST USER ID                            
         GOTO1 =V(SORTER),DMCB,GET                                              
         ICM   R3,15,4(R1)                                                      
         BZ    SUMM040             ALL DONE                                     
*                                                                               
         CLC   QLSRCID,OLDUSER     SAME USER                                    
         BE    SUMM060                                                          
         OC    OLDUSER,OLDUSER     FIRST TIME CLEAR COUNTS                      
         BZ    SUMM050                                                          
*                                                                               
SUMM040  MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
         MVC   PLINE(38),=C'REPORTS=      PAGES=       CIS=       '             
         EDIT  (2,OLDREPS),(5,PLINE+8),ALIGN=LEFT                               
         EDIT  (4,OLDPAGES),(5,PLINE+20),ALIGN=LEFT                             
         EDIT  (4,OLDCIS),(5,PLINE+31),ALIGN=LEFT                               
         PUT   SYSPRINT,PLINEA                                                  
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
         LTR   R3,R3                                                            
         BZ    SUMMARYX                                                         
*                                                                               
SUMM050  XC    OLDREPS,OLDREPS                                                  
         XC    OLDPAGES,OLDPAGES                                                
         XC    OLDCIS,OLDCIS                                                    
*                                                                               
SUMM060  LHI   R1,1                                                             
         AH    R1,OLDREPS                                                       
         STH   R1,OLDREPS                                                       
         SR    R1,R1                                                            
         ICM   R1,3,QLPAGES                                                     
         A     R1,OLDPAGES                                                      
         ST    R1,OLDPAGES                                                      
         SR    R1,R1                                                            
         IC    R1,QLNCI                                                         
         A     R1,OLDCIS                                                        
         ST    R1,OLDCIS                                                        
         SR    R1,R1                                                            
         IC    R1,QLNCIX                                                        
         A     R1,OLDCIS                                                        
         ST    R1,OLDCIS                                                        
*                                                                               
SUMM090  BRAS  RE,PQREPT                                                        
         B     SUMLOOP                                                          
*                                                                               
SUMMARYX B     EXITEQU                                                          
         EJECT                                                                  
*************************************************************                   
*        READ REPORTS AND SPOOL TO BACKUP                   *                   
*************************************************************                   
MAIN     NTR1                                                                   
*                                                                               
         LA    R2,PQBLK            DO PQ SCAN NOT BACKED UP                     
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
         OI    PQATTBN,QLATJOBI    IGNORE JCL                                   
         OI    PQTYP1N,QLTYBKU     NOT BACKED UP                                
*                                                                               
         GOTO1 =V(PQSCAN),DMCB,PQBLK,PQCNT,ASCNBLK                              
*                                                                               
         L     R2,ASCNBLK          R2=A(SCAN BLOCK)                             
         USING PQEDATAD,R2                                                      
*                                                                               
MAIN010  CLC   PQEUSER,FFS         LAST ENTRY                                   
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
         BAS   RE,TAPEREP          SPOOL REPORT TO BACKUP TAPE                  
*                                                                               
MAIN990  LA    R2,PQELNQ(R2)       BUMP TO NEXT SCAN TABLE ENTRY                
         B     MAIN010                                                          
*                                                                               
MAINX    B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        SPOOL REPORT TO TAPE                               *                   
*************************************************************                   
TAPEREP  NTR1                                                                   
         LR    R2,R1               R2=A(SCAN TABLE ENTRY)                       
         MVI   RESULT,0                                                         
         MVC   PRTQID,=CL8'PRTQU'                                               
         MVC   PRTQID+4(1),PQEPRTQ SET PRTQUE NAME                              
*                                                                               
         L     R3,APQLNE           R3=A(PQ HEADER LINE)                         
         USING PQPLD,R3                                                         
         XC    0(128,R3),0(R3)                                                  
*                                                                               
TREP001  XC    INDEX,INDEX         LOCATE REPORT AND READ FIRST CI              
         MVC   NXSRCID,PQEUSER                                                  
         MVC   NXSUBID,PQESUBID                                                 
         MVC   NXREPNO,PQEREPNO                                                 
         MVC   QLINDEX,NXNDX       MOVE BASIC DATA TO PQ HEADER LINE            
*                                                                               
         MVI   NXFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                   
         GOTO1 =V(DATAMGR),DMCB,(X'00',NDX),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   TREPERR1                                                         
*                                                                               
         CLC   PQEUSER,NXSRCID     CHECK REPORT DATA MATCHES SCANTBL            
         BNE   TREPERR1                                                         
         CLC   PQESUBID,NXSUBID                                                 
         BNE   TREPERR1                                                         
         CLC   PQEREPNO,NXREPNO                                                 
         BNE   TREPERR1                                                         
         MVC   QLINDEX,NXNDX       MOVE INDEX DATA TO PQ HEADER LINE            
*                                                                               
TREP002  CLI   PENDING,C'Y'        TEST TO SET BACKUP PENDING                   
         BNE   TREP003                                                          
         GOTO1 =V(DATAMGR),DMCB,(X'00',BKP),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   TREPERR2            ERROR IF WE CANT SET BACKUP PENDING          
*                                                                               
TREP003  MVI   NXFLAG,UKFLDAT      SET WANT DATA                                
         MVI   NXFLAG1,UKFLLOCI    SET HAVE LOCATED REPORT                      
         GOTO1 =V(DATAMGR),DMCB,(X'00',SEQ),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   TREPERR3            ERROR IF WE CANT READ FIRST LINE             
*                                                                               
         GOTO1 =V(SORTER),DMCB,PUT,(R3)                                         
*                                                                               
         BRAS  RE,PQREPT           REPORT ON PQ TO BE BACKED UP                 
*                                                                               
TREP004  XC    ARCLEN,ARCLEN       SET NO ARC=CARD PRESENT                      
         SR    R1,R1                                                            
         ICM   R1,3,NXUSRINF+6     GET DISPLACEMENT TO ARC=CARD RECORD          
         BZ    TREP011                                                          
         A     R1,ACIREC           R1 POINTS TO LLC<ARC=XX,......>              
         SR    RF,RF                                                            
         ICM   RF,3,0(R1)          RF=L'RECORD IN PQBUFF                        
         AHI   RF,2                                                             
         STCM  RF,3,ARCLEN         RF=L'DATA+4                                  
         AHI   RF,-5                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ARCLINE(0),2(R1)    EXTRACT FOR WRITING LATER                    
         B     TREP011                                                          
*                                                                               
TREP010  MVI   NXFLAG,UKFLDAT      SET WANT DATA                                
         MVI   NXFLAG1,UKFLLOCI    SET HAVE LOCATED REPORT                      
         GOTO1 =V(DATAMGR),DMCB,SEQ,PRTQID,INDEX,APQLNE,ACIREC                  
         CLI   8(R1),0                                                          
         BNE   TREPERR4            ERROR IF WE CANT READ NEXT LINE              
*                                                                               
TREP011  L     R0,APQLNEL          OUTPUT REPORT RECORD LINE                    
         PUT   TAPEOUT,(R0)                                                     
*                                                                               
TREP012  TM    NXUSRINF+5,X'80'    TEST EOF WRITTEN                             
         BO    TREP030                                                          
         TM    NXUSRINF+5,X'01'    TEST SOF WRITTEN                             
         BZ    TREP010                                                          
         OC    ARCLEN,ARCLEN       TEST IF ARC=CARD SAVED                       
         BZ    TREP010                                                          
         PUT   TAPEOUT,ARCLEN      OUTPUT ARC=CARD LINE                         
         XC    ARCLEN,ARCLEN                                                    
         B     TREP010                                                          
*                                                                               
TREP030  GOTO1 =V(DATAMGR),DMCB,(X'00',BKS),PRTQID,INDEX,APQLNE,ACIREC          
         CLI   8(R1),0                                                          
         BNE   TREPERR5            ERROR ON CHANGING STATUS TO ARCHIVED         
         B     TREPX                                                            
*                                                                               
TREPERR1 MVI   RESULT,1            REPORT NO LONGER EXISTS                      
         B     TREPBAD2                                                         
TREPERR2 MVI   RESULT,2            CANT SET BACKUP PENDING IN INDEX             
         B     TREPBAD2                                                         
TREPERR3 MVI   RESULT,3            CANT READ FIRST LINE                         
         B     TREPBAD1                                                         
TREPERR4 MVI   RESULT,4            CANT READ NEXT LINE                          
         B     TREPBAD1                                                         
TREPERR5 MVI   RESULT,5            CANT CHANGE STATUS TO BACKUP                 
         B     TREPBAD2                                                         
*                                                                               
TREPBAD1 CLI   PENDING,C'Y'        DID WE SET BACKUP PENDING                    
         BNE   TREPBAD2                                                         
TREPBAD2 LR    R1,R2               PUT BAD REPORT IN ERROR TABLE                
         BAS   RE,BADBOY                                                        
*                                                                               
TREPX    CLI   RESULT,0            EXIT WITH CC EQL IF BACKED UP OK             
         B     EXIT                                                             
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
BAD008   AHI   R6,-2               POINT TO L'ERROR MESSAGE                     
         WTO   TEXT=(R6),MCSFLAG=HRDCPY                                         
*                                                                               
BAD010   CLI   RESULT,2            TEST TO PRINT ERROR LINE                     
         BL    BAD012                                                           
         CLI   RESULT,6                                                         
         BH    BAD012                                                           
         AHI   R6,2                POINT TO ERROR MESSAGE TEXT                  
         MVC   PLINE,SPACES                                                     
         MVC   PLINE(24),0(R6)                                                  
         MVC   PLINE+25(38),30(R6)                                              
         MVI   PLINE+8,C'*'        SET EYECATCHER FOR ERROR                     
         PUT   SYSPRINT,PLINEA                                                  
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
AUTONOTE DC    CL09'AUTONOTE*'                                                  
EMAIL    DC    CL40' '             SET BY EMAIL=CARD                            
EMAILTXT DC    CL80' '                                                          
         EJECT                                                                  
*************************************************************                   
*        BUILD REPORTING VALUES FOR SYSPRINT                *                   
*************************************************************                   
PQREPT   NTR1                                                                   
         USING PQPLD,R3            R3=A(PQ HEADER RECORD)                       
         LA    R4,PLINE                                                         
         USING RLINED,R4           R4=A(PQ REPORT PRINT LINE)                   
         MVC   PLINE,SPACES                                                     
*                                                                               
         MVC   USERN,QLSRCID       GET USER ID ALPHA                            
         BAS   RE,GETUSER                                                       
         MVC   RLUSER,USERA                                                     
         MVC   RLREPID(3),QLSUBID                                               
         MVI   RLREPID+3,C','                                                   
         EDIT  (B2,QLREPNO),(6,RLREPID+4),ALIGN=LEFT                            
*                                                                               
         MVC   HALF2,QLDATEL                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(16,RLCREAT)                          
         ORG   *-2                                                              
         TM    QLTYP1,QLTYNCD                                                   
         BO    *+8                                                              
         MVI   DMCB,2              USE OLD DATE TYPE                            
         BASR  RE,RF                                                            
*                                                                               
         MVC   DUB(2),QLTIMEL      DISPLAY TIME CREATED                         
         BAS   RE,TIMEOUT                                                       
         MVC   RLCREAT+6(5),DUB+2                                               
*                                                                               
         MVC   RLCLASS,QLCLASS                                                  
         MVC   RLNAME,QLDESC                                                    
*                                                                               
         EDIT  (B2,QLPAGES),(5,RLPAGES)                                         
*                                                                               
         MVC   DUB,STAIND          STATUS                                       
         MVC   BYTE,QLSTAT                                                      
         BRAS  RE,SETBITS                                                       
         MVC   RLSTAT,DUB                                                       
*&&NOP                                                                          
         MVC   HALF2,QLDATED                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(16,RLPRINT)                          
         ORG   *-2                                                              
         TM    QLTYP1,QLTYNCD      SET HALF2 TO NEW CMPRSD DATE                 
         BO    *+8                                                              
         MVI   DMCB,2                                                           
         BASR  RE,RF                                                            
         MVC   DUB(2),QLTIMED      DISPLAY TIME PRINTED                         
         BAS   RE,TIMEOUT                                                       
         MVC   RLPRINT+6(5),DUB+2                                               
*&&                                                                             
         MVC   RLREF,QLMAKER       MAKER                                        
*                                                                               
         MVC   DUB,TYPIND          TYPE                                         
         MVC   BYTE,QLTYPE                                                      
         BRAS  RE,SETBITS                                                       
         MVC   RLTYPE,DUB                                                       
*                                                                               
         MVC   DUB,ATTIND          ATTB                                         
         MVC   BYTE,QLATTB                                                      
         BRAS  RE,SETBITS                                                       
         MVC   RLATTB,DUB                                                       
*                                                                               
         MVC   DUB,TY1IND          TYPE1                                        
         MVC   BYTE,QLTYP1                                                      
         BRAS  RE,SETBITS                                                       
         MVC   RLTYPE1,DUB                                                      
*                                                                               
         MVC   RLARCHV,QLARC       ARCHIVE CLASS                                
*                                                                               
         CLC   QLAGERD,PERMDAT                                                  
         BL    PQR010                                                           
         MVC   RLRETAN,SPACES                                                   
         MVC   RLRETAN(4),=CL4'PERM'                                            
         B     PQR020                                                           
*                                                                               
PQR010   MVC   HALF2,QLAGERD                                                    
         GOTO1 =V(DATCON),DMCB,(14,HALF2),(16,RLRETAN)                          
         ORG   *-2                                                              
         TM    QLTYP1,QLTYNCD                                                   
         BO    *+8                                                              
         MVI   DMCB,2              OLD COMP DATE                                
         BASR  RE,RF                                                            
*                                                                               
PQR020   SR    RF,RF                                                            
         IC    RF,QLAGERT                                                       
         MHI   RF,10                                                            
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         STC   RE,DUB+1                                                         
         STC   RF,DUB                                                           
         BAS   RE,TIMEOUT                                                       
         MVC   RLRETAN+6(5),DUB+2                                               
*                                                                               
         SR    R0,R0               NCI+NCIX                                     
         IC    R0,QLNCI                                                         
         SR    R1,R1                                                            
         IC    R1,QLNCIX                                                        
         AR    R0,R1                                                            
         EDIT  (R0),(3,RLNCIS),ALIGN=LEFT                                       
*                                                                               
         MVC   DUB,SEC1IND         TYPE1                                        
         MVC   BYTE,QLSECF1                                                     
         BRAS  RE,SETBITS                                                       
         MVC   RLSECUR,DUB                                                      
*                                                                               
         PUT   SYSPRINT,PLINEA                                                  
*                                                                               
PQREPTX  B     EXIT                                                             
*                                                                               
STAIND   DC    C'AHPSKDIT'         STATUS INDICATORS                            
ATTIND   DC    C'PNUKWEOI'         ATTRIBUTE INDICATORS                         
TYPIND   DC    C'UNXDQEIN'         TYPE INDICATORS                              
TY1IND   DC    C'EADBR...'         TYPE1 INDICATORS                             
SEC1IND  DC    C'CXDN.PBS'         SECURITY INDICATORS                          
         EJECT                                                                  
*************************************************************                   
*        EDIT TIME AND BIT SETTING ROUTINES                 *                   
*************************************************************                   
TIMEOUT  XC    DUB+2(6),DUB+2      EXPAND BINARY TIME IN DUB(2)                 
         CLI   DUB,23                                                           
         BH    TIMEOUTX                                                         
         CLI   DUB+1,59                                                         
         BH    TIMEOUTX                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+2(2),DUB1+6(2)                                               
*                                                                               
         MVI   DUB+4,C':'                                                       
         SR    R0,R0                                                            
         IC    R0,DUB+1                                                         
         CVD   R0,DUB1                                                          
         OI    DUB1+7,X'0F'                                                     
         UNPK  DUB+5(2),DUB1+6(2)                                               
*                                                                               
TIMEOUTX BR    RE                                                               
*                                                                               
SETBITS  LA    RF,DUB              DUB=ALL BITS ON SETTING (ABCDEFGH)           
         LA    R1,X'80'                                                         
SETBITS1 EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0              EXECUTED TM TO TEST BIT                      
         BO    *+8                                                              
         MVI   0(RF),C'.'          REPLACE WITH . IF ZERO                       
         LA    RF,1(RF)                                                         
         SRA   R1,1                                                             
         BNZ   SETBITS1            SRA UNTIL ZERO                               
         BR    RE                                                               
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
         MVC   USERL,18(R5)        MOVE OUT AGENCY LANG                         
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
         MHI   R1,6000                                                          
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
*                                                                               
SETWAITX MVC   TIMEOLD,TIMENOW     SET THE PREVIOUS TIME                        
         MVC   DATEOLD,DATENOW                                                  
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R1,DATENOW                                                       
         ST    R0,TIMENOW                                                       
*                                                                               
         CLI   NEWDSN,C'Y'         WANT A NEW DATA SET?                         
         BE    SETWX1              YES, DON'T WORRY ABOUT THE DATE              
         CLC   DATEOLD,DATENOW     HAS DATE CHANGED                             
         BE    EXITEQ                                                           
SETWX1   GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         GOTO1 =V(GETDAY),DMCB,(0,DUB),FULL                                     
         MVC   DAYNOW,0(R1)                                                     
         GOTO1 =V(DATCON),DMCB,(5,0),(1,DUB)                                    
         MVC   DATNOW(1),DUB+2                                                  
         MVC   MONNOW(1),DUB+1                                                  
*                                                                               
         CLI   NEWDSN,C'Y'         WANT A NEW DATA SET?                         
         BE    SETWX2              YES, DON'T WORRY ABOUT THE DATE              
         OC    DATEOLD,DATEOLD     NO RESET AFTER FIRST DATE/TIME SET           
         BZ    EXITEQ                                                           
SETWX2   BRAS  RE,DSNRESET         RUN SUMMARY AND RESET DSN                    
         MVI   NEWDSN,C'N'                                                      
*                                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
*************************************************************                   
*        CHECK OPER INTERRUPT                               *                   
*************************************************************                   
CHKOPER  NTR1                                                                   
*                                                                               
         L     RF,ACOMM            SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
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
*                                                                               
INIT010  GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT020                                                          
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO FIRST                           
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BAS   RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT010                                                          
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT020  GOTO1 =V(DATAMGR),DMCB,DMOPEN,CONTROL,=C'NCTFILE X',ACTREC,0           
*                                                                               
         GOTO1 =V(SORTER),DMCB,SRTCARD,RECCARD                                  
*                                                                               
         XC    DATEOLD,DATEOLD                                                  
         XC    TIMEOLD,TIMEOLD                                                  
*                                                                               
         B     EXITEQ                                                           
         EJECT                                                                  
                                                                                
*************************************************************                   
*        SUMMARY REPORT AND NEW TAPEOUT                                         
*************************************************************                   
DSNRESET NTR1  ,                   DO THIS AT CHANGE OF DAY                     
*                                                                               
         BRAS  RE,SUMMARY                                                       
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*                                                                               
         MVI   PLINEA,X'09'                                                     
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
*                                                                               
         MVI   PLINEA,X'09'                                                     
         MVC   PLINE(7),=C'CLOSING'                                             
         MVC   PLINE+9(4),=C'DSN=' PRINT OUTPUT TAPE DSN                        
         MVC   PLINE+13(L'DSNAME),DSNAME                                        
         PUT   SYSPRINT,PLINEA                                                  
*                                                                               
         MVI   PLINEA,X'89'        SET START OF NEW PAGE                        
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
*                                                                               
         BRAS  RE,PRINTX                                                        
*                                                                               
         GOTO1 =V(SORTER),DMCB,SRTCARD,RECCARD                                  
         BRAS  RE,PRINTI                                                        
*                                                                               
         J     EXITEQ                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* Open and close SYSPRINT, OUTPUT TAPE                                          
***********************************************************************         
PRINTI   NTR1  ,                                                                
*                                                                               
         CLI   PRINTOPN,C'Y'                                                    
         BE    PRINTI0                                                          
*                                                                               
         OPEN  (SYSPRINT,OUTPUT)                                                
*                                                                               
         BRAS  RE,DYNIT            DYNAMICALLY ALLOCATE DUMP TAPE               
*                                                                               
         OPEN  (TAPEOUT,OUTPUT),TYPE=J  PQ OUTPUT TAPE                          
*                                                                               
         MVI   PRINTOPN,C'Y'       INDICATE PRINT IS OPEN                       
*                                                                               
PRINTI0  MVI   PLINEA,X'89'        SET START OF NEW PAGE                        
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
         MVI   PLINEA,X'09'                                                     
         MVC   PLINE(4),=C'DSN='   PRINT OUTPUT TAPE DSN                        
         MVC   PLINE+4(L'DSNAME),DSNAME                                         
         PUT   SYSPRINT,PLINEA                                                  
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
         MVC   PLINE(16),REPNAME   SET REPORT NAME/DATE/TIME                    
         MVC   PLINE+22(4),REPDATE                                              
         GOTO1 =V(DATCON),DMCB,(5,0),(20,PLINE+27)                              
         MVC   PLINE+40(4),REPTIME                                              
         THMS                                                                   
         ST    R1,FULL                                                          
         OI    FULL+3,X'0F'                                                     
         UNPK  PLINE+45(6),FULL                                                 
         PUT   SYSPRINT,PLINEA                                                  
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
*                                                                               
         LA    R4,9                9 LINES OF INFO                              
         LA    R5,INFO00           STARTING HERE                                
PRINTI1  MVC   PLINE+00(40),0(R5)                                               
         MVC   PLINE+40(40),360(R5)                                             
         MVC   PLINE+80(40),720(R5)                                             
         PUT   SYSPRINT,PLINEA                                                  
         LA    R5,40(R5)                                                        
         BCT   R4,PRINTI1                                                       
         MVC   PLINE,SPACES                                                     
         PUT   SYSPRINT,PLINEA                                                  
*                                                                               
         MVC   PLINE,HEADER                                                     
         PUT   SYSPRINT,PLINEA                                                  
         MVC   PLINE,SPACES                                                     
*                                                                               
         J     EXIT                                                             
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         CLOSE TAPEOUT                                                          
         MVI   PRINTOPN,C'N'       INDICATE PRINT IS CLOSED                     
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* Dynamically Allocate Report Dump Tape                                         
***********************************************************************         
DYNIT    NTR1  ,                                                                
*                                                                               
         MVC   DSNDSPC,SSB+SSODSPAC-SSOOFF             DATA SPACE               
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(X'20',DSNDATE)   DATE                     
*                                                                               
         EDIT  (TIME,NOW),DUB1                         TIME                     
         MVC   DSNHOUR,DUB1                                                     
         MVC   DSNMINS,DUB1+3                                                   
         MVC   DSNSECS,DUB1+6                                                   
*                                                                               
         LA    R1,ARBLK            DYNAMIC ALLOCATION PARAMETER BLOCK           
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         JNZ   DIT010              ERROR CREATING DATA SET                      
*                                                                               
         RDJFCB TAPEOUT                                                         
         LTR   RF,RF                                                            
         JNZ   DIT010                                                           
*                                                                               
         LA    R3,JFCB                                                          
         USING INFMJFCB,R3                                                      
         MVC   JFCBXPDT,JFCBCRDT   COPY CREATION DATE TO EXPIRATION             
*                                                                               
* DATE is in YYDDDD (CL3). YY is relative to year 1900.  2016 is X'74'          
*                                                                               
         CLI   SSB+SSODSPAC-SSOOFF,C'T'                                         
         JE    DIT005                                                           
         LLC   R1,JFCBXPDT         Get relative year, YY, from 1900             
         AHI   R1,1                                                             
         STC   R1,JFCBXPDT         EXPIRES 1 YEAR AFTER CREATION                
         XR    R1,R1                                                            
         ICM   R1,B'0011',JFCBXPDT+1   GET DDDD part of YYDDDD                  
         CHI   R1,366                  Is this a leap year?                     
         JL    EXIT                    Maybe but not today do we know           
         SHI   R1,1                    Roll it back one day                     
         STCM  R1,B'0011',JFCBXPDT+1   Replace day with 365                     
         J     EXIT                                                             
*                                                                               
DIT005   XR    R1,R1               FOR TEST IT'S 5 DAYS                         
         ICM   R1,B'0011',JFCBXPDT+1                                            
         AHI   R1,5                                                             
         CHI   R1,365                                                           
         JNH   DIT008              Okay as is                                   
         LLC   RF,JFCBXPDT                                                      
         AHI   RF,1                Bump up one year                             
         STC   RF,JFCBXPDT                                                      
         SHI   R1,365              Adjust day (forget about leap year)          
*                                                                               
DIT008   STCM  R1,B'0011',JFCBXPDT+1                                            
         J     EXIT                                                             
         DROP  R3                                                               
*                                                                               
DIT010   MVC   PQBLK,SPACES                                                     
         MVC   PQBLK(2),=AL2(78)                                                
         MVC   PQBLK+2(13),=C'<<< ERROR >>>'                                    
         MVC   PQBLK+20(22),=C'DYNALLOC ERROR CODE = '                          
         GOTO1 =V(HEXOUT),DMCB,RBLK+4,PQBLK+42,2,=C'TOG'                        
         MVC   PQBLK+46(15),=C',  INFO CODE = '                                 
         GOTO1 =V(HEXOUT),DMCB,RBLK+6,PQBLK+61,2,=C'TOG'                        
*                                                                               
         LA    R3,PQBLK                                                         
         WTO   TEXT=(R3)                                                        
         DC    H'0'                COULD NOT ALLOCATE TAPE                      
                                                                                
*----------------------------------------------------------------------         
* DYNALLOC REQUEST BLOCK                                                        
*----------------------------------------------------------------------         
         DS    0F                                                               
ARBLK    DC    X'80',AL3(RBLK)     R1 POINTS TO THIS BEFORE DYNALLOC            
*                                                                               
RBLK     DC    X'1401000000000000',A(ATXT),X'0000000000000000'                  
*                                                                               
ATXT     DC    X'00',AL3(TXTDD)                                                 
         DC    X'00',AL3(TXTDSN)                                                
         DC    X'00',AL3(TXTUNIT)                                               
         DC    X'00',AL3(TXTSDISP)                                              
         DC    X'00',AL3(TXTNDISP)                                              
         DC    X'00',AL3(TXTCDISP)                                              
         DC    X'80',AL3(TXTCLOSE)                                              
*                                                                               
TXTDD    DC    AL2(DALDDNAM),X'00010007',CL7'TAPEOUT'  DDNAME=TAPEOUT           
TXTDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXTUNIT  DC    AL2(DALUNIT),X'00010003',CL3'VTS'       UNIT=VTS                 
TXTSDISP DC    AL2(DALSTATS),X'00010001',X'04'         DISP=(NEW,     )         
TXTNDISP DC    AL2(DALNDISP),X'00010001',X'02'              ( ,CATLG, )         
TXTCDISP DC    AL2(DALCDISP),X'00010001',X'02'              (   ,CATLG)         
TXTCLOSE DC    AL2(DALCLOSE),X'0000'                   FREE=CLOSE               
*                                                                               
         ORG   TXTDSN+6            BACK UP AND DEFINE DATA SET NAME             
DSNAME   DS    0CL44                                                            
         DC    C'FACTAPE.'         FACTAPE.                                     
         DC    C'PQBKUP.'          PQBKUP.                                      
DSNDSPC  DC    C'X'                DATA SPACE                                   
         DC    C'.D'               .D = DATE                                    
DSNDATE  DC    C'YYMMDD'           YEAR/MONTH/DAY                               
         DC    C'.T'               .T = TIME                                    
DSNHOUR  DC    C'HH'               HOURS                                        
DSNMINS  DC    C'MM'               MINUTES                                      
DSNSECS  DC    C'SS'               SECONDS                                      
         ORG                                                                    
         EJECT ,                                                                
                                                                                
*************************************************************                   
*        INITIALISE ERROR HANDLING                          *                   
*************************************************************                   
SETESTAE NTR1                                                                   
*                                                                               
         ESTAEX ERREXIT,CT,ASYNCH=YES,TERM=NO                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
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
                                                                                
         DS    0F                                                               
CARDTAB  DS    0CL14                                                            
         DC    C'DDSIO  ',AL1(4,07),X'0000',AL3(DDSIO)                          
         DC    C'DSPACE ',AL1(5,00),X'0000',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    C'WAIT   ',AL1(3,02),X'0800',AL3(WAIT)                           
         DC    C'ESTAE  ',AL1(4,00),X'0000',AL3(ESTAE)                          
         DC    C'EMAIL  ',AL1(4,39),X'0080',AL3(EMAIL)                          
         DC    C'ONCE   ',AL1(3,00),X'0000',AL3(ONCE)                           
         DC    C'PENDING',AL1(6,00),X'0000',AL3(PENDING)                        
         DC    C'TEST   ',AL1(3,00),X'0000',AL3(TEST)                           
         DC    C'NEWDSN ',AL1(5,00),X'0000',AL3(NEWDSN)                         
         DC    X'0000'                                                          
                                                                                
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*                                                                               
DDSIO    DC    CL8'DDSIO'                                                       
WAIT     DC    AL2(15)                                                          
EOJ      DC    C'N'                                                             
ESTAE    DC    C'Y'                                                             
ONCE     DC    C'N'                                                             
PENDING  DC    C'Y'                                                             
TEST     DC    C'N'                                                             
NEWDSN   DC    C'N'                                                             
         EJECT                                                                  
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
         IC    R1,2(RF)            GET EX LEN                                   
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
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                R1=L'DATA-1                                  
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
         TM    9(R4),X'04'         IF LIST                                      
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
         MVC   0(2,RF),WORK+PVALCSTA-PERVALD                                    
         B     VALC500                                                          
*                                                                               
VALC400  CLI   8(R4),0             DONT CARE                                    
         BE    VALC410                                                          
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         BNL   CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,8(R4)            GET MAXLEN-1 FROM TABLE                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES      PAD WITH SPACES                              
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
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
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
***********************************************************************         
* DCBS & ADCONS                                                                 
***********************************************************************         
JFCB     DS    44F                                                              
JFCBPTR  DC    X'87'                                                            
         DC    AL3(JFCB)                                                        
*                                                                               
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=PM,RECFM=FBA,LRECL=(166)          
*                                                                               
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=PM,RECFM=VB,              +        
               LRECL=4004,BLKSIZE=8200,EXLST=JFCBPTR                            
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
*                                                                               
         DROP  RB,RA,R9                                                         
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
*        LTORG AND CONSTANTS                                *                   
*************************************************************                   
         LTORG                                                                  
COND     DC    XL1'00'             RETURNED CONDITION CODE                      
         DS    0D                                                               
BKP      DC    CL8'BKPSET'                                                      
BKS      DC    CL8'BKSET'                                                       
NDX      DC    CL8'INDEX'                                                       
SEQ      DC    CL8'SEQ '                                                        
*                                                                               
DMREAD   DC    CL8'DMREAD '                                                     
DMOPEN   DC    CL8'DMOPEN'                                                      
CONTROL  DC    CL8'CONTROL'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
GET      DC    CL8'GET'                                                         
PUT      DC    CL8'PUT'                                                         
*                                                                               
DOTS     DC    16C'.'                                                           
STARS    DC    16C'*'                                                           
FFS      DC    16X'FF'                                                          
NULLS    DC    16X'00'                                                          
PERMDAT  DC    XL2'FF9F'                                                        
*                                                                               
SRTCARD  DC    C'SORT FIELDS=(29,7,BI,A),EQUALS '                               
RECCARD  DC    C'RECORD TYPE=F,LENGTH=166 '                                     
*                                                                               
SPACES   DC    CL198' '                                                         
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
*                                                                               
REPDATE  DC    CL04'Date'                                                       
REPTIME  DC    CL04'Time'                                                       
REPACTV  DC    CL16'Backup Activity'                                            
REPSUMM  DC    CL16'Backup Summary'                                             
REPNAME  DC    CL16' '                                                          
*                                                                               
HEADER   DC    CL50'UserId   Report Id Create-Date C Description Pages'         
         DC    CL50' -Status- Maker --Type-- --Attb-- --Typ1-- A Expir'         
         DC    CL50'y Date NCI Security                               '         
         DC    CL50'                                                  '         
*                                                                               
INFO00   DC    C'Stat AHPSKDIT         Attb PNUKWEOI     '                      
         DC    C'A - Active            P - Password      '                      
         DC    C'H - Hold              N - Non prntabl   '                      
         DC    C'P - Printed           U - User data     '                      
         DC    C'S - Sent              K - Wkfile data   '                      
         DC    C'K - Keep              W - Wide > 132    '                      
         DC    C'D - Deleted           E - Error         '                      
         DC    C'I - Invisible         O - Job output    '                      
         DC    C'T - Temp              I - Job input     '                      
*                                                                               
         DC    C'Type UNXDQEIN         Typ1 EADBR...     '                      
         DC    C'U - Updative          E - Eligible      '                      
         DC    C'N - New 20 bit        A - Archivable    '                      
         DC    C'X - Extension CI #2   D - Archived      '                      
         DC    C'D - Download          B - Backed up     '                      
         DC    C'Q - SQL Report        R - REP created   '                      
         DC    C'E - Extension CI                        '                      
         DC    C'I - Has ext CIs                         '                      
         DC    C'N - Created Online                      '                      
*                                                                               
         DC    C'Security CXDN.PBS                       '                      
         DC    C'C - Ctfile security                     '                      
         DC    C'X - Ingore security bits                '                      
         DC    C'D - PID Security                        '                      
         DC    C'N - PIN Security                        '                      
         DC    C'. - n/d                                 '                      
         DC    C'P - Payroll info                        '                      
         DC    C'B - Bank info                           '                      
         DC    C'S - Social security info                '                      
                                                                                
ERRMSGL  DC    AL2(ERRMSGLQ)                                                    
ERRMSG   DC    CL36'UUUUUUUU,XXX,NNNNN PrtqX BKUP Err#n ',CL32' '               
ERRMSGLQ EQU   *-ERRMSG                                                         
                                                                                
ERRMSG1  DC    CL32'Report no longer exists         '                           
ERRMSG2  DC    CL32'Cant set BACKUP pending in Index'                           
ERRMSG3  DC    CL32'Cant read first line            '                           
ERRMSG4  DC    CL32'Cant read next line             '                           
ERRMSG5  DC    CL32'Cant change status to BACKUP    '                           
*                                                                               
ERRMSG6  DC    CL32'Error PRTQBAC table is full     '                           
ERRMSG7  DC    CL32'Died in **????** at 00000000    '                           
                                                                                
*************************************************************                   
*        ERROR HANDLER                                      *                   
*************************************************************                   
ERREXIT  DS    0D                                                               
         USING *,RB                                                             
         LR    RB,RF                                                            
         CHI   R0,12               TEST SDWA ACQUIRED                           
         BNE   ERRX001             NO,JUST LET MVS PERC                         
         SR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
ERRX001  STM   RE,RC,12(RD)        SAVE CALLING REGS                            
         ST    RD,ESTAERD                                                       
         ST    R1,ESTAER1          AND POINTER TO SDWA                          
         MVC   MYSDWA,0(R1)                                                     
*                                                                               
         LA    R2,MYSDWA           COPY START OF SDWA                           
         USING SDWA,R2                                                          
         L     RC,MYRC             RELOAD RC FROM SAVED COPY                    
         USING WORKD,RC                                                         
         MVI   ERREXITF,0                                                       
         L     R6,=A(ERRMSG7)      R6=A(ABEND ERROR MESSAGE)                    
         CLI   RESULT,6                                                         
         BNE   *+8                                                              
         MVI   ENDTASK,C'Y'        END JOB IF DIED ON FULL ERROR TABLE          
*                                                                               
ERRX002  ICM   R1,15,SDWAGR13      FIND ABENDING RD                             
         JZ    *+2                                                              
         C     R1,MYRD             MUST BE BEYOND MAIN                          
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ERRX004  ICM   R1,15,SDWAGR11      FIND ABENDING RB                             
         JZ    *+2                                                              
         CLC   22(8,R1),=C'*PRTQBAC'                                            
         BNE   *+8                                                              
         MVI   ENDTASK,C'Y'        END JOB IF WE DIE IN PRTQBAC                 
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
         MVI   0(R5),C' '                                                       
         MVC   1(L'PLINE-1,R5),0(R5)                                            
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
ERRX022  OC    ERSRCID,ERSRCID     USE EMPTY SLOT                               
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
         DROP  RB                                                               
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
EXPDATE  DS    XL3                                                              
*                                                                               
DMCB     DS    6F                                                               
DMCB1    DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
*                                                                               
COUNTER  DS    F                                                                
*                                                                               
TIMENOW  DS    A                   BINARY TIME AT END OF WAIT                   
TIMEOLD  DS    A                   PREVIOUS POP TIME                            
DATENOW  DS    PL4                 BINARY DATE                                  
DATEOLD  DS    PL4                 BINARY DATE LAST TIME                        
*                                                                               
OLDUSER  DS    H                                                                
OLDREPS  DS    H                                                                
OLDPAGES DS    F                                                                
OLDCIS   DS    F                                                                
*                                                                               
RESULT   DS    X                                                                
ERREXITF DS    X                   01=PLINE BUILT,02=EMAIL BUILT                
PRINTOPN DS    C                                                                
ENDTASK  DS    C                   SET TO Y IF ABEND HAS HAD ENOUGH             
*                                                                               
         DS    0D                                                               
INDEX    DS    0XL56                                                            
*                                                                               
NXNDX    DS    0XL24               PRTQ INDEX ENTRY                             
NXKEY    DS    0CL7                REPORT KEY                                   
NXSRCID  DS    XL2                 USER ID NUMBER                               
NXSUBID  DS    CL3                 REPORT ID                                    
NXREPNO  DS    XL2                 FILE REPORT NUMBER WITHIN USERID             
         DS    XL2                                                              
NXATTB   DS    XL1                 REPORT ATTRIBUTE                             
NXSTAT   DS    XL1                 REPORT STATUS                                
         DS    XL13                                                             
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
USERN    DS    H                                                                
USERA    DS    CL10                                                             
USERP    DS    CL4                                                              
USERG    DS    CL2                                                              
USERL    DS    CL1                                                              
USERSPAR DS    CL1                                                              
*                                                                               
ARCLEN   DS    XL4                                                              
ARCLINE  DS    CL75                                                             
         DS    C                                                                
*                                                                               
PLINEA   DS    CL1                                                              
PLINE    DS    CL198                                                            
*                                                                               
TITLE    DS    CL198                                                            
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
                                                                                
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
RLINED   DSECT                                                                  
RLUSER   DS    CL8    DDS1                                                      
         DS    CL1                                                              
RLREPID  DS    CL9    ABC,12345                                                 
         DS    CL1                                                              
RLCREAT  DS    CL11   23MAR 16:11                                               
         DS    CL1                                                              
RLCLASS  DS    CL1    A                                                         
         DS    CL1                                                              
RLNAME   DS    CL11   REPORT DESC                                               
         DS    CL1                                                              
RLPAGES  DS    CL5    12345                                                     
         DS    CL1                                                              
RLSTAT   DS    CL8    ........                                                  
         DS    CL1                                                              
*&&NOP                                                                          
RLPRINT  DS    CL11   23MAR 16:11                                               
         DS    CL1                                                              
*&&                                                                             
RLREF    DS    CL5    M14                                                       
         DS    CL1                                                              
RLTYPE   DS    CL8    ........                                                  
         DS    CL1                                                              
RLATTB   DS    CL8    ........                                                  
         DS    CL1                                                              
RLTYPE1  DS    CL8    ........                                                  
         DS    CL1                                                              
RLARCHV  DS    CL1    A                                                         
         DS    CL1                                                              
RLRETAN  DS    CL11   23MAR 16:11                                               
         DS    CL1                                                              
RLNCIS   DS    CL3    999                                                       
         DS    CL1                                                              
RLSECUR  DS    CL8    ........                                                  
         DS    CL1                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* IBM MACRO DSECTS                                                              
***********************************************************************         
         IEFZB4D0                                                               
         IEFZB4D2                                                               
         IEFJFCBN                                                               
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
       ++INCLUDE FASSBOFF                                                       
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010DMPRTQBAC 07/27/20'                                      
         END                                                                    
