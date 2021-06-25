*          DATA SET SRSUB00    AT LEVEL 002 AS OF 10/02/08                      
*PHASE T10700A                                                                  
         TITLE '$SUB - SUBMIT JOBS TO JES INTERNAL READER'                      
SUB      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**$SUB**,CLEAR=YES                                   
         USING WORKD,RC            RC=A(W/S)                                    
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(PARMS)                                  
         MVC   ATIA,SRPARM2                                                     
         L     R9,SRPARM1                                                       
         USING SYSFACD,R9          R9=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         L     R4,ATIA                                                          
         USING SRSD,R4             R4=A(SPECIAL S/R SAVE AREA)                  
*                                                                               
         CLI   SSBJESIO,C' '       ARE WE RUNNING WITH MONSOON                  
         BE    EXIT                YES DONT NEED THIS PROG                      
*                                                                               
         GOTO1 VTICTOC,DUB,C'SSET' DISABLE TIMERS                               
*                                                                               
         TM    SSBJFLAG,SSBJFJS1+SSBJFJS2                                       
         BO    JOB2                POST JES JOB NUMBER TO S/R SAVE TWA          
         B     SUB2                SUBMIT A JOB TO JES INTERNAL READER          
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PASS UTL TO FIND A TERMINAL AWAITING SUBMISSION OF A JOB            *         
*                                                                     *         
* THE UTL IS PASSED IN TWO PARTS ON A ROUND-ROBIN BASIS -             *         
* FROM LAST+1 TO END AND FROM START TO LAST                           *         
***********************************************************************         
         SPACE 1                                                                
SUB2     NI    SSBJFLAG,255-SSBJFINQ                                            
         TM    SSBJFLAG,SSBJFJS1   TEST AWAITING JES JOB NUMBER                 
         BNZ   SUBX                YES - EXIT                                   
*                                                                               
         L     R1,SSBAJOB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING JOBTABD,R1          LOCATE A FREE JOB TABLE ENTRY                
         CLI   JOBSTAT,JOBSAVA                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-8                                                        
         B     SUBX                EXIT IF NOT AVAILABLE                        
*                                                                               
         L     R5,VUTL             SET BXLE REGS FOR UTL PASS                   
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING UTLD,R5             R5=A(UTL)                                    
         LA    RE,TLIST            RE=A(LIST OF TERMINALS)                      
*                                                                               
         MVI   UTLPASS,2           START SEARCH FROM TERMINAL THAT HAD          
         OC    SSBJLAST,SSBJLAST   LAST SUBMIT (UNLESS FIRST TIME)              
         BZ    SUB4                                                             
         L     R5,SSBJLAST         POINT TO TERM THAT HAD LAST SUBMIT           
         MVI   UTLPASS,1                                                        
         B     SUB5                EXCLUDE THIS TERM ON FIRST PASS              
*                                                                               
SUB4     OC    TPRNT,TPRNT         TEST PRINTER                                 
         BNZ   SUB5                                                             
         TM    TJOBFLAG,TJOBFINQ   TEST ANY JOBS IN TERMINAL QUEUE              
         BZ    SUB5                                                             
         ST    R5,0(RE)            SET A(UTL ENTRY) IN TLIST                    
         LA    RE,4(RE)                                                         
*                                                                               
SUB5     BXLE  R5,R6,SUB4                                                       
*                                                                               
         CLI   UTLPASS,2           TEST ALL TERMINALS PROCESSED                 
         BE    SUB6                                                             
         L     R5,VUTL             NO - SET BXLE REGS FOR FIRST HALF            
         LA    R5,6(R5)                                                         
         L     R7,SSBJLAST         INCLUDE LAST SUBD TERM ON THIS PASS          
         AR    R7,R6                                                            
         BCTR  R7,0                                                             
         MVI   UTLPASS,2           SET SECOND PASS IN PROGRESS                  
         B     SUB4                                                             
*                                                                               
SUB6     XC    0(4,RE),0(RE)                                                    
         LA    R3,TLIST            R3=A(LIST OF TERMINALS)                      
SUB7     ICM   R5,15,0(R3)         TEST E-O-L                                   
         BZ    SUBX                NO - EXIT                                    
         L     R1,SSBAJOB          COUNT N'JOBS FOR THIS TERMINAL               
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING JOBTABD,R1          R1=A(JOB TABLE)                              
         SR    R0,R0               R0=N'JOBS FOR TERMINAL                       
SUB8     TM    JOBSTAT,JOBSUSE     TEST USED ENTRY                              
         BZ    SUB9                                                             
         CLC   JOBTERM,TNUM        MATCH ON TERMINAL NUMBER                     
         BNE   SUB9                                                             
         AH    R0,=H'1'                                                         
SUB9     BXLE  R1,RE,SUB8                                                       
*                                                                               
         LTR   R0,R0               TEST ANY JOBS FOR TERMINAL                   
         BZ    SUB10                                                            
*&&US*&& CH    R0,=H'5'            TEST GREATER THAN MAX ALLOWED                
*&&UK*&& CH    R0,=H'2'                                                         
         BNH   SUB10                                                            
         LA    R3,4(R3)            YES - BUMP TO NEXT TERMINAL                  
         B     SUB7                                                             
*                                                                               
SUB10    LA    RF,SRPAGENO         SET S/R TWA SAVE PAGE & READ/LOCK IT         
         SLL   RF,32-8                                                          
         ICM   RF,3,TNUM                                                        
         ST    RF,TWAPAGE                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),SSBTWAL                                               
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,TWAPAGE,SRSD                
*                                                                               
SUB11    LA    R3,SRJOBQ           SEARCH TERMINAL QUEUE TO FIND A JOB          
         USING SRJOBQ,R3           R3=A(JOB QUEUE)                              
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=NUMBER OF JOBS IN QUEUE                   
         BZ    SUB14                                                            
         SR    RE,RE               RE=NUMBER OF JOBS TO SUBMIT                  
         SR    RF,RF               RF=A(FIRST JOB TO SUBMIT)                    
SUB12    CLI   SRJOBSTA,SRJOBAVA   TEST JOB SCHEDULED (NOT SUBMITTED)           
         BNE   SUB13                                                            
         LTR   RF,RF               TEST FIRST JOB FOUND                         
         BNZ   *+6                                                              
         LR    RF,R3               YES - SAVE A(FIRST SCHEDULED JOB)            
         LA    RE,1(RE)            BUMP N'JOBS TO BE SUBMITTED                  
SUB13    LA    R3,SRJOBQLN(R3)     BUMP TO NEXT JOB QUEUE ENTRY                 
         BCT   R0,SUB12                                                         
*                                                                               
         LTR   RE,RE               TEST ANY JOBS TO SUBMIT                      
         BZ    SUB14               NO                                           
         STC   RE,TJOBSINQ         SET N'JOBS NOT SUBMITTED                     
         LR    R3,RF                                                            
         BAS   RE,SUBJOB           SUBMIT FIRST JOB FOUND                       
         BL    SUBX                EXIT IF SUBMITTER IS NOT AVAILABLE           
         BE    SUB16               UPDATE QUEUE IF JOB SUBMITTED                
*                                                                               
         MVI   SRJOBSTA,SRJOBERR   SET ENTRY IN ERROR & WRITE PAGE BACK         
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRSD                         
         ZIC   R1,TJOBSINQ         DECREMENT TERMINAL NUMBER OF JOBS            
         SH    R1,=H'1'                                                         
         STC   R1,TJOBSINQ                                                      
         BP    SUB11               RESET UTL FLAG IF ALL JOBS SUBMITTED         
*                                                                               
SUB14    MVI   TJOBSINQ,0          RESET UTL VALUES ON ERROR/EMPTY Q            
         NI    TJOBFLAG,255-TJOBFINQ                                            
         B     SUB2                AND PASS UTL AGAIN                           
*                                                                               
SUB16    OI    SRJOBSTA,SRJOBPUT   SET THIS JOB SUBD & WRITE BACK PAGE          
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRSD                         
         ZIC   R1,TJOBSINQ         DECREMENT TERMINAL NUMBER OF JOBS            
         SH    R1,=H'1'                                                         
         STC   R1,TJOBSINQ                                                      
         BP    *+8                 RESET UTL FLAG IF ALL JOBS SUBMITTED         
         NI    TJOBFLAG,255-TJOBFINQ                                            
         OI    TJOBFLAG,TJOBFSUB+TJOBFANY                                       
*                                                                               
         ST    R5,SSBJLAST         SET A(UTL ENTRY) IN SSB                      
         OI    SSBJFLAG,SSBJFJS1   SET AWAITING JES JOB NUMBER                  
         MVC   SSBJUSR,SRJOBUSR    SET USERID NUM FOR JOB MATCH                 
         MVC   SSBJREP,SRJOBREP    SET REPORT NUM FOR JOB MATCH                 
*                                                                               
SUBX     B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ A JOB FROM PRTQUE AND SUBMIT TO JES INTERNAL READER *         
*                                                                     *         
* NTRY - R3=A(SRJOBQ ENTRY)                                           *         
* EXIT - CC=LOW IF SUBMIT FACILITY NOT AVAILABLE                      *         
*        CC=EQ IF JOB SUBMITTED                                       *         
*        CC=HIGH IF ERROR ON PRTQUE                                   *         
***********************************************************************         
         SPACE 1                                                                
SUBJOB   NTR1  ,                                                                
         MVC   SSBMVSID,=C'*UNKNOWN'                                            
         GOTO1 VPOWWOW,PARA,PUT,ADR,0,0                                         
         CLI   8(R1),X'FF'         TEST SUBMITTER BUSY                          
         BE    SUBJOBLO                                                         
*                                                                               
         LA    RF,NDX              READ INDEX ENTRY USING REPORT ID             
         USING UKRECD,RF                                                        
         XC    NDX,NDX                                                          
         MVC   UKKEY(7),SRJOBUSR                                                
         MVI   UKFLAG,UKFLNUM                                                   
         GOTO1 VDATAMGR,DMCB,INDEX,PRTQUE,NDX,LINE,CIREC                        
         CLI   8(R1),0                                                          
         BNE   SUBJOBHI                                                         
         LA    RF,NDX              EXTRACT RETURN VALUES                        
         MVC   PRTQID,PRTQUE                                                    
         MVC   PRTQID+4(1),UKUSRINF+4                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,READ,PRTQID,NDX,LINE,CIREC                         
         CLI   8(R1),0                                                          
         BNE   SUBJOBHI                                                         
         LA    RF,CIREC                                                         
         USING PQRECD,RF                                                        
         CLC   PQKEY,SRJOBUSR      TEST PQ KEY AGAINST JOBQ KEY                 
         BNE   SUBJOBHI                                                         
         TM    PQATTB,PQATJOBO     TEST RUNNING/COMPLETE                        
         BNZ   SUBJOBHI                                                         
         TM    PQATTB,PQATJOBI                                                  
         BZ    SUBJOBHI                                                         
         B     SUBJOB4                                                          
*                                                                               
SUBJOB2  GOTO1 VDATAMGR,DMCB       GET NEXT RECORD FROM PQ                      
         TM    8(R1),X'80'         TEST FOR E-O-F ON PQ                         
         BNZ   SUBJOBEQ                                                         
         TM    8(R1),X'7F'         TEST FOR ERRORS                              
         BNZ   SUBJOBHI                                                         
SUBJOB4  CLC   LINE+1(L'SPACES),SPACES                                          
         BE    SUBJOB2                                                          
         CLC   LINE+01(2),=C'//'   LOCATE JOB CARD                              
         BNE   SUBJOB6                                                          
         CLC   LINE+11(5),=C' JOB '                                             
         BNE   SUBJOB6                                                          
         MVC   SSBMVSID,LINE+3     EXTRACT MVS JOB NAME                         
*                                                                               
SUBJOB6  GOTO1 VPOWWOW,PARA        PUT CARD TO INTERNAL READER                  
         B     SUBJOB2                                                          
*                                                                               
SUBJOBLO MVI   DUB,0               SUBMITTER NOT AVAILABLE - EXIT               
         B     SUBJOBX                                                          
SUBJOBEQ MVI   DUB,1               JOB SUBD - POST JOB END & EXIT               
         GOTO1 VPOWWOW,PARA,PUT,EOJ,0,0                                         
         B     SUBJOBX                                                          
SUBJOBHI MVI   DUB,2               PQ ERROR - ERASE JOB & EXIT                  
         GOTO1 VPOWWOW,PARA,PUT,END,0,0                                         
SUBJOBX  CLI   DUB,1               SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* POST JES JOB NUMBER TO S/R SAVE PAGE FOR LAST SUBMITTED JOB         *         
***********************************************************************         
         SPACE 1                                                                
JOB2     ICM   R5,15,SSBJLAST      POINT TO UTL ENTRY                           
         BZ    JOB9                                                             
*                                                                               
         LA    RF,SRPAGENO         SET S/R TWA SAVE PAGE & READ/LOCK IT         
         SLL   RF,32-8                                                          
         ICM   RF,3,TNUM                                                        
         ST    RF,TWAPAGE                                                       
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),SSBTWAL                                               
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,TWAPAGE,SRSD                
*                                                                               
         LA    R3,SRJOBQ           SEARCH TERMINAL QUEUE TO FIND JOB            
         USING SRJOBQ,R3           R3=A(JOB QUEUE)                              
         SR    R0,R0                                                            
         ICM   R0,1,SRJOBINQ       R0=NUMBER OF JOBS IN QUEUE                   
         BZ    JOB9                                                             
         SR    RE,RE               RE=NUMBER OF JOBS TO SUBMIT                  
JOB3     CLI   SRJOBSTA,SRJOBPUT   TEST JOB SUBMITTED                           
         BNE   JOB4                                                             
         OC    SRJOBJES,SRJOBJES   AND JOB NUMBER NOT SET                       
         BNZ   JOB4                                                             
         CLC   SRJOBUSR,SSBJUSR    AND USERID NUMBER MATCHES                    
         BNE   JOB4                                                             
         CLC   SRJOBREP,SSBJREP    AND REPORT NUMBER MATCHES                    
         BE    JOB6                                                             
JOB4     LA    R3,SRJOBQLN(R3)     BUMP TO NEXT JOB QUEUE ENTRY                 
         BCT   R0,JOB3                                                          
         B     JOB9                                                             
*                                                                               
JOB6     MVC   SRJOBJES,SSBJESNO   GET JES JOB NUMBER                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,TWAPAGE,SRSD                         
*                                                                               
         XC    NDX,NDX             GET PRTQ FILE ID FROM USER ID NUM            
         MVC   NDX(2),SRJOBUSR                                                  
         GOTO1 VDATAMGR,DMCB,GFILE,PRTQUE,NDX,LINE,CIREC                        
         CLI   8(R1),0                                                          
         BNE   SUBJOBHI                                                         
         MVC   PRTQID(8),NDX+UKUSRINF-UKRECD                                    
*                                                                               
         GOTO1 VTICTOC,DUB,C'BGET' GET TIME IN BINARY                           
         L     R1,SSBAJOB          LOCATE A FREE JOB TABLE ENTRY                
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING JOBTABD,R1                                                       
         CLI   JOBSTAT,JOBSAVA     TEST AVAILABLE ENTRY                         
         BE    *+12                                                             
         BXLE  R1,RE,*-8                                                        
         B     SUBX                NO ENTRY SO GET OUT (WAS DC H'0')            
*                                                                               
         MVI   JOBSTAT,JOBSUSE     BUILD JOB TABLE ENTRY                        
         MVC   JOBTERM,TNUM                                                     
         MVC   JOBPQKEY,SRJOBUSR                                                
         MVC   JOBPQCIA,SRJOBCIA                                                
         MVC   JOBPQID,PRTQID+4                                                 
         SR    R0,R0                                                            
         ICM   R0,7,DUB+1          CONVERT TIME TO 4/3 SECOND UNITS             
         MH    R0,=H'3'                                                         
         SRA   R0,2                                                             
         STCM  R0,3,JOBSTIME                                                    
         MVC   JOBJESNO,SRJOBJES                                                
         MVC   JOBMVSID,SSBMVSID                                                
         OI    SSBSTAT1,SSBSCHK1                                                
*                                                                               
JOB9     NI    SSBJFLAG,255-SSBJFJS1-SSBJFJS2                                   
         B     SUB2                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
GFILE    DC    C'GFILE   '                                                      
INDEX    DC    C'INDEX   '                                                      
READ     DC    C'READ    '                                                      
DMREAD   DC    C'DMREAD  '                                                      
DMWRT    DC    C'DMWRT   '                                                      
TEMPSTR  DC    C'TEMPSTR '                                                      
PRTQUE   DC    C'PRTQU   '                                                      
SPACES   DC    CL80' '                                                          
         SPACE 1                                                                
PUT      DC    C'PUT'                                                           
ADR      DC    C'ADR'                                                           
END      DC    C'END'                                                           
EOJ      DC    C'EOJ'                                                           
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARA     DS    6F                                                               
*                                                                               
ATIA     DS    A                                                                
TWAPAGE  DS    A                                                                
JESNUM   DS    H                                                                
UTLPASS  DS    X                                                                
         DS    XL3                                                              
TLIST    DS    256A                                                             
*                                                                               
PRTQID   DS    CL8                                                              
NDX      DS    XL40                                                             
         DS    XL24                                                             
LINE     DS    CL256                                                            
CIREC    DS    14336C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DMPRTQS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRSUB00   10/02/08'                                      
         END                                                                    
