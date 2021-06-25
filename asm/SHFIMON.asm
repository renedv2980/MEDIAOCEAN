*          DATA SET SHFIMON    AT LEVEL 006 AS OF 12/05/12                      
*PHASE SHFIMONB                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE QSORT                                                                  
*INCLUDE DDWTO                                                                  
*                                                                               
         TITLE 'SHFIMON - SHARED MEMORY FILE INDEX MONITOR'                     
         PRINT NOGEN                                                            
SHFIMON  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**SFIM**,AWORK,CLEAR=YES                             
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         LARL  RA,COMMON                                                        
         USING COMMON,RA                                                        
*                                                                               
         USING UKRECD,APINDEX      WORKER FILE INDEX                            
         USING PLINED,PLINE        PRINT LINE                                   
         USING SIHDRD,R8           SHARED MEMORY HEADER                         
         USING SITABD,R9           SHARED MEMORY INDEX TABLE HEADER             
*                                                                               
         BRAS  RE,INIT             READ CARDS ECT                               
*                                                                               
         BRAS  RE,SETESTAE         SET UP ERROR HANDLING                        
*                                                                               
         LA    R1,RESTART          R1=RESTART POINT AFTER ABEND                 
         L     RF,=A(MYREGS)       SAVE CURRENT REGS FOR RESTARTING             
         STM   R0,RF,0(RF)                                                      
*                                                                               
RESTART  SAM24 ,                   MAKE SURE WE RESTART IN 24 BIT               
*                                                                               
         CLI   ENDTASK,C'Y'        DO WE WANT TO CANCEL                         
         BE    ENDOFJOB                                                         
*                                                                               
SHFI010  BRAS  RE,SETWAIT                                                       
         BRAS  RE,TIMECHK          CHECK TIMES                                  
         BRAS  RE,MAIN                                                          
*                                                                               
         CLI   MODE,C'M'           ARE WE RUNNING IN MONITORING MODE?           
         BNE   ENDOFJOB            . NO, THEN END THE JOB                       
         B     SHFI010                                                          
*                                                                               
AWORK    DC   A(WORKAREA)                                                       
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK TIMERS - SEE IF ANYTHING TO DO                                          
***********************************************************************         
TIMECHK  NTR1  ,                                                                
         LA    RF,TIMETAB                                                       
*                                                                               
TIMECHK0 MVI   0(RF),C'N'          LETS ASSUME NOT                              
         L     R1,TIMENOW          R1 = TIME NOW                                
         XR    R0,R0                                                            
         S     R1,4(RF)            SUBTRACT LAST TIME                           
         BM    TIMECHK1            -VE MUST BE MIDNIGHT                         
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,2(RF)                                                       
         MHI   RE,60*100           CALCULATE MINUTES                            
         CR    R1,RE               IS IT > TIME FREQ                            
         BL    TIMECHK2                                                         
*                                                                               
TIMECHK1 MVI   0(RF),C'Y'          OK DO IT                                     
         MVC   4(4,RF),TIMENOW     SET TIME WE DID IT                           
*                                                                               
TIMECHK2 LA    RF,8(RF)            NEXT                                         
         CLI   0(RF),X'FF'                                                      
         BNE   TIMECHK0                                                         
*                                                                               
TIMECHKX B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* MAIN PROGRAM LOOP                                                             
***********************************************************************         
MAIN     NTR1  ,                                                                
*                                                                               
         CLC   =C'NO',BUILD        INIT/REBUILD                                 
         BE    MAIN010                                                          
         BRAS  RE,RBUILD                                                        
         MVC   BUILD,=CL8'NO'                                                   
*                                                                               
MAIN010  CLI   MODE,C'B'           RUN IN BUILD MODE?                           
         BE    MAIN030             . THEN OUR JOB IS COMPLETE                   
*                                                                               
         CLI   TIMESCAN,C'N'       TIMER POP CI SCAN                            
         BNE   MAIN020                                                          
         CLC   =C'NO',FREE1        REQUESTED PART1 CI SCAN                      
         BNE   MAIN020                                                          
         CLC   =C'NO',FREE2        REQUESTED PART2 CI SCAN                      
         BE    MAIN030                                                          
*                                                                               
MAIN020  BRAS  RE,RSCAN                                                         
*                                                                               
MAIN030  LHI   RF,C'K'             CLEAN-UP TASK CALL                           
         GOTO1 VISGENQ,DMCB,(RF)                                                
*                                                                               
MAINX    B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* END OF JOB                                                                    
***********************************************************************         
ENDOFJOB GOTO1 VDATAMGR,DMCB,DMCLSE,CONTROL,FLIST,AIOAREA                       
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PMMARK,C'*'                                                      
         MVC   PMMSG,=CL12'END OF JOB'                                          
         MVI   PMLP,C'('                                                        
         EDIT  (TIME,NOW),(8,PMTIME)                                            
         MVC   PMDAYA,DAYNOWA                                                   
         MVC   PMDATEA,TODAYA                                                   
         MVI   PMRP,C')'                                                        
         BRAS  RE,PRINTL                                                        
         BRAS  RE,PRINTX           CLOSE SYSPRINT                               
*                                                                               
         B     XBASE                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALISE                                                                    
***********************************************************************         
INIT     NTR1  ,                                                                
*                                                                               
         LHI   R1,L'MSGW           INIT WTO MESSAGE                             
         STH   R1,MSGWLEN                                                       
         MVC   MSGWTXT,SPACES                                                   
*                                                                               
         BRAS  RE,PRINTI           INIT PRINTING                                
*                                                                               
         L     R1,=A(CTREC-WORKD)  IO AREAS                                     
         ALR   R1,RC                                                            
         ST    R1,ACTREC                                                        
         L     R1,=A(CIREC-WORKD)                                               
         ALR   R1,RC                                                            
         ST    R1,ACIREC                                                        
         L     R1,=A(IOAREA-WORKD)                                              
         ALR   R1,RC                                                            
         ST    R1,AIOAREA                                                       
*                                                                               
         LA    R3,CARD             PROCESS INPUT CARDS                          
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT04                                                           
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT THE PARAMETER CARD                     
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         MVI   CARDFROM,C'S'       CARD COMING FROM START OF JOB                
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         BNE   XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
         L     RF,=V(DDSIO)        DDSIO VERSION SPECIFICATION                  
         MVC   0(8,RF),DDSIO                                                    
         B     INIT02                                                           
*                                                                               
INIT04   BRAS  RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         CLC   MEMORY,SPACES       MAKE SURE SHARED MEMORY IS INIT'ED           
         JNH   *+2                 DEATH IF NOT INITIALIZED                     
*                                                                               
         GOTO1 VSHMUSS,DMCB,ATTACH,MEMORY,0,0                                   
         ICM   R1,15,DMCB+8        A(SHARED TABLE)                              
         JZ    *+2                 DEATH IF CAN'T ATTACH TO MEMORY              
         ST    R1,FIWSHA                                                        
*                                                                               
         ICM   R1,15,DMCB+12       MAX TABLE SIZE                               
         JZ    *+2                 DEATH IF TABLE SIZE IS ZERO                  
         ST    R1,SHMSIZE                                                       
*                                                                               
         MVC   FIWENQ,VISGENQ      A(ISGENQ)                                    
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,FLIST,AIOAREA                       
         CLI   8(R1),0                                                          
         JNE   *+2                 DEATH IF OPEN ERROR                          
*                                                                               
         EXTRACT RESULTS,'S',FIELDS=(TIOT,ASID)                                 
         ALESERV EXTRACTH,STOKEN=RXSTOKEN                                       
*                                                                               
         L     RF,RXTIOT                                                        
         MVC   RXJOB,0(RF)                                                      
*                                                                               
         CLI   MODE,C'M'                                                        
         BNE   INIT050                                                          
*                                                                               
         SAM31                                                                  
         L     R8,FIWSHA           SAVE JOB INFO IN SHARED MEMORY               
         MVC   SIHMNAME,RXJOB      JOB NAME                                     
         MVC   SIHMSTOK,RXSTOKEN   STOKEN                                       
         MVC   SIHMASID,RXASID+2   ASID                                         
         MVC   SIHMECB,ALERTECB    A(ECB)                                       
         SAM24                                                                  
*                                                                               
INIT050  LHI   R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET UP OPERATOR COMMS                                                         
***********************************************************************         
SETOPS   NTR1  ,                                                                
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         USING CIBNEXT,R2                                                       
         LA    R3,COMCIBPT         SET A(A(CIB))                                
         DROP  RF                                                               
         CLI   CIBVERB,CIBSTART    TEST FOR 'S JOBNAME' (IE NOT BATCH)          
         BNE   SETOP2                                                           
         DROP  R2                                                               
*                                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)        RELEASE THE CIB                    
*                                                                               
SETOP2   QEDIT ORIGIN=(R3),CIBCTR=1          ACCEPT MODIFY COMMANDS             
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* SETWAIT - SET TIMER AND WAIT                                                  
* RETURN WHEN AN INTERUPT IS DETECTED                                           
***********************************************************************         
SETWAIT  NTR1  ,                                                                
         MVI   HOWPOP,0                                                         
*                                                                               
         OC    DATENOW,DATENOW     FIRST TIME IN, DON'T WAIT                    
         BZ    SW100                                                            
         CLI   MODE,C'M'           ARE WE RUNNING IN MONITORING MODE?           
         BNE   SW100               . NO, THEN DON'T WAIT                        
*                                                                               
         XR    R0,R0                                                            
         LH    R1,POP                                                           
         MHI   R1,60*100           (WAIT TIME)*(1 MINUTE IS 60*100)             
         ST    R1,TIME                                                          
         XC    TIMRECB,TIMRECB                                                  
         STIMERM SET,ID=STIMERID,BINTVL=TIME,EXIT=TIMRXIT                       
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         WAIT  1,ECBLIST=ECBLST    WAIT FOR TIMER, OPERATOR, OR PROGRAM         
*                                                                               
         L     RF,ALERTECB         A(PROGRAM ECB)                               
         TM    0(RF),X'40'         IS IT A PROGRAMMATIC INTERRUPT?              
         BZ    SW010               NO                                           
         STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
         BRAS  RE,CHKALERT         EXAMINE THE ALERT INTERRUPT                  
         MVI   HOWPOP,C'A'         ALERT                                        
         B     SW100                                                            
*                                                                               
SW010    L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    SW020               NO                                           
         STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL TIMER CANCEL                    
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
         MVI   HOWPOP,C'O'         OPERATOR COMMAND                             
         B     SW100                                                            
*                                                                               
SW020    TM    TIMRECB,X'40'       DID THE TIMER POP?                           
         BO    *+6                 YES                                          
         DC    H'0'                HOW DID WE EXIT THE WAIT THEN?               
         MVI   HOWPOP,C'T'         TIMER                                        
*                                                                               
SW100    MVC   TIMEOLD,TIMENOW     SET THE PREVIOUS TIME                        
         MVC   DATEOLD,DATENOW                                                  
         MVC   TIMEOLDC,TIMENOWC                                                
*                                                                               
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R1,DATENOW                                                       
         ST    R0,TIMENOW                                                       
         LR    R1,R0                                                            
         XR    R0,R0                                                            
         D     R0,=F'60000'        GET 10 MINUTE INTERVALS FROM .01 SEC         
         STC   R1,TIMENOWM         TIME NOW BIN 10MINS                          
*                                                                               
         TIME  TU                  R0=TIME IN 1/38400 SECS                      
         SRDL  R0,32                                                            
         D     R0,=F'38400'        R1=TIME IN SECONDS                           
         MHI   R1,3                                                             
         SRL   R1,2                R1=(SECS*3)/4                                
         STH   R1,TIMENOWC         TIMEC=TIME IN SPECIAL UNITS                  
*                                                                               
         CLC   DATEOLD,DATENOW     HAS DATE CHANGED                             
         BE    SW150                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(0,DUB)                                       
         GOTO1 VADDAY,DMCB,(C'D',DUB),DUBDUB,F'-1'                              
         GOTO1 VDATCON,DMCB,(0,DUBDUB),(2,YESTRDAY)                             
*                                                                               
         GOTO1 VADDAY,DMCB,(C'D',DUB),DUBDUB,F'-15'                             
         GOTO1 VDATCON,DMCB,(0,DUBDUB),(2,TWOWEEKS)                             
*                                                                               
         GOTO1 VGETDAY,DMCB,(0,DUB),FULL                                        
         MVC   DAYNOW,0(R1)                                                     
         MVC   DAYNOWA,FULL                                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(1,DUB)                                       
         MVC   DATNOW(1),DUB+2                                                  
         MVC   MONNOW(1),DUB+1                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAY)                                     
         GOTO1 VDATCON,DMCB,(5,0),(13,TODAYA)                                   
*                                                                               
SW150    BRAS  RE,MEMEXAM          EXAMINE MEMORY FOR POSSIBLE ACTIONS          
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* INITIALISE ERROR HANDLING                                                     
***********************************************************************         
SETESTAE NTR1                                                                   
         CLI   USEESTAE,C'Y'                                                    
         BNE   EXITOK                                                           
         WTO   'ESTAE ON'                                                       
         ESTAEX ERREXIT,CT,ASYNCH=YES,TERM=NO                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK OPER INTERRUPT                                                          
***********************************************************************         
CHKOPER  NTR1                                                                   
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PMMARK,C'*'                                                      
         MVC   PMMSG,=CL12'OPERATOR'                                            
         MVI   PMLP,C'('                                                        
         EDIT  (TIME,NOW),(8,PMTIME)                                            
         MVC   PMDAYA,DAYNOWA                                                   
         MVC   PMDATEA,TODAYA                                                   
         MVI   PMRP,C')'                                                        
         BRAS  RE,PRINTL                                                        
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
         XR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
*                                                                               
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
         LA    R1,CARD                                                          
         MVI   CARDFROM,C'F'       CARD COMING FROM OPERATOR INTERUPT           
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         BE    CHEK020             NEQ SO TRY GROUP COMMAND                     
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         MVC   PLINE,SPACES                                                     
         B     CHEKRSET                                                         
*                                                                               
CHEK020  MVC   PLINE(L'CARD),CARD                                               
         BRAS  RE,PRINTL                                                        
*                                                                               
CHEKRSET L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
         DROP  R2                                                               
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* CARD HANDLING ROUTINE                                                         
***********************************************************************         
VALCARD  NTR1                                                                   
*                                                                               
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
VALC010  XR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,7(R4)                                                         
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         BE    VALC016                                                          
VALC015  LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC016  TM    10(R4),X'01'        START UP ONLY?                               
         BZ    VALC017                                                          
         CLI   CARDFROM,C'S'       FROM START?                                  
         BE    VALC020             YES                                          
         B     CERRKEY                                                          
*                                                                               
VALC017  TM    10(R4),X'02'        OPS MODIFY ONLY?                             
         BZ    VALC020                                                          
         CLI   CARDFROM,C'F'       FROM MODIFY?                                 
         BE    VALC020             YES                                          
         B     CERRKEY                                                          
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LA    RF,VALCDELS         DELIMITER TABLE                              
         B     *+8                                                              
VALC022  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         BE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         BE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         BZ    VALC022                                                          
*                                                                               
         XR    R1,R1                                                            
         IC    R1,2(RF)            GET EX LEN                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         BNE   VALC022                                                          
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
         XR    RF,RF                                                            
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
         XR    R0,R0                                                            
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
         BRAS  RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         BE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STH   R1,0(RF)            SAVE HALFWORD (DEFAULT)                      
         B     VALC500                                                          
*                                                                               
VALC070  TM    9(R4),X'02'         TIME INPUT                                   
         BZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
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
         XR    RE,RE                                                            
         IC    RE,8(R4)            PAD OUT TO SPACES                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SPACES                                                   
VALC410  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALC500  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         BE    VALC001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         BL    VALC500                                                          
         B     EXITOK                                                           
*                                                                               
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
CERRX    L     RD,CARDRD                                                        
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
         BRAS  RE,PRINTL                                                        
         B     EXITL                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU                             
***********************************************************************         
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
         BRAS  RE,VALNUM           VALIDATE HOURS                               
         L     RF,=A(60*60*100)                                                 
         BRAS  RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         BRAS  RE,VALNUM                                                        
         L     RF,=A(60*100)                                                    
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         BNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE SECS                                
         L     RF,=F'100'                                                       
         BRAS  RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         BNE   EXITOK                                                           
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         BRAS  RE,VALNUM           VALIDATE TUS                                 
         LA    RF,1                                                             
         BRAS  RE,VALTADD                                                       
         B     EXITOK                                                           
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         BE    CERRTIM                                                          
         XR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROGRAMMATIC ALERT ECB RECEIVED, CHECK CONDITIONS                             
***********************************************************************         
CHKALERT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PMMARK,C'*'                                                      
         MVC   PMMSG,=CL12'ALERTED'                                             
         MVI   PMLP,C'('                                                        
         EDIT  (TIME,NOW),(8,PMTIME)                                            
         MVC   PMDAYA,DAYNOWA                                                   
         MVC   PMDATEA,TODAYA                                                   
         MVI   PMRP,C')'                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,NOTYMSG          NOTIFY MESSAGE                               
*                                                                               
         XC    LERTECB,LERTECB     CLEAR ALERT ECB                              
         J     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXAMINE MEMORY AND DECIDE IF WE NEED BUILD OR SCAN                            
***********************************************************************         
MEMEXAM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM31 ,                                                                
         L     R8,FIWSHA           A(SHARED MEMORY FILE INDEX HEADER)           
         LA    R9,L'SIHDR(,R8)     A(FIRST RESOURCE TABLE HEADER)               
*                                                                               
         ICM   R5,15,SIHNOFR       PICK UP NUMBER OF RESOURCES                  
         BZ    MEX010              . NONE, INIT                                 
         TM    SIHLOCK,X'80'       RESET INDICATOR                              
         BO    MEX010              . INIT                                       
         CLI   SIHEYE,X'00'        EYECATCHER RESET                             
         BE    MEX010              . INIT                                       
         CLC   =C'INIT',BUILD      INITIALIZING THE ENTIRE INDEX                
         BNE   MEX020                                                           
MEX010   MVC   BUILD,=CL8'INIT'    INITIALIZE ALL INDEXES                       
         B     MEXX                                                             
*                                                                               
MEX020   MVC   FIWRES,SITFIL       RESOURCE NAME                                
*                                                                               
         TM    SITRIND,SITRBLD     REBUILD SET                                  
         BZ    *+10                                                             
         MVC   BUILD,=CL8'YES'                                                  
*                                                                               
         TM    SITRIND,SITRP1S     PART1 SCAN NEEDED?                           
         BZ    *+10                                                             
         MVC   FREE1,=CL8'YES'                                                  
*                                                                               
         TM    SITRIND,SITRP2S     PART2 SCAN NEEDED?                           
         BZ    *+10                                                             
         MVC   FREE2,=CL8'YES'                                                  
*                                                                               
         LA    R9,L'SITAB(,R9)     NEXT RESOURCE                                
         BCT   R5,MEX020                                                        
*                                                                               
MEXX     B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD SHARED MEMORY FILE INDEXES                                              
***********************************************************************         
RBUILD   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PMMARK,C'*'                                                      
         MVC   PMMSG,=CL12'BUILDING'                                            
         CLC   BUILD,=CL8'INIT'                                                 
         BNE   *+10                                                             
         MVC   PMMSG,=CL12'INITIALIZING'                                        
         MVI   PMLP,C'('                                                        
         EDIT  (TIME,NOW),(8,PMTIME)                                            
         MVC   PMDAYA,DAYNOWA                                                   
         MVC   PMDATEA,TODAYA                                                   
         MVI   PMRP,C')'                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         SAM31                                                                  
         L     R8,FIWSHA           R8= A(START OF SHARED MEMORY)                
         LA    R9,L'SIHDR(,R8)     R9= A(START OF RESOURCE HEADERS)             
*                                                                               
         CLC   BUILD,=CL8'INIT'                                                 
         BNE   RB010                                                            
         BRAS  RE,LOCKFI                                                        
         BRAS  RE,BLDHDR           BUILD SHARED MEMORY HEADER                   
*                                                                               
RB010    ICM   R5,15,SIHNOFR       NUMBER OF RESOURCES IN MEMORY                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
RB050    MVC   FIWRES,SITFIL       RESOURCE FILE NAME                           
         BRAS  RE,BLDTAB           BUILD RESOURCE SHARED MEMORY INDEX           
         LA    R9,L'SITAB(,R9)     NEXT RESOURCE                                
         BCT   R5,RB050                                                         
*                                                                               
         BRAS  RE,PRINTTOT         PRINT TOTALS                                 
*                                                                               
         CLC   BUILD,=CL8'INIT'                                                 
         BNE   RBX                                                              
         BRAS  RE,UNLKFI                                                        
*                                                                               
RBX      B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD SHARED MEMORY FILE QUEUE HEADER                                         
***********************************************************************         
BLDHDR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,FIWSHA           R8= A(START OF SHARED MEMORY)                
         LA    R9,L'SIHDR(,R8)     R9= A(START OF RESOURCE HEADERS)             
         ST    R9,FIWRHA                                                        
                                                                                
*----------------------------------                                             
* MVCL LOOP IS USED BECAUSE MVCLE                                               
* DOESN'T WORK ON USS SHARED MEMORY                                             
*----------------------------------                                             
         L     RE,FIWSHA           START OF SHARED MEMORY                       
         L     RF,SHMSIZE          MEMORY SIZE                                  
BHDR1    XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0               CLEAR AS MUCH AS MVCL WILL ALLOW             
         LTR   RF,RF               IS LENGTH ZERO?                              
         BZ    BHDR2               YES: THEN FINISHED                           
         MVI   0(RE),0             NO: CLEAR ONE MORE BYTE                      
         LA    RE,1(RE)            ADJUST MEMORY LOCATION                       
         AHI   RF,-1               AND LENGTH BY ONE                            
         B     BHDR1               THEN GO CLEAR SOME MORE                      
BHDR2    DC    0H                                                               
*----------------------------------                                             
* END OF MVCL LOOP                                                              
*----------------------------------                                             
                                                                                
         MVC   SIHEYE,EYESFI       EYECATCHER "**SHFI****SHFI**"                
         MVC   SIHSIZE,SHMSIZE     ALLOCATED SIZE OF SHARED MEMORY              
         MVC   SIHMNAME,RXJOB      MONITORING JOB NAME                          
         MVC   SIHMSTOK,RXSTOKEN   MONITORING JOB STOKEN                        
         MVC   SIHMASID,RXASID+2   MONITORING JOB ASID                          
         MVC   SIHMECB,ALERTECB    A(ALERT ECB)                                 
*                                                                               
         XR    R1,R1               INIT NUMBER OF RESOURCES                     
         LA    R4,FILES            FILES DEFINED IN PARAMETERS                  
BHDR040  CLI   0(R4),C' '          END OF LIST                                  
         BE    BHDR050                                                          
         MVC   SITFIL,0(R4)        RESOURCE NAME                                
         AHI   R1,1                BUMP RESOURCE NUMBER                         
         STC   R1,SITNUM                                                        
         LA    R9,L'SITAB(,R9)                                                  
         LA    R4,L'FILES(,R4)                                                  
         B     BHDR040                                                          
*                                                                               
BHDR050  ST    R9,ASFIPARN         A(FIRST PART1 AREA)                          
         ST    R1,SIHNOFR          NUMBER OF RESOURCES                          
         SLR   R9,R8               A(END OF HEADERS-START OF MEMORY)            
         ST    R9,SIHPARS          DISPLACEMENT TO START OF PART1 AREA          
*                                                                               
         L     R1,SIHNOFR          NUMBER OF RESOURCES                          
         MHI   R1,L'SITAB                                                       
         AHI   R1,L'SIHDR                                                       
         ST    R1,SIHUSED          STORAGE USED SO FAR                          
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD SHARED MEMORY TABLE FOR RESOURCE FILE                                   
***********************************************************************         
BLDTAB   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,FIRSET           SET A(RESOURCE HEADER) AND A(PARTS)          
         BE    BT010                                                            
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* RESOURCE NOT FOUND'                        
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         DC    H'0'                                                             
*                                                                               
BT010    L     R9,FIWRHA           R9= A(RESOURCE HEADER)                       
*                                                                               
         CLC   BUILD,=CL8'INIT'    INITIALIZING MEMORY?                         
         BE    BT030               . YES                                        
*                                                                               
         CLC   FIWRES,BUILD        REBUILD NEEDED FOR THIS RESOURCE?            
         BE    BT020               . YES                                        
         TM    SITRIND,SITRBLD     REBUILD NEEDED FOR THIS RESOURCE?            
         BZ    BLDTX                  . NO, EXIT                                
*                                                                               
BT020    OI    SITRIND,SITRBLD     SET INDICATOR                                
         BRAS  RE,FIRFLOCK         LOCK THE RESOURCE                            
*                                                                               
BT030    MVI   PRMARK,C'-'                                                      
         MVC   PRRESO(8),=C'RESOURCE'                                           
         LLC   R3,SITNUM                                                        
         EDIT  (R3),PRNUM                                                       
         MVC   PRNAME,FIWRES                                                    
         MVC   PRDESC(8),=C'BUILDING'                                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         BRAS  RE,SETFI            SET FILE INFORMATION                         
*                                                                               
         MVC   FIWCIA,SIT1STT      FIRST PART1 DISK ADDRESS                     
         L     R4,SIT1CIC          TOTAL NUMBER OF PART1 CIS                    
*                                                                               
BT100    BRAS  RE,READCI           READ THE CI FROM FILE                        
         BRAS  RE,ADDFI            ADD FILE TO SHARED STORAGE AND TREE          
         L     R1,FIWCIA                                                        
         AL    R1,SIT1TPT          BUMP TRACK TO NEXT CI                        
         ST    R1,FIWCIA                                                        
         BCT   R4,BT100            PROCESS THE NEXT PART1                       
*                                                                               
         BRAS  RE,CHP1AV           CHAIN AVAILABLE PART1S                       
         BRAS  RE,CHP2AV           CHAIN AVAILABLE PART2S                       
*                                                                               
         CLC   BUILD,=CL8'INIT'    INITIALIZING MEMORY?                         
         BE    BLDTX               . YES, UNLOCK HANDLED LATER                  
         BRAS  RE,FIRFUNLK         UNLOCK THE RESOURCE                          
*                                                                               
BLDTX    B     EXITOK                                                           
                                                                                
***********************************************************************         
* SET WORKER FILE INFORMATION                                                   
***********************************************************************         
SETFI    NTR1  ,                                                                
*                                                                               
         MVC   SITRFDA,=X'00010100'                                             
         MVC   FIWCIA,SITRFDA                                                   
*                                                                               
         SAM24                                                                  
         GOTO1 VDATAMGR,DMCB,FFINI,FIWRES,FIWKEY,FIWCIA,ACIREC                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDYNALLO,DMCB,(C'D',FIWRES),FILEDSN                              
         OC    FILEDSN,SPACES                                                   
         CLC   FILEDSN,SPACES                                                   
         BH    *+6                                                              
         DC    H'0'                                                             
         SAM31                                                                  
*                                                                               
         BRAS  RE,READCI           READ THE FIRST CI                            
*                                                                               
         L     R4,ACIREC                                                        
         USING CIFDATA,R4                                                       
*                                                                               
         MVC   SITFXID,CFFWFXID    FILE EXTERNAL ID (TST/ADV/REP)               
         MVC   SITDSN,FILEDSN                                                   
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',CIFBLKLN       BLOCK LENGTH                           
         ST    R1,SITDBS           DATA SET BLOCK LENGTH                        
*                                                                               
         MVC   HALF,CIFINDX        INDEX TOTAL NUMBER OF CIS                    
         NI    HALF,X'0F'          TURN OFF FLAGS                               
         XR    R2,R2                                                            
         ICM   R2,B'0011',HALF                                                  
         ST    R2,SITXCIC                                                       
         SLL   R2,16                                                            
         ST    R2,SITXCIT                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',CIFCITOT PART1 TOTAL NUMBER OF CI'S                   
         S     R1,SITXCIC          SUBTRACT # OF CIS USED FOR INDEX             
         ST    R1,SIT1CIC                                                       
         SLL   R1,16                                                            
         ST    R1,SIT1CIT                                                       
*                                                                               
         XR    R2,R2                                                            
         ICM   R2,B'0011',CJFCITOT PART2 TOTAL NUMBER OF CI'S                   
         ST    R2,SIT2CIC                                                       
         SLL   R2,16                                                            
         ST    R2,SIT2CIT                                                       
*                                                                               
         L     R1,SIT1CIC                                                       
         MHI   R1,L'SI1PAR         SIZE OF RESOURCE PART1 AREA                  
         L     R2,SIT2CIC                                                       
         MHI   R2,L'SI2PAR         SIZE OF RESOURCE PART2 AREA                  
*                                                                               
         L     RF,ASFIPARN         A(NEXT AVAILABLE INDEX AREA                  
         CLC   BUILD,=CL8'INIT'    ARE WE INITIALIZING                          
         BE    *+8                 . YES                                        
         L     RF,FIWP1A           A(RESOURCE PART1 INDEXES)                    
*                                                                               
         ST    RF,FIWP1A                                                        
         ALR   RF,R1                                                            
         ST    RF,FIWP2A                                                        
         ALR   RF,R2                                                            
         ST    RF,ASFIPARN         A(NEXT RESOURCE PART AREA)                   
*                                                                               
         AR    R1,R2               STORAGE USED FOR INDEXES                     
*                                                                               
         CLC   BUILD,=CL8'INIT'    ARE WE IN INIT MODE                          
         BE    SF040               . YES                                        
         C     R1,SITSIZE          . NO, BE SURE SIZE HASN'T CHANGED            
         BE    SF030               . EQUAL, WE'RE GOOD                          
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* IN REBUILD, SIZE MISMATCH'                 
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         DC    H'0'                                                             
*                                                                               
SF030    XR    R0,R0               CLEAR RESOURCE INDEX AREA                    
         XR    R1,R1                                                            
         L     RE,FIWP1A                                                        
         L     RF,SITSIZE                                                       
         MVCL  RE,R0                                                            
         LTR   RF,RF                                                            
         JNZ   *+2                 FAIL IF COULD NOT CLEAR ALL STORAGE          
*                                                                               
         XC    SITP1AV,SITP1AV     RESET COUNTS, QUEUE & TREE POINTERS          
         XC    SITP2AV,SITP2AV                                                  
         XC    SITP1VU,SITP1VU                                                  
         XC    SITP2VU,SITP2VU                                                  
         XC    SITP1HD,SITP1HD                                                  
         XC    SITP1TL,SITP1TL                                                  
         XC    SITP2HD,SITP2HD                                                  
         XC    SITP2TL,SITP2TL                                                  
         XC    SITTREE,SITTREE     ROOT OF INDEX KEY TREE                       
         B     SF060                                                            
*                                                                               
SF040    ST    R1,SITSIZE          RESOURCE TOTAL SIZE OF TABLE                 
*                                                                               
         A     R1,SIHUSED          STORAGE USED SO FAR                          
         C     R1,SIHSIZE          BE SURE IT CAN FIT                           
         BNH   SF050               . ALL GOOD                                   
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* NOT ENOUGH SHARED STORAGE'                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         DC    H'0'                                                             
*                                                                               
SF050    ST    R1,SIHUSED          IT IS NOW ACCOUNTED FOR                      
*                                                                               
SF060    L     R1,FIWP1A                                                        
         SLR   R1,R8               SUBTRACT START OF SHARED MEMORY              
         ST    R1,SITP1ST          DISPLACEMENT TO PART1S                       
*                                                                               
         L     R1,FIWP2A                                                        
         SLR   R1,R8               SUBTRACT START OF SHARED MEMORY              
         ST    R1,SITP2ST          DISPLACEMENT TO PART2S                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',CIFTRKS  NUMBER OF TRACKS PER PART1 CI                
         ST    R1,SIT1TPC                                                       
         SLL   R1,16                                                            
         ST    R1,SIT1TPT                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',CJFTRKS  NUMBER OF TRACKS PER PART2 CI                
         ST    R1,SIT2TPC                                                       
         SLL   R1,16                                                            
         ST    R1,SIT2TPT                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',CIFFDTRK STARTING TRACK                               
         SLL   R1,16                                                            
         OILL  GR1,X'0100'         R1                                           
         ST    R1,SIT1STT                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',CJFSTTRK STARTING TRACK                               
         SLL   R1,16                                                            
         OILL  GR1,X'0100'         R1                                           
         ST    R1,SIT2STT                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,B'0011',SITRFDA+2                                             
         NILL  GR1,X'0FFF'         R1 ISOLATE THE RECORD                        
         MH    R1,CIFHIREC                                                      
         ST    R1,SITRFHR          HIGHEST RECORD NUMBER                        
*                                                                               
         XC    CNTPAR1,CNTPAR1     CLEAR PART COUNTERS                          
         XC    CNTPAR2,CNTPAR2                                                  
*                                                                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD FILE INFORMATION TO SHARED QUEUE                                          
***********************************************************************         
ADDFI    NTR1  ,                                                                
*                                                                               
         MVC   FIRSTCI,FIWCIA      SAVE PART1 FIWCIA                            
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
         MVC   GOODCI,FIWCIA       CI IS ALL GOOD                               
         XC    ACURPAR2,ACURPAR2                                                
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R4,FIWNDA           R4= A(PART1 NODE)                            
         ST    R4,ACURPAR1         SAVE A(CURRENT PART1 NODE)                   
         USING SI1PARD,R4                                                       
*                                                                               
         L     R2,ACIREC                                                        
         USING W_RECD,R2                                                        
         MVC   SVINDEX(L'W_INDEX),W_INDEX                                       
*                                                                               
         CLI   W_STAT,0            IGNORE EMPTY/PURGED ENTRIES                  
         BE    AFX                                                              
         MVC   SI1NDX,W_INDEX      SET PART1 INDEX IN MEMORY                    
*                                                                               
         OC    W_CINEXT,W_CINEXT   IS THERE A NEXT CI                           
         BZ    AF080               NO, FINISH UP                                
*                                                                               
         LHI   R1,X'0100'                                                       
         ICM   R1,B'1100',W_CINEXT PICK UP NEXT CI IN CHAIN                     
         CL    R1,SIT2STT          CHECK PART2 HAS VALID DISK ADDRESS           
         BL    BADNXT              NO, CHOP IT OFF                              
         ST    R1,FIWCIA                                                        
*                                                                               
         BRAS  RE,READCI                                                        
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
*                                                                               
         CLC   W_KEY,SVINDEX       CHECK PART2 KEY IS SAME AS PART1 KEY         
         BNE   BADNDX              INDEX KEY MISMATCH, CHOP IT                  
         CLC   W_FILENO,SVINDEX+(W_FILENO-W_INDEX) FILE # MISMTCH, CHOP         
         BNE   BADFNO              INDEX KEY MISMATCH, CHOP IT                  
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI1NXT           NEXT PART2                                   
*                                                                               
         L     R4,FIWNDA           R4= A(PART2 NODE)                            
         ST    R4,ACURPAR2         SAVE A(CURRENT PART2 NODE)                   
         USING SI2PARD,R4                                                       
         MVC   SI2NDX,W_INDEX      SET PART2 INDEX IN MEMORY                    
*                                                                               
         L     R1,ACURPAR1         CURRENT PART1                                
         S     R1,FIWSHA                                                        
         ST    R1,SI2PRV           FIRST PREVIOUS IS THE PART1                  
         B     AF060                                                            
*                                                                               
AF040    BRAS  RE,READCI                                                        
         MVC   THISCI,FIWCIA       THIS CI UNDER INSPECTION                     
*                                                                               
         L     R4,FIWNDA                                                        
         ST    R4,ACURPAR2         SAVE CURRENT PART2                           
*                                                                               
         CLC   W_KEY,SVINDEX       CHECK PART2 KEY IS SAME AS PART1 KEY         
         BNE   BADNDX              INDEX KEY MISMATCH, CHOP IT                  
         CLC   W_FILENO,SVINDEX+(W_FILENO-W_INDEX) FILE # MISMTCH, CHOP         
         BNE   BADFNO              INDEX KEY MISMATCH, CHOP IT                  
*                                                                               
         LHI   R1,X'0100'                                                       
         ICM   R1,B'1100',W_CIPREV POINTER TO PREVIOUS CI IN CHAIN              
         CL    R1,SIT2STT          VALID PART2 WITHIN RANGE?                    
         BL    BADPRE              NO, CHOP IT OFF                              
*                                                                               
         MVC   SI2NDX,W_INDEX      SET PART2 INDEX IN MEMORY                    
*                                                                               
         ST    R1,FIWCIA                                                        
         BRAS  RE,FIRCN                                                         
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI2PRV           DISPLACEMENT TO PREVIOUS PART2               
*                                                                               
AF060    OC    W_CINEXT,W_CINEXT   IS THERE A NEXT CI?                          
         BZ    AF080               NO, FINISH UP                                
*                                                                               
         LHI   R1,X'0100'                                                       
         ICM   R1,B'1100',W_CINEXT PICK UP NEXT CI IN CHAIN                     
*                                                                               
         CL    R1,SIT2STT          VALID PART2 WITHIN RANGE?                    
         BL    BADNXT              NO, CHOP IT OFF                              
         ST    R1,FIWCIA                                                        
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R1,FIWNDA                                                        
         S     R1,FIWSHA           DISPLACEMENT FROM BEGINNING OF TABLE         
         ST    R1,SI2NXT           DIPLACEMENT TO NEXT PART2                    
*                                                                               
         MVC   GOODCI,THISCI       CI IS ALL GOOD                               
         B     AF040                                                            
*                                                                               
AF080    MVC   FIWNDA,ACURPAR1                                                  
         BRAS  RE,FIRITN           INSERT REPORT INTO THE TREE                  
*                                                                               
AFX      MVC   FIWCIA,FIRSTCI                                                   
         B     EXITOK                                                           
                                                                                
*----------------------------------------------------------------------         
* BAD FILE INFORMATION - ATTEMPT TO REPAIR                                      
*----------------------------------------------------------------------         
BADNDX   MVC   PLMSG,=CL20'*INDEX MISMATCH*'                                    
         B     BAD010                                                           
BADFNO   MVC   PLMSG,=CL20'*FILE # MISMATCH*'                                   
         B     BAD010                                                           
BADPRE   MVC   PLMSG,=CL20'*BAD PREV CI*'                                       
         B     BAD010                                                           
BADNXT   MVC   PLMSG,=CL20'*BAD NEXT CI*'                                       
         B     BAD020                                                           
*                                                                               
BAD010   MVC   APINDEX,W_INDEX     BAD INDEX                                    
         BRAS  RE,PRINTNDX         PRINT BAD INDEX                              
*                                                                               
         MVC   FIWCIA,GOODCI       READ LAST GOOD CI                            
         BRAS  RE,READCI                                                        
*                                                                               
BAD020   MVC   APINDEX,W_INDEX     INDEX OF LAST GOOD CI                        
         XC    W_CINEXT,W_CINEXT   CHOP IT OFF                                  
*                                                                               
         BRAS  RE,FIRCN            CONVERT A(CI) TO A(NODE)                     
         L     R4,FIWNDA                                                        
*                                                                               
         BRAS  RE,FIRC1            IS THIS A PART1                              
         BNE   BAD030              NO                                           
         MVI   W_SEQ,0             SET SEQUENCE TO INDICATE NO PART2S           
         USING SI1PARD,R4                                                       
         XC    SI1NXT,SI1NXT       REMOVE LINK IN INDEX                         
         MVI   SI1NDX+(W_SEQ-W_INDEX),0   SET SEQUENCE NUMBER IN INDEX          
         B     BAD040                                                           
*                                                                               
         USING SI2PARD,R4                                                       
BAD030   XC    SI2NXT,SI2NXT       REMOVE LINK IN INDEX                         
*                                                                               
BAD040   BRAS  RE,WRITECI          WRITE BACK THE FIX                           
*                                                                               
         BRAS  RE,PRINTNDX         PRINT LAST GOOD INDEX                        
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* REPORT FORMAT ERROR'                       
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         B     AF080                                                            
         DROP  R2,R4                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHAIN AVAILABLE PART1S                                                        
***********************************************************************         
CHP1AV   NTR1  ,                                                                
*                                                                               
         XC    CNTP1AV,CNTP1AV                                                  
         XC    CNTP1VU,CNTP1VU                                                  
*                                                                               
         L     R2,FIWP1A           LINK THE AVAILABLE PART1S                    
         L     R4,SIT1CIC          TOTAL NUMBER OF PART1S                       
         LR    R1,R4                                                            
         AHI   R1,-1                                                            
         MHI   R1,L'SI1PAR                                                      
         AR    R2,R1               POINT TO LAST PART1 FOR RESOURCE             
*                                                                               
         USING SI1PARD,R2                                                       
C1010    ST    R2,ACURPAR1         CURRENT PART1                                
         ST    R4,SI1NUM                                                        
*                                                                               
         LA    R5,SI1NDX                                                        
         USING W_RECD,R5                                                        
         CLI   W_STAT,0            ADD EMPTY/PURGED ENTRIES                     
         BE    C1030                                                            
*                                                                               
         TM    W_STAT,W_STSE       LOOK FOR VULNERABLE REPORTS - SENT?          
         BZ    C1050               . NO                                         
         TM    W_STAT,W_STKE       SENT, BUT IS IT ON KEEP?                     
         BO    C1050               . IT IS, THEN NOT VULNERABLE                 
*                                                                               
         L     R0,CNTP1VU          COUNT VULNERABLE PART1                       
         AHI   R0,1                                                             
         ST    R0,CNTP1VU                                                       
         B     C1050                                                            
         DROP  R5                                                               
*                                                                               
C1030    L     R0,CNTP1AV          COUNT TOTAL AVAILABLE PART1S                 
         AHI   R0,1                                                             
         ST    R0,CNTP1AV                                                       
*                                                                               
         LR    R3,R2               CURRENT PART1                                
         SLR   R3,R8               SUBTRACT START OF SHARED MEMORY              
*                                                                               
         OC    SITP1HD,SITP1HD     DO WE HAVE A NEXT AVAILABLE?                 
         BNZ   C1040               . YES                                        
         ST    R3,SITP1HD          MAKE THIS FIRST AVAILABLE                    
         ST    R3,SITP1TL          AND LAST AVAILABLE                           
         B     C1050                                                            
C1040    MVC   SI1NAV,SITP1HD      LINK TO NEXT AVAILABLE                       
         ST    R3,SITP1HD          MAKE THIS ONE NEXT AVAILABLE                 
         B     C1050                                                            
*                                                                               
C1050    AHI   R2,-(L'SI1PAR)      BUMP BACK                                    
         BCT   R4,C1010                                                         
*                                                                               
         MVC   SITP1AV,CNTP1AV     STORE NUMBER OF AVAILABLE PART1S             
         MVC   SITP1VU,CNTP1VU     AND NUMBER OF VULNERABLE PART1S              
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHAIN AVAILABLE PART2S                                                        
***********************************************************************         
CHP2AV   NTR1  ,                                                                
*                                                                               
         XC    CNTP2AV,CNTP2AV                                                  
         XC    CNTP2VU,CNTP2VU                                                  
*                                                                               
         L     R2,FIWP2A           LINK THE AVAILABLE PART2S                    
         L     R4,SIT2CIC          TOTAL NUMBER OF PART2S                       
         LR    R1,R4                                                            
         AHI   R1,-1                                                            
         MHI   R1,L'SI2PAR                                                      
         AR    R2,R1               POINT TO LAST PART2 FOR RESOURCE             
*                                                                               
         USING SI2PARD,R2                                                       
C2010    ST    R2,ACURPAR2         CURRENT PART2                                
         LR    R0,R4                                                            
         A     R0,SIT1CIC                                                       
         ST    R0,SI2NUM           STORE SLOT NUMBER                            
*                                                                               
         OC    SI2PRV,SI2PRV       IS THIS PART2 AVAILABLE?                     
         BZ    C2030               . YES                                        
*                                                                               
         LA    R5,SI2NDX                                                        
         USING W_RECD,R5                                                        
*                                                                               
         TM    W_STAT,W_STSE       LOOK FOR VULNERABLE REPORTS - SENT?          
         BZ    C2050               . NO                                         
         TM    W_STAT,W_STKE       SENT, BUT IS IT ON KEEP?                     
         BO    C2050               . IT IS, THEN NOT VULNERABLE                 
*                                                                               
         L     R0,CNTP2VU          COUNT VULNERABLE PART2                       
         AHI   R0,1                                                             
         ST    R0,CNTP2VU                                                       
         B     C2050                                                            
         DROP  R5                                                               
*                                                                               
C2030    L     R0,CNTP2AV          COUNT TOTAL AVAILABLE PART2S                 
         AHI   R0,1                                                             
         ST    R0,CNTP2AV                                                       
*                                                                               
         LR    R3,R2               CURRENT PART2                                
         SLR   R3,R8               SUBTRACT START OF SHARED MEMORY              
*                                                                               
         OC    SITP2HD,SITP2HD     DO WE HAVE A NEXT AVAILABLE?                 
         BNZ   C2040               . YES                                        
         ST    R3,SITP2HD          MAKE THIS FIRST AVAILABLE                    
         ST    R3,SITP2TL          AND LAST AVAILABLE                           
         B     C2050                                                            
C2040    MVC   SI2NAV,SITP2HD      LINK TO NEXT AVAILABLE                       
         ST    R3,SITP2HD          MAKE THIS ONE NEXT AVAILABLE                 
*                                                                               
C2050    AHI   R2,-(L'SI2PAR)      BUMP BACK                                    
         BCT   R4,C2010                                                         
*                                                                               
         MVC   SITP2AV,CNTP2AV     STORE NUMBER OF AVAILABLE PART2S             
         MVC   SITP2VU,CNTP2VU     AND NUMBER OF VULNERABLE PART2S              
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* READ A CONTROL INTERVAL                                                       
***********************************************************************         
READCI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM24                                                                  
         LA    RF,FIWCIA                                                        
         GOTO1 VDATAMGR,DMCB,DMREAD,FIWRES,(RF),ACIREC                          
         CLI   DMCB+8,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* UNABLE TO READ CI '                        
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* WRITE A CONTROL INTERVAL                                                      
***********************************************************************         
WRITECI  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM24                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   WCIERR                                                           
*                                                                               
         LA    RF,FIWCIA                                                        
         GOTO1 VDATAMGR,DMCB,DMWRT,FIWRES,(RF),ACIREC                           
         CLI   DMCB+8,0                                                         
         BE    EXITOK                                                           
*                                                                               
WCIERR   LA    R3,MSGW                                                          
         MVC   MSGWMSG,=CL40'*ERROR* UNABLE TO FIX CI '                         
         MVC   MSGWDET(5),=C'FILE='                                             
         MVC   MSGWDET+5(L'FIWRES),FIWRES                                       
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         DC    H'0'                                                             
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT TOTALS                                                                  
***********************************************************************         
PRINTTOT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R8,FIWSHA           R8= A(START OF SHARED MEMORY)                
         LA    R9,L'SIHDR(,R8)     START OF RESOURCES                           
*                                                                               
         L     R4,SIHNOFR          NUMBER OF RESOURCES                          
*                                                                               
         CLC   BUILD,=CL8'INIT'                                                 
         BNE   PT010                                                            
         MVI   PIPART,C'-'                                                      
         MVC   PIPART+1(PIEND-PIPART-1),PIPART                                  
         BRAS  RE,PRINTL                                                        
         MVC   PTDESC,=CL28'TOTAL NUMBER OF RESOURCES='                         
         EDIT  (R4),PTCOUNT                                                     
         BRAS  RE,PRINTL                                                        
         MVI   PIPART,C'='                                                      
         MVC   PIPART+1(PIEND-PIPART-1),PIPART                                  
         BRAS  RE,PRINTL                                                        
*                                                                               
PT010    CLC   BUILD,=CL8'INIT'                                                 
         BE    PT020                                                            
         TM    SITRIND,SITRBLD     WAS THIS JUST BUILT                          
         BZ    PT100                  . NO, DON'T PRINT                         
         NI    SITRIND,X'FF'-SITRBLD  . YES, TURN OFF BIT AND CONTINUE          
*                                                                               
PT020    MVC   PRRESO(8),=C'RESOURCE'                                           
         LLC   R2,SITNUM                                                        
         EDIT  (R2),PRNUM                                                       
         MVC   PRNAME,SITFIL                                                    
         MVC   PRDESC(16),=C'INDEX TABLE SIZE'                                  
         L     R2,SITSIZE                                                       
         EDIT  (R2),(8,PRVALU),ZERO=NOBLANK                                     
         BRAS  RE,PRINTL                                                        
         MVI   PIPART,C'-'                                                      
         MVC   PIPART+1(PIEND-PIPART-1),PIPART                                  
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PICIS,=CL12'# OF CIS'                                            
         MVC   PIAV,=CL12'AVAILABLE'                                            
         MVC   PIPAV,=CL12'% AVAILABLE'                                         
         MVC   PIVU,=CL12'VULNERABLE'                                           
         MVC   PIPVU,=CL12'% VULNERABLE'                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PICIS,=CL20'------------'                                        
         MVC   PIAV,=CL20'------------'                                         
         MVC   PIPAV,=CL20'------------'                                        
         MVC   PIVU,=CL20'------------'                                         
         MVC   PIPVU,=CL20'------------'                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVC   PIPART(5),=C'PART1'                                              
*                                                                               
         L     R2,SIT1CIC                                                       
         EDIT  (R2),(8,PICIS),ZERO=NOBLANK                                      
         L     R2,SITP1AV                                                       
         EDIT  (R2),(8,PIAV),ZERO=NOBLANK                                       
         L     R2,SITP1VU                                                       
         EDIT  (R2),(8,PIVU),ZERO=NOBLANK                                       
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP1AV          NUMBER OF AVAILABLE PART1S                   
         MHI   R1,10000                                                         
         D     R0,SIT1CIC          NUMBER OF PART1S FOR THIS RESOURCE           
         ST    R1,FULL             PART1 % AVAILABLE                            
         EDIT  (B4,FULL),(8,PIPAV),2,ZERO=NOBLANK,TRAIL=C'%'                    
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP1VU          NUMBER OF VULNERABLE PART1S                  
         MHI   R1,10000                                                         
         D     R0,SIT1CIC          NUMBER OF PART1S FOR THIS RESOURCE           
         ST    R1,FULL             PART1 % VULNERABLE                           
         EDIT  (B4,FULL),(8,PIPVU),2,ZERO=NOBLANK,TRAIL=C'%'                    
*                                                                               
         BRAS  RE,PRINTL                                                        
*                                                 PAPART1                       
         MVC   PIPART(5),=C'PART2'                                              
*                                                                               
         L     R2,SIT2CIC                                                       
         EDIT  (R2),(8,PICIS),ZERO=NOBLANK                                      
         L     R2,SITP2AV                                                       
         EDIT  (R2),(8,PIAV),ZERO=NOBLANK                                       
         L     R2,SITP2VU                                                       
         EDIT  (R2),(8,PIVU),ZERO=NOBLANK                                       
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP2AV          NUMBER OF AVAILABLE PART1S                   
         MHI   R1,10000                                                         
         D     R0,SIT2CIC          NUMBER OF PART2S FOR THIS RESOURCE           
         ST    R1,FULL             PART1 % AVAILABLE                            
         EDIT  (B4,FULL),(8,PIPAV),2,ZERO=NOBLANK,TRAIL=C'%'                    
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP2VU          NUMBER OF VULNERABLE PART1S                  
         MHI   R1,10000                                                         
         D     R0,SIT2CIC          NUMBER OF PART2S FOR THIS RESOURCE           
         ST    R1,FULL             PART2 % VULNERABLE                           
         EDIT  (B4,FULL),(8,PIPVU),2,ZERO=NOBLANK,TRAIL=C'%'                    
         BRAS  RE,PRINTL                                                        
*                                                                               
         MVI   PIPART,C'='                                                      
         MVC   PIPART+1(PIEND-PIPART-1),PIPART                                  
         BRAS  RE,PRINTL                                                        
*                                                                               
PT100    LA    R9,L'SITAB(,R9)                                                  
         BCT   R4,PT010                                                         
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* LOCK RESOURCES BEING BUILT/REBUILT                                            
***********************************************************************         
LOCKFI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XR    R5,R5               INIT NUMBER OF RESOURCES                     
         LA    R4,FILES            FILES DEFINED IN PARAMETERS                  
LF040    CLI   0(R4),C' '          END OF LIST                                  
         BE    LF050                                                            
*                                                                               
         MVC   FIWRES,0(R4)        RESOURCE NAME                                
         BRAS  RE,FIRFLOCK         LOCK THE RESOURCE                            
*                                                                               
         LA    R4,L'FILES(,R4)                                                  
         AHI   R5,1                BUMP RESOURCE NUMBER                         
         CHI   R5,MAXFILES                                                      
         BNH   LF040               MAY NEED TO INCREASE TABLE FOR MORE          
         WTO   '**ERROR** TOO MANY FILE PARAMETERS ',MCSFLAG=HRDCPY             
         DC    H'0'                                                             
*                                                                               
LF050    LTR   R5,R5               ANY RESOURCES DEFINED IN PARAMETERS?         
         BNZ   EXITOK                                                           
         WTO   '**ERROR** NO FILE PARAMETERS ',MCSFLAG=HRDCPY                   
         DC    H'0'                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* UNLOCK RESOURCES JUST BUILT/REBUILT                                           
***********************************************************************         
UNLKFI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,FILES            FILES DEFINED IN PARAMETERS                  
UF010    CLI   0(R4),C' '          END OF LIST                                  
         BE    EXITOK                                                           
*                                                                               
         MVC   FIWRES,0(R4)        RESOURCE NAME                                
         BRAS  RE,FIRFUNLK         UNLOCK THE RESOURCE                          
*                                                                               
         LA    R4,L'FILES(,R4)                                                  
         B     UF010                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* RESOURCE SCAN TO FREE UP REPORT ENTRIES                                       
***********************************************************************         
RSCAN    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,PRINTL                                                        
         MVI   PMMARK,C'*'                                                      
         MVC   PMMSG,=CL12'SCANNING'                                            
         MVI   PMLP,C'('                                                        
         EDIT  (TIME,NOW),(8,PMTIME)                                            
         MVC   PMDAYA,DAYNOWA                                                   
         MVC   PMDATEA,TODAYA                                                   
         MVI   PMRP,C')'                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         SAM31 ,                                                                
         L     R8,FIWSHA           A(SHARED MEMORY FILE INDEX HEADER)           
         LA    R9,L'SIHDR(,R8)     A(FIRST RESOURCE TABLE HEADER)               
*                                                                               
         ICM   R5,15,SIHNOFR       PICK UP NUMBER OF RESOURCES                  
         BZ    RSX                 . NONE                                       
*                                                                               
RS010    MVC   FIWRES,SITFIL       RESOURCE NAME                                
         BRAS  RE,FIRSET           SET FIW ADDRESSES                            
*                                                                               
         CLC   =C'NO',FREE2        ANY SPECIFIC REQUEST FOR A PART2 CI?         
         BE    RS020               . NO                                         
         CLC   =C'YES',FREE2       FILE FOR CI REQUEST SET IN MEMORY?           
         BE    RS015               . NO                                         
         CLC   FIWRES,FREE2        THIS FILE REQUESTED A PART2 CI?              
         BE    RS050               . YES, SCAN                                  
         B     RS060                                                            
*                                                                               
RS015    TM    SITRIND,SITRP2S     PART2 SCAN NEEDED?                           
         BZ    RS020                                                            
         NI    SITRIND,X'FF'-SITRP1S-SITRP2S                                    
         B     RS050                                                            
*                                                                               
RS020    CLC   =C'NO',FREE1        ANY SPECIFIC REQUEST FOR A PART1 CI?         
         BE    RS050               . NO, GENERAL TIMER SCAN                     
         CLC   =C'YES',FREE1       FILE FOR CI REQUEST SET IN MEMORY?           
         BE    RS025               . NO                                         
         CLC   FIWRES,FREE1        THIS FILE REQUESTED A PART1 CI?              
         BE    RS050               . NO, SKIP                                   
         B     RS060                                                            
*                                                                               
RS025    TM    SITRIND,SITRP1S     PART1 SCAN NEEDED?                           
         BZ    RS060                                                            
         NI    SITRIND,X'FF'-SITRP1S-SITRP2S                                    
*                                                                               
RS050    BRAS  RE,GOSCAN           SCAN RESOURCE                                
*                                                                               
RS060    LA    R9,L'SITAB(,R9)     NEXT RESOURCE                                
         BCT   R5,RS010                                                         
*                                                                               
RSX      MVC   FREE1,=CL8'NO'      RESET AFTER SCAN                             
         MVC   FREE2,=CL8'NO'                                                   
*                                                                               
         MVI   PMMARK,C'*'                                                      
         MVC   PMMSG,=CL12'END OF SCAN'                                         
         MVI   PMLP,C'('                                                        
         EDIT  (TIME,NOW),(8,PMTIME)                                            
         MVC   PMDAYA,DAYNOWA                                                   
         MVC   PMDATEA,TODAYA                                                   
         MVI   PMRP,C')'                                                        
         BRAS  RE,PRINTL                                                        
*                                                                               
         J     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* POPULATE AVAILABLE QUEUE FOR THE GIVEN RESOURCE                               
***********************************************************************         
GOSCAN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   HOWPOP,C'A'         DID WE GET HERE VIA AN ALERT?                
         BNE   GS010                                                            
*                                                                               
         CLC   =C'NO',FREE1        DO WE NEED TO FREE A CI?                     
         BE    *+8                 . NO                                         
         BRAS  RE,FIR1LOCK         . YES, LOCK THE PART1 QUEUE                  
*                                                                               
         CLC   =C'NO',FREE2        DO WE NEED TO FREE A CII?                    
         BE    *+8                 . NO                                         
         BRAS  RE,FIR2LOCK         . YES, LOCK THE PART2 QUEUE                  
*                                                                               
GS010    XC    CNTP1VU,CNTP1VU     INIT PART1 VULNERABLE COUNT                  
         XC    CNTP2VU,CNTP2VU     INIT PART2 VULNERABLE COUNT                  
         MVI   SINDI,0             SCAN INDICATOR                               
*                                                                               
         L     R5,ARVTAB           INIT VULNERABLE REPORT TABLE                 
         XC    0(2,R5),0(R5)                                                    
         LA    R5,2(,R5)                                                        
         USING RVTABD,R5                                                        
         XC    0(RVTABL,R5),0(R5)                                               
*                                                                               
         BRAS  RE,PACALC           CALCULATE % AVAILABLE                        
         BRAS  RE,PAPRINT          PRINT % AVAILABLE                            
*                                                                               
         SAM24 ,                                                                
         GOTO1 VDATAMGR,DMCB,BUFFER,FIWRES,UKINDEX,ACTREC,ACIREC                
         BE    *+6                                                              
         DC    H'0'                                                             
         SAM31 ,                                                                
*                                                                               
         L     R2,FIWP1A           POINT TO PART1S                              
         L     R4,SIT1CIC          TOTAL NUMBER OF PART1S                       
                                                                                
*----------------------------------------------------------------------         
* SCAN THROUGH THE INDEX                                                        
*----------------------------------------------------------------------         
         USING SI1PARD,R2                                                       
         USING W_RECD,R3                                                        
GS020    ST    R2,FIWNDA           CURRENT PART1                                
         LA    R3,SI1NDX           R3= A(INDEX)                                 
         BRAS  RE,FIRNC            CONVERT A(INDEX NODE) TO A(CI) REF#          
*                                                                               
         LR    R1,R2                                                            
         S     R1,FIWSHA           DISPL TO CURRENT PART1 NODE                  
         C     R1,SITP1HD          IS THIS NODE NEXT AVAILABLE?                 
         BE    GS200               . YES, LEAVE IT                              
         OC    SI1NAV,SI1NAV       IS IT ALREADY AVAILABLE?                     
         BNZ   GS200               . YES, LEAVE IT                              
         CLI   W_STAT,W_STPU       ALREADY PURGED?                              
         BE    GS200               . YES, SKIP IT                               
*                                                                               
         CLC   W_AGERD,TODAY       TEST RETAIN DATE WITH TODAY                  
         BL    GS030               . EXPIRED, PURGE THIS ONE                    
         BH    GS040               . NOT YET, KEEP IT                           
         CLI   W_AGERT,X'FE'       TEST TIME AVAILABLE                          
         BE    GS040               . NO, DON'T TOUCH IT                         
         CLC   W_AGERT,TIMENOWM    TEST RETAIN TIME VALUE                       
         BNL   GS040               . NOT TIME YET                               
         B     GS030                                                            
                                                                                
*----------------------------------------------------------------------         
* EXPIRED REPORTS                                                               
*----------------------------------------------------------------------         
GS030    TM    W_STAT,W_STKE       ON KEEP?                                     
         BZ    GS032               . NO                                         
         CLC   W_AGERD,TWOWEEKS    DID IT EXPIRE OVER TWO WEEKS AGO?            
         BL    GS150               . YES, THEN GET RID OF IT ALREADY            
         B     GS100               . ELSE VULNERABLE                            
*                                                                               
GS032    TM    W_STAT,W_STRUN      RUNNING - CREATION / SENDING                 
         BZ    GS150               . NO, PURGE                                  
         TM    W_STAT,W_STCRE      CREATION IN PROCESS                          
         BZ    GS034               . NO                                         
         CLC   W_AGELD,YESTRDAY    DID THIS START BEFORE YESTERDAY              
         BL    GS150               . YES, IT MUST BE BAD NOW, PURGE IT          
         B     GS200               . NO, LEAVE IT BE                            
GS034    TM    W_STAT,W_STSEN      IS IT SENDING                                
         BZ    GS200               . NO                                         
         CLC   W_AGERD,YESTRDAY    DID IT EXPIRE BEFORE YESTERDAY               
         BL    GS150               . YES, WHAT'S TAKING SO LONG, PURGE          
         B     GS200               KEEP IT, GET NEXT                            
                                                                                
*----------------------------------------------------------------------         
* NON-EXPIRED REPORTS                                                           
*----------------------------------------------------------------------         
GS040    TM    W_STAT,W_STDEAD     PRINTED / SENT / DEAD?                       
         BNZ   GS050               . YES, THEN VULNERABLE                       
*                                                                               
         TM    W_STAT,W_STRUN      RUNNING - CREATION / SENDING                 
         BZ    GS200               . NO, GET NEXT                               
         TM    W_STAT,W_STCRE      CREATION IN PROCESS                          
         BZ    GS200               . NO                                         
         CLC   W_AGELD,YESTRDAY    WAS IT CREATED AT LEAST A DAY AGO?           
         BL    GS150               . YES, IT MUST BE BAD NOW, PURGE IT          
         B     GS200               GET NEXT REPORT                              
*                                                                               
GS050    CLC   W_AGELD,TODAY       WAS IT CREATED BEFORE TODAY                  
         BL    GS100               . THEN WE CAN PURGE THIS VULNERABLE          
         BH    GS200               . CREATED IN THE FUTURE???? SURE...          
         CLC   W_AGELT,TIMEOLDC    WAS IT CREATED BEFORE THE LAST SCAN          
         BNL   GS200               . IF NOT, THEN DON'T PURGE IT                
                                                                                
*----------------------------------------------------------------------         
* VULNERABLE REPORTS - SAVE FOR LATER                                           
*----------------------------------------------------------------------         
GS100    XC    RVTABD(RVTABL),RVTABD                                            
*                                                                               
         CLI   W_SEQ,0             ANY PART2S?                                  
         BE    GS130                                                            
         ICM   R6,15,SI1NXT        ANY PART2S?                                  
         BZ    GS130                                                            
         XC    HALF,HALF                                                        
*                                                                               
         USING SI2PARD,R6                                                       
GS110    A     R6,FIWSHA           GO TO LAST PART2 AND GET SEQUENCE #          
         ICM   R1,15,SI2NXT                                                     
         BZ    GS120                                                            
         LR    R6,R1                                                            
         B     GS110                                                            
*                                                                               
GS120    LLC   R1,SI2NDX+(W_SEQ-W_INDEX)                                        
         TM    W_ATTB,W_ATXTN      DO WE HAVE EXTENSION CIS                     
         BZ    *+8                                                              
         AHI   R1,255              . YES, THEN 255 REGULAR PART2S               
         AHI   R1,-1               DECREMENT COUNT FOR THE PART1                
         STCM  R1,3,HALF           # OF PART2 CIS                               
         A     R1,CNTP2VU          UPDATE VULNERABLE COUNT                      
         ST    R1,CNTP2VU                                                       
         DROP  R6                                                               
*                                                                               
GS130    L     R0,CNTP1VU          COUNT SCANNED VULNERABLE PART1S              
         AHI   R0,1                                                             
         ST    R0,CNTP1VU                                                       
*                                                                               
         CHI   R0,RVMAX            MORE THAN WE CAN HANDLE?                     
         BH    GS200               . DON'T ADD IT TO THE TABLE                  
*                                                                               
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         ICM   R0,3,W_AGELD                                                     
         ICM   R1,3,TODAY          LIVE DATE MINUS TODAY                        
         SR    R0,R1               WILL GIVE ME SOME NUMBER                     
         AHI   R0,1                ADD ONE TO THAT TO AVOID ZERO                
         STCM  R0,3,RVONE          WHATEVER THAT IS, SAVE IT                    
         MVC   RVTWO,HALF          SAVE # OF PART2 CIS                          
*                                                                               
         CLC   =C'NO',FREE2        DO WE ABSOLUTELY NEED PART2S?                
         BE    GS140               . NO                                         
         MVC   RVONE,HALF          . YES, SORT WITH SIZE FIRST                  
         STCM  R0,3,RVTWO                                                       
*                                                                               
GS140    MVC   RVNDX,W_INDEX       FILE INDEX                                   
*                                                                               
         L     RF,ARVTAB           INCREMENT VULNERABLE TABLE COUNT             
         XR    R1,R1                                                            
         ICM   R1,3,0(RF)                                                       
         AHI   R1,1                                                             
         STCM  R1,3,0(RF)                                                       
*                                                                               
         LA    R5,RVTABL(,R5)                                                   
         B     GS200               NEXT                                         
                                                                                
*----------------------------------------------------------------------         
* PURGE THE REPORT                                                              
*----------------------------------------------------------------------         
GS150    BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         MVC   SVREF,FIWREF        SAVE THE REFERENCE FOR THE UNLOCK            
*                                                                               
         MVC   SVINDEX,W_INDEX                                                  
         MVC   UKINDEX,W_INDEX                                                  
         OI    UKFLAG,UKFLDAT      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 VDATAMGR,DMCB,INDEX,FIWRES,UKINDEX,ACTREC,ACIREC                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SAM31                                                                  
*                                                                               
         CLC   UKINDEX,SVINDEX     MAKE SURE IT'S THE SAME REPORT               
         BE    GS190               . NO, LEAVE IT ALONE                         
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER AND UNLOCK          
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     GS200                                                            
*                                                                               
GS190    CLI   RCWRITE,C'Y'                                                     
         BNE   GS195                                                            
*                                                                               
         SAM24                                                                  
         GOTO1 VDATAMGR,DMCB,PURGE,FIWRES,UKINDEX,ACTREC,ACIREC                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SAM31                                                                  
*                                  PURGE WILL UNLOCK REPORT WHEN DONE           
GS195    OI    SINDI,SIPURGE                                                    
         MVC   PLMSG,=CL20'*REPORT PURGE*'                                      
         BRAS  RE,PRINTNDX                                                      
*                                                                               
GS200    LA    R2,L'SI1PAR(,R2)    GET NEXT INDEX                               
         BCT   R4,GS020                                                         
                                                                                
*----------------------------------------------------------------------         
* FINISHED SCAN                                                                 
*----------------------------------------------------------------------         
         BRAS  RE,PUVU             PURGE VULNERABLES IF NECESSARY               
*                                                                               
         MVC   SITP1VU,CNTP1VU     UPDATE VULNERABLE COUNTS                     
         MVC   SITP2VU,CNTP2VU      IN SHARED MEMORY                            
*                                                                               
         TM    SINDI,SIPURGE       WAS ANYTHING PURGED?                         
         BZ    GSX                 . NO,DON'T PRINT AGAIN                       
         BRAS  RE,PACALC           CALCULATE % AVAILABLE                        
         BRAS  RE,PAPRINT          PRINT % AVAILABLE                            
*                                                                               
GSX      B     EXITOK                                                           
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* PURGE VULNERABLE REPORTS IF NECESSARY                                         
***********************************************************************         
PUVU     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R5,ARVTAB           VULNERABLE REPORT TABLE                      
         XR    R4,R4                                                            
         ICM   R4,3,0(R5)          ANY VULNERABLES FOR THIS RESOURCE?           
         BZ    PVX                 . NO                                         
*                                                                               
         LA    R5,2(,R5)                                                        
         USING RVTABD,R5                                                        
*                                  SORT IN DESCENDING ORDER                     
         SAM24                                                                  
         GOTO1 =V(QSORT),DMCB,(1,(R5)),(R4),RVTABL,5,0,0                        
         SAM31                                                                  
*                                                                               
PV010    BRAS  RE,PACALC           CALCULATE % AVAILABLE                        
*                                                                               
         L     R1,PAPART1          # OF AVAILABLE PART1S                        
         CHI   R1,500              LESS THEN 5.00% AVAILABLE?                   
         BL    PV020               . YES, PURGE VULNERABLE                      
         L     R1,PAPART2          # OF AVAILABLE PART2S                        
         CHI   R1,1500             LESS THEN 15.00% AVAILABLE?                  
         BL    PV020               . YES, PURGE VULNERABLE                      
         B     PVX                                                              
                                                                                
*----------------------------------------------------------------------         
* PURGE THE REPORT                                                              
*----------------------------------------------------------------------         
PV020    XC    FIWREF,FIWREF                                                    
         MVC   FIWREF+2(2),RVNDX+(W_FILENO-W_INDEX)                             
         OC    FIWREF,FIWREF                                                    
         BZ    PV050                                                            
*                                                                               
         BRAS  RE,FIRRLOCK         LOCK THE REPORT                              
         MVC   SVREF,FIWREF        SAVE THE REFERENCE NUMBER FOR UNLOCK         
*                                                                               
         MVC   UKINDEX,RVNDX                                                    
         OI    UKFLAG,UKFLDAT      PASSING REF#                                 
*                                                                               
         SAM24                                                                  
         GOTO1 VDATAMGR,DMCB,INDEX,FIWRES,UKINDEX,ACTREC,ACIREC                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SAM31                                                                  
*                                                                               
         CLC   RVNDX,UKINDEX       MAKE SURE SAME INDEX                         
         BE    PV040               . YES, PURGE                                 
         MVC   FIWREF,SVREF        RESTORE REFERENCE NUMBER                     
         BRAS  RE,FIRRUNLK         UNLOCK THE REPORT                            
         B     PV050                                                            
*                                                                               
PV040    CLI   RCWRITE,C'Y'                                                     
         BNE   PV045                                                            
*                                                                               
         SAM24                                                                  
         GOTO1 VDATAMGR,DMCB,PURGE,FIWRES,UKINDEX,ACTREC,ACIREC                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SAM31                                                                  
*                                  PURGE WILL UNLOCK REPORT WHEN DONE           
PV045    L     R0,CNTP1VU          DECREMENT VULNERABLE COUNT                   
         AHI   R0,-1                                                            
         ST    R0,CNTP1VU                                                       
*                                                                               
         L     R0,CNTP2VU          REDUCE PART2 VULNERABLE COUNT                
         XR    R1,R1                                                            
         ICM   R1,3,RVTWO                                                       
         CLC   =C'NO',FREE2        DO WE ABSOLUTELY NEED PART2S?                
         BE    *+8                 . NO                                         
         ICM   R1,3,RVONE                                                       
         SR    R0,R1                                                            
         ST    R0,CNTP2VU                                                       
*                                                                               
         OI    SINDI,SIPURGE                                                    
         MVC   PLMSG,=CL20'*VULNERABLE PURGE*'                                  
         BRAS  RE,PRINTNDX                                                      
*                                                                               
PV050    LA    R5,RVTABL(,R5)      GET NEXT INDEX FROM VULNERABLE TABLE         
         BCT   R4,PV010                                                         
*                                                                               
PVX      J     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CALCULATE PERCENT AVAILABLE                                                   
***********************************************************************         
PACALC   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP1AV          NUMBER OF AVAILABLE PART1S                   
         MHI   R1,10000                                                         
         D     R0,SIT1CIC          NUMBER OF PART2S FOR THIS RESOURCE           
         ST    R1,PAPART1          PART1 % AVAILABLE                            
*                                                                               
         XR    R0,R0                                                            
         L     R1,SITP2AV          NUMBER OF AVAILABLE PART2S                   
         MHI   R1,10000                                                         
         D     R0,SIT2CIC          NUMBER OF PART2S FOR THIS RESOURCE           
         ST    R1,PAPART2          PART2 % AVAILABLE                            
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT PERCENT AVAILABLE                                                       
***********************************************************************         
PAPRINT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    SINDI,SIPURGE                                                    
         BO    *+8                                                              
         MVI   PLINE+3,C'-'                                                     
*                                                                               
         MVC   PLINE+5(L'FIWRES),FIWRES                                         
         MVC   PLINE+13(12),=CL12'PART1 AVAIL='                                 
         EDIT  (B4,PAPART1),(8,PLINE+26),2,TRAIL=C'%'                           
         MVC   PLINE+38(12),=CL12'PART2 AVAIL='                                 
         EDIT  (B4,PAPART2),(8,PLINE+51),2,TRAIL=C'%'                           
         BRAS  RE,PRINTL                                                        
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT THE INDEX                                                               
***********************************************************************         
PRINTNDX NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SAM24                                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CTIREC,R2                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,UKUSRID                                                  
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,IOKEY,AIOAREA                        
         BNE   PNDX020                                                          
*                                                                               
         L     R2,AIOAREA                                                       
         LA    R3,CTIDATA                                                       
PNDX010  CLI   0(R3),0                                                          
         BE    PNDX020                                                          
         CLI   0(R3),CTDSCELQ                                                   
         BE    PNDX012                                                          
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PNDX010                                                          
*                                                                               
         USING CTDSCD,R3                                                        
PNDX012  MVC   PLUSER,CTDSC                                                     
         B     PNDX022                                                          
         DROP  R2                                                               
         DROP  R3                                                               
                                                                                
PNDX020  GOTO1 VHEXOUT,DMCB,UKUSRID,PLUSER,L'UKUSRID,0                          
PNDX022  MVC   PLSYSP,UKSYSPRG                                                  
         GOTO1 VHEXOUT,DMCB,UKDAY,PLDAY,L'UKDAY,0                               
         MVC   PLCLASS,UKCLASS                                                  
         XC    FULL,FULL                                                        
         MVC   FULL+2(2),UKFILENO                                               
         EDIT  (B4,FULL),(5,PLREFNO)                                            
         GOTO1 VHEXOUT,DMCB,UKTYPE,PLTYPE,L'UKTYPE,0                            
         GOTO1 VHEXOUT,DMCB,UKATTB,PLATTB,L'UKATTB,0                            
         GOTO1 VHEXOUT,DMCB,UKSTAT,PLSTAT,L'UKSTAT,0                            
         EDIT  (B1,UKSEQ),(3,PLSEQ)                                             
         GOTO1 VDATCON,DMCB,(2,UKAGERD),(21,PLAGE)                              
*                                                                               
         XR    R0,R0               RETAIN TIME                                  
         LLC   R1,UKAGERD+L'UKAGERD                                             
         MHI   R1,10               CONVERT 10 MIN INCREMENTS                    
         D     R0,=F'60'                                                        
         STC   R1,DUB                                                           
         STC   R0,DUB+1                                                         
         BRAS  RE,TIMEOUT                                                       
         MVC   PLAGET,DUB+2                                                     
*                                                                               
         MVC   FULL,FIWCIA                                                      
         GOTO1 VHEXOUT,DMCB,FULL,PLCIA,L'FIWCIA,0                               
*                                                                               
         BRAS  RE,PRINTL                                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* FORMAT THE TIME                                                               
***********************************************************************         
TIMEOUT  XC    DUB+2(6),DUB+2      EXPAND BINARY TIME IN DUB(2)                 
         CLI   DUB,23                                                           
         BH    TIMEOUTX                                                         
         CLI   DUB+1,59                                                         
         BH    TIMEOUTX                                                         
*                                                                               
         XR    R0,R0                                                            
         IC    R0,DUB                                                           
         CVD   R0,DUBDUB                                                        
         OI    DUBDUB+7,X'0F'                                                   
         UNPK  DUB+2(2),DUBDUB+6(2)                                             
*                                                                               
         MVI   DUB+4,C':'                                                       
         XR    R0,R0                                                            
         IC    R0,DUB+1                                                         
         CVD   R0,DUBDUB                                                        
         OI    DUBDUB+7,X'0F'                                                   
         UNPK  DUB+5(2),DUBDUB+6(2)                                             
         MVI   DUB+7,C':'                                                       
         MVC   DUB+8(2),=C'00'                                                  
*                                                                               
TIMEOUTX BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* NOTIFICATION MESSAGE                                                          
***********************************************************************         
NOTYMSG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MSGWTXT,SPACES                                                   
*                                                                               
         MVC   MSGWTXT(9),=C'AUTONOTE*'                                         
         MVC   MSGWTXT+9(L'NOTIFY),NOTIFY                                       
         LA    R2,MSGWTXT+9+L'NOTIFY-1                                          
*                                                                               
NM010    CLI   0(R2),C' '                                                       
         BH    NM020                                                            
         BCT   R2,NM010                                                         
*                                                                               
NM020    MVI   1(R2),C':'                                                       
         MVC   2(11,R2),=CL11' *WARNING* '                                      
         MVC   2+11(12,R2),=CL12' JOB ALERTED'                                  
         LA    R2,2+11+12(,R2)                                                  
         LA    R1,MSGW                                                          
         SLR   R2,R1                                                            
         STCM  R2,3,MSGWLEN                                                     
         WTO   TEXT=MSGW                                                        
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* SHARED MEMORY FILE INDEX COMMON ROUTINES                                      
***********************************************************************         
         DROP  R8                                                               
         DROP  R9                                                               
*                                                                               
       ++INCLUDE SHFIR                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXITS AND COMMON STORAGE                                                      
***********************************************************************         
COMMON   DS    0D                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         J     EXIT                                                             
EXITL    CLI   *,255                                                            
         J     EXIT                                                             
EXITH    CHI   RB,0                                                             
         J     EXIT                                                             
EXIT     XIT1  ,                                                                
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT ROUTINES                                                                
***********************************************************************         
PRINTI   ST    RE,SAVERE                                                        
         MVC   PLINE,SPACES                                                     
         OPEN  (SYSPRINT,OUTPUT)   PRINT INIT                                   
         ZAP   LINE,=P'0'                                                       
         ZAP   PAGE,=P'1'                                                       
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
PRINTT   ST    RE,SAVERE           PRINT TITLES                                 
         ZAP   LINE,=P'0'          RESET LINECOUNT                              
         AP    PAGE,=P'1'          BUMP PAGECOUNT                               
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
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
         PUT   SYSPRINT,TITLE      PRINT TITLE                                  
*                                                                               
PRINTL2  PUT   SYSPRINT,PLINE      PRINT LINE                                   
         MVC   PLINE,SPACES                                                     
         L     RE,SAVERE                                                        
         BR    RE                  EXIT                                         
*                                                                               
PRINTX   ST    RE,SAVERE           CLOSE PRINT                                  
         CLOSE SYSPRINT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* PARAMETER CARDS AND HANDLING ROUTINE                                          
***********************************************************************         
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
* FLAGS  X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*        X'0002'                   ONLY VALID ON OPS MODIFY                     
*        X'0001'                   ONLY VALID ON START                          
*----------------------------------------------------------------------         
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,09),X'0001',AL3(DDSIO)                          
         DC    C'DSPACE ',AL1(5,00),X'0001',AL3(SSB+SSODSPAC-SSOOFF)            
         DC    C'MEMORY ',AL1(5,07),X'0001',AL3(MEMORY)                         
         DC    C'WRITE  ',AL1(4,03),X'0001',AL3(RCWRITE)                        
         DC    C'MODE   ',AL1(3,09),X'0001',AL3(MODE)                           
         DC    C'ESTAE  ',AL1(4,02),X'0001',AL3(USEESTAE)                       
         DC    C'FILE   ',AL1(3,07),X'0401',AL3(FILES)                          
         DC    C'POP    ',AL1(2,02),X'0800',AL3(POP)                            
         DC    C'SCAN   ',AL1(3,02),X'0800',AL3(TIMESCAN+2)                     
         DC    C'BUILD  ',AL1(4,07),X'0000',AL3(BUILD)                          
         DC    C'FREE1  ',AL1(4,07),X'0000',AL3(FREE1)                          
         DC    C'FREE2  ',AL1(4,07),X'0000',AL3(FREE2)                          
         DC    C'NOTIFY ',AL1(5,19),X'0000',AL3(NOTIFY)                         
         DC    X'0000'                                                          
*----------------------------------------------------------------------         
* CARD OUTPUT AREAS SET WITH DEFAULTS                                           
*----------------------------------------------------------------------         
DDSIO    DC    CL10'DDSIO'                                                      
MEMORY   DC    CL10' '           SHARED MEMORY NAME                             
MODE     DC    CL10'MONITOR'     PROGRAM RUN MODE                               
USEESTAE DC    CL3'NO '                                                         
RCWRITE  DC    CL3'YES'                                                         
FILES    DC    (MAXFILES)CL7' '  FILES TO BE INDEXED IN MEMORY                  
FILESX   DC    CL7' '            END OF FILES LIST                              
*                                                                               
POP      DC    H'5'              TIMER POP EVERY X MINUTES                      
*                                                                               
BUILD    DC    CL8'NO'           (YES/NO/FILE) DEFAULT CHECK FOR BUILD          
FREE1    DC    CL8'NO'           (YES/NO/FILE) DEFAULT RELY ON TIMER            
FREE2    DC    CL8'NO'           (YES/NO/FILE) DEFAULT RELY ON TIMER            
NOTIFY   DC    CL20'AWIL'                                                       
*                                                                               
MAXFILES EQU   16                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* TIMER TABLES  CL1'Y/N',SPARE,AL2(FREQ IN MINUTES),AL4(LAST TIME)              
***********************************************************************         
TIMETAB  DS    0F                                                               
TIMESCAN DC    CL1'N',AL1(0),AL2(15),AL4(0)                                     
TIMEEOT  DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
*        CONSTANTS & LTORG                                            *         
***********************************************************************         
SPACES   DC    CL166' '                                                         
STARS    DC    16C'*'                                                           
EFFS     DC    16X'FF'                                                          
ZEROS    DC    16X'00'                                                          
MAXLINE  DC    PL3'60'                                                          
*                                                                               
CONTROL  DC    CL8'CONTROL'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
ATTACH   DC    CL8'ATTACH'                                                      
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
BUFFER   DC    CL8'BUFFER'                                                      
FFINI    DC    CL8'FFINI'          FORCED FILE INITIALIZATION                   
INDEX    DC    CL8'INDEX'                                                       
PURGE    DC    CL8'PURGE'                                                       
*                                                                               
T1HOUR   DC    AL2(2700)           1 HOUR  IN (SECS*3)/4                        
T1205AM  DC    AL2(225)            12:05AM IN (SECS*3)/4                        
T1155PM  DC    AL2(64500)          11:55PM IN (SECS*3)/4                        
*                                                                               
EYESFI   DC    CL16'**SHFI****SHFI**'                                           
*                                                                               
FLIST    DC    CL8'NCTFILE'                                                     
         DC    CL8'X'                                                           
*                                                                               
ABENDSOK DC    AL2(20)             ABENDS ALLOWED BEFORE JOB STOPPED            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* DCBS & ADCONS                                                                 
***********************************************************************         
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
VDATAMGR DC    V(DATAMGR)                                                       
VHEXOUT  DC    V(HEXOUT)                                                        
VDATCON  DC    V(DATCON)                                                        
VGETDAY  DC    V(GETDAY)                                                        
VADDAY   DC    V(ADDAY)                                                         
VSHMUSS  DC    V(DMSHMUSS)                                                      
VISGENQ  DC    V(DMISGENQ)                                                      
VDYNALLO DC    V(DYNALLOC)                                                      
*                                                                               
ARVTAB   DC    A(RVTAB)                                                         
ACOMM    DC    A(0)                                                             
*                                                                               
UTL      DC    F'0',AL1(10),XL250'00'                                           
SSB      DC    H'0',X'FF',X'14',1022X'00'                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* EXTRACT RESULTS, ECBS, AND TIMER FIELDS                                       
***********************************************************************         
         DS    0D                                                               
         DC    CL8'EXTRACT '                                                    
RESULTS  DS    0X                                                               
RXTIOT   DC    A(0)                TIOT                                         
RXASID   DC    A(0)                ASID                                         
*                                                                               
RXSTOKEN DC    D'0'                STOKEN                                       
RXJOB    DC    D'0'                JOB NAME                                     
*                                                                               
         DS    0D                                                               
         DC    CL8'ECBLIST '                                                    
ECBLST   DS    0X                  ECB LIST                                     
AOPERECB DC    A(0)                . OPERATOR ISSUE COMMAND                     
ALERTECB DC    A(LERTECB)          . PROGRAM INITIATED ALERT                    
ATIMRECB DC    X'80',AL3(TIMRECB)  . TIMER                                      
*                                                                               
         DS    0D                                                               
STIMERID DS    XL4                                                              
TIME     DS    XL4                                                              
*                                                                               
LERTECB  DC    F'0'                PROGRAM INITIATED ALERT ECB                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* THE TIMER EXIT ROUTINE.  IT POSTS AN ECB.                                     
***********************************************************************         
TIMRXIT  SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TIMRXIT,RB                                                       
         POST  TIMRECB                                                          
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
TIMRECB  DC    F'0'                ECB OF TASK TIMER                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE DC                                                            
***********************************************************************         
         DC    0D                                                               
         DC    CL16'**RVTB****RVTB**'                                           
RVTAB    DC    XL2'00'             NUMBER OF ENTRIES                            
         DC    (RVMAX*RVTABL)X'00' VULNERABLE REPORT TABLE                      
*                                                                               
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR HANDLER                                                                 
***********************************************************************         
         DROP  RB                                                               
ERREXIT  DS    0D                                                               
         USING *,RB                                                             
         LR    RB,RF                                                            
         C     R0,=F'12'           TEST SDWA ACQUIRED                           
         BNE   *+8                 NO, JUST LET MVS PERC                        
         XR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
         STM   RE,RC,12(RD)        SAVE CALLING REGS                            
         ST    RD,ESTAERD          AND POINTER                                  
         ST    R1,ESTAER1          AND POINTER TO SDWA                          
*                                                                               
         MVC   MYSDWA,0(R1)                                                     
         LA    R2,MYSDWA           COPY START OF SDWA                           
         USING SDWA,R2                                                          
         L     RC,MYRC             RELOAD RC FROM SAVED COPY                    
         USING WORKD,RC                                                         
*                                                                               
         ICM   R1,15,SDWAGR13      FIND ABENDING RD                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
         C     R1,MYRD             MUST BE BEYOND MAIN                          
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,SDWAGR11      FIND ABENDING RB                             
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   22(8,R1),=C'**DMRC**'                                            
         BNE   *+8                                                              
         MVI   ENDTASK,C'Y'        END JOB IF WE DIE IN **DMRC**                
*                                                                               
         LH    RF,ABENDS                                                        
         AHI   RF,1                                                             
         STH   RF,ABENDS                                                        
         CLC   ABENDS,ABENDSOK                                                  
         BL    *+8                                                              
         MVI   ENDTASK,C'Y'        END JOB AFTER N ABENDS                       
*                                                                               
         XC    PLINE,PLINE                                                      
         MVC   PLINE(2),=X'0020'                                                
         MVC   PLINE+2(L'MESSAGE),MESSAGE                                       
         MVC   PLINE+14(8),22(R1)                                               
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,7,SDWANXTP                                                    
         S     RF,SDWAGR11                                                      
*                                                                               
         LA    R0,8                QUICK HEXOUT                                 
         XR    R5,R5                                                            
         XR    R4,R4                                                            
HEXOUT1  XR    RE,RE                                                            
         SLDL  RE,4                                                             
         IC    R5,HEXTAB(RE)                                                    
         SLDL  R4,8                                                             
         BCT   R0,HEXOUT1                                                       
         STCM  R4,15,PLINE+26                                                   
         STCM  R5,15,PLINE+30                                                   
*                                                                               
         WTO   TEXT=PLINE,MCSFLAG=HRDCPY                                        
         B     ERR000                                                           
*                                                                               
HEXTAB   DC    C'0123456789ABCDEF'                                              
MESSAGE  DC    C'YOU DIED IN **????** AT 00000000'                              
         DROP  R2                                                               
*                                                                               
         USING SDWA,R1                                                          
ERR000   L     R1,ESTAER1          R1 MUST POINT TO SDWA                        
         L     RD,ESTAERD                                                       
         LA    R2,ERRRETN          SET 'RETRY' ADDRESS                          
         SETRP DUMP=YES,RC=4,RETADDR=(2),FRESDWA=YES                            
         LM    RE,RC,12(RD)                                                     
         BR    RE                  CONTROL SHOULD RETURN TO ERRRETN             
*                                                                               
         USING *,RF                                                             
ERRRETN  LM    R0,RF,MYREGS        LOAD REGS SAVED BEFORE RESTART               
         BR    R1                  BRANCH TO RESTART                            
*                                                                               
         DROP  R1                                                               
         DROP  RF                                                               
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
         LTORG                                                                  
*                                                                               
ERRWORK  DC    600D'00'                                                         
         EJECT                                                                  
                                                                                
***********************************************************************         
* WORKING STORAGE                                                               
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
AIOAREA  DS    A                                                                
ACTREC   DS    A                                                                
ACIREC   DS    A                                                                
*                                                                               
ASIHDR   DS    A                   A(SHARED MEMORY FILE INDEX HEADER)           
ASITAB   DS    A                   A(START OF RESOURCE HEADERS)                 
ASFIPARN DS    A                   A(START OF NEXT PART1 AREA)                  
ACURPAR1 DS    A                   A(CURRENT PART1 ENTRY)                       
ACURPAR2 DS    A                   A(CURRENT PART2 ENTRY)                       
FIRSTCI  DS    A                   SAVED PART1 CI ADDRESS                       
GOODCI   DS    A                   LAST GOOD CI ADDRESS                         
THISCI   DS    A                   CURRENT CI ADDRESS                           
*                                                                               
DUB      DS    D                                                                
DUBDUB   DS    2D                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
DMCB     DS    7F                  SHARED MEMORY CALL NEEDS 7 PARAMS            
*                                                                               
SVREF    DS    F                   SAVED REFERENCE NUMBER                       
*                                                                               
SHMSIZE  DS    F                   SIZE OF SHARED MEMORY STORAGE                
CNTPAR1  DS    F                   COUNT PART1S                                 
CNTPAR2  DS    F                   COUNT PART2S                                 
CNTP1AV  DS    F                   NUMBER OF AVAILABLE  PART1S                  
CNTP2AV  DS    F                   NUMBER OF AVAILABLE  PART2S                  
CNTP1VU  DS    F                   NUMBER OF VULNERABLE PART1S                  
CNTP2VU  DS    F                   NUMBER OF VULNERABLE PART2S                  
*                                                                               
PAPART1  DS    F                   PART1 % AVAILABLE                            
PAPART2  DS    F                   PART2 % AVAILABLE                            
*                                                                               
TIMENOW  DS    F                   BINARY TIME AT END OF WAIT                   
TIMEOLD  DS    F                   PREVIOUS POP TIME                            
DATENOW  DS    PL4                 BINARY DATE                                  
DATEOLD  DS    PL4                 BINARY DATE LAST TIME                        
DAYNOW   DS    XL1                                                              
DATNOW   DS    XL1                                                              
MONNOW   DS    XL1                                                              
TODAY    DS    XL2                 TODAY'S DATE          COMPRESSED             
YESTRDAY DS    XL2                 YESTERDAY'S DATE      COMPRESSED             
TWOWEEKS DS    XL2                 TWO WEEKS AGO'S DATE  COMPRESSED             
DAYNOWA  DS    XL3                 DAY OF THE WEEK (SUN,MON,...)                
TIMENOWM DS    XL1                 TIME NOW IN 10 MINUTE INTERVALS              
TIMENOWC DS    XL2                 TIME OF SCAN (BINARY (SECS*2/4)              
TIMEOLDC DS    XL2                 TIME OF LAST SCAN (BINARY (SECS*3/4)         
TODAYA   DS    CL8                 TODAY'S DATE FULL ALPHA                      
*                                                                               
WORK     DS    CL64                                                             
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
FILEDSN  DS    CL44                FILE DATA SET NAME                           
*                                                                               
SINDI    DS    X                   SCAN INDICATOR                               
SIPURGE  EQU   X'80'               . REPORT PURGED                              
*                                                                               
APINDEX  DS    CL96                INDEX                                        
SVINDEX  DS    CL96                                                             
*                                                                               
       ++INCLUDE SHFIW                                                          
*                                                                               
       ++INCLUDE DMWRKFW                                                        
*                                                                               
HOWPOP   DS    C                   (A=ALERT,T=TIMER,O=OPERATOR)                 
*                                                                               
ENDTASK  DS    X                   FLAG Y IF WE MUST ABEND                      
ABENDS   DS    H                   COUNT ABENDS                                 
*                                                                               
MSGW     DS    0CL82               WTO OUTPUT MESSAGE                           
MSGWLEN  DS    XL2                                                              
MSGWTXT  DS    0CL80                                                            
MSGWMSG  DS    CL40                WTO INFORMATION MESSAGE                      
MSGWDET  DS    CL40                WTO INFORMATION DETAIL                       
*                                                                               
CARDFROM DS    C                   C'S'=START, C'F'=MODIFY                      
CARD     DS    CL80                                                             
*                                                                               
IOKEY    DS    CL44                                                             
IOKEYSV  DS    CL44                                                             
*                                                                               
IOAREA   DS    2048C                                                            
*                                                                               
CTREC    DS    4096C                                                            
*                                                                               
CIREC    DS    14336C                                                           
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
                                                                                
**********************************************************************          
* VULNERABLE TABLE DSECT                                                        
**********************************************************************          
RVTABD   DSECT                                                                  
RVONE    DS    XL2                 NUMBER WEIGHTED BY AGE AND SIZE              
RVTWO    DS    XL2                                                              
RVNDX    DS    XL(L'W_INDEX)       INDEX                                        
RVTABL   EQU   *-RVTABD                                                         
*                                                                               
RVMAX    EQU   2000                                                             
                                                                                
**********************************************************************          
* PRINT LINE DSECT                                                              
**********************************************************************          
PLINED   DSECT                                                                  
         DS    CL5                                                              
PLMSG    DS    CL18                                                             
         DS    CL1                                                              
PLUSER   DS    CL8                                                              
         DS    CL1                                                              
PLSYSP   DS    CL4                                                              
PLDAY    DS    CL2                                                              
PLCLASS  DS    CL1                                                              
         DS    CL1                                                              
PLREFNO  DS    CL5                                                              
         DS    CL1                                                              
PLTYPE   DS    CL2                                                              
         DS    CL1                                                              
PLATTB   DS    CL2                                                              
         DS    CL1                                                              
PLSTAT   DS    CL2                                                              
         DS    CL1                                                              
PLSEQ    DS    CL3                                                              
         DS    CL1                                                              
PLAGE    DS    CL10                                                             
         DS    CL1                                                              
PLAGET   DS    CL8                                                              
         DS    CL1                                                              
PLCIA    DS    CL8                                                              
*                                                                               
         ORG   PLINED                                                           
         DS    CL1                                                              
PMMARK   DS    CL1                                                              
         DS    CL1                                                              
PMMSG    DS    CL12                                                             
         DS    CL1                                                              
PMLP     DS    CL1                                                              
PMTIME   DS    CL8                                                              
         DS    CL1                                                              
PMDAYA   DS    CL3                                                              
         DS    CL1                                                              
PMDATEA  DS    CL8                                                              
PMRP     DS    CL1                                                              
         DS    CL1                                                              
PMMSG2   DS    CL20                                                             
*                                                                               
         ORG   PLINED                                                           
         DS    CL5                                                              
PTDESC   DS    CL28                                                             
         DS    CL1                                                              
PTCOUNT  DS    CL6                                                              
*                                                                               
         ORG   PLINED                                                           
         DS    CL3                                                              
PRMARK   DS    CL1                                                              
         DS    CL1                                                              
PRRESO   DS    CL8                                                              
         DS    CL1                                                              
PRNUM    DS    CL3                                                              
         DS    CL4                                                              
PRNAME   DS    CL7                                                              
         DS    CL2                                                              
PRDESC   DS    CL16                                                             
         DS    CL1                                                              
PRVALU   DS    CL8                                                              
*                                                                               
         ORG   PLINED                                                           
         DS    CL5                                                              
PIPART   DS    CL5                 PART1/PART2                                  
         DS    CL1                                                              
PICIS    DS    CL10                NUMBER OF CIS                                
         DS    CL2                                                              
PIAV     DS    CL10                NUMBER OF CIS AVAILABLE                      
         DS    CL2                                                              
PIPAV    DS    CL12                % AVAILABLE                                  
         DS    CL2                                                              
PIVU     DS    CL10                NUMBER OF CIS VULNERABLE                     
         DS    CL2                                                              
PIPVU    DS    CL12                % VULNERABLE                                 
PIEND    DS    CL1                                                              
                                                                                
**********************************************************************          
* OTHER DSECTS                                                                  
**********************************************************************          
         DSECT                                                                  
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
         IHASDWA GR32=YES                                                       
         POP   ACONTROL                                                         
*                                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*                                                                               
* FASSBOFF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*                                                                               
* SHFID                                                                         
* DMWRKFD                                                                       
* DMWRKFK                                                                       
* DMWRKFX                                                                       
       ++INCLUDE SHFID                                                          
       ++INCLUDE DMWRKFD                                                        
       ++INCLUDE DMWRKFK                                                        
       ++INCLUDE DMWRKFX                                                        
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SHFIMON   12/05/12'                                      
         END                                                                    
