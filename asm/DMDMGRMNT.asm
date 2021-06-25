*          DATA SET DMDMGRMNT  AT LEVEL 031 AS OF 09/17/20                      
*PROCESS USING(WARN(15))                                                        
*&&      SET   CL=Y,AG=N                                                        
*PHASE DMGRMNTA                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE FILMAN                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE PQSCAN                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE RERUN                                                                  
*INCLUDE DDWTO                                                                  
*&&UK                                                                           
*INCLUDE USSQINQ                                                                
*&&                                                                             
*INCLUDE MDUMPER                                                                
         TITLE 'DMDMGRMNT - MAINTAIN DATASPACE AREAS'                           
         PRINT NOGEN                                                            
DMGRMNT  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**DMMT**,=A(WORKAREA),RA,R9,R8,CLEAR=YES             
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
         MVI   REBUILD,NO          DEFAULT NO REBUILD                           
         L     R1,0(R1)                                                         
         LH    R2,0(R1)                                                         
         LTR   R2,R2               R2=L'PARM DATA                               
         BZ    DMGRMNT1                                                         
         LA    R1,2(R1)            R1=A(PARM DATA)                              
         MVC   REBUILD,0(R1)       SET REBUILD VALUE                            
DMGRMNT1 EQU   *                                                                
*                                                                               
         BRAS  RE,PRINTI           INIT PRINTING                                
         BRAS  RE,INIT             READ CARDS ETC (WAS *&&US NOW US/UK)         
*                                                                               
         BRAS  RE,ENQJOB           SET JTHDR JOBTAB                             
JT       USING DMSPACED,JTHDR                                                   
         BRAS  RE,ENQCLS           SET CLHDR CLASSTAB                           
CL       USING DMSPACED,CLHDR                                                   
*                                                                               
*        BRAS  RE,INIT             READ CARDS ETC (was *&&UK)                   
*                                                                               
         CLI   ESTAESW,YES                                                      
         JNE   DMGRMNT2                                                         
         XC    DMCB(24),DMCB                                                    
         LARL  RF,ERREXT           EXIT ROUTINE                                 
         ST    RF,DMCB+4                                                        
         MVI   DMCB+4,20           MAX 20 DUMPS                                 
         GOTO1 =V(MDUMPER),DMCB,=C'INIT'                                        
*                                                                               
DMGRMNT2 BRAS  RE,INISTATE         INITIALISE STATIC STATE TABLE                
*                                                                               
RESTART  SAM24 ,                   MAKE SURE WE RESTART IN 24 BIT               
         SAC   0                                                                
         CLI   ENDTASK,YES         DO WE WANT TO CANCEL                         
         BE    ENDOFJOB                                                         
*                                                                               
FIRST    CLI   FILMANI,YES         HAS FILMAN INIT BEEN DONE                    
         JE    DEM010                                                           
         GOTO1 =V(FILMAN),DMCB,(C'I',REBUILD),SEFILN                            
         MVI   FILMANI,YES                                                      
*&&UK                                                                           
         LA    R1,=C'MO#00001/N/J/SYSTEM INITIALISATION COMPLETE//'             
         ST    R1,DMCB                                                          
         GOTO1 =V(DDWTO),DMCB      OLD MESSAGE **TEMP** UNTIL RULE CHG          
*&&                                                                             
         LA    R1,=C'<SYS0001D>/N/J/SYSTEM INITIALISATION COMPLETE//H'          
         ST    R1,DMCB                                                          
         GOTO1 =V(DDWTO),DMCB                                                   
*                                                                               
DEM010   BRAS  RE,ARSOFF                                                        
         BRAS  RE,SETWAIT          MAIN LOOP                                    
         BRAS  RE,TIMECHK          CHECK TIMES                                  
         BRAS  RE,MAIN                                                          
         BRAS  RE,ARSOFF                                                        
         B     DEM010                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK TIMERS - SEE IF ANYTHING TO DO                                *         
***********************************************************************         
TIMECHK  NTR1  ,                                                                
         LA    RF,TIMETAB                                                       
*                                                                               
TIMECHK0 MVI   0(RF),NO            LETS ASSUME NOT                              
         L     R1,TIMENOW          R1 = TIME NOW                                
         SR    R0,R0                                                            
         S     R1,4(RF)            SUBTRACT LAST TIME                           
*                                                                               
         BM    TIMECHK1            -VE MUST BE MIDNIGHT                         
*                                                                               
         CLM   R1,7,1(RF)          IS IT > TIME FREQ                            
         BL    TIMECHK2                                                         
*                                                                               
TIMECHK1 MVI   0(RF),YES           OK DO IT                                     
         MVC   4(4,RF),TIMENOW     SET TIME WE DID IT                           
*                                                                               
TIMECHK2 LA    RF,8(RF)            NEXT                                         
         CLI   0(RF),X'FF'                                                      
         BNE   TIMECHK0                                                         
*                                                                               
TIMECHKX B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM LOOP                                                   *         
***********************************************************************         
MAIN     NTR1  ,                                                                
         CLI   RUNQ,YES            DO WE WANT TO MAINTAIN RUNQ                  
         JNE   MAIN04                                                           
*                                                                               
         CLI   TIMEPQ,YES          TIME TO CHECK RUN QUEUE                      
         JNE   *+12                                                             
         BRAS  RE,PQRCHK                                                        
         MVI   TIMEPQ,NO                                                        
*                                                                               
         CLI   DSPACE,C'R'         REP DATASPACE RECOVERY?                      
         BE    MAIN01              YES - SKIP THE JOB TABLE CLEAN UP            
*                                        WE ONLY NEED TO DO IT IN ADV           
         CLI   TIMERUN,YES         TIME TO CLEAN RUN QUEUE                      
         JNE   *+12                                                             
         BRAS  RE,RUNCHK           MAINTAIN RUN QUEUE                           
         MVI   TIMERUN,NO                                                       
*                                                                               
MAIN01   CLI   TIMERERU,YES         STATUS CHECK FOR RERUN?                     
         JNE   MAIN01X              NO                                          
         GOTO1 ARERUN,DMCB,(C'S',0) DO STATUS CHECK                             
         MVI   TIMERERU,NO                                                      
MAIN01X  EQU   *                                                                
*                                                                               
         CLI   TIMEHELP,YES        CHECK FOR RUNQ PROBLEMS                      
         BNE   MAIN04                                                           
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST                                                
         SAC   512                                                              
         USING TABJOBS,R2                                                       
*                                                                               
         CLI   DSPACE,C'R'         REP DATASPACE RECOVERY?                      
         BE    MAIN02                                                           
*                                                                               
         CLC   TBJTIME,EFFS        EMERGENCY REBUILD                            
         BNE   MAIN03                                                           
         B     MAIN02A                                                          
*                                                                               
MAIN02   CLC   =X'FFFFFFFE',TBJTIME                                             
         BNE   MAIN03                                                           
         DROP  R2                                                               
*                                                                               
MAIN02A  BRAS  RE,ARSOFF                                                        
         LA    R0,ST3H             EMERGENCY RERUN                              
         BRAS  RE,WTO                                                           
         CLI   SOONCPY,YES                                                      
         JNE   *+8                                                              
         BRAS  RE,SOONCOPY                                                      
         MVI   SOONCPY,NO                                                       
         BRAS  RE,ARSOFF                                                        
*                                                                               
         GOTO1 ARERUN,DMCB,(C'E',0)                                             
*                                                                               
MAIN03   BRAS  RE,ARSOFF                                                        
*                                                                               
MAIN04   CLI   TIMERUNR,YES        TIME TO CHECK RUNNERS                        
         JNE   *+12                NOP CAUSING IDF PROBLEMS                     
         BRAS  RE,RUNRCHK                                                       
         MVI   TIMERUNR,NO         CHECK RUNNERS                                
*                                                                               
         CLI   TIMETRAC,YES        TIME TO WRITE TRACE DATA                     
         JNE   *+12                                                             
         BRAS  RE,TRACEOUT         WRITE TRACE DATA                             
         MVI   TIMETRAC,NO                                                      
*                                                                               
         CLI   TIMEBDAT,YES        TIME TO CHECK BILLDATES                      
         JNE   *+12                                                             
         BRAS  RE,BILLDATE         CHECK BILLDATE                               
         MVI   TIMEBDAT,NO                                                      
*                                                                               
         CLI   TIMEWAIT,YES        TIME TO CHECK FOR WAITS                      
         JNE   *+12                                                             
         BRAS  RE,WAITCHK          CHECK WAITS                                  
         MVI   TIMEWAIT,NO                                                      
*                                                                               
         CLI   TIMELONG,YES        TIME TO CHECK FOR LONG RUNNERS               
         BNE   *+12                                                             
         BRAS  RE,LONGCHK          CHECK LONG RUNNERS                           
         MVI   TIMELONG,NO                                                      
*                                                                               
         CLI   TIMESTAT,YES        TIME TO CHECK SYS STATE                      
         BNE   *+12                                                             
         BRAS  RE,STATCHK          CHECK SYSTEM STATE                           
         MVI   TIMESTAT,NO                                                      
*                                                                               
         CLI   ABEND,YES           FORCE ABEND FOR TESTING                      
         BNE   *+10                                                             
         MVI   ABEND,NO                                                         
         DC    H'0'                                                             
*                                                                               
**********************************************************************          
* FSTATS  - is called by a modified command to report the file stats            
*         on a time interval based on operations. For now we need to            
*         be able to suspend this reporting if they are doing some              
*         special work on a file with a disposition of DISP=OLD or if           
*         they dump and load the file. Adding new over-ride..                   
* SUSPEND - If they issue a command called suspend this will skip the           
*         call to STATS until they issue a new modify command to                
*         suspend. Anything but a C'Y'                                          
**********************************************************************          
         CLI   SUSPEND,YES         Suspend call to report stats                 
         BE    MAIN05                                                           
         XC    STATSYS,STATSYS                                                  
         CLC   FSTATS,=CL8'NO'     DUMP FILE STATS                              
         JE    MAIN05                                                           
         CLI   FSTATS,YES          CAN BE EITHER Y OR SYS                       
         JE    MAIN04A                                                          
         CLC   FSTATS(3),=C'ALL'   CAN BE EITHER Y OR ALL OR SYS                
         JE    MAIN04A                                                          
         MVC   STATSYS,FSTATS      MUST BE SYS THEN                             
         MVC   PLINE(2),=AL2(08)                                                
         MVC   PLINE+2(8),FSTATS                                                
         WTO   TEXT=PLINE,MCSFLAG=HRDCPY                                        
*                                                                               
MAIN04A  GOTO1 =V(FILMAN),DMCB,(C'R',0),STATSYS                                 
         MVC   FSTATS,=CL8'NO'     DONE                                         
*                                                                               
MAIN05   ICM   RF,15,=V(FILMAN)    CALL FILMAN IF THERE                         
         JZ    MAIN05X                                                          
         CLI   SUSPEND,YES                                                      
         BE    MAIN05X                                                          
         CLI   TIMESPAC,YES        FILE SPACE CHECK                             
         JNE   MAIN05X                                                          
         GOTO1 (RF),DMCB,(C'S',0),SEFILN                                        
         MVI   TIMESPAC,NO                                                      
MAIN05X  EQU   *                                                                
*                                                                               
MAINX    B     EXITOK                                                           
*                                                                               
ST1H     DC    AL2(L'ST1)                                                       
ST1      DC    C'Call to RERUN for class table f st1s check'                    
ST2H     DC    AL2(L'ST2)                                                       
ST2      DC    C'Back from RERUN after class table status check'                
ST3H     DC    AL2(L'ST3)                                                       
ST3      DC    C'EMERGENCY RERUN requested - rebuilding tables'                 
         EJECT                                                                  
***********************************************************************         
* CHECK RUNNERS EVERY 5 MINS IN CASE CANCELLED                        *         
***********************************************************************         
RUNRCHK  NTR1  ,                                                                
         BRAS  RE,ARSOFF           AT LEAST START CLEAN                         
         SAM31                                                                  
*                                                                               
         LAM   AR2,AR2,TBLET       GET ADDRESS OF SERVER TABLE                  
         SAC   512                                                              
         XR    R2,R2                                                            
         USING FATABSD,R2                                                       
         ICM   R2,15,TABSRUN                                                    
         BZ    RUNRCHKX                                                         
         USING TABSRUND,R2                                                      
*                                                                               
         CLC   TABSLRUN,=C'*RUN*RUN'                                            
         BNE   RUNRCHKX                                                         
         SR    R6,R6                                                            
         ICM   R6,3,TABSNSVR       NUMBER OF SERVER ENTRIES (R6)                
         ICM   R2,15,TABSASVR      ADDRESS OF SERVER ENTRIES                    
         BZ    RUNRCHKX                                                         
*                                                                               
         USING TABSSVRD,R2                                                      
RUNRCH01 MVC   DUB,TSJOB                                                        
         OC    DUB,DUB                                                          
         BZ    RUNR050             NO JOB FINE                                  
*                                                                               
RUNR011  LH    R4,TSASID           GET THE ASID                                 
         MVC   DUB1,=C'LOCASCB '                                                
         SAC   0                                                                
         SAM24                                                                  
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         SAM31                                                                  
         SAC   512                                                              
         LTR   RF,RF                                                            
         BNZ   RUNRNO              ERROR FROM MACRO                             
*                                                                               
         USING ASCB,R3                                                          
         MVC   DUB1,=C'CHKASCB '                                                
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   RUNRNO              NOT AN ASCB                                  
*                                                                               
         L     R1,ASCBCSCB         GET CSCB                                     
         MVC   DUB1,CHKEY-CHAIN(R1)                                             
         CLC   DUB,CHKEY-CHAIN(R1) CHECK STEPNAME                               
         BE    RUNR050                                                          
         CLC   DUB1(3),=C'IDF'     ALLOW IDF... STEPNAME                        
         BNE   RUNRNO                                                           
*                                                                               
RUNR050  LA    R2,TABSSVRL(,R2)    NEXT SERVER                                  
         BCT   R6,RUNRCH01                                                      
         B     RUNRCHKX                                                         
*                                  THE JOB HAS GONE                             
*                                  CLEAR THIS SERVER ENTRY                      
*                                                                               
RUNRNO   LA    R4,WORK             SET UP REMOVAL MESSAGE                       
         MVC   0(2,R4),=AL2(33)                                                 
         MVC   2(23,R4),=C'RUNNER ........ REMOVED'                             
         MVC   9(8,R4),TSJOB                                                    
         MVC   26(8,R4),DUB1                                                    
         MVC   TSTIND1+1(3),=C'XXX'                                             
         XC    0(TABSSVRL,R2),0(R2)    REMOVE SERVER ENTRY                      
         SAM24                                                                  
         SAC   0                                                                
         WTO   TEXT=(R4),MCSFLAG=HRDCPY                                         
         SAM31                                                                  
         SAC   512                                                              
         B     RUNR050                                                          
*                                                                               
RUNRCHKX SAC   0                                                                
         SAM24                                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK RUN QUEUE                                                     *         
***********************************************************************         
RUNCHK   NTR1  ,                                                                
         BRAS  RE,ARSOFF           AT LEAST START CLEAN                         
         SAM31                                                                  
*                                                                               
         XC    RQPURGE,RQPURGE                                                  
         MVI   BADCLS,NO           REBUILD CLASS TABLE FLAG                     
         MVI   BADJOB,NO           REBUILD JOB/CLASS TABLES FLAG                
         XC    SAVEJOB#,SAVEJOB#                                                
*                                                                               
         BRAS  RE,LOCK             LOCK JOB AND CLASS TABLES                    
         BRAS  RE,ARSOFF           AT LEAST START CLEAN                         
         ICM   R2,15,JT.DSPTFRST                                                
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         MVC   JOBHDRL,TBJLHEAD    SAVE SOFT LENGTHS TABLE HEADER               
         MVC   JOBTABL,TBJLNTRY                      JOB ENTRY                  
*                                                                               
         ICM   R0,15,TBJNUM        ANY JOBS IN TABLE?                           
         BNZ   RC02                YES                                          
*                                                                               
         ICM   R3,15,JT.DSPTEND    CLEAR WHOLE TABLE IF NO JOBS                 
         AHI   R3,1                                                             
         SR    R3,R2                                                            
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         ICM   R2,15,JT.DSPTFRST                                                
         MVC   TBJLHEAD,JOBHDRL    RESET SOFT LENGTHS                           
         MVC   TBJLNTRY,JOBTABL                                                 
         MVC   TBJTIME,TIMENOW     SET TIME AND EXIT                            
*                                                                               
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,SCANCLS          SET FLAGS IN CASE CLASS TABLE IS ILL         
         B     RC30                                                             
*                                                                               
RC02     MVC   TBJTIME,TIMENOW     SET TIME OF THIS CLEANUP                     
         BRAS  RE,ARSOFF                                                        
         BRAS  RE,SCANCLS          CHECK CLASS TABLE IS GOOD RIGHT NOW          
*                                                                               
         ICM   R2,15,JT.DSPTFRST                                                
         AH    R2,JOBHDRL          GO PAST HEADER (ADD LOCAL LNGTH CPY)         
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                 ARS ARE NOW ON                               
         USING TBJOBTAB,R2                                                      
*                                                                               
         XR    R6,R6               R6 = NEW COUNT OF JOBS                       
RC04     OC    TBJCLASS(4),TBJCLASS                                             
         BZ    RC18                SKIP ZERO ENTRIES                            
         CLC   TBJCLASS(4),EFFS                                                 
         BE    RC16                IGNORE (BUT COUNT) TEMP ENTRIES              
*                                                                               
         CLI   TBJSTAT,0           ENTRY FLAGGED TO BE DELETED?                 
         BNE   RC12                NO                                           
         CLC   TBJCLASS,EFFS       OLD TEMP ENTRY?                              
         BE    RC08                YES - DON'T LOG THEN                         
*&&DO                                                                           
* Not wanted - it reports jobs deleted by users in =JOB as errors               
         CLI   TBJMONS,0           ANY MONSOON ASID?                            
         JNE   RC06X                                                            
         MVI   BADJOB,YES          LATER TO REBUILD JOB/CLASS TABLES            
         LH    R0,SAVEJOB#         WHY DEL IT IF NOT YET RUN??                  
         CHI   R0,SAVEJBXQ              - LOG THIS IN SAVEJOB TABLE             
         JNL   RC06X                          UP TO SAVEJBXQ ENTRIES            
         LR    R1,R0                                                            
         MHI   R1,L'SAVEJOB                                                     
         A     R1,ASAVEJOB                                                      
         MVC   0(L'SAVEJOB,R1),TBJNTRY                                          
         AHI   R0,1                                                             
         STH   R0,SAVEJOB#                                                      
RC06X    DS    0H                                                               
*&&                                                                             
         XC    CARD,CARD           LOG ENTRY                                    
         MVC   CARD+00(1),DAYNOW                                                
         MVC   CARD+01(1),DATNOW                                                
         MVC   CARD+02(1),MONNOW                                                
         MVC   CARD+03(4),TIMENOW                                               
         MVC   CARD+16(L'TBJNTRY),TBJNTRY                                       
*                                                                               
         CLI   LOGEOF,YES          LOG FILE ERROR                               
         BE    RC08                YES                                          
                                                                                
***********************************************************************         
* Log deleted entries                                                           
***********************************************************************         
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         PUT   RUNLOG,CARD                                                      
         B     *+8                                                              
LOGFULL  MVI   LOGEOF,YES          EOF ON LOGFILE DATASET                       
         REAR  ARS=ON                                                           
                                                                                
**********************************************************************          
* BUILD  STATS INFO HERE WHEN IMPLEMENTED                                       
**********************************************************************          
RC08     CLI   BADCLS,YES          CLASS TABLE IS KNOWN TO BE BAD?              
         BE    RC10                YES                                          
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         ST    R2,FULL                                                          
         MVI   BYTE,0              SET CHECK ALL CLASSES                        
*                                                                               
         BRAS  RE,INACLS           TEST IF IN A CLASS LIST SOMEWHERE            
*NOP     BNE   *+8                 NO - GOOD                                    
*NOP     MVI   BADCLS,YES                                                       
         REAR  ARS=ON                                                           
*                                                                               
RC10     LH    R1,JOBTABL          CLEAR JOB ENTRY                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    TBJNTRY(0),TBJNTRY                                               
*                                                                               
         L     RF,RQPURGE          ADD 1 TO NUMBER OF ENTRIES PURGED            
         AHI   RF,1                                                             
         ST    RF,RQPURGE                                                       
         B     RC18                YES - NEXT ENTRY                             
                                                                                
***********************************************************************         
* GOOD ENTRY - NOW MAKE SURE THAT IT IS IS IN CLASS TABLE PROPERLY              
***********************************************************************         
RC12     OC    TBJRTIME,TBJRTIME   IGNORE ANYTHING RUNNING OR READY             
         BNZ   RC16                                                             
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         ST    R2,FULL             SAVE OFF CURRENT ENTRY ADDRESS               
         MVI   BYTE,1              SET CHECK MY SUBMITTED CLASS                 
         BRAS  RE,INACLS           TEST IF IN MY CLASS                          
         BNE   RC14                NO - GOOD                                    
         REAR  ARS=ON                                                           
         B     RC16                                                             
*                                                                               
RC14     MVI   BADCLS,YES          SET CLASS TABLE IS UNWELL                    
         REAR  ARS=ON                                                           
*                                                                               
         XC    CARD,CARD           SET JOB NOT FOUND DETAILS                    
         MVC   CARD+00(02),=AL2(L'TBJNTRY+4)                                    
         MVC   CARD+02(05),=CL4'NIC='                                           
         MVC   CARD+06(L'TBJNTRY),TBJNTRY                                       
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         LA    R0,CARD             OUTPUT TO CONSOLE                            
         BRAS  RE,WTO                                                           
         REAR  ARS=ON                                                           
*                                                                               
RC16     AHI   R6,1                #JOBS = #JOBS+1                              
*                                                                               
RC18     AH    R2,JOBTABL          NEXT ENTRY                                   
         CLM   R2,15,JT.DSPTEND                                                 
         BL    RC04                                                             
*                                                                               
         ICM   R2,15,JT.DSPTFRST   REACHED END OF JOB TABLE                     
         USING TABJOBS,R2                                                       
         CLM   R6,15,TBJNUM        NUMBERS MATCH?                               
         BE    RC30                YES                                          
*                                                                               
         ICM   R0,15,TBJNUM        SAVE OLD COUNT                               
         STCM  R6,15,TBJNUM        SET NEW COUNT                                
         CVD   R6,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RM2TO,DUB                                                        
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RM2WAS,DUB                                                       
         L     R0,RQPURGE                                                       
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  RM2PRG,DUB                                                       
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LA    R0,RM2H                                                          
         BRAS  RE,WTO              RUN COUNTER RESET MESSAGE                    
*                                                                               
RC30     CLI   BADJOB,YES          BAD JOB                                      
         JE    RC31                                                             
         CLI   BADCLS,YES          OR BAD CLASS                                 
         JNE   RC33                                                             
*                                                                               
RC31     CLI   SOONCPY,YES         DO WE WANT A SOON COPY                       
         JNE   RC33                                                             
         BRAS  RE,SOONCOPY         TAKE COPY                                    
         MVI   SOONCPY,NO                                                       
*                                                                               
RC33     BRAS  RE,ARSOFF                                                        
         BRAS  RE,UNLK             FREE JOB AND CLASS TABLES                    
         BRAS  RE,ARSOFF                                                        
*                                                                               
*********************************************************************           
         CLI   BADJOB,YES          BAD JOB ENTRY?                               
         BNE   RC40                NO                                           
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         LH    R2,SAVEJOB#                                                      
         L     R6,ASAVEJOB                                                      
RC38     MVC   WORK+00(02),=AL2(L'SAVEJOB+16)                                   
         MVC   WORK+02(16),=CL16'Bad job purged=>'                              
         MVC   WORK+18(L'SAVEJOB),0(R6)                                         
         LA    R0,WORK             OUTPUT COPY OF EACH SAVE JOB ENTRY           
         BRAS  RE,WTO                                                           
         AHI   R6,L'SAVEJOB                                                     
         JCT   R2,RC38                                                          
         REAR  ARS=ON                                                           
*                                                                               
*NOP     LA    R0,ST3H             EMERGENCY RERUN                              
*NOP     BRAS  RE,WTO                                                           
*NOP                                                                            
*NOP     GOTO1 ARERUN,DMCB,(C'E',0)                                             
*NOP     B     RC40                                                             
*********************************************************************           
*                                                                               
RC40     BRAS  RE,ARSOFF                                                        
*                                                                               
         CLI   BADCLS,YES          CLASS TABLE IS BUGGERED?                     
         BNE   RC90                NO                                           
*                                                                               
         LA    R0,RM5H                                                          
         BRAS  RE,WTO              RUN COUNTER RESET MESSAGE                    
         GOTO1 ARERUN,DMCB,(C'C',0),0                                           
*                                                                               
RC90     DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
RM1H     DC    AL2(L'RM1)                                                       
RM1      DC    C'Starting scan of Job table vs Class table'                     
*                                                                               
RM2H     DC    AL2(RM2L)                                                        
RM2      DC    C'Job table counter was reset from:'                             
RM2WAS   DC    C'00000'                                                         
         DC    C' to:'                                                          
RM2TO    DC    C'00000'                                                         
         DC    C' purged:'                                                      
RM2PRG   DC    C'00000'                                                         
RM2L     EQU   *-RM2                                                            
*                                                                               
RM3H     DC    AL2(L'RM3)                                                       
RM3      DC    C'Finished scan of Job table vs Class table'                     
*                                                                               
RM4H     DC    AL2(L'RM4)                                                       
RM4      DC    C'Job table is empty'                                            
*                                                                               
RM5H     DC    AL2(L'RM5)                                                       
RM5      DC    C'Class table corrupted - calling rerun for rebuild'             
         EJECT                                                                  
***********************************************************************         
* SCAN CLASS TABLE TO CHECK IF THIS ENTRY IS THERE SOMEWHERE          *         
* NTRY: FULL   = A(JOBTAB ENTRY)                                      *         
*       BYTE   = ZERO - CHECK ALL CLASSES                             *         
*              = NZ   - CHECK ONLY MY CLASS SUBMITTED QUEUE           *         
* EXIT: CC HI  = ENTRY IS IN A CLASS TABLE SOMEWHERE                  *         
*       CC EQ  = ENTRY IS WHERE IT IS SUPPOSED TO BE                  *         
***********************************************************************         
INACLS   NTR1  ,                                                                
         CLI   BADCLS,YES                                                       
         BE    EXITL                                                            
*                                                                               
         BRAS  RE,ARSOFF                                                        
         ICM   R2,15,CL.DSPTFRST    CLASS TABLE                                 
         LAM   AR2,AR2,TBLET                                                    
         USING JCLASSD,R2                                                       
         SAC   512                                                              
*                                                                               
         CPYA  AR3,AR2                                                          
*                                                                               
INCL02   OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    INCL22                                                           
*                                                                               
         CLI   BYTE,0              SCAN ALL CLASSES                             
         BE    INCL04              YES                                          
*                                                                               
         ICM   R3,15,FULL          R3 = A(JOB TABLE ENTRY)                      
         USING TBJOBTAB,R3                                                      
*                                                                               
*&&CL*&& CLC   JCCLASS,TBJCLASS    MATCH CLASS?                                 
*&&CL*&& BNE   INCL18              NO                                           
*&&AG*&& CLC   JCAGY,TBJAGY        MATCH AGENCY?                                
*&&AG*&& BNE   INCL18              NO                                           
*                                                                               
         CLI   JCTYPE,JCLONG       LONG CLASS TABLE?                            
         BNE   *+16                NO                                           
         TM    TBJSTAT,TBJLONG     JOB IS LONG?                                 
         BZ    INCL18                                                           
         B     INCL04                                                           
*                                                                               
         TM    TBJSTAT,TBJLONG     JOB IS LONG?                                 
         BO    INCL18                                                           
*                                                                               
INCL04   ICM   R3,15,JCFSUB        ANY SUBMITTED JOBS HERE?                     
         BZ    INCL08              NO                                           
*                                                                               
INCL06   CLM   R3,15,FULL                                                       
         BE    INCL20              HERE I BE                                    
         ICM   R3,15,TBJNXT                                                     
         BNZ   INCL06                                                           
*                                                                               
INCL08   CLI   BYTE,0              CHECK MY SUBMITTED QUEUE?                    
         BNE   INCL18              NOT THERE KEEP GOING (LONG SOONS)            
*                                                                               
         ICM   R3,15,JCFRUN        ANY RUNNING?                                 
         BZ    INCL12                                                           
*                                                                               
INCL10   CLM   R3,15,FULL                                                       
         BE    INCL20              I'M IN HERE                                  
         ICM   R3,15,TBJNXT                                                     
         BNZ   INCL10                                                           
*                                                                               
INCL12   DS    0H                  READY CODE HERE IF IMPLEMENTED               
*                                                                               
INCL18   AHI   R2,JCLASSL          NEXT CLASS ENTRY                             
         B     INCL02                                                           
*                                                                               
INCL20   CLI   BYTE,0              BYTE=0 THEN PURGE JOB FROM CLASS             
         JNE   INCL21                                                           
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,JCNSUB         REDUCE SUBMITTED COUNT                       
         BZ    *+8                                                              
         AHI   R0,-1                                                            
         STCM  R0,3,JCNSUB                                                      
*                                                                               
         CLC   JCFSUB,FULL         AM I FIRST SUBMIITED JOB?                    
         BNE   INCL20A             NO                                           
         MVC   JCFSUB,TBJNXT       SET NEXT AS FIRST                            
         CLC   JCLSUB,FULL         AM I ALSO LAST SUBMITTED JOB?                
         BNE   INCL21              NO                                           
         XC    JCLSUB,JCLSUB       CLEAR LAST SUBMITTED                         
*                                                                               
INCL20A  CPYA  AR4,AR3                                                          
         ICM   R4,15,JCFSUB        FIRST SUBMITTED JOB                          
PS       USING TBJOBTAB,R4         PS = PRIOR SUBMITTED JOB                     
*                                                                               
INCL20B  CLM   R3,15,PS.TBJNXT     GET JOB THAT POINTS TO ME                    
         BE    INCL20C                                                          
         ICM   R4,15,PS.TBJNXT                                                  
         BNZ   INCL20B                                                          
         MVI   BADCLS,YES                                                       
*                                                                               
INCL20C  MVC   PS.TBJNXT,TBJNXT    FIX UP LINKS (TAKE ME OUT)                   
         XC    TBJNXT,TBJNXT                                                    
*                                                                               
         CLM   R3,15,JCLSUB        WAS I THE LAST SUBMITTED?                    
         BNE   *+8                 NO                                           
         STCM  R4,15,JCLSUB        FIX LIST TAIL                                
         DROP  PS                                                               
*                                                                               
INCL21   BRAS  RE,ARSOFF                                                        
         B     EXITOK              SET CC OK IF MATCH IN TABLE                  
*                                                                               
INCL22   BRAS  RE,ARSOFF                                                        
         B     EXITL               SET CC LOW IF NO MATCH                       
         EJECT                                                                  
***********************************************************************         
* SCAN CLASS TABLE TO MAKE SURE LINKS ALL LOOK GOOD                   *         
***********************************************************************         
SCANCLS  NTR1  ,                                                                
         MVI   BADCLS,NO           SET CLASS TABLE IS OK                        
*                                                                               
         BRAS  RE,ARSOFF                                                        
         ICM   R2,15,CL.DSPTFRST                                                
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
*                                                                               
         USING JCLASSD,R2                                                       
SCLS02   OC    JCLASSD(JCKEYL),JCLASSD                                          
         BZ    SCLSXIT             JUST EXIT                                    
*                                                                               
         ICM   R3,15,JCFSUB        ANY SUBMITTED? A(FIRST ENTRY)                
         BNZ   SCLS04                                                           
         XC    JCNSUB,JCNSUB       RESET NUMBER OF ENTRIES                      
         XC    JCLSUB,JCLSUB       RESET A(LAST ENTRY)                          
         B     SCLS10                                                           
*                                                                               
SCLS04   CPYA  AR3,AR2                                                          
         USING TBJOBTAB,R3         CHECK EVERYTHING LOOKS GOOD                  
*                                                                               
SCLS06   LHI   R0,1                R0 = 1 ENTRY IS CLEARED                      
         OC    TBJCLASS(4),TBJCLASS                                             
         BZ    SCLSBAD             NO CLASS INFO SET                            
*                                                                               
         LHI   R0,2                R0 = 2 ENTRY IS FLAGGED AS TEMP              
         CLC   TBJCLASS(4),EFFS                                                 
         BE    SCLSBAD                                                          
*NOP     LHI   R0,3                R0 = 3 ENTRY IS FLAGGED TO CLEAR             
*NOP     CLI   TBJSTAT,0                                                        
*NOP     BE    SCLSBAD                                                          
*                                                                               
         LHI   R0,4                R0 = 4 ENTRY IS SET RUNNING                  
         OC    TBJRTIME,TBJRTIME                                                
         BNZ   SCLSBAD             SUBMIT QUEUE SHOULDN'T HAVE RUN TIME         
*                                                                               
         LHI   R0,5                R0 = 5 ENTRY IS SET FINISHED                 
         OC    TBJETIME,TBJETIME                                                
         BNZ   SCLSBAD             SUBMIT QUEUE SHOULDN'T HAVE END TIME         
*                                                                               
         OC    TBJNXT,TBJNXT       SUB LINK LIST HAS FORWARD POINTER?           
         BNZ   SCLS08              YES                                          
*                                                                               
         CLM   R3,15,JCLSUB        LIST TAIL SET CORRECTLY?                     
         BE    SCLS10              YES - FINISHED                               
         LHI   R0,6                R0 = 6 LIST TAIL IS BAD                      
         B     SCLSBAD                                                          
*                                                                               
SCLS08   LHI   R0,7                R0 = 7 NEXT POINTER IS TOO LOW               
         CLC   TBJNXT,JT.DSPTFRST  SCOPE FOR RANGE                              
         BL    SCLSBAD                                                          
*                                                                               
         LHI   R0,8                R0 = 8 NEXT POINTER IS TOO HIGH              
         CLC   TBJNXT,JT.DSPTEND                                                
         BH    SCLSBAD                                                          
         ICM   R3,15,TBJNXT        GO TO NEXT POINTER IN SUB LL                 
         B     SCLS06                                                           
*                                                                               
SCLS10   ICM   R3,15,JCFRUN        ANY RUNNING? A(FIRST ENTY)                   
         BNZ   SCLS12                                                           
         XC    JCNRUN,JCNRUN       RESET NUMBER OF JOBS RUNNING                 
         XC    JCLRUN,JCLRUN       RESET A(LAST ENTRY)                          
         B     SCLS20                                                           
*                                                                               
SCLS12   CPYA  AR3,AR2                                                          
         USING TBJOBTAB,R3         CHECK EVERYTHING LOOKS GOOD                  
*                                                                               
SCLS14   LHI   R0,9                R0 = 9 ENTRY IS EMPTY                        
         OC    TBJCLASS(4),TBJCLASS                                             
         BZ    SCLSBAD                                                          
         LHI   R0,10               R0 = 10 ENTRY IS FLAGGED AS TEMP             
         CLC   TBJCLASS(4),EFFS                                                 
         BE    SCLSBAD                                                          
*NOP     LHI   R0,11               R0 = 11 ENTRY IS FLAGGED TO DELETE           
*NOP     CLI   TBJSTAT,0                                                        
*NOP     BE    SCLSBAD                                                          
         LHI   R0,12               R0 = 12 ENTRY HAS COMPLETED                  
         OC    TBJETIME,TBJETIME   IF RUNNING CAN'T BE FINISHED                 
         BNZ   SCLSBAD                                                          
*                                                                               
         OC    TBJNXT,TBJNXT       RUN LL HAS FORWARD POINTER?                  
         BNZ   SCLS16              YES                                          
         CLM   R3,15,JCLRUN        LIST TAIL SET CORRECTLY?                     
         BE    SCLS20              YES - FINISHED                               
         LHI   R0,13               R0 = 13 LIST TAIL SET WRONG                  
         B     SCLSBAD                                                          
*                                                                               
SCLS16   LHI   R0,14               R0 = 14 LIST TAIL TOO LOW                    
         CLC   TBJNXT,JT.DSPTFRST  SCOPE FOR RANGE                              
         BL    SCLSBAD                                                          
         LHI   R0,15               R0 = 15 LIST TAIL TOO LOW                    
         CLC   TBJNXT,JT.DSPTEND                                                
         BH    SCLSBAD                                                          
         ICM   R3,15,TBJNXT        GO TO NEXT POINTER IN SUB LL                 
         B     SCLS14                                                           
*                                                                               
SCLS20   DS    0H                                                               
***********************************************************************         
* WHEN (IF) WE EVER GET TO REPORTING READY JOBS - STATS - THEN THE CODE         
* TO HANDLE THEM WILL NEED TO BE ADDED HERE                                     
***********************************************************************         
                                                                                
SCSL40   LAM   AR3,AR3,ARZERO                                                   
         AHI   R2,JCLASSL                                                       
         B     SCLS02              NEXT CLASS ENTRY                             
*                                                                               
SCLSXIT  BRAS  RE,ARSOFF           GOOD EXIT                                    
         B     EXITOK                                                           
*                                                                               
SCLSBAD  MVC   CARD,SPACES                                                      
         MVC   CARD1,SPACES                                                     
         MVC   CARD2,SPACES                                                     
*                                                                               
         MVC   CARD+00(02),=AL2(68)                                             
         MVC   CARD+02(20),=CL20'**SCANCLS ERROR**'                             
         MVC   CARD+22(03),=CL03'R0='                                           
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  CARD+25(3),DUB                                                   
*                                                                               
         MVC   CARD1+00(02),=AL2(68)                                            
         MVC   CARD1+02(04),=CL4'B/E='                                          
         MVC   CARD1+06(L'TBJNTRY),TBJNTRY                                      
*                                                                               
         MVC   CARD2+00(02),=AL2(68)                                            
         MVC   CARD2+02(04),=CL4'B/C='                                          
         MVC   CARD2+06(JCLASSL),JCLASSD                                        
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LA    R0,CARD                                                          
         BRAS  RE,WTO                                                           
         LA    R0,CARD1                                                         
         BRAS  RE,WTO                                                           
         LA    R0,CARD2                                                         
         BRAS  RE,WTO                                                           
*                                                                               
         MVI   BADCLS,YES          FORCE REBUILD OF CLASS TABLE                 
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SCAN RUN TABLE AND COMPARE VS PQ TO MAKE SURE JOB LOOKS GOOD        *         
***********************************************************************         
PQRCHK   NTR1  ,                                                                
         BRAS  RE,ARSOFF           START OFF CLEAN                              
         LA    R0,PQ1H                                                          
         BRAS  RE,WTO                                                           
*                                                                               
         BRAS  RE,ARSOFF           CLEAR AGAIN BECAUSE OF WTO                   
         SAM31                                                                  
         XC    RUNQPURG,RUNQPURG   CLEAR PURGED COUNTER                         
*                                                                               
         LAM   AR2,AR2,TBLET                                                    
         ICM   R2,15,JT.DSPTFRST   A(JOB TABLE @ FIXED LOCATION IN DS)          
         SAC   512                                                              
         USING TABJOBS,R2                                                       
         XR    R4,R4                                                            
         ICM   R4,3,TBJLNTRY       LENGTH OF JOB ENTRY                          
         ICM   R5,15,JT.DSPTEND    A(END OF JOB TABLE)                          
*                                                                               
         ICM   R0,15,TBJNUM        R0 NUMBER OF ENTIRES IN QUEUE                
         BZ    PQR16               NONE                                         
*                                                                               
         AH    R2,TBJLHEAD         ADD LENGTH OF JOB HEADER                     
         USING TBJOBTAB,R2         POINT TO START OF EACH ENTRY                 
PQR02    OC    TBJCLASS(4),TBJCLASS                                             
         BZ    PQR14               IGNORE BLACK ENTRIES                         
         CLC   TBJCLASS(4),EFFS                                                 
         BE    PQR14               IGNORE TEMP ENTRIES                          
         OC    TBJMONS,TBJMONS                                                  
         BNZ   PQR14               IGNORE RUNNING/READY ENTRIES                 
         CLI   TBJSTAT,0                                                        
         BE    PQR14               IGNORE IF READY TO PURGE ENTRIES             
*                                                                               
         OC    TBJPQUSR,TBJPQUSR   CHECK KEY (USER ID)                          
         BNZ   PQR04               YES                                          
         OC    TBJPQSEQ,TBJPQSEQ   REPORT NUMBER ASSIGNED ?                     
         BZ    PQR14               IGNORE BLANK ENTRIES                         
*                                  ENSURE ENTRY IS FOR RIGHT (MY) PQ            
PQR04    LLC   RF,TBJADV           GET ADV NUMBER                               
         NILL  GRF,X'000F'                                                      
         MHI   RF,FACITLNQ                                                      
         A     RF,AFACITAB         ADD BASE TABLE BY DSPACE                     
*                                                                               
         USING FACITABD,RF                                                      
         CLC   FACISN4,SPACES                                                   
         BNH   PQR14               SOMETHING IS NOT RIGHT                       
*&&US                                                                           
         CLI   DSPACE,C'R'         CHECK REP REPORTS ONLY                       
         BNE   PQR05               NOT REP                                      
         TM    FACIFL,FACIREP                                                   
         BZ    PQR14               IGNORE SINCE NOT REP ENTRY                   
         B     PQR06                                                            
*                                                                               
PQR05    CLI   DSPACE,C'A'         CHECK ADV REPORTS ONLY                       
         BNE   PQR06               DON'T BOTHER FOR TST,CSC,FQA                 
         TM    FACIFL,FACIREP                                                   
         BO    PQR14               IGNORE SINCE IS REP ENTRY                    
*&&                                                                             
         DROP  RF                                                               
*                                                                               
PQR06    MVC   CARD(2),=AL2(L'TBJNTRY)                                          
         MVC   CARD+2(L'TBJNTRY),TBJNTRY   SAVE OFF JOB TABLE ENTRY             
         STAR  CLEAR=ARZERO,ARS=OFF                                             
LCL      USING TBJOBTAB,CARD+2                                                  
*                                                                               
PQ       USING UKRECD,NDX                                                       
         MVC   PRTQID,=CL8'PRTQUE' SET PRTQ                                     
         XC    NDX,NDX                                                          
         MVC   PQ.UKSRCID,LCL.TBJPQUSR                                          
         MVC   PQ.UKREPNO,LCL.TBJPQSEQ                                          
         OI    PQ.UKFLAG,UKFLNUM+UKFLCIA+UKFLCIR                                
         DROP  PQ                                                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,(0,=C'INDEX'),PRTQID,NDX,SAVE,CXREC                
         CLI   8(R1),0                                                          
         BE    PQR08                                                            
         CLI   8(R1),X'41'         IGNORE FORMAT ERRORS FOR NOW                 
         BE    PQR10                                                            
         CLI   8(R1),X'80'         AND EOFS                                     
         BE    PQR10                                                            
*                                                                               
         USING PQRECD,R3                                                        
PQR08    LA    R3,CXREC                                                         
         CLC   LCL.TBJPQKEY,PQKEY  IS IT A GOOD ENTRY                           
         BNE   PQR10               NO                                           
         TM    PQATTB,PQATJOBO     IS JOB OUTPUT ON?                            
         BZ    PQR12               NO                                           
         TM    LCL.TBJSTAT,TBJKILL WAS THE JOB KILLED                           
         BO    PQR12               LEAVE IT ALONE THEN                          
*                                                                               
PQR10    REAR  ARS=ON              YOU HAVE A BAD ENTRY                         
*                                                                               
         OC    TBJMONS,TBJMONS                                                  
         BNZ   PQR14               IGNORE IF YOU STARTED RUNNING                
         CLI   TBJSTAT,0           WAS IT MARK PURGED BY USER?                  
         BE    PQR14               YES, SO SKIP                                 
                                                                                
*********************************************************************           
* REPORT OUT THE PQ ENTRY ERROR                                                 
*********************************************************************           
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         MVC   WORK+00(02),=AL2(L'PQINDEX+10)                                   
         MVC   WORK+02(10),=CL10'Bad PQ ==>'                                    
         MVC   WORK+12(L'PQINDEX),PQINDEX                                       
         LA    R0,WORK             OUTPUT COPY OF PQ INDEX                      
         BRAS  RE,WTO                                                           
         LA    R0,CARD             OUTPUT LOCAL JOBTAB ENTRY                    
         BRAS  RE,WTO                                                           
         REAR  ARS=ON                                                           
*                                                                               
         MVI   TBJSTAT,0           BAD - FLAG FOR REMOVAL                       
         LHI   R1,1                                                             
         A     R1,RUNQPURG         COUNT PURGES                                 
         ST    R1,RUNQPURG                                                      
         B     PQR14                                                            
         DROP  R3,LCL                                                           
*                                                                               
PQR12    REAR  ARS=ON                                                           
*                                                                               
PQR14    BXLE  R2,R4,PQR02         BUMP TO NEXT                                 
*                                                                               
PQR16    BRAS  RE,ARSOFF                                                        
         ICM   R0,15,RUNQPURG      REPORT ON PURGED ENTRIES                     
         BZ    PQR18                                                            
*                                                                               
         MVC   CARD,SPACES                                                      
         MVC   CARD+00(2),MSGRQFH                                               
         MVC   CARD+02(L'MSGRQF),MSGRQF                                         
         EDIT  (R0),(3,CARD+02)                                                 
         LA    R0,CARD                                                          
         BRAS  RE,WTO                                                           
*                                                                               
PQR18    GOTO1 =V(DMISGENQ),DMCB,C'TASK'                                        
         B     EXITOK                                                           
*                                                                               
PQ1H     DC    AL2(L'PQ1)                                                       
PQ1      DC    C'Starting PQ vs Run table scan'                                 
PQ2H     DC    AL2(L'PQ2)                                                       
PQ2      DC    C'Finished PQ vs Run table scan'                                 
*                                                                               
MSGRQFH  DC    AL2(L'MSGRQF)                                                    
MSGRQF   DC    C'XXX Run queue entries flagged for removal'                     
         DROP  R2                                                               
                                                                                
***********************************************************************         
* COPY SOON TABLES IN TABS FOR DEBUGGING BEFORE REBUILD OCCURS        *         
***********************************************************************         
SOONCOPY NTR1                                                                   
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,TBLET                                                    
         LAM   AR4,AR4,TBLET                                                    
         SAC   512                                                              
*                                  SET UP ADCONS FOR COPY                       
         LA    R2,DTSVJOB-X'8000'  Copy of Job table                            
         SLL   R2,6                                                             
         ST    R2,ASOONSV                                                       
*                                                                               
         LA    R2,DTSVCLS-X'8000'  Copy of class table                          
         SLL   R2,6                                                             
         ST    R2,ACLASSV                                                       
*                                                                               
         LA    R2,DTJOB-X'8000'    Actual Job table                             
         SLL   R2,6                                                             
         ST    R2,AJOBS                                                         
*                                                                               
         LA    R2,DTDCLASS-X'8000' Actual Class table                           
         SLL   R2,6                                                             
         ST    R2,ACLAS                                                         
*                                                                               
         L     R2,AJOBS            COPY JOBSTAB                                 
LIV      USING DMSPACED,R2                                                      
         OC    LIV.DSPTFRST,LIV.DSPTFRST                                        
         JZ    SOONCOPX                                                         
         L     RE,LIV.DSPTEND                                                   
         L     RF,LIV.DSPTFRST                                                  
         N     RF,=X'3FFFFFFF'                                                  
         SR    RE,RF                                                            
         LR    R3,RE                                                            
         LR    R2,RF                                                            
*                                                                               
         L     R4,ASOONSV                                                       
CPY      USING DMSPACED,R4                                                      
         OC    CPY.DSPTFRST,CPY.DSPTFRST                                        
         JZ    SOONCOPX                                                         
         L     RE,CPY.DSPTEND                                                   
         L     RF,CPY.DSPTFRST                                                  
         N     RF,=X'3FFFFFFF'                                                  
         SR    RE,RF                                                            
         LR    R5,RE                                                            
         LR    R4,RF                                                            
*                                                                               
         LR    RE,R4                                                            
         SR    RE,R2                                                            
         ST    RE,JCOFFS                                                        
*                                                                               
         MVCL  R4,R2                                                            
*                                                                               
         L     R2,ACLAS            COPY CLASS TAB                               
         OC    LIV.DSPTFRST,LIV.DSPTFRST                                        
         JZ    SOONCOPX                                                         
         L     RE,LIV.DSPTEND                                                   
         L     RF,LIV.DSPTFRST                                                  
         N     RF,=X'3FFFFFFF'                                                  
         SR    RE,RF                                                            
         LR    R3,RE                                                            
         LR    R2,RF                                                            
*                                                                               
         L     R4,ACLASSV                                                       
         OC    CPY.DSPTFRST,CPY.DSPTFRST                                        
         JZ    SOONCOPX                                                         
         L     RE,CPY.DSPTEND                                                   
         L     RF,CPY.DSPTFRST                                                  
         N     RF,=X'3FFFFFFF'                                                  
         SR    RE,RF                                                            
         LR    R5,RE                                                            
         LR    R4,RF                                                            
*                                                                               
         MVCL  R4,R2                                                            
*                                                                               
         L     R4,ACLASSV                                                       
         L     RE,CPY.DSPTEND                                                   
         L     RF,CPY.DSPTFRST                                                  
         N     RF,=X'3FFFFFFF'                                                  
         LR    R4,RF                                                            
         L     RF,JCOFFS                                                        
*                                                                               
         USING JCLASSD,R4                                                       
SOONC050 ICM   RF,15,JCFSUB                                                     
         JZ    *+8                                                              
         A     RF,JCOFFS                                                        
         ST    RF,JCFSUB                                                        
*                                                                               
         ICM   RF,15,JCLSUB                                                     
         JZ    *+8                                                              
         A     RF,JCOFFS                                                        
         ST    RF,JCLSUB                                                        
*                                                                               
         ICM   RF,15,JCFRUN                                                     
         JZ    *+8                                                              
         A     RF,JCOFFS                                                        
         ST    RF,JCFRUN                                                        
*                                                                               
         ICM   RF,15,JCLRUN                                                     
         JZ    *+8                                                              
         A     RF,JCOFFS                                                        
         ST    RF,JCLRUN                                                        
*                                                                               
         AHI   R4,JCLASSL                                                       
         CR    R4,RE                                                            
         JL    SOONC050                                                         
*                                                                               
         L     R4,ASOONSV                                                       
         L     RE,CPY.DSPTEND                                                   
         L     RF,CPY.DSPTFRST                                                  
         N     RF,=X'3FFFFFFF'                                                  
         LR    R4,RF                                                            
         AHI   R4,L'TBJNTRY                                                     
         L     RF,JCOFFS                                                        
*                                                                               
         USING TBJOBTAB,R4                                                      
SOONC150 ICM   RF,15,TBJNXT                                                     
         JZ    *+8                                                              
         A     RF,JCOFFS                                                        
         ST    RF,TBJNXT                                                        
*                                                                               
         AHI   R4,L'TBJNTRY                                                     
         CR    R4,RE                                                            
         JL    SOONC150                                                         
         DROP  R4                                                               
         DROP  LIV,CPY                                                          
*                                                                               
SOONCOPX BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE TRACE DATA                                                    *         
***********************************************************************         
TRACEOUT NTR1                                                                   
         CLI   TRCEOF,YES          DID THE LOG FILE EOF                         
         JE    TRACEXIT                                                         
*                                                                               
TRAC000  BRAS  RE,ARSOFF           GET ACCESS TO TRACE BUFFER                   
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,DHATRCB-DMDSHDR(R2)                                        
         BZ    TRACEX                                                           
         AHI   R2,256              SKIP OVER TRACE TABLE                        
         ST    R2,TRCADFI          SET TRACE TABLE ADDRESS                      
*                                                                               
         OC    0(16,R2),0(R2)      IS THIS FIRST TIME FOR BUFFER                
         JNZ   TRAC001                                                          
*                                                                               
         MVC   0(4,R2),=C'TRCB'    SET EYECATCHER                               
         LA    R1,16(R2)           SET A(FIRST ENTRY)                           
         ST    R1,4(,R2)           SET READ POINT                               
         ST    R1,8(,R2)           SET WRITE POINT                              
         A     R1,=A((32*4096)-16-256) (32*4096) PAGES FROM DMDMGRDSP           
         ST    R1,12(,R2)          SET BUFFER END ADDR                          
         MVC   16(16,R2),=C'**EMPTY BUFFER**'                                   
*                                                                               
         XR    RE,RE               ZAP IT ALL TO ZERO                           
         XR    RF,RF                                                            
         L     R3,12(,R2)                                                       
         LA    R2,32(R2)                                                        
         SR    R3,R2                                                            
         MVCL  R2,RE                                                            
         L     R2,TRCADFI          SET TRACE TABLE ADDRESS AGAIN                
*                                                                               
TRAC001  CLC   0(4,R2),=C'TRCF'    FAST TRACE JOB IS ACTIVE                     
         JE    TRACEXIT                                                         
         CLC   0(4,R2),=C'TRCB'    BUFFER MUST BE GOOD                          
         JE    TRAC002                                                          
         CLC   0(4,R2),=C'TRCX'    BUFFER OVERFLOW                              
         JNE   TRAC002                                                          
*                                                                               
         SAC   0                                                                
         MVC   CXREC(4),=X'000C0000'                                            
         MVC   CXREC+4(8),=C'OVERFLOW'                                          
         PUT   TRACELOG,CXREC      SEND OVERFLOW RECORD                         
         L     R2,TRCADFI                                                       
         SAC   512                                                              
         MVC   0(4,R2),=C'TRCB'    RESTORE HEADER AND GO READ                   
*                                                                               
TRAC002  ICM   R1,15,12(R2)        GET BUFFER END POINT                         
         JZ    TRACEX                                                           
         ST    R1,TRCADMX                                                       
*                                                                               
         ICM   R1,15,4(R2)         GET CURRENT READ POINT                       
         JZ    TRACEX                                                           
         ST    R1,TRCADDR                                                       
*                                                                               
         ICM   R1,15,8(R2)         GET CURRENT ADD POINT                        
         JZ    TRACEX                                                           
         ST    R1,TRCADCUR                                                      
*                                                                               
         L     R2,TRCADDR          R2 = READ POINT                              
         C     R2,TRCADCUR         SAME AS ADD POINT                            
         JE    TRACEXIT            EXIT                                         
*                                                                               
TRAC010  CLC   14(2,R2),=C'TR'     NEW TRACE ENTRY                              
         JNE   TRACEND             MUST BE ELSE TRY TOP                         
         MVC   14(2,R2),=C'TW'     SET IT WRITTEN                               
*                                                                               
*        BUILD A TRACE RECORD AND PUT IT TO TRACELOG                            
*                                                                               
         MVC   CXREC(4),=X'00000000'                                            
         SR    R1,R1                                                            
         ICM   R1,3,2(R2)          GET LENGTH OF ENTRY                          
         JZ    *+2                                                              
*                                                                               
         LA    R1,4(R1)                                                         
         STCM  R1,3,CXREC          SET RECORD LEN                               
         SHI   R1,4                                                             
*                                                                               
         LR    R0,R2               THE TRACE BUFFER IS FOLLOWED                 
         LR    R3,R1               MOVE DATA TO SAVE FOR WRITING                
         LR    RF,R3                                                            
         LA    RE,CXREC+4                                                       
         MVCL  RE,R2               MOVE INTO CXREC                              
         LR    R2,R0                                                            
*                                                                               
         XR    R1,R1               ADD RECORD LEN                               
         ICM   R1,3,2(R2)                                                       
         LA    R3,0(R1,R2)                                                      
         L     R2,TRCADFI                                                       
         ST    R3,4(,R2)           SAVE POINT OF NEXT READ                      
         LR    R2,R3                                                            
*                                                                               
         SAC   0                                                                
         PUT   TRACELOG,CXREC                                                   
         SAC   512                                                              
*                                                                               
         C     R2,TRCADCUR         ARE WE AT ADD POINT                          
         JE    TRACEXIT            OK DONE                                      
*                                                                               
         CLI   0(R2),0             TEST EOB                                     
         JE    TRACEND                                                          
         C     R2,TRCADMX          OR EOB ADDRESS                               
         JNL   TRACEND                                                          
         XR    R1,R1                                                            
         ICM   R1,3,2(R2)          OR INVALID LENGTH                            
         JZ    TRACEND                                                          
         AR    R1,R2               OR CURRENT + LEN > EOB                       
         C     R1,TRCADMX                                                       
         JL    TRAC010                                                          
*                                                                               
TRACEND  L     R3,TRCADFI          TRY TOP OF BUFFER THEN                       
         LA    R3,16(R3)                                                        
         CR    R2,R3               ARE WE ALREADY AT TOP                        
         JE    TRACEX              BAD BUFFER THEN                              
*                                                                               
         L     R2,TRCADFI                                                       
         ST    R3,4(,R2)           SAVE POINT OF NEXT READ                      
         LR    R2,R3                                                            
         J     TRAC010                                                          
*                                                                               
TRCFULL  MVI   TRCEOF,YES          FLAG EOF AND CLEAN EXIT                      
         J     TRACEXIT                                                         
*                                                                               
TRCX     DC    H'0'                                                             
*                                                                               
TRACEX   SAC   512                 FLAG BUFFER AS BAD AND EXIT                  
         L     R2,TRCADFI                                                       
         MVC   0(4,R2),=C'XXXX'                                                 
TRACEXIT SAC   0                                                                
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK BILLDATES ARE INITIALISED AND SENSIBLE                        *         
***********************************************************************         
BILLDATE NTR1                                                                   
         CLI   BILLDAT,NO          DO WE WANT BILLDATES PROCESSED               
         BE    BILLDATX                                                         
*                                                                               
         TIME  TU                                                               
         ST    R1,TODAY            SAVE DATE                                    
         ST    R0,BILLTIME         SAVE TIME                                    
*                                                                               
         GOTO1 =V(DATCON),DMCB,(6,TODAY),(0,DUB)                                
         GOTO1 =V(ADDAY),DMCB,(C'D',DUB),DUB1,F'1'                              
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(15,TOMORROW)                           
         GOTO1 =V(ADDAY),DMCB,(C'D',DUB),DUB1,F'-1'                             
         GOTO1 =V(DATCON),DMCB,(0,DUB1),(15,YESTERDY)                           
         GOTO1 =V(DATCON),DMCB,(0,DUB),(15,TODAY)                               
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
*                                                                               
         LHI   R0,1                SCAN 256 SYSTEMS                             
BILLD010 STC   R0,BYTE                                                          
*                                                                               
         LR    R2,R0               INDEX TO SYSTEM                              
         SLL   R2,6                                                             
         USING DMSPACED,R2                                                      
         MVC   SYSNAME,DSPNAME                                                  
         ICM   R2,15,DSPECB        PICK UP SYSTEM BLOCK                         
         BZ    BILLD050            IGNORE IF NOT THERE                          
         SLL   R2,2                DROP HIGH BITS                               
         SRL   R2,2                                                             
*                                                                               
         USING DMSYSHDR,R2         IS BILLDATE UNINITIALISED                    
         OC    DSYBDATE,DSYBDATE                                                
         BNZ   BILLD020                                                         
*                                                                               
BILLD015 MVC   DSYBDATE,TODAY      SET TO TODAY                                 
         MVC   FULL,DSYBDATE                                                    
         SAC   0                                                                
         MVC   PLINE(36),=C'BILLDATE ........ SET TO          //'               
         MVC   PLINE+9(8),SYSNAME                                               
         GOTO1 =V(DATCON),DMCB,(6,FULL),(17,PLINE+25)                           
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         SAC   512                                                              
*                                                                               
BILLD020 CLC   DSYBDATE,TODAY      TODAY IS ALWAYS OK FOR A BILL DATE           
         BE    BILLD050                                                         
*                                                                               
         CLI   BILLDAT,C'T'        TEST MODE - IF NOT SET TO TODAY              
         JE    BILLD015            THEN SET TO TODAY                            
*                                                                               
         CLC   BILLTIME,BILLMAX    IS TIME < MAX FOR YESTERDAY (06:00)          
         BH    BILLD030                                                         
         CLC   DSYBDATE,YESTERDY   THEN YESTERDAY IS OK                         
         BE    BILLD050                                                         
*                                                                               
BILLD030 CLC   BILLTIME,BILLMIN    IS TIME > MIN FOR TOMORROW (18:00)           
         BL    BILLD040                                                         
         CLC   DSYBDATE,TOMORROW   THEN TOMORROW IS OK                          
         BE    BILLD050                                                         
*                                                                               
BILLD040 MVC   FULL,DSYBDATE                                                    
         SAC   0                                                                
         MVC   PLINE(40),=C'BILLDATE ........         OUT OF RANGE//'           
         MVC   PLINE+9(8),SYSNAME                                               
         GOTO1 =V(DATCON),DMCB,(6,FULL),(17,PLINE+18)                           
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         SAC   512                                                              
*                                                                               
BILLD050 CLC   BYTE,=AL1(255)      UP TO SYS 255                                
         BNL   BILLDATX                                                         
         LLC   R0,BYTE                                                          
         AHI   R0,1                                                             
         B     BILLD010                                                         
*                                                                               
BILLDATX SAC   0                                                                
         B     EXITOK                                                           
*                                                                               
BILLMIN  DC    XL4'8C136000'                17:00                               
BILLMAX  DS    0F                                                               
*&&US*&& DC    AL4(((06*60)+30)*(60*38400)) 06:30AM                             
*&&UK*&& DC    AL4(((06*60)+45)*(60*38400)) 06:45AM                             
         EJECT                                                                  
***********************************************************************         
* CHECK WAIT TABLE AND REPORT ON LONG WAITS                           *         
***********************************************************************         
WAITCHK  NTR1                                                                   
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         USING DMDSHDR,R2                                                       
         L     R2,DHAECBS          FIND ECBS                                    
*                                                                               
         ICM   R6,15,0(R2)         NONE SO OK                                   
         BZ    WAITCHKX                                                         
         LR    R4,R2                                                            
         AHI   R4,4096             SET R4 TO MAX                                
*                                                                               
WAITCHK1 LA    R2,16(,R2)                                                       
         CR    R2,R4               IF EOT THEN DONE                             
         BNL   WAITCHKX                                                         
*                                                                               
         OC    0(16,R2),0(R2)      IGNORE ZEROS                                 
         BZ    WAITCHK1                                                         
         OC    8(4,R2),8(R2)                                                    
         BNZ   WAITCHK2                                                         
         MVC   8(4,R2),TIMENOW     SAVE TIME FIRST ENCOUNTERED                  
*                                                                               
WAITCHK2 L     R1,TIMENOW          CALCULATE TIME DIFF                          
         S     R1,8(,R2)                                                        
         CHI   R1,6000             LESS THAN 60 SECONDS IGNORE                  
         BL    WAITCHK1                                                         
*                                                                               
WAITCHK3 MVC   PLINE(32),=C'....... ........... ....... //'                     
         XC    FULL,FULL                                                        
         MVC   FULL(3),4(R2)                                                    
         BRAS  RE,DISJOB                                                        
         MVC   PLINE+0(7),DUB1                                                  
         XC    FULL,FULL                                                        
         MVC   FULL(3),0(R2)                                                    
         BRAS  RE,DISJOB                                                        
         MVC   PLINE+20(7),DUB1                                                 
*                                                                               
         MVC   PLINE+8(11),=C'WAITING FOR'                                      
         TM    12(R2),X'40'                                                     
         BZ    *+10                                                             
         MVC   PLINE+8(11),=C'POSTED BY  '                                      
*                                                                               
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,(X'80',0)                                   
         SAC   512                                                              
*                                                                               
         BCT   R6,WAITCHK1                                                      
*                                                                               
WAITCHKX SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK FOR LONG RUNNING ADV TASKS AND WARN IF OVER 15 MINUTES        *         
***********************************************************************         
LONGCHK  NTR1                                                                   
         CLI   DSPACE,C'T'         TEST DATASPACE - OK MAY BE =DEBUG            
         BE    LONGCHKX                                                         
         CLI   DSPACE,C'C'         CSC DATASPACE - OK MAY BE =DEBUG             
         BE    LONGCHKX                                                         
         TIME  TU                                                               
         ST    R0,TIMENTU                                                       
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         USING DMDSHDR,R2                                                       
         L     R3,DHAMQRTN                                                      
         L     R2,DHATOR           FIND START OF TOR BLOCK                      
         USING TORFACD,R2                                                       
LONGC000 ST    R2,CURRTOR                                                       
         CR    R2,R3                                                            
         BNL   LONGCHKX                                                         
*                                                                               
         CLC   TOREYE(3),=C'TOR'   MAKE SURE WE HAVE A TOR                      
         BNE   LONGC040                                                         
         MVC   CURRFACN(4),TOREYE+4                                             
         CLI   TORSTRTD,YES        STARTED                                      
         BNE   LONGC040                                                         
         CLI   TORAVLBL,YES        AVAILABLE                                    
         BNE   LONGC040                                                         
         LA    R1,TORFACLQ+6       LINE UP ON EXCHANGE BLOCK                    
         AR    R2,R1                                                            
         USING SBEXCHD,R2                                                       
         LA    R5,SBFACMAX                                                      
LONGC010 ST    R2,CURRFAC                                                       
         MVI   CURRFACN+4,C' '                                                  
         CLI   SBFACID,1           IS IT TOR                                    
         JE    LONGC019                                                         
         LH    R1,SBFACID          ADJUST FOR AOR BOR ETC                       
         SHI   R1,2                                                             
         AHI   R1,C'A'                                                          
         STC   R1,CURRFACN+4                                                    
LONGC019 EQU   *                                                                
*                                                                               
         CLI   SBSTRTD,YES         STARTED                                      
         BNE   LONGC030                                                         
         CLI   SBAVLBL,YES         AVAILABLE                                    
         BNE   LONGC030                                                         
         L     R4,SBTSKMAX                                                      
         LA    R2,SBTSKBLK                                                      
         USING EXCHNGD,R2                                                       
LONGC020 OC    EXCTIME,EXCTIME     IGNORE ZERO TIME                             
         BZ    LONGC025                                                         
*                                                                               
         CLC   TIMENTU,EXCTIME     TIMENTU MUST BE > EXCTIME                    
         BL    LONGC025                                                         
*                                                                               
         L     R1,TIMENTU                                                       
         S     R1,EXCTIME                                                       
         C     R1,=A(38400*60*15)  IGNORE < 15 MINS                             
         BL    LONGC025                                                         
*                                                                               
         MVC   PLINE(42),=C'WARNING LONG RUNNING TASK ADV1A *TASK 1*//'         
         MVC   PLINE+32(8),0(R2)                                                
         MVC   PLINE+26(5),CURRFACN                                             
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         SAC   512                                                              
*                                                                               
LONGC025 AHI   R2,EXCHNGLQ         NEXT TASK                                    
         BCT   R4,LONGC020                                                      
*                                                                               
LONGC030 L     R2,CURRFAC          NEXT FACPAK                                  
         A     R2,=A(SBEXCHLQ)                                                  
         BCT   R5,LONGC010                                                      
*                                                                               
LONGC040 L     R2,CURRTOR          NEXT TOR                                     
         LA    R1,SBFACMAX                                                      
         MHI   R1,SBEXCHLQ                                                      
         AHI   R1,TORFACLQ+6                                                    
         AR    R2,R1                                                            
         B     LONGC000                                                         
*                                                                               
LONGCHKX SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DO JOBS QUERY FOR SYSTEM IN JOBSQ                                   *         
***********************************************************************         
JOBSQRY  NTR1                                                                   
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         USING DMSPACED,R2                                                      
         XR    R0,R0                                                            
JOBSQ010 AHI   R2,64                                                            
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         JNL   JOBSQ090                                                         
         CLC   JOBSQ,DSPNAME                                                    
         JNE   JOBSQ010                                                         
*                                                                               
         XC    HALF,HALF                                                        
         ICM   R2,15,DSPECB        SET R2 TO SYSTEM ENTRY                       
         N     R2,=X'3FFFFFFF'                                                  
         USING DMSYSHDR,R2                                                      
         ICM   R2,15,DSYAJOBS                                                   
         USING DSJOBHDR,R2                                                      
         LA    R6,DSJBHMXQ         MAX JOBS IN TABLE                            
JOBSQ020 OC    DSJOBNAM,DSJOBNAM                                                
         JZ    JOBSQ029                                                         
         CLI   DSJOBADV,0          COUNT NON ADVS                               
         JE    *+12                                                             
         TM    DSJOBFLG,DSJOBSHQ   AND ADVS WITH SHARE                          
         JZ    JOBSQ029                                                         
*                                                                               
         LA    R0,1                BUMP COUNTER                                 
         AH    R0,HALF                                                          
         STH   R0,HALF                                                          
*                                                                               
         MVC   PLINE(8),DSJOBNAM                                                
         MVC   PLINE+9(6),=C'J00000'                                            
         EDIT  (2,DSJOBNUM),(5,PLINE+10),FILL=0                                 
         MVC   PLINE+15(2),=C'//'                                               
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,(X'80',0)                                   
         SAC   512                                                              
*                                                                               
JOBSQ029 LA    R2,DSJBHLNQ(R2)                                                  
         BCT   R6,JOBSQ020                                                      
         MVC   PLINE(8),JOBSQ                                                   
         MVC   PLINE+8(8),=C'UPDJOBS= '                                         
         EDIT  (2,HALF),(2,PLINE+16)                                            
         MVC   PLINE+18(2),=C'//'                                               
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,(X'80',0)                                   
         SAC   512                                                              
*                                                                               
         J     JOBSQXXX                                                         
*                                                                               
JOBSQ090 MVC   PLINE(25),=C'INVALID SYSTEM XXXXXXXX//'                          
         MVC   PLINE+15(8),JOBSQ                                                
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         SAC   512                                                              
*                                                                               
JOBSQXXX SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DO USS QUEUE QUERY FOR SYSTEM IN USSQQ                              *         
***********************************************************************         
USSQQRY  NTR1                                                                   
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         USING DMSPACED,R2                                                      
         XR    R0,R0                                                            
USSQQ010 AHI   R2,64                                                            
         AHI   R0,1                                                             
         CHI   R0,255                                                           
         JNL   USSQQ090                                                         
         CLC   USSQQ,DSPNAME                                                    
         JNE   USSQQ010                                                         
*                                                                               
USSQQ020 MVC   PLINE(8),USSQQ                                                   
         MVC   PLINE+8(7),=C'USSQUE='                                           
         XC    UNXQKEY,UNXQKEY                                                  
         MVI   UNXQTYPE,UNXQTY00                                                
         L     R1,=A(SSB)                                                       
         USING SSBD,R1                                                          
         MVC   UNXQDSPC,SSODSPAC                                                
         STCM  R0,3,UNXQSE#                                                     
         DROP  R1                                                               
*                                                                               
         SAC   0                                                                
         GOTO1 =V(USSQINQ),DMCB,UNXQKEY                                         
         SAC   512                                                              
         JE    USSQQ022            QUEUE FOUND                                  
*                                                                               
         MVC   PLINE+15(7),=C'ERROR//' PRESET ERROR                             
         CLI   DMCB+12,3           TEST QUEUE DOES NOT EXIST ERROR              
         JNE   USSQQ030            NO, SO UNKNOWN ERROR                         
         MVC   PLINE+15(10),=C'INACTIVE//'                                      
         J     USSQQ030                                                         
*                                                                               
USSQQ022 MVC   PLINE+15(7),=C'EMPTY//'                                          
         LT    R1,DMCB+4                                                        
         JZ    USSQQ030                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLINE+15(6),DUB                                                  
         MVC   PLINE+21(2),=C'//'                                               
*                                                                               
USSQQ030 SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,(X'80',0)                                   
         SAC   512                                                              
*                                                                               
         J     USSQQXXX                                                         
*                                                                               
USSQQ090 MVC   PLINE(25),=C'INVALID SYSTEM XXXXXXXX//'                          
         MVC   PLINE+15(8),USSQQ                                                
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         SAC   512                                                              
*                                                                               
USSQQXXX SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK STATUS TABLES FOR DEAD LOCAL ENTRIES                          *         
***********************************************************************         
STATCHK  NTR1                                                                   
*                                                                               
         MVC   STATETIT(256),STATETIM   COPY MAIN TO TEMP                       
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         LAM   AR3,AR3,ALET                                                     
         LAM   AR4,AR4,ALET                                                     
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         XR    R4,R4                                                            
         SAC   512                                                              
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHASTATE      GET STATE AREA                               
         JZ    STATCHKX                                                         
         USING DSSHDR,R2                                                        
         ICM   R2,15,DSATSTAT      TASK AREAS                                   
         JZ    STATCHKX                                                         
         USING DSLSTATE,R2                                                      
*                                                                               
         LA    R0,DSLMAX           LOOP THROUGH ALL                             
STATCHK1 ICM   R3,15,DSLJOBD       ANY POINTER TO JOB ENTRY                     
         JZ    STATCHK9                                                         
*                                                                               
         USING DSJOBD,R3                                                        
         C     R2,DSJLOCAL         DOES BACKWARD POINTER MATCH                  
         JE    STATCHK9            OK GOOD                                      
*                                                                               
         ST    R2,SAVELOCL                                                      
         LA    R3,DSLLEN           NO MUST BE OLD - REMOVE IT                   
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
         L     R2,SAVELOCL                                                      
                                                                                
STATCHK9 AHI   R2,DSLLEN           CHECK EACH ENTRY                             
         JCT   R0,STATCHK1                                                      
                                                                                
***********************************************************************         
* CHECK STATUS TABLES. CHECK LOCAL STATES ARE CORRECT.                *         
***********************************************************************         
                                                                                
STATCH00 XR    R2,R2               NOW DO STATE CHECK                           
         XR    R3,R3                                                            
         USING DMDSHDR,R2                                                       
         ICM   R3,15,DHAJOBS       GET JOBS TABLE                               
         JZ    STATCHKX                                                         
         LA    R1,DSJOBMXQ         NUMBER OF POSSIBLE ENTRIES                   
         ST    R1,JOBMAX                                                        
         USING DSJOBD,R3                                                        
         ICM   R2,15,DHASTATE                                                   
         ICM   R2,15,DSAGSTAT-DSSHDR(R2)  R2=GLOBAL STATE TABLE                 
*                                                                               
         LA    R5,SAVE             COPY CURRENT STATES TO SAVE                  
         LA    R6,256                                                           
STATCH01 MVC   0(1,R5),0(R2)       COPY CURRENT STATE ONLY                      
*                                                                               
         CLI   1(R2),0             IF NEW STATE IS ZERO OK                      
         JE    STATCH02                                                         
         CLI   0(R2),0             IF STATE IS NOW ZERO NOT OK                  
         JNE   STATCH02                                                         
*                                                                               
         MVC   HALF,0(R2)          SEND WARNING TO LOG                          
         ST    R2,FULL             SAVE R2                                      
         STM   R5,R6,DUB           SAVE R5,R6                                   
         MVC   PLINE+2(32),STATEWR2                                             
         MVC   PLINE+0(2),=AL2(32)                                              
         LA    R2,256                                                           
         SR    R2,R6                                                            
         SLL   R2,6                                                             
         MVC   PLINE+13(8),DSPNAME-DSPHDR(R2)                                   
         MVC   PLINE+33(1),HALF+1                                               
         SAC   0                                                                
         WTO   TEXT=PLINE,MCSFLAG=HRDCPY                                        
         SAC   512                                                              
         L     R2,FULL             RESTORE R2                                   
         LM    R5,R6,DUB           RESTORE R5,R6                                
         MVC   0(1,R2),1(R2)       FIX STATE                                    
*                                                                               
STATCH02 LA    R5,1(R5)                                                         
         LA    R2,4(R2)                                                         
         JCT   R6,STATCH01         NEXT SYSTEM                                  
*                                                                               
STATCH10 ICM   R2,15,DSJLOCAL      LOCAL STATE ENTRY FOR JOB                    
         JZ    STATCH20            NO - SKIP IT (WARN MAYBE?)                   
         USING DSLSTATE,R2                                                      
         ICM   R4,15,DSLSTAT       R4=STATIC TABLE                              
         JZ    *+2                                                              
         USING DSSSTATE,R4                                                      
*                                                                               
         ST    R2,DUB              SAVE ADDRESS OF LOCAL AND STATIC             
         ST    R4,DUB+4                                                         
         LA    R2,DSLSYS           R2=LOCAL SYS TABLE                           
         LA    R4,DSSSYS           R4=STATIC SYS TABLE                          
         DROP  R2,R4                                                            
*                                                                               
         LA    R5,SAVE             R5=GLOBAL TABLE COPY                         
         LA    R6,256              DO FOR EACH SYSTEM                           
*                                                                               
STATCH11 LA    RE,0                                                             
         CLI   0(R5),C'O'          OPEN (0)                                     
         JE    STATCH12                                                         
         LA    RE,1                                                             
         CLI   0(R5),C'R'          READ (1)                                     
         JE    STATCH12                                                         
         LA    RE,2                                                             
         CLI   0(R5),C'C'          CLOSE (2)                                    
         JE    STATCH12                                                         
         LA    RE,3                                                             
         CLI   0(R5),C'U'          USER (3)                                     
         JE    STATCH12                                                         
         J     STATCH19                                                         
*                                                                               
STATCH12 AR    R4,RE               INDEX TO STATIC STATE                        
         MVC   BYTE,0(R4)                                                       
         MVC   BYTE1,0(R2)                                                      
         SR    R4,RE               SET R4 BACK TO TABLE HEAD                    
         CLC   BYTE,BYTE1          CORRECT STATE                                
         JE    STATCH19                                                         
         CLI   BYTE,0              IF STATIC DEFINED AS 0                       
         JNE   *+12                                                             
         CLI   BYTE1,C'C'          CLOSE IS AN OK MATCH                         
         JE    STATCH19                                                         
*                                                                               
         ST    R2,SAVELOCL         SAVE A(LOCAL)                                
*                                                                               
         MVC   PLINE+2(56),STATEWRN                                             
         MVC   PLINE+0(2),=AL2(56)                                              
         L     R2,DUB                                                           
         MVC   PLINE+13(16),0(R2)                                               
         LA    R2,256                                                           
         SR    R2,R6                                                            
         SLL   R2,6                                                             
         MVC   PLINE+30(8),DSPNAME-DSPHDR(R2)                                   
*                                                                               
         LA    R1,STATES                                                        
STATCH13 CLI   0(R1),NO                                                         
         JE    STATCH14                                                         
         CLC   0(1,R1),BYTE1                                                    
         JE    STATCH14                                                         
         LA    R1,6(R1)                                                         
         J     STATCH13                                                         
*                                                                               
STATCH14 MVC   PLINE+38(6),0(R1)                                                
*                                                                               
         LA    R1,STATES                                                        
STATCH15 CLI   0(R1),NO                                                         
         JE    STATCH16                                                         
         CLC   0(1,R1),BYTE                                                     
         JE    STATCH16                                                         
         LA    R1,6(R1)                                                         
         J     STATCH15                                                         
*                                                                               
STATCH16 MVC   PLINE+52(6),0(R1)                                                
*                                                                               
         XR    R0,R0               SET BYTE TO HOUR                             
         L     R1,TIMENOW                                                       
         D     R0,=A(60*60*100)                                                 
         STC   R1,BYTE                                                          
*                                                                               
         LR    R1,R5               CALCULATE OFFSET IN SAVE                     
         LA    RF,SAVE                                                          
         SR    R1,RF                                                            
         LA    R1,STATETIM(R1)     APPLY OFFSET TO STATETIM                     
*                                                                               
         CLC   BYTE,0(R1)                                                       
         JNH   STATCH18                                                         
*                                                                               
         LR    R1,R5               CALCULATE OFFSET IN SAVE                     
         LA    RF,SAVE                                                          
         SR    R1,RF                                                            
         LA    R1,STATETIT(R1)     APPLY OFFSET TO STATETIT                     
         MVC   0(1,R1),BYTE        SET HOUR IN TEMP COPY                        
*                                                                               
STATCH17 SAC   0                                                                
         WTO   TEXT=PLINE,MCSFLAG=HRDCPY                                        
         SAC   512                                                              
*                                                                               
STATCH18 L     R2,SAVELOCL         RESTORE A(LOCAL)                             
*                                                                               
STATCH19 LA    R5,1(R5)            BUMP GLOBAL                                  
         LA    R4,4(R4)            BUMP STATIC                                  
         LA    R2,2(R2)            BUMP LOCAL                                   
         JCT   R6,STATCH11         NEXT SYS                                     
*                                                                               
STATCH20 AHI   R3,DSJOBLNQ                                                      
         L     R1,JOBMAX                                                        
         AHI   R1,-1                                                            
         ST    R1,JOBMAX                                                        
         OC    JOBMAX,JOBMAX                                                    
         JNZ   STATCH10            NEXT JOB                                     
*                                                                               
STATCHKX BRAS  RE,ARSOFF                                                        
*                                                                               
         MVC   STATETIM(256),STATETIT   COPY TEMP TO MAIN                       
         B     EXITOK                                                           
         DROP  R3                                                               
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
* SETWAIT - SET TIMER AND WAIT 2 MINUTES                              *         
* RETURN WHEN AN INTERUPT IS DETECTED                                 *         
***********************************************************************         
SETWAIT  NTR1  ,                                                                
*                                                                               
         SR    R0,R0                                                            
         LHI   R1,10               1/10 SECOND ONLY                             
         ST    R1,TIME                                                          
         XC    TASKECB,TASKECB                                                  
         STIMERM SET,ID=STIMERID,BINTVL=TIME,EXIT=TASKXIT                       
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         JNZ   *+2                                                              
*                                                                               
         CLI   TODSET,YES                                                       
         BE    WB10                                                             
         XC    TODECB,TODECB                                                    
         STIMERM SET,ID=TODID,LT=MIDNIGHT,EXIT=TODXIT                           
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         JNZ   *+2                                                              
         MVI   TODSET,YES                                                       
*                                                                               
WB10     WAIT  1,ECBLIST=ECBLST2   WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         JO    WB12                                                             
         TM    TODECB,X'40'        Was it the stroke of midnight?               
         JO    WB12                Yes, so cancel STIMERID for now              
         TM    DSPACECB,X'40'      OR DATASPACE COMMAND                         
         JNO   WB15                NO                                           
*                                                                               
WB12     STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         JNZ   *+2                 UNSUCCESSFUL TIMER CANCEL                    
                                                                                
*********************************************************************           
* Process the operator command                                                  
*********************************************************************           
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         JZ    WB15                                                             
         WTO   'OPER INTERRUPT',MCSFLAG=HRDCPY                                  
*                                                                               
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
*                                                                               
         CLC   JOBSQ,=C'XXXXXXXX'  ANY JOBS QUERY                               
         JE    WB14                                                             
         BRAS  RE,JOBSQRY          DO JOB QUERY OUTPUT                          
         MVC   JOBSQ,=C'XXXXXXXX'  DONE                                         
*                                                                               
WB14     CLC   USSQQ,=C'XXXXXXXX'  ANY USS QUEUE QUERY                          
         JE    WB14X                                                            
         BRAS  RE,USSQQRY          DO USS QUEUE QUERY OUTPUT                    
         MVC   USSQQ,=C'XXXXXXXX'  DONE                                         
WB14X    B     SETWAITX                                                         
                                                                                
*********************************************************************           
* Process midnight command                                                      
*********************************************************************           
WB15     TM    TODECB,X'40'        WAS IT MIDNIGHT?                             
         JZ    WB20                NO                                           
         MVI   TODSET,NO           Set to NO for next time in                   
         BRAS  RE,SINSET                                                        
         B     SETWAITX                                                         
                                                                                
*********************************************************************           
* Process Task that are set process this time around                            
*********************************************************************           
WB20     TM    TASKECB,X'40'       DID THE TIMER POP?                           
         BO    SETWAITX            YES, so process the tasks at hand            
                                                                                
*********************************************************************           
* Process data space commands                                                   
*********************************************************************           
         TM    DSPACECB,X'40'      DO WE HAVE A DATASPACE COMMAND               
         JZ    *+2                                                              
         WTO   'DSPACECB POST',MCSFLAG=HRDCPY                                   
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,DHACOMM-DMDSHDR(R2)                                        
         AHI   R2,4096             SKIP HEADERS         Why 4096 ?              
         LHI   R0,512              UP TO 512 COMMANDS   add equate              
*                                                                               
         USING DSCOMM,R2                                                        
TSTOPS62 TM    DSCFLAG,DSCDONEQ    IGNORE PROCESSED                             
         JO    TSTOPS86                                                         
         CLC   MYASID,DSCDEST      IGNORE IF NOT FOR ME                         
         JNE   TSTOPS86                                                         
         CLC   DSCCOMM,=AL2(15)    STATUS IS ALL I'LL ACCEPT                    
         JNE   TSTOPS84                                                         
*                                                                               
         MVC   STATSYS,DSCDATA+1   SET SE# IN STATSYS                           
         LA    R1,STATSYS                                                       
         BRAS  RE,DISSYS                                                        
         MVC   FSTATS,0(R1)        COPY NAME TO FSTATS                          
         OI    DSCFLAG,DSCDONEQ    FLAG IT AS DONE                              
         SAC   0                                                                
         J     SETWAITX                                                         
*                                                                               
TSTOPS84 OI    DSCFLAG,DSCDONEQ    FLAG IT AS DONE                              
                                                                                
         CLC   =X'FFFE',DSCSORC    WAS IT DDOPER                                
         JE    TSTOPS86                                                         
         XC    0(L'DSCOMMS,R2),0(R2)                                            
                                                                                
TSTOPS86 AHI   R2,L'DSCOMMS        POINT TO NEXT COMMS ENTRY                    
         JCT   R0,TSTOPS62         NEXT COMMAND                                 
         MVI   DSPACECB,0                                                       
         SAC   0                                                                
*                                                                               
SETWAITX MVC   TIMEOLD,TIMENOW     SET THE PREVIOUS TIME                        
         MVC   DATEOLD,DATENOW                                                  
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R1,DATENOW                                                       
         ST    R0,TIMENOW                                                       
*                                                                               
         CLC   DATEOLD,DATENOW     HAS DATE CHANGED                             
         BE    EXITOK                                                           
*                                                                               
         GOTO1 =V(DATCON),DMCB,(5,0),(0,DUB)                                    
         GOTO1 =V(GETDAY),DMCB,(0,DUB),FULL                                     
         MVC   DAYNOW,0(R1)                                                     
         GOTO1 =V(DATCON),DMCB,(5,0),(1,DUB)                                    
         MVC   DATNOW(1),DUB+2                                                  
         MVC   MONNOW(1),DUB+1                                                  
         B     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        Set SIN to 1 if it is midnight                     *                   
*************************************************************                   
SINSET   NTR1                                                                   
*&&US*&& TIME  DEC,TIMEDATE,DATETYPE=YYYYMMDD,LINKAGE=SYSTEM                    
*&&UK*&& TIME  DEC,TIMEDATE,DATETYPE=DDMMYYYY,LINKAGE=SYSTEM                    
         XR    R2,R2                                                            
         LAM   AR2,AR2,TBLET                                                    
         SAC   512                                                              
         USING FATABSD,R2                                                       
         L     R2,TABSCOMM                                                      
         USING TCOMMOND,R2                                                      
         CLC   SIN_DATE,TCOTODAY                                                
         BE    SINSETX                                                          
         MVC   TCOTODAY,SIN_DATE         Set to new day                         
*                                                                               
SINSET10 LA    RF,1                                                             
         L     R1,TCORSIN                                                       
         CS    R1,RF,TCORSIN             Reset to 1, start at 2                 
         BNE   SINSET10                                                         
*                                                                               
         SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         MVC   PLINE(20),=C'GLOBAL SIN RESET. //'                               
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         B     EXITOK                                                           
*                                                                               
SINSETX  SAC   0                                                                
         LAM   AR2,AR2,ARZERO                                                   
         B     EXITOK                                                           
*                                                                               
TIMEDATE DS    0CL16                                                            
         DS    CL8                       HHMMSSthmiju0000                       
SIN_DATE DS    CL4                       DATE YYYYMMDD or DDMMYYYY              
         DS    CL4                                                              
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
*                                                                               
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CARD(0),CIBDATA                                                  
*                                                                               
         LA    R1,CARD                                                          
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         BE    CHEK020             NEQ SO TRY GROUP COMMAND                     
         MVC   PLINE(32),=C'INVALID KEYWORD               //'                   
         MVC   PLINE+16(13),CARD                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         B     CHEKRSET                                                         
*                                                                               
CHEK020  EQU   *                                                                
*                                                                               
CHEKRSET L     RF,ACOMM            RESET OPER COMMS                             
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         BZ    EXITOK                                                           
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        DISPLAY JOB NUMBER OR ADV TASK FROM FULL TO DUB1   *                   
*************************************************************                   
DISJOB   TM    FULL+2,X'80'        TEST OFFLINE JOB                             
         BO    DJOB085                                                          
         CLI   FULL+3,0                                                         
         BE    *+14                                                             
         MVC   FULL+0(1),FULL+3                                                 
         MVI   FULL+1,0                                                         
         MVC   BYTE,FULL+0                                                      
*                                                                               
         USING FACITABD,R1                                                      
         NI    BYTE,X'0F'                                                       
         LLC   R1,BYTE                                                          
         MHI   R1,FACITLNQ                                                      
         A     R1,AFACITAB         Base A(FACIDTAB)                             
         CLC   BYTE,FACIID         FIND ADV SYSTEM                              
         JNE   *+2                                                              
*                                                                               
         CLC   FACISN4,SPACES                                                   
         JNH   *+2                 Something is wrong                           
         MVC   DUB1(4),FACISN4                                                  
         DROP  R1                                                               
*                                                                               
         CLI   DUB1+3,C' '                                                      
         BE    *+14                                                             
         MVC   DUB1+2(1),DUB1+3                                                 
         MVI   DUB1+3,C' '                                                      
*                                                                               
         MVC   DUB1+4(2),=C'/#'                                                 
         MVC   DUB1+6(1),FULL+1                                                 
         MVC   FULL+3(1),FULL+0                                                 
*                                                                               
         TM    FULL+3,X'F0'                                                     
         BZR   RE                                                               
         SR    R1,R1                                                            
         IC    R1,FULL+3                                                        
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,DUB1+3                                                        
         BR    RE                                                               
*                                                                               
DJOB085  MVC   DUB1,=C'J       '   JOB NO                                       
         SR    R1,R1                                                            
         ICM   R1,3,FULL                                                        
         N     R1,=X'0000FFFF'                                                  
         EDIT  (R1),(5,DUB1+1),FILL=0                                           
         BR    RE                                                               
*************************************************************                   
*        END OF JOB                                         *                   
*************************************************************                   
ENDOFJOB EQU   *                                                                
         BRAS  RE,PRINTX                                                        
         CLOSE TRACELOG                                                         
         CLOSE RUNLOG                                                           
         WTO   'END OF MAINTENANCE JOB ',MCSFLAG=HRDCPY                         
         B     XBASE                                                            
         EJECT                                                                  
*************************************************************                   
*        PRINT ROUTINES                                     *                   
*************************************************************                   
PRINTI   ST    RE,SAVERE                                                        
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
         PUT   SYSPRINT,T1         PRINT TITLE                                  
         PUT   SYSPRINT,T1                                                      
         PUT   SYSPRINT,T1                                                      
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
*************************************************************                   
*        PARAMETER CARDS AND HANDLING ROUTINE               *                   
*************************************************************                   
*                                                                               
*        CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)             
*                                                                               
*FLAGS   X'8000'                   A(OUTPUT) IS A(ROUTINE)                      
*        X'4000'                   ACCEPT =,/=                                  
*        X'2000'                   ACCEPT <,>,<=,=>                             
*        X'1000'                   HEX VALUE                                    
*        X'0800'                   DEC VALUE                                    
*        X'0400'                   OUTPUT IS A LIST                             
*        X'0200'                   TIME VALUE                                   
*        X'0100'                   DATE VALUE                                   
*        X'0080'                   NO DELIMETER KEYWORD ONLY                    
***********************************************************************         
                                                                                
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,10,00),X'0000',AL3(DDSIO)                       
         DC    C'MODE   ',AL1(3,10,00),X'0000',AL3(MODE)                        
         DC    C'DSPACE ',AL1(5,01,00),X'0000',AL3(SSB+SSODSPAC-SSOOFF)         
         DC    C'ESTAE  ',AL1(4,01,00),X'0000',AL3(ESTAESW)                     
         DC    C'RUNQ   ',AL1(3,01,00),X'0000',AL3(RUNQ)                        
         DC    C'MONSTER',AL1(6,03,00),X'0000',AL3(MONSTER)                     
         DC    C'RQSTAT ',AL1(5,01,00),X'0000',AL3(RQSTAT)                      
         DC    C'BILLDAT',AL1(6,01,00),X'0000',AL3(BILLDAT)                     
         DC    C'STATS  ',AL1(4,08,00),X'8000',AL3(VALSTAT)                     
         DC    C'SUSPEND',AL1(6,01,00),X'0000',AL3(SUSPEND)                     
         DC    C'STATE  ',AL1(4,01,00),X'8080',AL3(DOSTATE)                     
         DC    C'ABEND  ',AL1(4,01,00),X'8080',AL3(DOABEND)                     
         DC    C'JOBS   ',AL1(3,07,00),X'0000',AL3(JOBSQ)                       
         DC    C'USSQ   ',AL1(3,07,00),X'0000',AL3(USSQQ)                       
         DC    C'CTBUFF ',AL1(5,01,00),X'8000',AL3(VALCTBUF)                    
*&&US                                                                           
         DC    C'DEMOSMF',AL1(6,03,00),X'8000',AL3(VALDSMF)                     
         DC    C'DEMDIRA',AL1(6,01,00),X'8000',AL3(VALDEM)                      
         DC    C'DEMDIRN',AL1(6,01,00),X'8000',AL3(VALDEM)                      
         DC    C'DEMDIRR',AL1(6,01,00),X'8000',AL3(VALDEM)                      
         DC    C'NTIDIR ',AL1(5,01,00),X'8000',AL3(VALDEM)                      
         DC    C'PAVDIR ',AL1(5,01,00),X'8000',AL3(VALDEM)                      
         DC    X'0000'                                                          
*&&                                                                             
DOSTATE  MVI   TIMESTAT,YES                                                     
         XC    TIMESTAT+4(4),TIMESTAT+4                                         
         BR    RE                                                               
*                                                                               
DOABEND  MVI   ABEND,YES                                                        
         BR    RE                                                               
***************************************************                             
*        CARD OUTPUT AREAS SET WITH DEFAULTS      *                             
***************************************************                             
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
DDSIO    DC    CL10'DDSIO'                                                      
MODE     DC    CL10'INIT'          DEFAULT TO INIT                              
ESTAESW  DC    AL1(YES)                                                         
MONSTER  DC    CL3'OLD'            DEFAULT TO OLD                               
SYLIST   DC    AL1(NO)                                                          
SYSTEM   DC    X'00'                                                            
RUNQ     DC    AL1(YES)                                                         
RQSTAT   DC    AL1(NO)                                                          
BILLDAT  DC    AL1(NO)                                                          
FSTATS   DC    CL8'NO'                                                          
SUSPEND  DC    AL1(NO)             Suspend stats (FSTAT)                        
JOBSQ    DC    CL8'XXXXXXXX'                                                    
USSQQ    DC    CL8'XXXXXXXX'                                                    
*&&US                                                                           
SOONCPY  DC    AL1(NO)                                                          
*&&                                                                             
*&&UK                                                                           
SOONCPY  DC    AL1(YES)                                                         
*&&                                                                             
TODSET   DC    AL1(NO)                                                          
*                                                                               
         EJECT                                                                  
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
*                                                                               
VALC001  LA    R4,CARDTAB                                                       
         USING CARDD,R4                                                         
*                                                                               
         ST    R2,CARDR2                                                        
VALC010  SR    R1,R1               GET LEN FOR COMPARE                          
         IC    R1,CDKYLEN                                                       
         EX    R1,*+8              EXECUTE KEYWORD TEST                         
         B     *+10                                                             
         CLC   0(0,R2),CDKYWORD                                                 
         BE    VALC020                                                          
         AHI   R4,CARDLNQ          TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         BNE   VALC010                                                          
         B     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALC020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         TM    CDFLAGS2,CDF2DOIT   IF NO DELIMENTER JUST DO IT                  
         JO    VALC025                                                          
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
         NC    BYTE,CDFLAGS1                                                    
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
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,CDFUNCT        GET ADDRESS FOR MOVE                         
*                                                                               
         TM    CDFLAGS1,CDF1ROUT   IF ROUTINE                                   
         BZ    *+10                                                             
         BASR  RE,RF               GOTO ROUTINE                                 
         B     VALC500                                                          
*                                                                               
         TM    CDFLAGS1,CDF1LIST   IF LIST                                      
         BNO   VALC050                                                          
VALC040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         BE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         BE    VALC050                                                          
         CLC   0(2,RF),=C'  '      EMPTY ENTRY                                  
         BE    VALC050                                                          
         SR    R0,R0                                                            
         IC    R0,CDFLDLN                                                       
         AR    RF,R0                                                            
         TM    CDFLAGS1,CDF1OPER      /<=>                                      
         BZ    VALC040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         B     VALC040                                                          
*                                                                               
VALC050  TM    CDFLAGS1,CDF1OPER   IF /<=>                                      
         BZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    CDFLAGS1,CDF1IHEX   HEX INPUT                                    
         BNO   VALC060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 =V(HEXIN),DMCB,(R2),(RF),(R0)                                    
         ICM   R1,15,12(R1)                                                     
         BZ    CERRHEX                                                          
         B     VALC500                                                          
*                                                                               
VALC060  TM    CDFLAGS1,CDF1DEC       DEC INPUT                                 
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
VALC070  TM    CDFLAGS1,CDF1TIME   TIME INPUT                                   
         BZ    VALC080                                                          
         BRAS  RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         B     VALC500                                                          
*                                                                               
VALC080  TM    CDFLAGS1,CDF1DATE   DATE INPUT                                   
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
VALC400  CLI   CDFLDLN,0           DONT CARE. JUST USE LENGTH                   
         BE    VALC410                                                          
         CLM   R1,1,CDFLDLN        CHECK MAX LEN                                
         BH    CERRMAX                                                          
         SR    RE,RE                                                            
         IC    RE,CDFLDLN          PAD OUT TO SPACES                            
         BCTR  RE,0                                                             
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
         DROP  R4                                                               
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
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
*************************************************************                   
*   GET SENUM FROM DMGR dataspace                                               
*   For now must be in AR mode with AR2 set to DMGR DS                          
*   R1 point to field with system name                                          
*   Returns SE# in R1 or zero                                                   
*************************************************************                   
VALSYS   NTR1                                                                   
         LR    RF,R1               R1 point to System name                      
         LA    R0,255              Max systems                                  
         LA    R1,1                Start with SE#=1                             
         XR    R2,R2                                                            
         AHI   R2,L'DMDHDR         Point to first system                        
*                                                                               
         USING DMSPACED,R2                                                      
VALSYS2  CLC   0(7,RF),DSPNAME     Match on system                              
         JE    VALSYSX                                                          
         AHI   R1,1                Count by 1 for SE# entry                     
         AHI   R2,L'DSPHDR         Next system                                  
         JCT   R0,VALSYS2                                                       
         XR    R1,R1                                                            
*                                                                               
VALSYSX  XIT1  REGS=(R1)                                                        
         DROP  R2                                                               
                                                                                
*************************************************************                   
*   Get System name from DMGR dataspace                                         
*************************************************************                   
DISSYS   NTR1                                                                   
         LLC   RF,0(R1)            GET SE# in RF                                
         CLI   0(R1),X'FF'                                                      
         JNE   DISSYS2                                                          
         MVC   0(3,R1),=C'ALL'                                                  
         J     EXITOK                                                           
*                                                                               
DISSYS2  XR    R2,R2                                                            
         MHI   RF,L'DMDHDR         Base 1 since 1st entry is unrelated          
         AR    R2,RF                 but same length.                           
         USING DMSPACED,R2                                                      
         MVC   0(8,R1),DSPNAME     Return name                                  
         B     EXITOK                                                           
         DROP  R2                                                               
*************************************************************                   
*        VALIDATE STATS COMMAND                             *                   
*        R1 = LENGTH INPUT STRING                           *                   
*        R2 = A(INPUT STRING)                               *                   
* SUSPENDS BOTH FILE SPACE AND STATS CALL TO FILMAN         *                   
*************************************************************                   
VALSTAT  NTR1                                                                   
         MVC   FSTATS,SPACES                                                    
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   FSTATS(0),0(R2)                                                  
         CLI   SUSPEND,NO          IS SUSPEND ON?                               
         BE    EXITOK              NO, SO DON'T CARE                            
         CLI   FSTATS,YES          DID THEY REQUEST TO REPORT STATS?            
         BNE   EXITOK              DON'T CARE THEN                              
         WTO   'STATS SUPPRESSED WITH SUSPEND=Y',MCSFLAG=HRDCPY                 
         B     EXITOK                                                           
         EJECT ,                                                                
*&&US                                                                           
*************************************************************                   
*        VALIDATE DEMO ACCESS METHOD COMMANDS               *                   
*        R1 = LENGTH INPUT STRING                           *                   
*        R2 = A(INPUT STRING)                               *                   
*        R4 = A(CARDTAB ENTRY)                              *                   
* SETS A DEMOFILE STATUS FLAG IN THE DATAMGR DATASPACE      *                   
*************************************************************                   
VALDEM   NTR1                                                                   
*                                                                               
         MVC   PLINE(30),=C'XXXXXXX STATUS SET TO XXXXX //'                     
         MVC   PLINE(7),0(R4)        SET FILENAME IN CONSOLE MESSAGE            
         SELECT CLI,0(R2),EQ         CHECK FOR FILE=ACCESSMETHOD                
           WHEN (C'D')                                                          
             MVC PLINE+22(5),=C'DANDX'                                          
           WHEN (C'V')                                                          
             MVC PLINE+22(5),=C'VSAM '                                          
           WHEN (C'X')                                                          
             MVC PLINE+22(5),=C'XVSAM'                                          
           OTHRWISE                                                             
             MVC   PLINE,SPACES                                                 
             MVC   PLINE(19),=C'INVALID OPTION   //'                            
             MVC   PLINE+15(1),0(R2)                                            
             GOTO1 =V(DDWTO),DMCB,PLINE,0                                       
             B     EXITOK            GET OUT NOW                                
         ENDSEL                                                                 
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         SAC   512                                                              
         LR    R3,R2                 R3 = A(INPUT CHARACTER, E.G. "V")          
*                                                                               
         SR    R2,R2                                                            
         L     R2,DHASSBG-DMDHDR(,R2)    A(OFFLINE SSB IN DATASPACE)            
         USING FASSBG,R2                                                        
         SELECT CLC,CDKYWORD-CARDD(,R4),EQ   SET CORRESPONDING FLAG             
           WHEN (=C'DEMDIRA')                                                   
             MVC SSGDMDRA,0(R3)                                                 
           WHEN (=C'DEMDIRN')                                                   
             MVC SSGDMDRN,0(R3)                                                 
           WHEN (=C'DEMDIRR')                                                   
             MVC SSGDMDRR,0(R3)                                                 
           WHEN (=C'NTIDIR ')                                                   
             MVC SSGDMNTI,0(R3)                                                 
           WHEN (=C'PAVDIR ')                                                   
             MVC SSGDMPAV,0(R3)                                                 
           OTHRWISE                                                             
             J *+2                   HOW DID WE GET HERE ?!?                    
         ENDSEL                                                                 
         DROP  R2                                                               
*                                                                               
         SAC   0                                                                
         GOTO1 =V(DDWTO),DMCB,PLINE,0  WRITE REASSURING MSG TO CONSOLE          
*                                                                               
         B     EXITOK                                                           
*&&                                                                             
         EJECT ,                                                                
*************************************************************                   
*        VALIDATE DEMOSMF SWITCH                            *                   
*        R1 = Length input string                           *                   
*        R2 = A(input string)                               *                   
*        R4 = A(CARDTAB entry)                              *                   
* Sets the demo directory SFM logging flag in global SSB    *                   
*************************************************************                   
*&&US                                                                           
VALDSMF  NTR1  ,                                                                
*                                                                               
         MVC   PLINE(29),=C'Demos SMF logging xxxxxxxx //'                      
         SELECT CLC,0(3,R2),EQ         check command parameter                  
           WHEN (=C'ON ')                                                       
             MVC   PLINE+18(8),=C'enabled '                                     
             MVI   BYTE1,C'Y'                                                   
             NEXTWHEN ,                                                         
           WHEN (=C'OFF')                                                       
             MVC   PLINE+18(8),=C'disabled'                                     
             MVI   BYTE1,C'N'                                                   
             NEXTWHEN ,                                                         
           WHEN (=C'ON ',=C'OFF')                                               
             LAM   AR2,AR2,ALET                                                 
             SAC   512                                                          
             SR    R2,R2                                                        
             L     R2,DHASSBG-DMDHDR(,R2)   A(offline SSB in dataspace)         
             MVC   SSGDMSMF-FASSBG(,R2),BYTE1                                   
             SAC   0                                                            
             GOTO1 =V(DDWTO),DMCB,PLINE,0                                       
           OTHRWISE                                                             
             MVC   PLINE,SPACES                                                 
             MVC   PLINE(54),=C'INVALID INPUT: DEMOSMF OPTION MUST BE "+        
               ON" OR "OFF" //'                                                 
             GOTO1 =V(DDWTO),DMCB,PLINE,0                                       
             J     EXITL                                                        
         ENDSEL                                                                 
                                                                                
         J     EXITOK                                                           
*&&                                                                             
         EJECT ,                                                                
*************************************************************                   
*        VALIDATE CTBUFF switch                             *                   
*        R1 = Length input string                           *                   
*        R2 = A(input string)                               *                   
*        R4 = A(CARDTAB entry)                              *                   
* Sets TCOCTBFL flag to disable/enable CTBUFF buffer search *                   
*************************************************************                   
VALCTBUF NTR1  ,                                                                
*                                                                               
         MVC   PLINE(32),=C'CTBUFF buffer search xxxxxxxx //'                   
         SELECT CLC,0(3,R2),EQ         check command parameter                  
           WHEN (=C'ON ')                                                       
             MVC   PLINE+21(8),=C'enabled '                                     
             MVI   BYTE1,0                                                      
             NEXTWHEN ,                                                         
           WHEN (=C'OFF')                                                       
             MVC   PLINE+21(8),=C'disabled'                                     
             MVI   BYTE1,TCOCTBXQ                                               
             NEXTWHEN ,                                                         
           WHEN (=C'ON ',=C'OFF')                                               
             LAM   AR2,AR2,TBLET                                                
             SAC   512                                                          
             XR    R2,R2                                                        
             L     R2,TABSCOMM-FATABSD(,R2) A(TABS common area)                 
             NI    TCOCTBFL-TCOMMOND(R2),255-TCOCTBXQ Clear CTBUFF flag         
             OC    TCOCTBFL-TCOMMOND(,R2),BYTE1 Set to new value                
             BRAS  RE,ARSOFF                                                    
             GOTO1 =V(DDWTO),DMCB,PLINE,0                                       
           OTHRWISE                                                             
             MVC   PLINE,SPACES                                                 
             MVC   PLINE(53),=C'INVALID INPUT: CTBUFF OPTION MUST BE "O+        
               N" OR "OFF" //'                                                  
             GOTO1 =V(DDWTO),DMCB,PLINE,0                                       
             J     EXITL                                                        
         ENDSEL                                                                 
                                                                                
         J     EXITOK                                                           
         EJECT ,                                                                
*************************************************************                   
*        Common BXLE set up  6 bytes prior to table -       *                   
*        AL2(Table entry length)                            *                   
*        AL4(End of table-1)                                *                   
*************************************************************                   
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LOCK TABLES                                                         *         
***********************************************************************         
LOCK     NTR1  ,                                                                
         BRAS  RE,LOCKCLS          LOCK CLASS TABLE                             
         BRAS  RE,LOCKJOB          LOCK JOBTAB                                  
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE TABLES                                                         *         
***********************************************************************         
UNLK     NTR1  ,                                                                
         BRAS  RE,FREEJOB          UNLK JOBTAB                                  
         BRAS  RE,FREECLS          UNLK CLASS TABLE                             
         BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET COPY OF JOB TABLE HEADER INTO JTHDR                             *         
***********************************************************************         
ENQJOB   NTR1  ,                                                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'20'          SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         MVC   JTHDR,0(RF)                                                      
         NC    JT.DSPTFRST,=XL4'3FFFFFFF'                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET COPY OF CLASS TABLE HEADER INTO CLHDR                           *         
***********************************************************************         
ENQCLS   NTR1  ,                                                                
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'20'          SET ENQUIRE                                  
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         MVC   CLHDR,0(RF)                                                      
         NC    JT.DSPTFRST,=XL4'3FFFFFFF'                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCK JOB TABLE AND GET COPY OF HEADER                               *         
***********************************************************************         
LOCKJOB  NTR1  ,                   GET JOB TABLE HEADER INTO DSHDR              
         CLI   LOCKEDJB,YES                                                     
         JE    EXITOK                                                           
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'80'          SET LOCK                                     
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         MVC   JTHDR,0(RF)                                                      
         NC    JT.DSPTFRST,=XL4'3FFFFFFF'                                       
         MVI   LOCKEDJB,YES                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LOCK CLASS TABLE AND GET COPY OF HEADER                             *         
***********************************************************************         
LOCKCLS  NTR1  ,                   LOCK CLASS TABLE                             
         CLI   LOCKEDCL,YES                                                     
         JE    EXITOK                                                           
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         MVI   DMCB,X'80'          SET LOCK                                     
         GOTO1 ALOCKSPC,DMCB                                                    
         ICM   RF,15,4(R1)                                                      
         MVC   CLHDR,0(RF)                                                      
         NC    CL.DSPTFRST,=XL4'3FFFFFFF'                                       
         MVI   LOCKEDCL,YES                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE JOB TABLE                                                      *         
***********************************************************************         
FREEJOB  NTR1  ,                   FREE JOB TABLE                               
         CLI   LOCKEDJB,YES                                                     
         JE    FREEJOB2                                                         
         WTO   '**WARN** JOB TABLE WAS NOT LOCKED',MCSFLAG=HRDCPY               
*                                                                               
FREEJOB2 XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'10'          SET UNLOCK                                   
         GOTO1 ALOCKSPC,DMCB                                                    
         MVI   LOCKEDJB,NO                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE CLASS TABLE                                                    *         
***********************************************************************         
FREECLS  NTR1  ,                   FREE CLASS TABLE                             
         CLI   LOCKEDCL,YES                                                     
         JE    FREECLS2                                                         
         WTO   '**WARN** CLASS TABLE WAS NOT LOCKED',MCSFLAG=HRDCPY             
*                                                                               
FREECLS2 XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=AL4(DTDCLASS)                                           
         MVI   DMCB,X'10'          SET UNLOCK                                   
         GOTO1 ALOCKSPC,DMCB                                                    
         MVI   LOCKEDCL,NO                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE TO CONSOLE                                         *         
* NTRY   R0 = A(MESSAGE HEADER TO WRITE)                              *         
***********************************************************************         
WTO      NTR1  ,                                                                
         WTO   TEXT=(R0),MCSFLAG=HRDCPY                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE                                                          *         
***********************************************************************         
INIT     NTR1  ,                                                                
*                                                                               
         LA    R2,FULL                                                          
         EXTRACT (2),'S',FIELDS=(ASID)                                          
         MVC   MYASID,FULL+2                                                    
*                                                                               
         L     R1,=A(CIREC-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ACIREC                                                        
*                                                                               
         L     R1,=A(SAVEJOB-WORKD)                                             
         AR    R1,RC                                                            
         ST    R1,ASAVEJOB                                                      
*                                                                               
         LA    R3,CARD                                                          
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT04                                                           
*                                                                               
         L     RF,=V(DDSIO)        SET UP DDSIO FIRST                           
         MVC   0(8,RF),DDSIO                                                    
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         CLC   0(7,R3),=C'**FIL**' PASS THESE CARDS ON TO FILMAN                
         BE    INIT04                                                           
*                                                                               
INIT03   LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT02                                                           
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT04   OPEN  (RUNLOG,OUTPUT)     OPEN RUNLOG                                  
         OPEN  (TRACELOG,OUTPUT)   OPEN TRACE LOG                               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 ALOCKSPC,DMCB,X'00030000',0,ADSPECB,AOPERECB                     
*                                                                               
*&&UK                                                                           
         XC    DMCB(24),DMCB       ENQUIRE ON JOB TABLE                         
         MVC   DMCB(4),=AL4(DTJOB)                                              
         MVI   DMCB,X'20'          TO ALLOCATE SSOTBLET                         
         GOTO1 ALOCKSPC,DMCB                                                    
*&&                                                                             
*                                                                               
         L     RF,=A(SSB)                                                       
         USING SSBD,RF                                                          
         MVC   ALET,SSOALET                                                     
         MVC   TBLET,SSOTBLET                                                   
         MVC   DSPACE,SSODSPAC                                                  
         MVC   AFACITAB,SSOAFID    FACIDTAB entry                               
*&&DO                              Work done in DDSIO                           
         USING FACIDD,RF                                                        
         L     RF,AFACITAB                                                      
INIT05   CLI   0(RF),X'FF'         End of table?                                
         JE    *+2                 Invalid DSPACE DIE                           
         CLC   FACIDSPC,DSPACE     Match on dataspace                           
         BE    INIT06                                                           
         AHI   RF,FACIDLNQ                                                      
         B     INIT05                                                           
INIT06   MVC   AFACITAB,FACAID     A(FACIDxxx) table                            
*&&                                                                             
*                                                                               
         XC    DMCB,DMCB           DO SYSFLES call to get table                 
         MVI   DMCB+11,1           Get first list                               
         GOTO1 ADATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         ICM   R1,15,12(R1)                                                     
         JZ    *+2                 Bad call or something                        
         ST    R1,ASYSFLES                                                      
*                                                                               
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DMCB+16                                                    
         BZ    INIT10                                                           
*                                                                               
         SAC   512                                                              
         USING DSJOBD,R2                                                        
         LA    R1,DSPACECB                                                      
         ST    R1,DSJDSECB                                                      
         BRAS  RE,ARSOFF                                                        
         DROP  R2                                                               
*                                                                               
INIT10   BRAS  RE,SINSET           See if we need to reset RSIN                 
         BRAS  RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         LHI   R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALISE STATIC STATE TABLE                                       *         
***********************************************************************         
INISTATE NTR1  ,                                                                
*                                                                               
         OPEN  (SYSSTATE,INPUT)                                                 
*                                                                               
         BRAS  RE,ARSOFF           ACCESS TO STATIC STATE TABLE                 
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,DHASTATE-DMDSHDR(R2)                                       
         ST    R2,ASTATE           SAVE ADDRESS OF STATE TABLE                  
         JZ    INISTATX                                                         
*                                                                               
         USING DSSHDR,R2                                                        
         MVC   DSSEYE,=C'****SYSSTATE****'                                      
         A     R1,=A(((DSSLEN*DSSMAX)+(DSLLEN*DSLMAX)+1024+64)/4096+1)          
         ST    R1,DSSLAST                                                       
         LA    R1,DSSHDLNQ(,R2)                                                 
         ST    R1,DSAGSTAT         SET A(GLOBAL STATES)                         
         AHI   R1,1024             Random constant ????                         
         ST    R1,DSASSTAT         SET A(STATIC STATE TABLE)                    
         LA    R0,DSSMAX                                                        
         MHI   R0,DSSLEN                                                        
         AR    R1,R0               INDEX PAST STATIC TABLES                     
         ST    R1,DSATSTAT         SET A(TASK STATES)                           
         SAC   0                                                                
*                                                                               
INIS001  GET   SYSSTATE            GET STATE CARDS                              
         MVC   CARD,0(R1)                                                       
         CLI   0(R1),C'*'          IGNORE COMMENTS                              
         JE    INIS001                                                          
*                                                                               
         ICM   R2,15,ASTATE        GET ADDRESS OF STATE TABLES                  
         JZ    *+2                                                              
         SAC   512                                                              
         L     R2,DSASSTAT         GET A(STATIC STATE TABLE)                    
*                                                                               
         LA    R0,DSSMAX           ROOM FOR 32 JOBS 32K                         
         USING DSSSTATE,R2                                                      
INIS010  CLC   DSSJOB,CARD+10      FIND ENTRY FOR JOB                           
         JE    INIS020                                                          
         OC    DSSJOB,DSSJOB       OR FREE ENTRY                                
         JE    INIS020                                                          
         AHI   R2,DSSLEN           ENTRIES ARE 1K                               
         JCT   R0,INIS010                                                       
         DC    H'0'                TOO MANY JOBS                                
*                                                                               
INIS020  MVC   DSSJOB,CARD+10      SET JOBNAME FOR ENTRY                        
         MVC   DUB,CARD                                                         
         LA    R1,DUB                                                           
         BRAS  RE,VALSYS                                                        
         LTR   R1,R1               R1 has SE# or zero                           
         JNZ   INIS030                                                          
*                                                                               
         SAC   0                                                                
         MVC   PLINE(25),=C'INVALID SYSTEM ....... //'                          
         MVC   PLINE+15(7),CARD                                                 
         GOTO1 =V(DDWTO),DMCB,PLINE,0                                           
         DC    H'0'                                                             
*                                                                               
INIS030  LA    R2,DSSSYS                                                        
         LR    R0,R1               Leave value in R1 if dumps                   
         SLL   R0,2                *4                                           
         AR    R2,R0               INDEX INTO STATUS TABLE FOR JOB              
         SAC   512                                                              
*                                                                               
         MVC   0(1,R2),CARD+20     OPEN  (COPY STATUS CHARACTERS)               
         MVC   1(1,R2),CARD+30     READ                                         
         MVC   2(1,R2),CARD+40     CLOSE                                        
         MVC   3(1,R2),CARD+50     USER DEF                                     
         SAC   0                                                                
*                                                                               
         J     INIS001             NEXT LINE                                    
         DROP  R2                                                               
*                                                                               
STATEEND CLOSE SYSSTATE                                                         
*                                                                               
         L     R2,ASTATE           STATE TABLE                                  
         USING DSSHDR,R2                                                        
         SAC   512                                                              
         L     R2,DSAGSTAT         GET A(GLOBAL STATE TABLE)                    
*                                                                               
         USING DSGSTATS,R2                                                      
         OC    DSGSTAT,DSGSTAT     ANY GLOBAL STATE SET                         
         JNZ   INISTATX            YES THEN LEAVE IT ALONE                      
*                                                                               
         LA    R0,256              LOOP THROUGH ALL SYSTEMS                     
         LA    R2,DSGSTAT                                                       
         USING DSGSTATE,R2                                                      
*                                                                               
INIS050  MVI   DSGCURS,C'O'        SET DEFAULT OPEN STATE                       
         MVI   DSGNEWS,C'O'                                                     
         MVI   DSGFLAG1,X'00'                                                   
         MVI   DSGFLAG2,X'00'                                                   
         LA    R2,4(R2)                                                         
         JCT   R0,INIS050          DO FOR ALL SYSTEMS                           
         SAC   0                                                                
*                                                                               
INISTATX SAC   0                                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
ARSOFF   SAC   0                                                                
         LAM   AR0,ARF,ARZERO                                                   
         BR    RE                                                               
*                                                                               
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
XBASE    L     RD,SAVERD           EXIT FROM TOP                                
         XBASE                                                                  
*                                                                               
CVDR0    CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        TIMER TABLES  CL1'Y/N',AL3(FREQ),AL4(LAST TIME)              *         
***********************************************************************         
TIMETAB  DS    0F                                                               
*&&UK                                                                           
TIMEHELP DC    AL1(NO),AL3(1*100),AL4(0)     DAMAGE CHECK 1 SECOND              
TIMETRAC DC    AL1(NO),AL3(1*100),AL4(0)     TRACE RUN 1 SECONDS                
TIMEWAIT DC    AL1(NO),AL3(30*100),AL4(0)    WAIT CHECK 30 SECONDS              
TIMELONG DC    AL1(NO),AL3(60*100),AL4(0)    LONG CHECK 1 MINUTE                
TIMESTAT DC    AL1(NO),AL3(60*100),AL4(0)    STATE CHECK 1 MINS                 
TIMERUN  DC    AL1(NO),AL3(01*60*100),AL4(0) RUNQ CHECK 01 MIN                  
TIMERUNR DC    AL1(NO),AL3(05*60*100),AL4(0) RUNNER CHECK 05 MIN                
TIMESPAC DC    AL1(NO),AL3(05*60*100),AL4(0) SPACE CHECK 5 MINS                 
TIMERERU DC    AL1(NO),AL3(15*60*100),AL4(0) RERUN CHECK 15 MINS                
TIMEBDAT DC    AL1(NO),AL3(15*60*100),AL4(0) BILLDATE CHECKS 15 MINS            
TIMEPQ   DC    AL1(NO),AL3(30*60*100),AL4(0) PQ CHECK 30 MINS                   
*&&                                                                             
*&&US                                                                           
TIMEHELP DC    AL1(NO),AL3(1*100),AL4(0)     DAMAGE CHECK 1 SECOND              
TIMETRAC DC    AL1(NO),AL3(10*100),AL4(0)    TRACE RUN 10 SECONDS               
TIMEWAIT DC    AL1(NO),AL3(30*100),AL4(0)    WAIT CHECK 30 SECONDS              
TIMELONG DC    AL1(NO),AL3(60*100),AL4(0)    LONG CHECK 1 MINUTE                
TIMESTAT DC    AL1(NO),AL3(60*100),AL4(0)    STATE CHECK 1 MINS                 
TIMERUN  DC    AL1(NO),AL3(05*60*100),AL4(0) RUNQ CHECK 05 MIN                  
TIMERUNR DC    AL1(NO),AL3(05*60*100),AL4(0) RUNNER CHECK 05 MIN                
TIMESPAC DC    AL1(NO),AL3(15*60*100),AL4(0) SPACE CHECK 15 MINS                
TIMERERU DC    AL1(NO),AL3(15*60*100),AL4(0) RERUN CHECK 15 MINS                
TIMEBDAT DC    AL1(NO),AL3(15*60*100),AL4(0) BILLDATE CHECKS 15 MINS            
TIMEPQ   DC    AL1(NO),AL3(30*60*100),AL4(0) PQ CHECK 30 MINS                   
*&&                                                                             
TIMEEOT  DC    X'FFFFFFFF'                                                      
         EJECT                                                                  
***********************************************************************         
*        CONSTANTS & LTORG                                            *         
***********************************************************************         
ARZERO   DC    16F'0'                                                           
READ     DC    CL8'READ   '                                                     
BUFFER   DC    CL8'BUFFER '                                                     
DMREAD   DC    CL8'DMREAD '                                                     
DMRSEQ   DC    CL8'DMRSEQ '                                                     
DMRDHI   DC    CL8'DMRDHI '                                                     
OPEN     DC    CL8'OPEN'                                                        
CTFILE   DC    CL8'CTFILE'                                                      
LOCKSPC  DC    CL8'LOCKSPC'                                                     
SPACES   DC    CL166' '                                                         
STARS    DC    16C'*'                                                           
EFFS     DC    16X'FF'                                                          
MAXLINE  DC    PL3'60'                                                          
LWAIT    DC    AL4(30*100)                                                      
LOCKEDJB DC    AL1(NO)                                                          
LOCKEDCL DC    AL1(NO)                                                          
                                                                                
T1       DC    166C' '                                                          
*                                                                               
STATEWRN DC C'*SYSSTATE* JOBNAME J0000000 SYSNAME ...... Expect ......'         
STATEWR2 DC C'*SYSSTATE* SYSNAME NULL SET TO X'                                 
*                                                                               
STATES   DC    C'Closed'                                                        
         DC    C'Read  '                                                        
         DC    C'Write '                                                        
         DC    C'None  '                                                        
*                                                                               
STATETIM DC    256X'00'            WARN TIME BY SYSTEM                          
STATETIT DC    256X'00'            TEMPORARY STATETIM                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
RUNLOG   DCB   DSORG=PS,MACRF=PM,DDNAME=RUNLOG,RECFM=FB,LRECL=(80),    +        
               EODAD=LOGFULL                                                    
*                                                                               
TRACELOG DCB   DDNAME=TRACELOG,DSORG=PS,MACRF=(PM),RECFM=VB,           *        
               BLKSIZE=8200,LRECL=4096,BUFNO=2,EODAD=TRCFULL,SYNAD=TRCX         
*                                                                               
SYSSTATE DCB   DDNAME=SYSSTATE,DSORG=PS,MACRF=(GL),RECFM=FB,           *        
               LRECL=80,EODAD=STATEEND                                          
*                                                                               
ADATAMGR DC    V(DATAMGR)                                                       
ALOCKSPC DC    V(LOCKSPC)                                                       
ARERUN   DC    V(RERUN)                                                         
*                                                                               
ACOMM    DC    A(0)                                                             
MYASID   DC    H'0'                                                             
                                                                                
*************************************************************                   
*        ECBS AND TIMER FIELDS                              *                   
*************************************************************                   
         DS    0F                                                               
ECBLST2  EQU   *                                                                
ADSPECB  DC    A(DSPACECB)                                                      
AOPERECB DC    A(0)                                                             
ATODECB  DC    A(TODECB)                                                        
         DC    X'80',AL3(TASKECB) A(TIMER ECB)                                  
*                                                                               
STIMERID DS    XL4           STIMERM ID for Task interval                       
TIME     DS    XL4                                                              
TODID    DS    XL4           STIMERM ID for TOD  event                          
MIDNIGHT DC    C'24000000'                                                      
DSPACECB DS    XL4                                                              
         DROP  RB,RA,R9,R8                                                      
         EJECT                                                                  
*************************************************************                   
* THE TIMER EXIT ROUTINE.  IT POSTS AN ECB.                 *                   
*************************************************************                   
TASKXIT  SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TASKXIT,RB                                                       
         POST  TASKECB                                                          
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
TASKECB  DC    F'0'                ECB OF ATTACHED TASK TIMER                   
         DROP  RB                                                               
         EJECT                                                                  
*************************************************************                   
* THE Event timer exit routine - midnight run               *                   
*************************************************************                   
TODXIT   SAVE  (14,12),,*                                                       
         LR    RB,RF                                                            
         USING TODXIT,RB                                                        
         POST  TODECB                                                           
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
TODECB   DC    F'0'                ECB OF TOD event                             
         DROP  RB                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    CL16'******UTL******'                                            
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    AL1(10)                                                          
         ORG                                                                    
*                                                                               
         DS    0D                                                               
         DC    CL16'******SSB******'                                            
SSB      DC    (SSOLNQ)X'00'                                                    
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'                                                            
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSGALO+SSOSLOCK)                                           
         ORG                                                                    
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
*&&DO                                                                           
         DS    0D                                                               
SORTBLK  DS    1024CL16                                                         
*&&                                                                             
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        ERROR HANDLER MDUMPER EXIT ROUTINE                 *                   
*************************************************************                   
         DS    0D                                                               
         USING *,RB                                                             
ERREXT   LR    RB,RF               ESTABLISH BASE                               
         L     R2,0(R1)                                                         
         USING SDWA,R2             R2=SDWA                                      
         ST    R1,MYR1                                                          
         ST    RE,MYRE                                                          
*                                                                               
         ICM   R1,15,SDWAGR11      FIND ABENDING RB                             
         JNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   22(8,R1),=C'**DMRC**' DIED IN DMRC - END JOB                     
         JE    ERREOJ                                                           
         CLC   SDWAABCC+2(2),=H'990' U990 JUST MESSAGE                          
         JNE   ERRRET                                                           
*                                                                               
         XC    MLINE,MLINE         SEND ALLOCATION MESSAGE                      
         MVC   MLINE(2),=X'0020'                                                
         MVC   MLINE+2(L'MESS990),MESS990                                       
         XR    RF,RF                                                            
         ICM   RF,15,SDWAGR02                                                   
         MVC   MLINE+22(8),ISFDD-ISDTF(RF)                                      
*                                                                               
         WTO   TEXT=MLINE,MCSFLAG=HRDCPY                                        
         L     R1,MYR1                                                          
         LA    RF,=C'NODUMP'       IF ALLOC ERR RETURN NODUMP                   
         ST    RF,4(R1)                                                         
         J     ERRRET                                                           
*                                                                               
ERREOJ   L     R1,MYR1             IF ERREOJ RETURN ENDJOB                      
         LA    RF,=C'ENDJOB'                                                    
         ST    RF,4(R1)                                                         
*                                                                               
ERRRET   L     R1,MYR1             RESTORE R1 AND RE                            
         L     RE,MYRE                                                          
         BR    RE                                                               
*                                                                               
MYR1     DC    A(0)                SAVED R1 AND RE                              
MYRE     DC    A(0)                                                             
MLINE    DC    CL64' '                                                          
MESS990  DC    C'ALLOCATION ERROR ON XXXXXXXX    '                              
         LTORG                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
K        EQU   1024                                                             
                                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
MAINRD   DS    A                                                                
SAVERE   DS    A                                                                
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
*                                                                               
ACIREC   DS    A                                                                
ASAVEJOB DS    A                                                                
AJOBTAB  DS    A                                                                
AJCLASS  DS    A                                                                
ACLASS   DS    A                                                                
JCOFFS   DS    A                                                                
JOBHDRL  DS    H                                                                
JOBTABL  DS    H                                                                
RQPURGE  DS    F                                                                
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
EDUB     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
*                                                                               
SAVEF01  DS    4A                                                               
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
SAVELOCL DS    A                                                                
JOBMAX   DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
RUNQPURG DS    F                   COUNT RUNQ ENTRIES REMOVED                   
*                                                                               
JTHDR    DS    XL(L'DSPHDR)        JOB TABLE HEADER                             
CLHDR    DS    XL(L'DSPHDR)        CLASS TABLE HEADER                           
*                                                                               
ALET     DS    A                   DMGR ALET                                    
TBLET    DS    A                   TABS ALET                                    
*                                                                               
AFACITAB DS    A                   A(FACIDTAB) BY DSPACE                        
ASYSFLES DS    A                   A(SYSFLES)                                   
ASTATE   DS    A                   A(STATE TABLE)                               
*                                                                               
WORK     DS    CL64                                                             
WORK1    DS    CL64                                                             
MYWORK   DS    CL64                                                             
*                                                                               
SVJNAM   DS    CL8                                                              
SVJNUM   DS    CL2                                                              
SVJASID  DS    CL2                                                              
SVJTYPE  DS    CL1                                                              
SVJADV   DS    CL1                                                              
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
DINDEX   DS    A                   DATASPACE INDEX                              
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
STATSYS  DS    CL8                 STAT=SYS VALUE                               
*                                                                               
DAYNOW   DS    XL1                                                              
DATNOW   DS    XL1                                                              
MONNOW   DS    XL1                                                              
OPENFLG  DS    XL1                                                              
BADCLS   DS    X                                                                
BADJOB   DS    X                                                                
ENDTASK  DS    X                   FLAG Y IF WE MUST EOJ                        
ABEND    DS    X                   FLAG Y IF WE MUST ABEND                      
ABENDS   DS    H                   COUNT ABENDS                                 
*                                                                               
TIMENTU  DS    A                   TIME NOW TUS USED FOR LOCGCHK                
CURRTOR  DS    A                   A CURRENT TOR                                
CURRFAC  DS    A                   A CURRENT FAC                                
CURRFACN DS    CL8                 CURRENT FAC NAME                             
*                                                                               
TIMENOW  DS    A                   BINARY TIME AT END OF WAIT                   
TIMEOLD  DS    A                   PREVIOUS POP TIME                            
DATENOW  DS    PL4                 BINARY DATE                                  
DATEOLD  DS    PL4                 BINARY DATE LAST TIME                        
*                                                                               
TOPJOB   DS    F                                                                
RESNUM   DS    XL1                 RESOURCE NUMBER                              
SEOPN    DS    CL1                 SE NUMBER                                    
SEOPNN   DS    CL7                 SE NAME                                      
SEFILN   DS    CL256               SYSTEMS FOR OPEN                             
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
TODAY    DS    F                   WORKING AREAS FOR BILLDATE                   
TOMORROW DS    F                                                                
YESTERDY DS    F                                                                
BILLTIME DS    F                                                                
SYSNAME  DS    CL8                                                              
*                                                                               
ASOONSV  DS    A                   ADDCONS FOR SOONCOPY                         
ACLASSV  DS    A                                                                
ACLAS    DS    A                                                                
AJOBS    DS    A                                                                
*                                                                               
LOGEOF   DS    X                                                                
DSPACE   DS    X                   DATASPACE TYPE - R(EP)/A(DV)/T(ST)           
REBUILD  DS    X                   DATASPACE TYPE - R(EP)/A(DV)/T(ST)           
FILMANI  DS    X                   FILMAN INIT DONE                             
*                                                                               
TRCADDR  DS    F                   TRACE POINTER                                
TRCADFI  DS    F                   FIRST TRACE ADDRESS                          
TRCADCUR DS    F                   CURRENT ADD POINT                            
TRCADMX  DS    F                   MAX TRACE ADDRESS                            
TRCEOF   DS    X                                                                
*                                                                               
CLASSAVG DS    50CL4                                                            
*                                                                               
PRTQID   DS    CL8                                                              
NDX      DS    CL40                                                             
SAVE     DS    360C                                                             
*                                                                               
         DS    0F                                                               
BUFFDATA DS    0XL12                                                            
BUPQTAB  DS    V                                                                
BUPQFILE DS    V                                                                
BUSAVE   DS    XL4                                                              
*                                                                               
CARD     DS    CL80                                                             
CARD1    DS    CL80                                                             
CARD2    DS    CL80                                                             
*                                                                               
       ++INCLUDE DMUSSKEY                                                       
*                                                                               
SAVEJBXQ EQU   40                                                               
SAVEJOB# DS    H                                                                
*                                                                               
CXREC    DS    (14*K)C                                                          
CIREC    DS    (14*K)C                                                          
*                                                                               
*                                                                               
SAVEJOB  DS    (SAVEJBXQ)CL(L'TBJNTRY)                                          
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
CARDD    DSECT                                                                  
CDKYWORD DS    CL7                 KEYWORD                                      
CDKYLEN  DS    AL1                 LENGTH - 1                                   
CDFLDLN  DS    AL1                 FLD LENGTH                                   
         DS    AL1                                                              
CDFLAGS1 DS    XL1                 FLAGS                                        
CDF1ROUT EQU   X'80'               . ROUTINE                                    
CDF1OPER EQU   X'60'               . OPERATOR                                   
CDF1EQNE EQU   X'40'               . =, /=                                      
CDF1GTLT EQU   X'20'               . <, >, <=, >=                               
CDF1IHEX EQU   X'10'               . HEX INPUT                                  
CDF1DEC  EQU   X'08'               . DECIMALS                                   
CDF1LIST EQU   X'04'               . LIST                                       
CDF1TIME EQU   X'02'               . TIME                                       
CDF1DATE EQU   X'01'               . DATE                                       
                                                                                
CDFLAGS2 DS    XL1                                                              
CDF2DOIT EQU   X'80'               . NO PARAMETER JUST RUN ROUTINE              
                                                                                
CDFIELD  DS    0AL3                A(FIELD)                                     
CDFUNCT  DS    AL3                 A(ROUTINE)                                   
CARDLNQ  EQU   *-CARDD                                                          
*************************************************************                   
*        SORT DSECT                                         *                   
*************************************************************                   
SORTD    DSECT                                                                  
SORTNAME DS    CL8                 JOBNAME                                      
SORTPRTY DS    CL1                 PRIORITY                                     
SORTWAIT DS    CL1                 WAIT SEQUENCE                                
SORTTIME DS    CL3                 SUBMIT TIME                                  
SORTINDX DS    XL2                 INDEX TO ENTRY                               
SORTPOS  DS    XL1                 POSITION                                     
                                                                                
         DSECT                                                                  
         PUSH  ACONTROL                                                         
         ACONTROL COMPAT(NOCASE)                                                
***********************************************************************         
* IBM MACRO DSECTS                                                              
***********************************************************************         
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAASCB                                                                
         IHAASSB                                                                
         IEECHAIN                                                               
         IAZJSAB                                                                
         POP   ACONTROL                                                         
*                                                                               
*IHASDWA MACRO                                                                  
         IHASDWA GR32=YES                                                       
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DMDSYSHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMDSYSHDR                                                      
         PRINT ON                                                               
* FATABSDEQU                                                                    
         PRINT OFF                                                              
       ++INCLUDE FATABSDEQU                                                     
         PRINT ON                                                               
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DMSPACED                                                                      
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
* DMSYSFD                                                                       
       ++INCLUDE DMSYSFD                                                        
         PRINT ON                                                               
* DMDFTPH                                                                       
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* FAUTL                                                                         
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSBOFF                                                                      
       ++INCLUDE FASSB                                                          
         ORG SSBD                                                               
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         ORG                                                                    
SSOLNQ   EQU   *-SSBD                                                           
         PRINT ON                                                               
* FATABSPQ                                                                      
       ++INCLUDE FATABSPQ                                                       
         PRINT ON                                                               
* FACIDTAB                                                                      
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
* FATABSCOM                                                                     
       ++INCLUDE FATABSCOM                                                      
* FATABSJOB                                                                     
       ++INCLUDE FATABSJOB                                                      
* FATABSRUN                                                                     
       ++INCLUDE FATABSRUN                                                      
* FAPIGFACD                                                                     
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
* FAD                                                                           
*      ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031DMDMGRMNT 09/17/20'                                      
         END                                                                    
