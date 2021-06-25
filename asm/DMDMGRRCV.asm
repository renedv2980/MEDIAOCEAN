*          DATA SET DMDMGRRCV  AT LEVEL 065 AS OF 09/17/20                      
*PHASE DMGRRCVA                                                                 
*INCLUDE FATABOFF                                                               
*INCLUDE SORTER                                                                 
*INCLUDE CARDS                                                                  
*INCLUDE DMDMGRL                                                                
*INCLUDE DATCON                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE DDRCVR                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE DDWTO                                                                  
         TITLE 'DMDMGRRCV - HANDLE DATA SPACE RECOVERY '                        
         PRINT NOGEN                                                            
DMGRRCV  CSECT                                                                  
         ENTRY UTL                                                              
         ENTRY SSB                                                              
         NBASE WORKX-WORKD,**DMRC**,=A(WORKAREA),RA,R9,R8,CLEAR=YES             
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
*                                                                               
DMGRRCV0 BRAS  RE,PRINTI           INIT PRINTING                                
         BRAS  RE,INIT             READ CARDS ECT                               
         BRAS  RE,INITMQ           RELATED TO RUNNERS                           
*                                                                               
         BRAS  RE,SETESTAE         SET UP ERROR HANDLING                        
         LA    R1,RESTART          R1=RESTART POINT AFTER ABEND                 
         L     RF,=A(MYREGS)       SAVE CURRENT REGS FOR RESTARTING             
         STM   R0,RF,0(RF)                                                      
*                                                                               
RESTART  SAM24 ,                   MAKE SURE WE RESTART IN 24 BIT               
         CLI   ENDTASK,YES         DO WE WANT TO CANCEL                         
         BE    ENDOFJOB                                                         
*                                                                               
LOOP010  BRAS  RE,SETWAIT          MAIN LOOP                                    
         BRAS  RE,TIMECHK          CHECK TIMES                                  
         BRAS  RE,MAIN                                                          
         BRAS  RE,ARSOFF                                                        
         B     LOOP010                                                          
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
*                                                                               
         CLI   TIMEQJOB,YES        QUICK JOB CHECK                              
         JNE   MAIN010                                                          
         BRAS  RE,JOBCHK           ONLY TEST ABEND FLAGS                        
         MVI   TIMEQJOB,NO                                                      
*                                                                               
MAIN010  CLI   TIMEADVS,YES        ADV CHECK                                    
         JNE   MAIN020                                                          
         BRAS  RE,ADVCHK           TEST ADVS                                    
         MVI   TIMEADVS,NO                                                      
*                                                                               
MAIN020  CLI   TIMEJOBS,YES        JOB CHECK                                    
         JNE   MAIN030                                                          
         BRAS  RE,JOBCHK           TEST JOBS FIRST                              
         BRAS  RE,SECHK            THEN TEST FOR SE LOCKS                       
         BRAS  RE,TABCHK           THEN TEST FOR TABS LOCKS                     
         MVI   TIMEJOBS,NO                                                      
*                                                                               
MAIN030  CLI   OPERMQ,YES          MQ ENABLED?                                  
         JNE   MAINXIT                                                          
         BRAS  RE,MQPUTQ                                                        
         MVI   OPERMQ,NO                                                        
*                                                                               
MAINXIT  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK ON RUNNING ADVS OR RUNNERS                                    *         
***********************************************************************         
ADVCHK   NTR1  ,                                                                
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET        CHECK ADVS FIRST                             
         XR    R2,R2                                                            
         SAC   512                                                              
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHAJOBS                                                    
         USING DSJOBD,R2                                                        
*                                                                               
         LHI   R5,DSJOBMXQ+1                                                    
ADVS02   OC    DSJNAM,DSJNAM                                                    
         BZ    ADVS04                                                           
         MVC   SVJNAM,DSJNAM                                                    
         MVC   SVJNUM,DSJNUM                                                    
         MVC   SVJASID,DSJASID                                                  
         MVI   SVJTYPE,C'A'                                                     
         MVC   SVJADV,DSJADV                                                    
         MVI   BYTE,0              CLEAR THIS FLAG FOR ADVS                     
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         BRAS  RE,ASIDTEST         CHECK THE JOBS ASID OUT                      
         REAR  ARS=ON                                                           
*                                                                               
ADVS04   LA    R2,DSJOBLNQ(,R2)                                                 
         BCT   R5,ADVS02           NEXT ADV                                     
         BRAS  RE,ARSOFF                                                        
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NOW CHECK INDIVIDUAL SYSTEMS                                        *         
***********************************************************************         
JOBCHK   NTR1  ,                                                                
*                                                                               
JOBS05   BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         L     R2,DMOFFS                                                        
         SAC   512                 SET UP AMODE                                 
*                                                                               
JOBS06   LA    R2,L'DMDHDR(,R2)    FIRST / NEXT SYSTEM                          
         ST    R2,@DSYSTEM         SAVE CURRENT SYSTEM                          
         C     R2,=X'00004000'     EOT = 64*256 SYSTEM ENTRIES                  
         BE    JOBCHKX             DONE                                         
         USING DMSPACED,R2                                                      
         MVI   SVRCVR,NO           SET FLAG RECOVERY NOT CALLED                 
*                                                                               
         ICM   R1,15,DSPTFRST      ONLY IF DEFINED                              
         BZ    JOBS06              NEXT SYSTEM                                  
         NILF  GR1,X'3FFFFFFF'     PROPERLY...                                  
         BZ    JOBS06                                                           
*                                                                               
         LR    R2,R1                                                            
         ST    R2,@DSYSHDR         SAVE A(SYSTEM HEADER)                        
         USING DMSYSHDR,R2                                                      
         L     R2,DSYAJOBS         GET JOB TABLE                                
         USING DSJOBHDR,R2                                                      
*                                                                               
         LHI   R0,DSJBHMXQ         MAX ENTRIES                                  
         MVI   OPENFLG,NO                                                       
*                                                                               
JOBS08   MVC   SVJNAM,DSJOBNAM                                                  
         MVC   SVJNUM,DSJOBNUM                                                  
         MVC   SVJASID,DSJOASID                                                 
         MVI   SVJTYPE,C'J'                                                     
         OC    SVJNAM,SVJNAM                                                    
         BZ    JOBS10                                                           
*                                                                               
         MVC   BYTE,DSJOBFLG       SAVE THIS FLAG FOR ASIDTEST                  
*                                                                               
         STAR  CLEAR=ARZERO,ARS=OFF                                             
         BRAS  RE,ASIDTEST         CHECK THE JOBS ASID                          
         REAR  ARS=ON                                                           
*                                                                               
JOBS10   OC    DSJOBHDR(DSJBHLNQ),DSJOBHDR  ANY ENTRYS?                         
         BZ    *+8                 NOT YET                                      
         MVI   OPENFLG,YES         FLAG SYSTEM AS OPEN                          
         AHI   R2,DSJBHLNQ         NEXT JOBTAB ENTRY                            
         BCT   R0,JOBS08                                                        
*                                                                               
         CLI   OPENFLG,YES         ANYTHING OPEN                                
         BE    JOBS12              YEP                                          
*&&DO                                                                           
         L     R2,@DSYSTEM         NO                                           
         USING DMSPACED,R2                                                      
         ICM   R1,15,DSPTFRST                                                   
         NILF  GR1,X'3FFFFFFF'                                                  
         BZ    JOBS12                                                           
         ST    R1,@DSYSHDR         SET FOR CLRSYS ROUTINE                       
*&&                                                                             
         CLI   SVRCVR,YES          DID WE CALL RECOVERY                         
         BNE   JOBS12              NO                                           
*                                                                               
         STAR  CLEAR=ARZERO,ARS=SAME                                            
         BRAS  RE,CLRSYS           CLEAR THIS SYSTEM DOWN                       
         REAR  ARS=ON                                                           
*                                                                               
JOBS12   L     R2,@DSYSTEM         NEXT SYSTEM                                  
         B     JOBS06                                                           
*                                                                               
JOBCHKX  BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NOW CHECK LOW LEVEL SE LOCKS.                                       *         
***********************************************************************         
SECHK    NTR1  ,                                                                
         MVI   BYTE,C'D'           DMGR DATASPACE                               
         MVC   FULL,ALET                                                        
         BRAS  RE,LOCKCHK                                                       
         J     EXITOK                                                           
                                                                                
***********************************************************************         
* NOW CHECK TABS LOCKS.                                               *         
***********************************************************************         
TABCHK   NTR1  ,                                                                
         MVI   BYTE,C'T'           TABS DATASPACE                               
         MVC   FULL,TBLET                                                       
         BRAS  RE,LOCKCHK                                                       
         J     EXITOK                                                           
                                                                                
***********************************************************************         
* CHECK LOCKS ON ALL ENTRY IN DATA SPACE                              *         
***********************************************************************         
LOCKCHK  NTR1  ,                                                                
*                                                                               
LKCH05   BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,FULL        FULL IS THE ALET DMGR OR TABS                
         L     R2,DMOFFS                                                        
         SAC   512                 SET UP AMODE                                 
*                                                                               
LKCH06   AHI   R2,64               FIRST / NEXT SYSTEM                          
         ST    R2,DINDEX                                                        
         C     R2,=X'00004000'     EOT = 64*256 SYSTEM OR TABS TABLE            
         BE    JOBCHKX                                                          
         USING DMSPACED,R2                                                      
*                                                                               
         ICM   R1,15,DSPTFRST      ONLY IF DEFINED                              
         JZ    LKCH06                                                           
         N     R1,=XL4'3FFFFFFF'   PROPERLY...                                  
         JZ    LKCH06                                                           
*                                                                               
         OC    DSPLOCK,DSPLOCK     IS THE ENTRY LOCKED                          
         JZ    LKCH06                                                           
*                                                                               
         ICM   R1,15,DSPTIME       MUST HAVE A LOCK TIME                        
         JZ    LKCH06                                                           
         L     R4,TIMENOW                                                       
         SR    R4,R1               R4=AGE OF LOCK IN MS                         
         JM    LKCH06                                                           
         CHI   R4,100              MUST BE AT LEAST 1 SECOND OLD                
         JL    LKCH06                                                           
*                                                                               
         LH    R4,DSPLOCK+2        GET THE ASID                                 
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         JNZ   LKCHNO              ERROR FROM MACRO                             
*                                                                               
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         JNE   LKCHNO              NOT AN ASCB                                  
*                                                                               
         L     R4,ASCBASSB                                                      
         SAM31                                                                  
         ICM   R4,15,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                             
         JZ    LKCHNO              NO JSAB                                      
*                                                                               
         ICM   R1,15,ASCBJBNI      ANY JOBNAME                                  
         JZ    LKCH10                                                           
         CLC   DSPMVS,0(R1)        MUST MATCH                                   
         JE    LKCH99                                                           
         J     LKCHNO                                                           
*                                                                               
LKCH10   ICM   R1,15,ASCBJBNS      ANY TASKNAME                                 
         JZ    LKCH99                                                           
         CLC   DSPMVS,0(R1)        MUST MATCH                                   
         JE    LKCH99                                                           
         J     LKCHNO                                                           
*                                                                               
LKCHNO   OC    DSPMVS,DSPMVS       IGNORE IF NO OWNER IN LOCK                   
         JZ    LKCH99                                                           
*                                                                               
         MVC   WORK1,SPACES        UNLOCKING RESOURCE FOR JOBNAME               
         MVC   WORK1+00(2),=H'32'                                               
         MVC   WORK1+02(9),=C'UNLOCKING'                                        
         MVC   WORK1+21(3),=C'FOR'                                              
         MVC   WORK1+12(8),DSPNAME                                              
         MVC   WORK1+25(8),DSPMVS                                               
         SAC   0                                                                
         WTO   TEXT=WORK1,MCSFLAG=HRDCPY                                        
*                                                                               
         XC    DMCB(24),DMCB       FORCE FREE IT                                
         LR    R1,R2                                                            
         SRL   R1,6                                                             
         ST    R1,DMCB                                                          
*                                                                               
         CLI   BYTE,C'T'           TABS RESOURCE                                
         JNE   *+8                                                              
         OI    DMCB+2,X'80'        FLAG IT WITH X'80'                           
*                                                                               
         MVI   DMCB,X'11'          RELEASE                                      
         GOTO1 ALOCKSPC,DMCB                                                    
*                                                                               
         LAM   AR2,AR2,FULL        MAKE SURE ALET IS GOOD                       
         SAC   512                                                              
*                                                                               
LKCH99   L     R2,DINDEX           NEXT SYSTEM                                  
         J     LKCH06                                                           
*                                                                               
LKCHKXX  BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK ASID AND RUN RECOVERY ACTIONS IF REQUIRED                     *         
* NTRY: SVJNAM  = DSJNAM                                              *         
*       SVJNUM  = DSJNUM                                              *         
*       SVJASID = DSJASID                                             *         
*       R2          = A(JOB TABLE ENTRY)                              *         
***********************************************************************         
ASIDTEST NTR1  ,                                                                
*                                                                               
         TM    BYTE,DSJOBABN       FLAG TO FORCE RECOVRY                        
         BO    ASIDNO                                                           
         CLI   TIMEQJOB,YES        FULL TEST FOR ADVS AND JOB CHK               
         BE    ASIDX                                                            
*                                                                               
ASID001  LH    R4,SVJASID          GET THE ASID                                 
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         BNZ   ASIDNO              ERROR FROM MACRO                             
*                                                                               
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   ASIDNO              NOT AN ASCB                                  
*                                                                               
         L     R4,ASCBASSB                                                      
         SAM31                                                                  
         ICM   R4,15,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                             
         BZ    ASIDNO              NO JSAB                                      
*                                                                               
         ICM   R1,15,ASCBJBNI      ANY JOBNAME                                  
         BZ    ASID005                                                          
         CLC   SVJNAM,0(R1)        MUST MATCH                                   
         BE    ASID010                                                          
         BNE   ASIDNO                                                           
*                                                                               
ASID005  ICM   R1,15,ASCBJBNS      ANY TASKNAME                                 
         BZ    ASID010                                                          
         CLC   SVJNAM,0(R1)        MUST MATCH                                   
         BNE   ASIDNO                                                           
*                                                                               
         USING JSAB,R4                                                          
ASID010  MVC   DUB1,JSABJBID       GET JOBID (E.G., JOB12345)                   
         PACK  DUB,DUB1+3(5)                                                    
         CVB   R1,DUB                                                           
         STCM  R1,3,HALF           CONVERT TO JOBNO                             
         CLC   SVJNUM,HALF                                                      
         BE    EXITOK              MATCH OK                                     
*                                                                               
ASIDNO   SAM24                                                                  
         BRAS  RE,RECOVER          RUN RECOVERY                                 
         BRAS  RE,CLEANUP          CLEAN UP DSPACE AREAS                        
         SAM31                                                                  
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         SAC   512                 SET UP AMODE                                 
*                                                                               
         CLI   SVJTYPE,C'A'        IS THIS AN ADV ENTRY                         
         BNE   ASIDX                                                            
         USING DSJOBD,R2                                                        
         MVC   DUB(4),DSJMQRET         ANY MQ RETURN                            
         MVC   DUB+4(4),DSJLOCAL       ANY LOCAL STATE TABLE                    
         XC    DSJOBD(DSJOBLNQ),DSJOBD CLEAR ADV TABLE ENTRIES HERE             
*                                                                               
         ICM   R2,15,DUB+4         ANY LOCAL STATE ENTRY                        
         JZ    ASID990                                                          
*                                                                               
         LA    R3,DSLLEN           REMOVE LOCAL ENTRY                           
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
ASID990  ICM   R2,15,DUB           ANY MQ RETURN                                
         JZ    ASIDX                                                            
         BRAS  RE,MQPUTQ           PUT IT TO MQ                                 
         DROP  R2                                                               
*                                                                               
ASIDX    BRAS  RE,ARSOFF                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RUN RECOVERY FOR A JOB                                              *         
***********************************************************************         
RECOVER  NTR1  ,                                                                
         MVC   WORK1,SPACES                                                     
         MVC   WORK1+2(8),SVJNAM                                                
         MVC   WORK1+11(8),=C'J       '                                         
         SR    R1,R1                                                            
         ICM   R1,3,SVJNUM                                                      
         EDIT  (R1),(5,WORK1+12),WRK=MYWORK,FILL=0                              
         MVC   WORK1+20(18),=C' RECOVERY STARTED '                              
         MVC   WORK1+0(2),=H'36'                                                
         WTO   TEXT=WORK1                                                       
*                                                                               
         MVI   SVRCVR,YES          SET FLAG WE DID RECOVERY                     
         XC    SVSHRMEM,SVSHRMEM   CLEAR SHRMEM BUFFER ADDRESS                  
*                                                                               
         GOTO1 =V(DDRCVR),DMCB,SVJNAM,0                                         
         MVC   SVSHRMEM,4(R1)      DDRCVR RETURNS SHRMEM ADDR IN P2             
*                                                                               
         MVC   WORK1+20(18),=C' RECOVERY ENDED   '                              
         WTO   TEXT=WORK1                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CLEAN UP ANY COMMS FOR FAILING JOB                                  *         
***********************************************************************         
CLEANUP  NTR1                                                                   
         XR    R2,R2                                                            
         LAM   AR2,AR2,ALET                                                     
         USING DMDSHDR,R2                                                       
         SAC   512                                                              
         L     R2,DHACOMM          SET R2 TO COMMS BLOCK                        
         AH    R2,=H'4096'         SKIP HEADER PAGE                             
         LA    R0,512                                                           
         USING DSCOMM,R2                                                        
*                                                                               
CLEARC01 OC    SVJADV,SVJADV       IS THIS AN ADV RECOVERING                    
         BZ    CLEARCJB                                                         
*                                                                               
         CLC   SVJADV,DSCDEST      MSG DESTED TO THIS ADV?                      
         BNE   CLEARCNX                                                         
*                                                                               
         OI    DSCFLAG,DSCDONEQ    FLAG IT AS DONE                              
         B     CLEARCNX                                                         
*                                                                               
CLEARCJB CLC   SVJASID,DSCDEST     TEST ASID IF NOT ADV                         
         BNE   CLEARCNX                                                         
*                                                                               
         OI    DSCFLAG,DSCDONEQ    FLAG IT AS DONE                              
         B     CLEARCNX                                                         
*                                                                               
CLEARCNX LA    R2,L'DSCOMMS(,R2)   NEXT COMMS ENTRY                             
         BCT   R0,CLEARC01                                                      
         EJECT                                                                  
***********************************************************************         
* AND CLEAN UP AOR AREAS IF AOR FAILED                                *         
***********************************************************************         
         XR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         L     R3,DHAMQRTN         A(MQ RETURN ADDRESSES)                       
         L     R2,DHATOR           SCAN TOR/AOR TABLES AND CLEANUP              
         USING TORFACD,R2                                                       
CLEAN000 ST    R2,CURRTOR                                                       
         CR    R2,R3                                                            
         BNL   CLEANHKX                                                         
*                                                                               
         CLC   TOREYE(3),=C'TOR'   MAKE SURE WE HAVE A TOR                      
         BNE   CLEAN040                                                         
         CLI   TORSTRTD,YES        STARTED                                      
         BNE   CLEAN040                                                         
         CLI   TORAVLBL,YES        AVAILABLE                                    
         BNE   CLEAN040                                                         
*                                                                               
         LA    R1,TORFACLQ+6       LINE UP ON EXCHANGE BLOCK                    
         AR    R2,R1                                                            
         USING SBEXCHD,R2                                                       
         LA    R5,SBFACMAX                                                      
CLEAN010 ST    R2,CURRFAC                                                       
         MVI   CURRFACN+4,C' '                                                  
         CLC   SBASID,SVJASID      IS THIS THE FAILING ENGINE                   
         BNE   CLEAN030                                                         
*                                                                               
         XC    0(32,R2),0(R2)      CLEAN THIS BLOCK ON ABEND                    
*                                                                               
CLEAN030 L     R2,CURRFAC          NEXT FACPAK                                  
         A     R2,=A(SBEXCHLQ)                                                  
         BCT   R5,CLEAN010                                                      
*                                                                               
CLEAN040 L     R2,CURRTOR          NEXT TOR                                     
         LA    R1,SBFACMAX                                                      
         MHI   R1,SBEXCHLQ                                                      
         AHI   R1,TORFACLQ+6                                                    
         AR    R2,R1                                                            
         B     CLEAN000                                                         
*                                                                               
CLEANHKX SAC   0                                                                
         B     EXITOK                                                           
*                                                                               
         SAC   0                                                                
         B     EXITOK                                                           
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
*NOP     WTO   'WAITING - ',MCSFLAG=HRDCPY                                      
*                                                                               
         SR    R0,R0                                                            
         LHI   R1,10               1/10 SECOND ONLY                             
         ST    R1,TIME                                                          
         XC    TIMRECB2,TIMRECB2                                                
         STIMERM SET,ID=STIMERID,BINTVL=TIME,EXIT=TIMRXIT2                      
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         JNZ   *+2                                                              
*                                                                               
         WAIT  1,ECBLIST=ECBLST2   WAIT FOR TIMER POP OR OPERATOR               
*                                                                               
         L     RF,AOPERECB         A(OPERATOR ECB)                              
         TM    0(RF),X'40'         DID THE OPERATOR INTERRUPT?                  
         BZ    WB15                NO                                           
*                                                                               
         STIMERM CANCEL,ID=STIMERID  YES, SO CANCEL THE TIMER                   
         LTR   RF,RF                                                            
         JNZ   *+2                 UNSUCCESSFUL TIMER CANCEL                    
         WTO   'OPER INTERRUPT',MCSFLAG=HRDCPY                                  
*                                                                               
         BRAS  RE,CHKOPER          EXAMINE THE OPERATOR INTERRUPT               
         B     SETWAITX                                                         
*                                                                               
WB15     TM    TIMRECB2,X'40'      DID THE TIMER POP?                           
         JZ    *+2                 HOS DID WE GET OUT OF THE WAIT?              
*NOP     WTO   'TIMER INTERRUPT',MCSFLAG=HRDCPY                                 
*                                                                               
SETWAITX MVC   TIMEOLD,TIMENOW     SET THE PREVIOUS TIME                        
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R0,TIMENOW                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* CHECK BILLDATES ARE INITIALISED AND SENSIBLE                        *         
***********************************************************************         
BILLDATE NTR1                                                                   
         WTO   'BILLDATE- ',MCSFLAG=HRDCPY                                      
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
         CLI   BILLDAT,C'N'        DO WE WANT BILLDATES PROCESSED               
         BE    BILLDATX                                                         
*                                                                               
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         XR    R2,R2                                                            
         SAC   512                                                              
*                                                                               
         CLI   BILLDAT,C'T'        TEST MODE                                    
         BNE   *+12                                                             
         LHI   R0,X'1E'            DO FOR PER2 ONLY                             
         B     BILLD010                                                         
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
         MVC   DSYBDATE,TODAY      SET TO TODAY                                 
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
         CLI   BILLDAT,C'T'        OR JUST ONE IF TEST                          
         BE    BILLDATX                                                         
         LLC   R0,BYTE                                                          
         AHI   R0,1                                                             
         B     BILLD010                                                         
*                                                                               
BILLDATX SAC   0                                                                
         B     EXITOK                                                           
*                                                                               
BILLMIN  DC    XL4'8C136000'       17:00                                        
BILLMAX  DC    AL4(((06*60)+30)*(60*38400)) 06:30AM                             
         EJECT                                                                  
*************************************************************                   
* MQTEST - PUT TO MQ                                        *                   
*************************************************************                   
                                                                                
MQPUTQ   NTR1                                                                   
         LARL  R1,BUFFER                                                        
         MVC   0(64,R1),0(R2)      COPY MESSAGE TO BUFFER                       
         XC    0(64,R2),0(R2)                                                   
         MVC   BUFFLEN,=A(64)                                                   
*                                                                               
         MVC   64(20,R1),SPACES                                                 
         MVC   84(20,R1),=C'ERROR-RUNNER ABENDED'                               
         MVC   BUFFLEN,=A(104)                                                  
         SAC   0                                                                
*                                                                               
         BRAS  RE,MQCONNEC         CONNECT TO MQ QUEUE MANAGER                  
*                                                                               
         MVC   OBJDESC_OBJECTTYPE,=A(MQOT_Q)  OBJECT IS A QUEUE                 
         MVC   OBJDESC_OBJECTNAME,QUEUE       INPUT QUEUE NAME                  
*                                                                               
         LA    RF,MQOO_OUTPUT                                                   
         ST    RF,OPENOPT                                                       
*                                                                               
         BRAS  RE,MQOPEN           OPEN THE QUEUE                               
         BRAS  RE,MQPUT            PUT THE MESSAGE TO THE MQ QUEUE              
         BRAS  RE,MQCOMMIT         COMMIT                                       
         BRAS  RE,MQCLOSE          CLOSE QUEUE                                  
         BRAS  RE,MQDISCON         DISCONNECT FROM MQ QUEUE MANAGER             
         J     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        INITIALISE ERROR HANDLING                          *                   
*************************************************************                   
SETESTAE NTR1                                                                   
         ESTAEX ERREXIT,CT,ASYNCH=YES,TERM=NO                                   
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         B     EXITOK                                                           
         EJECT                                                                  
*************************************************************                   
*        CHECK OPER INTERRUPT                               *                   
*************************************************************                   
CHKOPER  NTR1                                                                   
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
**TEMP                                                                          
         MVI   OPERMQ,YES                                                       
         B     CHEKRSET                                                         
**TEMP                                                                          
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
***********************************************************************         
* CLEAR SYSTEM DOWN AND SET FORMALLY CLOSED                           *         
* THIS CODE IS ALSO IN DDLOCKSPC                                      *         
***********************************************************************         
CLRSYS   NTR1  ,                                                                
         BRAS  RE,ARSOFF                                                        
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,@DSYSHDR      FULL=A(SYSTEM HEADER)                        
         SAC   512                                                              
         USING DMSYSHDR,R2                                                      
*                                                                               
         CLI   DSYSSTAT,0          TEST ALREADY CLEARED                         
         BE    CLRSY990                                                         
         MVI   DSYSSTAT,0          SET SYSTEM TO CLOSED                         
*                                                                               
         XR    R1,R1               CLEAR FILES FIRST                            
         ICM   R1,3,DSYFILES                                                    
         LA    R2,DSYDATA          R2=A(FILES)                                  
         USING DSFILHDR,R2                                                      
*                                                                               
CLRSY010 XC    DSEOF1,DSEOF1       REMOVE EOF1 & 2                              
         XC    DSEOF2,DSEOF2                                                    
         XC    DSFILACT,DSFILACT   AND ACTIONS                                  
         LA    R2,DSFILLNQ(,R2)    NEXT FILE                                    
         BCT   R1,CLRSY010                                                      
*                                                                               
         LT    R2,SVSHRMEM         TEST RECOVERY IN SHRMEM                      
         BZ    CLRSY012            NO                                           
         SAC   0                                                                
         SAM31                                                                  
         B     CLRSY014                                                         
*                                                                               
CLRSY012 L     R2,@DSYSHDR         FULL=A(SYSTEM HEADER)                        
         USING DMSYSHDR,R2                                                      
         ICM   RF,15,DSYABUFF      CLEAR RECOVERY HEADER                        
         BZ    CLRSY020                                                         
         LR    R2,RF                                                            
         USING VRCVHDR,R2          VERMONT DSECT                                
*                                                                               
CLRSY014 SR    R3,R3                                                            
         ICM   R3,3,VRCVNXT        PICK UP LEN OF DATA                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR WHOLE BUFFER                           
         SAC   512                                                              
         SAM24                                                                  
*                                                                               
CLRSY020 L     R2,@DSYSHDR         FULL=A(SYSTEM HEADER)                        
         USING DMSYSHDR,R2                                                      
         ICM   RF,15,DSYALOCK      CLEAR LOCKTAB                                
         LR    R2,RF                                                            
         LH    R3,0(,R2)           R3=MAX NUMBER OF LOCKS                       
         SLL   R3,3                *8                                           
         XC    2(2,R2),2(R2)       CLEAR NUMBER OF LOCKS                        
         LA    R2,16(RF)                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               CLEAR LOCK TABLE                             
*                                                                               
CLRSY990 BRAS  RE,ARSOFF           RETURN TO NORMAL MODE                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY JOB NUMBER OR ADV TASK FROM FULL TO DUB1   *                   
*************************************************************                   
DISJOB   TM    FULL+2,X'80'        TEST OFFLINE JOB                             
         BO    DJOB085                                                          
         CLI   FULL+3,0                                                         
         BE    DJOB080                                                          
         MVC   FULL+0(1),FULL+3                                                 
         MVI   FULL+1,0                                                         
*                                                                               
DJOB080  MVC   BYTE,FULL+0                                                      
*                                                                               
         USING FACITABD,R1                                                      
         L     R1,AFACITAB                                                      
         NI    BYTE,X'0F'          REMOVE AOR FLAG IF ON                        
DJOB081  CLC   BYTE,FACIID         FIND ADV SYSTEM                              
         JE    DJOB082                                                          
         AHI   R1,L'FACITAB                                                     
         CLI   FACIID,X'FF'        CHECK EOT                                    
         JNE   DJOB081                                                          
         DC    H'0'                                                             
*                                                                               
DJOB082  MVC   DUB1(4),FACISN4                                                  
         DROP  R1                                                               
         CLI   DUB1+3,C' '                                                      
         BE    *+14                                                             
         MVC   DUB1+2(1),DUB1+3                                                 
         MVI   DUB1+3,C' '                                                      
*                                                                               
         MVC   DUB1+4(2),=C'/#'                                                 
         MVC   DUB1+6(1),FULL+1                                                 
*                                                                               
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
         WTO   'END OF RECOVERY JOB ',MCSFLAG=HRDCPY                            
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
         PUT   SYSPRINT,T2                                                      
         PUT   SYSPRINT,T3                                                      
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
*                                                                               
*************************************************************                   
CARDTAB  DS    0F                                                               
         DC    C'DDSIO  ',AL1(4,09),X'8000',AL3(SETDDSIO)                       
         DC    C'MODE   ',AL1(3,09),X'0000',AL3(MODE)                           
         DC    C'DSPACE ',AL1(5,00),X'8000',AL3(GETDSPC)                        
*        DC    C'MONSTER',AL1(6,02),X'0000',AL3(MONSTER)                        
         DC    C'BILLDAT',AL1(6,00),X'0000',AL3(BILLDAT)                        
         DC    C'QMGR   ',AL1(3,47),X'0000',AL3(QMGR)                           
         DC    C'QUEUE  ',AL1(4,47),X'0000',AL3(QUEUE)                          
         DC    X'0000'                                                          
                                                                                
*************************************************************                   
*        CARD OUTPUT AREAS SET WITH DEFAULTS                                    
*************************************************************                   
DDSIO    DC    CL10'DDSIO'                                                      
MODE     DC    CL10'INIT'          DEFAULT TO INIT                              
*ONSTER  DC    CL3'OLD'            DEFAULT TO OLD                               
SYLIST   DC    AL1(NO)                                                          
SYSTEM   DC    X'00'                                                            
BILLDAT  DC    AL1(NO)                                                          
*                                                                               
QMGR     DC    CL48' '                                                          
QUEUE    DC    CL48' '                                                          
*                                                                               
QCORID   DC    XL(L'MSGDESC_CORRELID)'00'                                       
QMSGID   DC    XL(L'MSGDESC_MSGID)'00'                                          
         EJECT                                                                  
**********************************************************************          
* VALIDATE INPUT CARDS                                                          
**********************************************************************          
VALCARD  NTR1                                                                   
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,79(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         BE    EXITOK                                                           
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
*                                                                               
VALC030  SR    R1,R2                                                            
*                                                                               
VALC031  BCTR  R1,0                                                             
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
         SR    RE,RE                                                            
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
*        GET & SET DSPACE VALUE                                                 
*************************************************************                   
SETDDSIO NTR1                                                                   
         L     RF,=V(DDSIO)       SET UP DDSIO FIRST                            
         MVC   0(8,RF),0(R2)      SET IN FROM CARD DDSIO=                       
         B     EXITOK                                                           
                                                                                
*************************************************************                   
*        GET & SET DSPACE VALUE                                                 
*************************************************************                   
GETDSPC  NTR1                                                                   
         MVC   DSPACE,0(R2)       VALUE SET IN CARD DSPACE=                     
         L     RF,=A(SSB)                                                       
         USING SSBD,RF                                                          
         MVC   SSODSPAC,DSPACE                                                  
         DROP  RF                                                               
         B     EXITOK                                                           
                                                                                
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
*&&DO                                                                           
*************************************************************                   
*        GET FSESYS FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
VALSYSI  NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYSI0 EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYSI1                                                         
         BXLE  R4,R0,VALSYSI0      NEXT                                         
         B     EXITL               ERROR SE SYS NOT FOUND                       
VALSYSI1 LR    R1,R4                                                            
         B     EXITOK                                                           
                                                                                
*************************************************************                   
*        GET FSESYS FROM 0(R2) (R1)=EX LEN                  *                   
*************************************************************                   
VALSYS   NTR1                                                                   
         LR    RF,R1                                                            
         L     R4,=V(SELIST)       MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYS0  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         BE    VALSYS1                                                          
         BXLE  R4,R0,VALSYS0       NEXT                                         
         B     CERRSES             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSYS1  LA    R1,SEFILN           FIND A ZERO ENTRY                            
VALSYS1A CLI   0(R1),0                                                          
         BE    VALSYS2                                                          
         LA    R1,1(R1)                                                         
         B     VALSYS1A                                                         
*                                                                               
VALSYS2  MVC   0(1,R1),SESYS       SET NUMBER IN LIST                           
         B     EXITOK                                                           
*                                                                               
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         DROP  R4                                                               
*&&                                                                             
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
         LA    R3,CARD                                                          
INIT02   GOTO1 =V(CARDS),DMCB,(R3),=C'RE00'                                     
         CLC   =C'/*',0(R3)                                                     
         BE    INIT04                                                           
*                                                                               
         MVC   PLINE+1(80),0(R3)                                                
         BRAS  RE,PRINTL           PRINT PARAMETER CARD                         
*                                                                               
         LR    R1,R3               PASS TO VALCARD                              
         BRAS  RE,VALCARD          READ KEYWORD=VALUES                          
         BE    INIT02                                                           
         B     XBASE               NEQ MEANS INVALID KEYWORD                    
*                                                                               
INIT04   XC    DMCB(24),DMCB                                                    
         MVC   DMCB(4),=XL4'00030000'                                           
         GOTO1 ALOCKSPC,DMCB                                                    
*&&UK                                                                           
         XC    DMCB(24),DMCB       DO TABS ENQUIRY                              
         MVC   DMCB(4),=XL4'20008001'                                           
         GOTO1 ALOCKSPC,DMCB       MAKE SURE WE HAVE TBLET                      
*&&                                                                             
         L     RF,=A(SSB)                                                       
         USING SSBD,RF                                                          
         MVC   ALET,SSOALET                                                     
         MVC   TBLET,SSOTBLET                                                   
         MVC   AFACITAB,SSOAFID                                                 
         DROP  RF                                                               
*&&DO                                                                           
         USING FACIDD,RF                                                        
         LARL  RF,FACIDTAB                                                      
INIT05   CLI   0(RF),X'FF'         END OF TABLE?                                
         JE    *+2                 DEATH=INVALID DSPACE DIE                     
         CLC   FACIDSPC,DSPACE     MATCH ON DATASPACE                           
         BE    INIT06                                                           
         AHI   RF,FACIDLNQ                                                      
         B     INIT05                                                           
INIT06   MVC   AFACITAB,FACAID     A(FACIDXXX) TABLE                            
         DROP  RF                                                               
*&&                                                                             
         LAM   AR2,AR2,ALET                                                     
         ICM   R2,15,DMCB+16                                                    
         BZ    INIT09                                                           
*                                                                               
INIT09   BRAS  RE,SETOPS           SET UP OPER COMMS                            
*                                                                               
         LHI   R0,4                MAKE JOB NON SWAPPABLE                       
         LNR   R0,R0                                                            
         SVC   247                                                              
         B     EXITOK                                                           
         EJECT                                                                  
************************************************************                    
*        INIT MQ / READ CARDS /OPEN DATASETS               *                    
************************************************************                    
INITMQ   NTR1                                                                   
*                                                                               
         BLDL  0,ENTRYPTS            BUILD LIST OF APPC/MVS ENTRY PTS           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         LOAD  DE=CSQBCONN                                                      
         ST    R0,AMQBCONN                                                      
         LOAD  DE=CSQBOPEN                                                      
         ST    R0,AMQBOPEN                                                      
         LOAD  DE=CSQBPUT                                                       
         ST    R0,AMQBPUTN                                                      
         LOAD  DE=CSQBGET                                                       
         ST    R0,AMQBGETN                                                      
         LOAD  DE=CSQBCOMM                                                      
         ST    R0,AMQBCOMM                                                      
         LOAD  DE=CSQBCLOS                                                      
         ST    R0,AMQBCLOS                                                      
         LOAD  DE=CSQBDISC                                                      
         ST    R0,AMQBDISC                                                      
*                                                                               
         LA    R3,CARD                                                          
*                                                                               
         J     EXITOK                                                           
*                                                                               
         CMQA  LIST=YES,EQUONLY=NO                                              
*                                                                               
OBJDESC  CMQODA  LIST=YES  OBJECT DESCRIPTOR                                    
*                                                                               
MSGDESC  CMQMDA  LIST=YES  MESSAGE DESCRIPTOR                                   
         ORG     MSGDESC_FORMAT                                                 
         DC      CL8'MQSTR   '     MQFMT_STRING  FOR DATA FORMAT                
         ORG                                                                    
*                                                                               
PUTOPTS  CMQPMOA LIST=YES  PUT MESSAGE OPTIONS                                  
GETOPTS  CMQGMOA LIST=YES  GET MESSAGE OPTIONS                                  
         EJECT                                                                  
************************************************************                    
* MQSERIES CALL PARAMETERS                                 *                    
************************************************************                    
*                                                                               
HCONN    DS      F         MQ QMGR CONNECTION HANDLE                            
HOBJ     DS      F         OBJECT HANDLE                                        
OPENOPT  DS      F         MQOPEN OPTIONS                                       
CLOSEOPT DS      F         MQCLOSE OPTIONS                                      
COMPCODE DS      F         COMPLETION CODE                                      
REASON   DS      F         QUALIFIES COMPLETION CODE                            
BUFFLEN  DS      F         LENGTH OF MQBUFFER AREA                              
DATALEN  DS      F         LENGTH OF THE MESSAGE                                
IOL      DS      XL4                                                            
         EJECT                                                                  
************************************************************                    
* PARAMETER LISTS FOR MQSERIES CALLS                       *                    
*  F    A(ROUTINE)                                         *                    
*  CL16 EBCDIC ROUTINE NAME                                *                    
*  XL1  FLAGS                                              *                    
*  XL3  SPARE                                              *                    
*  PARAMETERS (STANDARD IBM FORMAT)                        *                    
************************************************************                    
*                                                                               
AMQBCONN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_CONNECT'                                                 
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(QMGR)                                                  
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBOPEN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_OPEN'                                                    
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(OBJDESC)                                               
         DC    X'00',AL3(OPENOPT)                                               
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBPUTN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_PUT'                                                     
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(MSGDESC)                                               
         DC    X'00',AL3(PUTOPTS)                                               
         DC    X'00',AL3(BUFFLEN)                                               
         DC    X'00',AL3(BUFFER)                                                
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBGETN DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_GET'                                                     
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(MSGDESC)                                               
         DC    X'00',AL3(GETOPTS)                                               
         DC    X'00',AL3(BUFFLEN)                                               
         DC    X'00',AL3(BUFFER)                                                
         DC    X'00',AL3(DATALEN)                                               
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBCOMM DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_COMMIT'                                                  
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBCLOS DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_CLOSE'                                                   
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(HOBJ)                                                  
         DC    X'00',AL3(CLOSEOPT)                                              
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
AMQBDISC DC    A(0)                STRUCTURE                                    
         DC    CL16'MQ_DISCONNECT'                                              
         DC    X'00'                                                            
         DC    XL3'000000'                                                      
         DC    X'00',AL3(HCONN)                                                 
         DC    X'00',AL3(COMPCODE)                                              
         DC    X'80',AL3(REASON)                                                
*                                                                               
ENTRYPTS DS    0F                                                               
         DC    Y((ENTRYLSQ-ENTRYSTQ)/60)   NUMBER OF TABLE ENTRIES              
         DC    H'60'                       MUST REMAIN AS 60                    
ENTRYSTQ EQU   *                                                                
*                                                                               
CSQBCLOS DC    CL8'CSQBCLOS'                                                    
         DC    XL52'00'                                                         
CSQBCOMM DC    CL8'CSQBCOMM'                                                    
         DC    XL52'00'                                                         
CSQBCONN DC    CL8'CSQBCONN'                                                    
         DC    XL52'00'                                                         
CSQBDISC DC    CL8'CSQBDISC'                                                    
         DC    XL52'00'                                                         
CSQBGET  DC    CL8'CSQBGET'                                                     
         DC    XL52'00'                                                         
CSQBOPEN DC    CL8'CSQBOPEN'                                                    
         DC    XL52'00'                                                         
CSQBPUT  DC    CL8'CSQBPUT'                                                     
         DC    XL52'00'                                                         
*                                                                               
ENTRYLSQ EQU   *                                                                
         EJECT                                                                  
************************************************************                    
* CALL MQ                                                  *                    
* UPON RETURN                                              *                    
* COMPCODE CONTAINS THE MQ COMPLETION CODE                 *                    
* REASON   CONTAINS THE MQ REASON CODE                     *                    
************************************************************                    
MQCONNEC LA    R2,AMQBCONN         CONNECT                                      
         J     CALLMQ                                                           
MQOPEN   LA    R2,AMQBOPEN         OPEN                                         
         J     CALLMQ                                                           
MQPUT    LA    R2,AMQBPUTN         PUT                                          
         J     CALLMQ                                                           
MQGET    LA    R2,AMQBGETN         GET                                          
         J     CALLMQ                                                           
MQCOMMIT LA    R2,AMQBCOMM         COMMIT                                       
         J     CALLMQ                                                           
MQCLOSE  LA    R2,AMQBCLOS         CLOSE                                        
         J     CALLMQ                                                           
MQDISCON LA    R2,AMQBDISC         DISCONNECT                                   
         J     CALLMQ                                                           
                                                                                
************************************************************                    
* CALL MQ COMMAND TO EXECUTE                                                    
************************************************************                    
CALLMQ   NTR1                                                                   
*                                                                               
         SAM31                     SWITCH TO 31-BIT MODE                        
         L     RF,0(R2)            RF = A(MQ ROUTINE)                           
         LA    R3,24(R2)           R3 = A(PARAMETER LIST)                       
         CALL  (15),MF=(E,(R3))                                                 
         SAM24                     SWITCH BACK TO 24-BIT MODE                   
*                                                                               
         MVC   PLINE+0(16),4(R2)   PRINT ROUTINE NAME AND RETURN CODES          
         MVC   PLINE+17(8),=C'COMP. OK'                                         
         CLC   COMPCODE,=A(MQCC_OK)                                             
         BE    CALLMQ50                                                         
         MVC   PLINE+17(8),=C'WARNING!'                                         
         CLC   COMPCODE,=A(MQCC_WARNING)                                        
         BE    CALLMQ10                                                         
         MVC   PLINE+17(8),=C'*FAILED!'                                         
         CLC   COMPCODE,=A(MQCC_FAILED)                                         
         BE    CALLMQ10                                                         
         DC    H'0'                UNKNOWN COMPLETION CODE                      
*                                                                               
CALLMQ10 EQU   *                                                                
         MVC   PLINE+26(7),=C'REASON='                                          
         EDIT  REASON,(5,PLINE+34),ZERO=NOBLANK                                 
         MVI   PLINE+39,C':'                                                    
*                                                                               
         L     RF,=A(MQ_REASON_CODE_TABLE)                                      
CALLMQ20 CLI   0(RF),X'FF'         END OF TABLE?                                
         BNE   *+14                NO                                           
         MVC   PLINE+41(22),=C'*** UNKNOWN REASON ***'                          
         B     CALLMQ30            REASON CODE NOT IN TABLE                     
*                                                                               
         CLC   REASON,0(RF)        FIND MATCH ON REASON CODE                    
         BE    *+12                GOT IT                                       
         LA    RF,28(RF)           BUMP TO NEXT TABLE ENTRY                     
         B     CALLMQ20                                                         
*                                                                               
         MVC   PLINE+41(24),4(RF)                                               
*                                                                               
CALLMQ30 CLC   REASON,=F'2033'     2033 IS JUST EOF SO OK                       
         JE    CALLMQ40            BUT MUST EXIT NEQ                            
*                                                                               
         BRAS  RE,PRINTL                                                        
*                                                                               
CALLMQ40 CLC   COMPCODE,=A(MQCC_OK)                                             
*                                                                               
CALLMQ50 BRAS  RE,PRINTL                                                        
         XIT1                                                                   
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
*        TIMER TABLES  CL1'Y/N',AL3(FREQ),AL4(LAST TIME)    *                   
***********************************************************************         
TIMETAB  DS    0F                                                               
TIMEQJOB DC    AL1(NO),AL3(10),AL4(0)        QUICK CHECK FOR ABND FLG           
TIMEADVS DC    AL1(NO),AL3(1*100),AL4(0)     ADV CHECK 1 SECOND                 
TIMEJOBS DC    AL1(NO),AL3(10*100),AL4(0)    JOB CHECK 10 SECS                  
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
                                                                                
T1       DC    166C' '                                                          
T2       DC    166C' '                                                          
T3       DC    166C' '                                                          
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        DCBS & ADCONS                                      *                   
*************************************************************                   
SYSPRINT DCB   DSORG=PS,MACRF=PM,DDNAME=SYSPRINT,RECFM=FBA,LRECL=(166)          
*                                                                               
ADATAMGR DC    V(DATAMGR)                                                       
ALOCKSPC DC    V(LOCKSPC)                                                       
*                                                                               
ACOMM    DC    A(0)                                                             
*                                                                               
*      ++INCLUDE FACIDTABL                                                      
*                                                                               
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
*        ECBS AND TIMER FIELDS                              *                   
*************************************************************                   
         DS    0F                                                               
ECBLST2  EQU   *                                                                
ADSPECB  DC    A(DSPACECB)                                                      
AOPERECB DC    A(0)                                                             
         DC    X'80',AL3(TIMRECB2) A(TIMER ECB)                                 
*                                                                               
STIMERID DS    XL4                                                              
TIME     DS    XL4                                                              
DSPACECB DS    XL4                                                              
         EJECT                                                                  
*************************************************************                   
* THE TIMER EXIT ROUTINE.  IT POSTS AN ECB.                 *                   
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
*        WORKING STORAGE DC                                 *                   
*************************************************************                   
         DS    0D                                                               
SORTBLK  DS    1024CL16                                                         
*                                                                               
         DS    0D                                                               
WORKAREA DC    60000D'00'                                                       
         EJECT                                                                  
*************************************************************                   
*        ERROR HANDLER                                      *                   
*************************************************************                   
*                                                                               
         DROP  RB                                                               
ERREXIT  DS    0D                                                               
         USING *,RB                                                             
         LR    RB,RF                                                            
         C     R0,=F'12'           TEST SDWA ACQUIRED                           
         BNE   *+8                 NO, JUST LET MVS PERC                        
         SR    RF,RF                                                            
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
         MVI   ENDTASK,YES         END JOB IF WE DIE IN **DMRC**                
*                                                                               
         LH    RF,ABENDS                                                        
         AHI   RF,1                                                             
         STH   RF,ABENDS                                                        
         CLC   ABENDS,=Y(200)                                                   
         BL    *+8                                                              
         MVI   ENDTASK,YES         END JOB AFTER 200 ABENDS                     
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
*                                                                               
         DROP  R2                                                               
*                                                                               
ERR000   EQU   *                                                                
*                                                                               
         L     R1,ESTAER1          AND POINTER TO SDWA                          
         USING SDWA,R1                                                          
*                                                                               
         L     R1,ESTAER1          R1 MUST POINT TO SDWA                        
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
         EJECT                                                                  
ERRWORK  DC    600D'00'                                                         
*                                                                               
       ++INCLUDE DDMQREASON                                                     
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
K        EQU   1024                                                             
*                                                                               
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
HALF     DS    H                                                                
BYTE     DS    X                                                                
FLAGS    DS    X                                                                
*                                                                               
SAVEF01  DS    4A                                                               
*                                                                               
DMCB     DS    6F                                                               
CARDEND  DS    A                                                                
*                                                                               
PAGES    DS    F                   NUMBER OF 4K PAGES                           
ADSDATA  DS    A                   ADDRESS OF DS BLOCK                          
AHEADER  DS    A                   ADDRESS OF CURRENT HEADER                    
*                                                                               
*TEMP                                                                           
*                                                                               
TODAY    DS    F                   WORKING AREAS FOR BILLDATE                   
TOMORROW DS    F                                                                
YESTERDY DS    F                                                                
BILLTIME DS    F                                                                
*                                                                               
*TEMP                                                                           
*                                                                               
ALET     DS    A                   DMGR ALET                                    
TBLET    DS    A                   TABS ALET                                    
*                                                                               
AFACITAB DS    A                   A(FACIDTAB) BY DSPACE                        
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
SVRCVR   DS    CL1                                                              
*                                                                               
LINE     DS    PL3                                                              
PAGE     DS    PL3                                                              
*                                                                               
SVSHRMEM DS    A                                                                
@DSYSTEM DS    A                   A(CURRENT SYSTEM IN DATASPACE)               
@DSYSHDR DS    A                   A(CURRENT DMSYSHDR ENTRY)                    
DINDEX   DS    A                   A(CURRENT RESOURCE) DMGR OR TABS             
DMOFFS   DS    A                   DATASPACE OFFSET                             
DMALET   DS    A                   ALET                                         
DMTOKN   DS    CL8                 TOKEN                                        
*                                                                               
OPENFLG  DS    XL1                                                              
ENDTASK  DS    X                   FLAG Y IF WE MUST ABEND                      
ABENDS   DS    H                   COUND ABENDS                                 
*                                                                               
TIMENTU  DS    A                   TIME NOW TUS USED FOR LOCGCHK                
CURRTOR  DS    A                   A CURRENT TOR                                
CURRFAC  DS    A                   A CURRENT FAC                                
CURRFACN DS    CL8                 CURRENT FAC NAME                             
*                                                                               
TIMENOW  DS    A                   BINARY TIME AT END OF WAIT                   
TIMEOLD  DS    A                   PREVIOUS POP TIME                            
*                                                                               
ASYSFLES DS    A                                                                
TOPJOB   DS    F                                                                
RESNUM   DS    XL1                 RESOURCE NUMBER                              
SEOPN    DS    CL1                 SE NUMBER                                    
SEOPNN   DS    CL7                 SE NAME                                      
SEFILN   DS    CL256               SYSTEMS FOR OPEN                             
*                                                                               
PLINE    DS    CL166                                                            
TITLE    DS    CL166                                                            
*                                                                               
SYSNAME  DS    CL8                                                              
*                                                                               
DSPACE   DS    C                   DATASPACE TYPE - R(EP)/A(DV)/T(ST)           
OPERMQ   DS    C                   MQ OPENED                                    
*                                                                               
CARD     DS    CL80                                                             
CARD1    DS    CL80                                                             
CARD2    DS    CL80                                                             
*                                                                               
IOAREA   DS    4096C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
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
         IEZCIB                                                                 
         IEZCOM                                                                 
         IHAASCB                                                                
         IHAASSB                                                                
         IEECHAIN                                                               
         IAZJSAB                                                                
         POP   ACONTROL                                                         
*IHASDWA MACRO                                                                  
         IHASDWA GR32=YES                                                       
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
* DMDFTPH                                                                       
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DMRCVRD                                                                       
       ++INCLUDE DMRCVRD                                                        
         PRINT ON                                                               
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* FASSBOFF                                                                      
SSBD     DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
SSOLNQ   EQU   *-SSBD                                                           
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FATABSPQ                                                                      
       ++INCLUDE FATABSPQ                                                       
         PRINT ON                                                               
* FACIDTAB                                                                      
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
* FATABSD                                                                       
       ++INCLUDE FATABSD                                                        
* FATABSJOB                                                                     
       ++INCLUDE FATABSJOB                                                      
* FAPIGFACD                                                                     
       ++INCLUDE FAPIGFACD                                                      
         PRINT ON                                                               
* FAD                                                                           
*      ++INCLUDE FAD                                                            
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065DMDMGRRCV 09/17/20'                                      
         END                                                                    
