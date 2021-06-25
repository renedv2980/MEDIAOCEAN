*          DATA SET FAMQIO     AT LEVEL 039 AS OF 07/19/12                      
*PHASE FAMQIOA                                                                  
         TITLE 'MQIO - MQ SERIES ASYCNCHRONOUS FACPAK INTERFACE'                
***********************************************************************         
* FAMQIO - FACPAK ONLINE MQ SERIES SCHEDULER                          *         
*          RUNS AS SUB TASK ATTACHED TO FACPAK                        *         
*                                                                     *         
* PROCESSES INCLUDE:                                                  *         
*                                                                     *         
* 1) SCHEDULE MQIO REQUESTS FROM APPLICATION PROGRAMS RUNNING IN      *         
*    FACPAK TASKS.                                                    *         
*                                                                     *         
* 2) HANDLE MQIO REQUESTS DIRECT FROM FACPAK TO RESET INPUT QUEUES.   *         
*                                                                     *         
* 3) HANDLE CONTROL REQUESTS DIRECT FROM FACPAK.                      *         
*                                                                     *         
* 4) PERFORM REGULAR SELF MONITORING AND ERROR HANDLING.              *         
*                                                                     *         
*                                                                     *         
* PARAMETERS:                                                         *         
*                                                                     *         
* 0(R1) = A(FAATC - FAMQIO ATTACHED SUB-TASK CONTROL BLOCK)           *         
*                                                                     *         
***********************************************************************         
FAMQIO   CSECT                                                                  
FAMQIO   AMODE 31                                                               
FAMQIO   RMODE ANY                                                              
         ENTRY TIMERECB                                                         
         PRINT NOGEN                                                            
         NBASE WORKL,**FAMQ**,RA,R9,WORK=WORKC                                  
         USING WORKD,RC            RC=A(W/S)                                    
         ST    RD,SAVERD                                                        
         L     R8,0(R1)                                                         
         USING ATCD,R8             R9=A(ATC ENTRY)                              
         ST    R8,AATC                                                          
         L     RF,ATCSYSFC                                                      
         L     RE,VSSB-SYSFACD(RF)                                              
         ST    RE,ASSB                                                          
         L     RE,VTCB-SYSFACD(RF)                                              
         ST    RE,ATCB                                                          
         MVC   ATCNAME,ANAME                                                    
*                                  INIT. FAATC CONTROL BLOCK VALUES             
         LA    RF,FAMQIO                                                        
         ST    RF,ATCASYNC                                                      
         XC    ATCPCNT(16),ATCPCNT                                              
         XC    ATCMQCC,ATCMQCC                                                  
         XC    ATCMQRC,ATCMQRC                                                  
         LA    RF,ATCSATCH                                                      
         STC   RF,ATCSTAT1         INITIALISE FAMQIO TASK STATUS                
         OI    ATCSTAT1,ATCSAACT                                                
         LA    RF,ATCCSNOQ                                                      
         STC   RF,ATCSTATI         INITIALISE FAMQIO CONNECTION STATUS          
         XC    ATCERRCD,ATCERRCD                                                
*                                                                               
         MVI   RETCODE,0                                                        
         MVI   OPERSTOP,C'N'                                                    
*                                  EXTRACT SSB MQ PARAMETERS                    
         USING SSBD,R3                                                          
         L     R3,ASSB                                                          
         MVC   SVMQION,SSBMQION                                                 
         ICM   RF,15,SSBAMQM                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MQMNAME,0(RF)                                                    
         ICM   RF,15,SSBAMQIN                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MQINNAME,0(RF)                                                   
         ICM   RF,15,SSBAMQOU                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MQOUNAME,0(RF)                                                   
         ICM   RF,15,SSBAMQWK                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   MQWKNAME,0(RF)                                                   
         ICM   RF,15,SSBAMQCT                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   MQCTNAME,0(RF)                                                   
         DROP  R3                                                               
*                                                                               
         MVI   QMGRFAIL,C'N'                                                    
         XC    ATCMQCC,ATCMQCC                                                  
         XC    ATCMQRC,ATCMQRC                                                  
         XC    ATCERRCD,ATCERRCD                                                
         B     MAIN                GOTO MAIN PROCESS CODE                       
         EJECT                                                                  
***********************************************************************         
* FAMQIO MAIN PROCESS CODE                                            *         
***********************************************************************         
MAIN     DS    0H                                                               
*                                                                               
         BAS   RE,LOADMQ           DYNAMICALLY LOAD MQ ROUTINES                 
         BAS   RE,BLDECBL          Build ECB LIST                               
*                                                                               
MAINRES  MVI   QMGRFAIL,C'N'       RETURN HERE ON RESTART                       
         XC    ATCMQCC,ATCMQCC                                                  
         XC    ATCMQRC,ATCMQRC                                                  
         XC    ATCERRCD,ATCERRCD                                                
*                                                                               
         BAS   RE,CLEARISP         CLEAR INPUT SIGNAL PENDING FLAGS             
*                                                                               
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R0,LOOPTIME                                                      
*                                                                               
         BAS   RE,CONNECT          CONNECT TO FACPAK MQ MANAGER                 
         BNE   MAINERR                                                          
*                                                                               
         BAS   RE,OPENQ            OPEN FACPAK MQ QUEUES                        
         BNE   MAINERR                                                          
*                                                                               
         BAS   RE,INQUIRY          INQUIRE STATUS OF MQ CONNECTION              
         BNE   MAINERR4                                                         
         XC    MQFRCVAL,MQFRCVAL   CLEAR MQ FAILURE RETRY COUNT IF OK           
*                                                                               
MAINLOOP BAS   RE,INITTIME                                                      
*                                                                               
         TBIN  MILI                                                             
         ST    R1,ATCWTIME         SET WAIT TIME                                
         L     R1,ATCWCNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ATCWCNT          SET WAIT COUNT                               
         TIME  BIN                                                              
         ST    R0,LOOPTIME                                                      
*                                                                               
         USING SSBD,RF                                                          
         L     RF,ASSB                                                          
         TM    SSBSTAT1,SSBSEOJ    EXIT IF FACPAK END OF JOB SHUTDOWN           
         BNZ   MAINCLOS                                                         
         DROP  RF                                                               
*                                  SET FAMQIO TASK ACTIVE FLAG                  
         NI    ATCSTAT1,255-ATCSAACT                                            
*                                  WAIT ON LIST FOR EVENT COMPLETION            
         LA    R1,ECBLST                                                        
         WAIT  ECBLIST=(1)                                                      
*                                                                               
         USING SSBD,RF                                                          
         OI    ATCSTAT1,ATCSAACT   SET FAQMIO TASK BUSY FLAG                    
         L     RF,ASSB                                                          
         TM    SSBSTAT1,SSBSEOJ    EXIT IF FACPAK END OF JOB SHUTDOWN           
         BO    MAINCLOS                                                         
         DROP  RF                                                               
*                                  PROCESS EVENT IN ECB LIST                    
         MVI   ECBFOUND,C'N'       SET FLAG FOR ECB EVENT NOT FOUND             
*                                                                               
MAINPINP DS    0H                  PROCESS FACPAK MQ INPUT QUEUES               
         BAS   RE,PINPUT                                                        
         BNE   MAINERR1                                                         
*                                                                               
MAINRQIO DS    0H                  PROCESS FACPAK TASK MQIO REQUEST             
         BAS   RE,PREQMQIO                                                      
         BNE   MAINERR2                                                         
*                                                                               
MAINRSET TM    ATCECB,X'40'        PROCESS FACPAK CONTROL REQUEST               
         BZ    MAINTIM                                                          
         MVI   ATCECB,0                                                         
         MVI   ECBFOUND,C'Y'                                                    
         OC    ATCECB,ATCECB                                                    
         BNZ   MAINCLOS                                                         
         BAS   RE,PRESET           PROCESS RESET FACPAK INPUT QUEUES            
         BNE   MAINERR3                                                         
*                                                                               
MAINTIM  TM    TIMERECB,X'40'      PROCESS TIMER TIMEOUT EVENT                  
         BZ    MAINCHK                                                          
         MVI   ECBFOUND,C'Y'                                                    
         XC    TIMERECB,TIMERECB                                                
         BAS   RE,INQUIRY          INQUIRE STATUS OF MQ CONNECTION              
         BNE   MAINERR4                                                         
*                                                                               
MAINCHK  DS    0H                  CHECK AN ECB EVENT WAS FOUND                 
         CLI   ECBFOUND,C'Y'                                                    
         BE    MAINLOOP                                                         
         DC    H'0'                                                             
*                                                                               
*                                  HANDLE ERROR EXCEPTION CONDITIONS            
*                                                                               
MAINERR  DS    0H                                                               
         LA    R0,0                                                             
         CLI   QMGRFAIL,C'Y'                                                    
         BE    MAINMQF                                                          
         DC    H'0'                                                             
*                                                                               
MAINERR1 DS    0H                                                               
         LA    R0,1                                                             
         CLI   QMGRFAIL,C'Y'                                                    
         BE    MAINMQF                                                          
         DC    H'0'                                                             
*                                                                               
MAINERR2 DS    0H                                                               
         LA    R0,2                                                             
         CLI   QMGRFAIL,C'Y'                                                    
         BE    MAINMQF                                                          
         DC    H'0'                                                             
*                                                                               
MAINERR3 DS    0H                                                               
         LA    R0,3                                                             
         CLI   QMGRFAIL,C'Y'                                                    
         BE    MAINMQF                                                          
         DC    H'0'                                                             
*                                                                               
MAINERR4 DS    0H                                                               
         LA    R0,4                                                             
         CLI   QMGRFAIL,C'Y'                                                    
         BE    MAINMQF                                                          
         DC    H'0'                                                             
*                                                                               
MAINMQF  DS    0H                  PROCES MQ FAILURE                            
         BAS   RE,PMQFAIL                                                       
         BNE   MAINEND             EXIT IF UNRECOVERABLE                        
*                                                                               
         USING SSBD,RF                                                          
         L     RF,ASSB                                                          
         TM    SSBSTAT1,SSBSEOJ    EXIT IF FACPAK END OF JOB SHUTDOWN           
         BNZ   MAINCLOS                                                         
         DROP  RF                                                               
         B     MAINRES             RETURN TO RESTART MAIN PROCESSING            
*                                                                               
MAINCLOS DS    0H                  CLOSE DOWN FAMQIO REQUEST                    
         BAS   RE,CLOSEQ           CLOSE FACPAK MQ QUEUES                       
         BAS   RE,DISCONN          DISCONNECT FROM FACPAK MQ MANAGER            
*                                                                               
MAINEND  BAS   RE,DELMQ            DELETE LOADED MQ ROUTINES                    
*                                                                               
         NI    ATCSTAT1,ATCSATCH   RESET TASK STATUS                            
*                                                                               
MXBASE   XBASE RC=RETCODE,RL=1     RETURN FROM ATTACHED SUB TASK                
         EJECT                                                                  
***********************************************************************         
*  REINITIALISE TIMEOUT TIMER                                         *         
***********************************************************************         
INITTIME NTR1                                                                   
*                                                                               
         TIME  BIN                 GET THE CURRENT TIME                         
         S     R0,LOOPTIME         R0 = ELASPED TIME SINCE LAST LOOP            
         L     R2,WAITSECS         R2 = THE WAIT INTERVAL                       
         CR    R0,R2               HAVE WE WAITED THE FULL INTERVAL?            
         B     ITIM010                                                          
* ??     BL    ITIM010             YES                                          
*                                                                               
         LA    R1,DMCB                                                          
         L     RF,ATIMRECB         POST TIME OUT ECB                            
         ST    RF,4(R1)                                                         
         BAS   RE,TIMEOUT                                                       
         B     ITIMX                                                            
*                                                                               
ITIM010  DS    0H                                                               
*        SR    R2,R0               R2 = REMAINING TIME TO WAIT                  
         OC    STIMERID,STIMERID                                                
         BZ    ITIM020                                                          
         STIMERM CANCEL,ID=STIMERID                                             
*                                                                               
ITIM020  DS    0H                                                               
         L     R2,WAITSECS         R2 = THE WAIT INTERVAL                       
         ST    R2,TIMERFWD                                                      
         STIMERM SET,                                                  +        
               ID=STIMERID,                                            +        
               BINTVL=TIMERFWD,                                        +        
               PARM=ATIMRECB,                                          +        
               EXIT=TIMEOUT                                                     
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ITIMX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS MQ RESET CONTROL                                            *         
***********************************************************************         
         USING MQIOD,R7                                                         
PRESET   NTR1  ,                                                                
         USING MQIOLD,R3                                                        
         ICM   R3,15,ATCAIOPL                                                   
         BZ    PRESOK                                                           
PRES010  TM    MQIOLFLG,MQIOLBSY                                                
         BO    PRES020                                                          
         TM    MQIOLFLG,MQIOLCTL                                                
         BO    PRES100                                                          
         TM    MQIOLFLG,MQIOLRD                                                 
         BO    PRES200                                                          
*                                                                               
PRES020  ICM   R3,15,MQIOLNXT                                                   
         BZ    PRESOK                                                           
         B     PRES010                                                          
*                                                                               
PRES100  MVC   HOBJ,HCOBJ                                                       
         B     PRES300                                                          
*                                                                               
PRES200  MVC   HOBJ,HIOBJ                                                       
         B     PRES300                                                          
*                                                                               
PRES300  ICM   R7,15,MQIOLC                                                     
         BAS   RE,SETMSG                                                        
         BNE   PRESNO                                                           
         LA    R0,MQCC_OK                                                       
         C     R0,MQIOCC                                                        
         BE    PRES310                                                          
         OI    MQIOLFLG,MQIOLBSY+MQIOLISP                                       
         B     PRES020                                                          
*                                                                               
PRES310  DS    0H                                                               
         BAS   RE,GETMSG                                                        
         BNE   PRESNO                                                           
         LA    R0,MQRC_NO_MSG_AVAILABLE                                         
         C     R0,MQIORC                                                        
         BNE   PRES320                                                          
         CLC   SVMQION,=F'3'                                                    
         BNE   WTOLAB4                                                          
         WTO   '*FACPAK* FAMQIO PRESET NO_MSG_AVAILABLE',MCSFLAG=HRDCPY         
*                                                                               
WTOLAB4  DS    0H                                                               
         B     PRES020                                                          
*                                                                               
PRES320  TM    MQIOLFLG,MQIOLRD                                                 
         BZ    PRES400                                                          
         BAS   RE,PUTWRK                                                        
         BNE   PRESNO                                                           
*                                                                               
PRES400  BAS   RE,COMMIN                                                        
         BNE   PRESNO                                                           
*                                                                               
         NI    MQIOLFLG,X'FF'-MQIOLISP                                          
         OI    MQIOLFLG,MQIOLBSY                                                
*                                                                               
         LA    R1,MQIOLECB         POST MQIOLST ENTRY RETURN ECB                
         POST  (1)                                                              
*                                                                               
         ICM   R1,15,ATCARECB      POST RETURN ECB                              
         POST  (1)                                                              
*                                                                               
         B     PRES020                                                          
*                                                                               
PRESOK   B     YES                                                              
PRESNO   B     NO                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS INPUT QUEUE CONTROL                                         *         
***********************************************************************         
         USING MQIOD,R7                                                         
PINPUT   NTR1  ,                                                                
*                                                                               
PINP100  ICM   R3,15,ATCAIOPL                                                   
         BZ    PINPOK                                                           
         USING MQIOLD,R3                                                        
*                                                                               
PINP110  TM    MQIOLFLG,MQIOLCTL                                                
         BO    PINP120                                                          
         TM    MQIOLFLG,MQIOLRD                                                 
         BO    PINP120                                                          
         B     PINP130                                                          
*                                                                               
PINP120  ICM   R7,15,MQIOLC                                                     
         TM    MQIOGECB,X'40'                                                   
         BZ    PINP130                                                          
         XC    MQIOGECB,MQIOGECB                                                
         B     PINP200                                                          
*                                                                               
PINP130  ICM   R3,15,MQIOLNXT                                                   
         BZ    PINPOK                                                           
         B     PINP110                                                          
*                                                                               
PINP200  MVI   ECBFOUND,C'Y'                                                    
         TM    MQIOLFLG,MQIOLCTL                                                
         BZ    PINP204                                                          
*                                                                               
         MVC   HOBJ,HCOBJ                                                       
         B     PINP210                                                          
                                                                                
PINP204  TM    MQIOLFLG,MQIOLRD                                                 
         BZ    *+14                                                             
         MVC   HOBJ,HIOBJ                                                       
         B     PINP210                                                          
         DC    H'0'                                                             
*                                                                               
PINP210  BAS   RE,GETMSG                                                        
         BNE   PINPNO                                                           
         LA    R0,MQRC_NO_MSG_AVAILABLE                                         
         C     R0,MQIORC                                                        
         BNE   PINP212                                                          
         CLC   SVMQION,=F'3'                                                    
         BNE   WTOLAB5                                                          
         WTO   '*FACPAK* FAMQIO PINPUT NO_MSG_AVAILABLE',MCSFLAG=HRDCPY         
*                                                                               
WTOLAB5  NI    MQIOLFLG,255-(MQIOLISP+MQIOLBSY)                                 
         B     PINP130                                                          
*                                                                               
PINP212  TM    MQIOLFLG,MQIOLRD                                                 
         BZ    PINP220                                                          
         BAS   RE,PUTWRK                                                        
         BNE   PINPNO                                                           
*                                                                               
PINP220  BAS   RE,COMMIN                                                        
         BNE   PINPNO                                                           
         NI    MQIOLFLG,X'FF'-MQIOLISP                                          
         OI    MQIOLFLG,MQIOLBSY                                                
*                                                                               
         LA    R1,MQIOLECB         POST MQIOLST ENTRY RETURN ECB                
         POST  (1)                                                              
*                                                                               
         ICM   R1,15,ATCARECB      POST RETURN ECB                              
         POST  (1)                                                              
         B     PINP130                                                          
*                                                                               
PINPOK   B     YES                                                              
PINPNO   B     NO                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* Process MQIO request                                                *         
* DDMQIO post to awake FAMQIO, find task that did it                  *         
***********************************************************************         
         USING MQIOD,R7                                                         
PREQMQIO NTR1                                                                   
         L     R3,ATCB             Set up BXLE and run down task                
         LH    R4,0(,R3)                                                        
         L     R5,2(,R3)                                                        
         LA    R3,6(,R3)                                                        
         USING TCBD,R3                                                          
*                                                                               
PREQ010  LA    RF,TCBMQERQ                                                      
         TM    0(RF),X'40'         Posted                                       
         BO    PREQ030             Yes                                          
*                                                                               
PREQ020  BXLE  R3,R4,PREQ010       No, next task                                
         B     PREQOK                                                           
*                                                                               
PREQ030  MVI   ECBFOUND,C'Y'       Set found an ECB posted                      
         XC    0(4,RF),0(RF)       Clear ECB in Task                            
         ICM   R7,15,TCBMQDAT                                                   
         MVC   AECBRETN,TCBMQERT   ECB to post to for return                    
         DROP  R3                                                               
*                                                                               
PREQ100  ST    R9,MQIOAATC                                                      
         CLI   MQIOACT,MQIOAPUT                                                 
         BE    MQIOPUT             Put to OUTPUT queue                          
         CLI   MQIOACT,MQIOAPTQ                                                 
         BE    MQIOPUTQ            Put to specific queue                        
         CLI   MQIOACT,MQIOARPY                                                 
         BE    MQIORPY                                                          
         CLI   MQIOACT,MQIOACMT                                                 
         BE    MQIOCMT                                                          
         CLI   MQIOACT,MQIOAWRK                                                 
         BE    MQIOWRK                                                          
         DC    H'0'                                                             
*                                                                               
PREQSTOP MVI   OPERSTOP,C'Y'                                                    
         B     PREQOK                                                           
*                                                                               
PREQFAIL DS    0H                                                               
         LA    R2,MQCC_FAILED                                                   
         ICM   R1,15,AECBRETN      POST ECB IN CALLING TASK                     
         POST  (1),(R2)                                                         
         CLI   QMGRFAIL,C'Y'                                                    
         BE    PREQNO                                                           
         B     PREQOK                                                           
*                                                                               
PREQOK   B     YES                                                              
PREQNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* PUT MESSAGE TO MQIO OUTPUT QUEUE                                    *         
* BRANCH HERE FROM FAMQIO PROCESS REQUEST PREQMQIO                    *         
* R7=A(MQIOD CALLERS MQIO CONTROL PARAMETER BLOCK)                    *         
* MQIOD INCLUDES MESSAGE DESCRIPTOR SET BY CALLER IN DDMQIO           *         
***********************************************************************         
MQIOPUT  DS    0H                                                               
*                                                                               
         LA    R0,MQIOPOPT         LOAD MQ PUT OPTIONS FROM DEFAULT             
         L     RE,=A(PUTMSGOPTS)   IN CSECT INTO CALLERS MQIOD PARMS            
         ICM   R1,15,=AL4(PUTMSGOPTS_LENGTH)                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  SET OVERRIDE MQPUT OPTIONS                   
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,MQIOPOPT_OPTIONS                                              
*                                                                               
         ICM   R2,15,MQIOABUF      R2=A(CALLER'S DATA BUFFER)                   
         ICM   RF,15,AMQRPUT       GET ADDRESS LOADED MQPUT ROUTINE             
*                                  CALL MQPUT TO PUT TO FACPAK OUTPUT Q         
         CALL  (15),                                                   +        
               (HCONN,                                                 +        
               HOOBJ,                                                  +        
               MQIOMSGD,                                               +        
               MQIOPOPT,                                               +        
               MQIOLBUF,                                               +        
               (R2),                                                   +        
               MQIOCC,                                                 +        
               MQIORC),                                                +        
               MF=(E,PPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,MQIOCC      SAVE MQ COMPLETION AND RETURN CODE           
         MVC   ATCMQRC,MQIORC      IN FAATC ENTRY FOR FAMQIO                    
*                                                                               
         LA    R0,MQCC_OK          TEST MQ COMPLETION CODE OK                   
         C     R0,MQIOCC                                                        
         BE    MPUTOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING     TEST MQ COMPLETION CODE WARNING              
         C     R0,MQIOCC                                                        
         BE    MPUTWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED      TEST MQ COMPLETION CODE FAILED               
         C     R0,MQIOCC                                                        
         BE    MPUTFAIL                                                         
         DC    H'0'                                                             
*                                                                               
MPUTWARN DS    0H                  TREAT MQ COMPLETION WARNING SAME             
         B     MPUTFAIL            AS FAILED                                    
*                                                                               
MPUTFAIL DS    0H                                                               
         LA    RF,ATCESPUQ         RECORD STATE OF FAMQIO IN CASE OF MQ         
         STC   RF,ATCERRCD         MQ PUT FAILED                                
         MVC   ATCERRCD+1(3),=CL3'PUT'                                          
         BAS   RE,CHKMQFL          CALL TO CHECK FAILED MQ RETURN CODE          
         B     PREQFAIL            EXIT TO FAQMIO PROCESS REQUEST FAIL          
*                                                                               
MPUTOK   DS    0H                  HERE IF MQPUT CALL OK                        
         ICM   R1,15,AECBRETN      POST ECB IN CALLING TASK                     
         POST  (1)                                                              
*                                                                               
         B     PREQOK              EXIT TO FAMQIO PROCESS REQUEST OK            
         EJECT                                                                  
***********************************************************************         
* GET ALL FROM WORK QUEUE AND ADD TO INPUT QUEUE                      *         
***********************************************************************         
         USING SSBD,RF                                                          
MQIOWRK  L     RF,ASSB                                                          
         TM    SSBSTAT4,SSBSAOR    AOR?                                         
         BO    MWRKOK              Yes, then don't do this, error               
         DROP  RF                                                               
*                                                                               
         MVC   HOBJ,HWOBJ                                                       
         BRAS  RE,GETMSG                                                        
         BNE   MWRKOK                                                           
         LA    R0,MQRC_NO_MSG_AVAILABLE                                         
         C     R0,MQIORC                                                        
         BE    MWRKOK                                                           
*                                                                               
         LA    R0,MQIOPOPT         LOAD MQ PUT OPTIONS FROM DEFAULT             
         L     RE,=A(PUTMSGOPTS)   IN CSECT INTO CALLERS MQIOD PARMS            
         ICM   R1,15,=AL4(PUTMSGOPTS_LENGTH)                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  SET OVERRIDE MQPUT OPTIONS                   
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,MQIOPOPT_OPTIONS                                              
*                                                                               
         CLC   MQIOLDAT,MQIOLBUF   USE DATA LENGTH FORM LAST GET                
         BNH   *+10                    IF NOT BIGGER THAN GET BUFFER            
         MVC   MQIOLDAT,MQIOLBUF                                                
*                                                                               
         ICM   R2,15,MQIOABUF      R2=A(CALLER'S DATA BUFFER)                   
         ICM   RF,15,AMQRPUT       GET ADDRESS LOADED MQPUT ROUTINE             
*                                  CALL MQPUT TO PUT TO FACPAK OUTPUT Q         
         CALL  (15),                                                   +        
               (HCONN,                                                 +        
               HCOBJ,                                                  +        
               MQIOMSGD,                                               +        
               MQIOPOPT,                                               +        
               MQIOLDAT,                                               +        
               (R2),                                                   +        
               MQIOCC,                                                 +        
               MQIORC),                                                +        
               MF=(E,PPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,MQIOCC      SAVE MQ COMPLETION AND RETURN CODE           
         MVC   ATCMQRC,MQIORC      IN FAATC ENTRY FOR FAMQIO                    
*                                                                               
         LA    R0,MQCC_OK          TEST MQ COMPLETION CODE OK                   
         C     R0,MQIOCC                                                        
         BNE   MWRKNO                                                           
*                                                                               
         BRAS  RE,COMMIN           COMMIT                                       
         B     MQIOWRK                                                          
*                                                                               
MWRKNO   DCHO                                                                   
         LA    RF,ATCESPUQ         RECORD STATE OF FAMQIO IN CASE OF MQ         
         STC   RF,ATCERRCD         MQ PUT FAILED                                
         MVC   ATCERRCD+1(3),=CL3'PUT'                                          
         BAS   RE,CHKMQFL          CALL TO CHECK FAILED MQ RETURN CODE          
         B     PREQFAIL                                                         
*                                                                               
*                                                                               
         USING SSBD,RF                                                          
MWRKOK   L     RF,ASSB                                                          
         NI    SSBSTAT6,X'FF'-SSB6MQWI   TURN BIT OFF FOR 1ST SYSOPEN           
         DROP  RF                                                               
*                                                                               
         ICM   R1,15,AECBRETN      POST ECB IN CALLING TASK                     
         POST  (1)                                                              
         B     PREQOK              EXIT TO FAQMIO PROCESS REQUEST FAIL          
         EJECT                                                                  
***********************************************************************         
* PUT MESSAGE TO SPECIFIC MQ OUTPUT QUEUE NAMED IN MQIOAQNM           *         
* BRANCH HERE FROM FAMQIO PROCESS REQUEST PREQMQIO                    *         
* R7=A(MQIOD CALLERS MQIO CONTROL PARAMETER BLOCK)                    *         
* MQIOD INCLUDES MESSAGE DESCRIPTOR SET BY CALLER IN DDMQIO           *         
***********************************************************************         
MQIOPUTQ DS    0H                                                               
*                                                                               
         LA    R0,MQIOPOPT         LOAD MQ PUT OPTIONS FROM DEFAULT             
         L     RE,=A(PUTMSGOPTS)   IN CSECT INTO CALLERS MQIOD PARMS            
         ICM   R1,15,=AL4(PUTMSGOPTS_LENGTH)                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                  SET OVERRIDE MQPUT OPTIONS                   
         LA    RF,MQPMO_NO_SYNCPOINT                                            
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,MQIOPOPT_OPTIONS                                              
*                                                                               
         LA    RF,MQOT_Q                    OBJECT IS A QUEUE                   
         ST    RF,OBJDESC_OBJECTTYPE        IN OBJECT TYPE FIELD                
         ICM   RF,15,MQIOAQNM               GET A(QUEUE NAME)                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   OBJDESC_OBJECTNAME,0(RF)     MOVE IN QUEUE NAME                  
*                                                                               
         ICM   R2,15,MQIOABUF      R2=A(CALLER'S DATA BUFFER)                   
         ICM   RF,15,AMQRPUT1      GET ADDRESS LOADED MQPUT1 ROUTINE            
*                                  CALL MQPUT1 TO OPEN/PUT TO QUEUE             
         CALL  (15),                                                   +        
               (HCONN,                                                 +        
               OBJDESC,                                                +        
               MQIOMSGD,                                               +        
               MQIOPOPT,                                               +        
               MQIOLBUF,                                               +        
               (R2),                                                   +        
               MQIOCC,                                                 +        
               MQIORC),                                                +        
               MF=(E,PPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,MQIOCC      SAVE MQ COMPLETION AND RETURN CODE           
         MVC   ATCMQRC,MQIORC      IN FAATC ENTRY FOR FAMQIO                    
*                                                                               
         LA    R0,MQCC_OK          TEST MQ COMPLETION CODE OK                   
         C     R0,MQIOCC                                                        
         BE    MPTQOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING     TEST MQ COMPLETION CODE WARNING              
         C     R0,MQIOCC                                                        
         BE    MPTQWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED      TEST MQ COMPLETION CODE FAILED               
         C     R0,MQIOCC                                                        
         BE    MPTQFAIL                                                         
         DC    H'0'                                                             
*                                                                               
MPTQWARN DS    0H                  TREAT MQ COMPLETION WARNING SAME             
         B     MPTQFAIL            AS FAILED                                    
*                                                                               
MPTQFAIL DS    0H                                                               
         LA    RF,ATCESPUQ         RECORD STATE OF FAMQIO IN CASE OF MQ         
         STC   RF,ATCERRCD         MQ PUT FAILED                                
         MVC   ATCERRCD+1(3),=CL3'PUT'                                          
         BAS   RE,CHKMQFL          CALL TO CHECK FAILED MQ RETURN CODE          
         B     PREQFAIL            EXIT TO FAQMIO PROCESS REQUEST FAIL          
*                                                                               
MPTQOK   DS    0H                  HERE IF MQPUT CALL OK                        
*                                                                               
         ICM   R1,15,AECBRETN      POST ECB IN CALLING TASK                     
         POST  (1)                                                              
*                                                                               
         B     PREQOK              EXIT TO FAMQIO PROCESS REQUEST OK            
         EJECT                                                                  
***********************************************************************         
* PUT REPLY MESSAGE TO MQIO OUTPUT QUEUE                              *         
***********************************************************************         
MQIORPY  DS    0H                                                               
*                                                                               
         LA    R0,MQIOPOPT                                                      
         L     RE,=A(PUTMSGOPTS)                                                
         ICM   R1,15,=AL4(PUTMSGOPTS_LENGTH)                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,MQIOPOPT_OPTIONS                                              
*                                                                               
         ICM   R2,15,MQIOABUF                                                   
*                                                                               
         ICM   RF,15,AMQRPUT                                                    
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOOBJ,                                                  X        
               MQIOMSGD,                                               X        
               MQIOPOPT,                                               X        
               MQIOLBUF,                                               X        
               (R2),                                                   X        
               MQIOCC,                                                 X        
               MQIORC),                                                X        
               MF=(E,PPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,MQIOCC                                                   
         MVC   ATCMQRC,MQIORC                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,MQIOCC                                                        
         BE    MRPYOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,MQIOCC                                                        
         BE    MRPYWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,MQIOCC                                                        
         BE    MRPYFAIL                                                         
         DC    H'0'                                                             
*                                                                               
MRPYWARN DS    0H                                                               
         B     MRPYFAIL                                                         
*                                                                               
MRPYFAIL DS    0H                                                               
         LA    RF,ATCESRPQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'PUT'                                          
         BAS   RE,CHKMQFL                                                       
         B     PREQFAIL                                                         
*                                                                               
MRPYOK   BAS   RE,GETWRK                                                        
         BNE   PREQFAIL                                                         
         BAS   RE,COMMOU                                                        
         BNE   PREQFAIL                                                         
*                                                                               
         ICM   R1,15,AECBRETN      POST ECB IN CALLING TASK                     
         POST  (1)                                                              
         B     PREQOK                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS MQ FAILURE CONDITION                                        *         
***********************************************************************         
PMQFAIL  NTR1                                                                   
         OC    STIMERID,STIMERID                                                
         BZ    PMQF010                                                          
         STIMERM CANCEL,ID=STIMERID                                             
*                                                                               
PMQF010  BAS   RE,CLOSEQ           CLOSE FACPAK MQ QUEUES                       
         BAS   RE,DISCONN          DISCONNECT FROM FACPAK MQ MANAGER            
*                                                                               
         XC    TIMERECB,TIMERECB                                                
         CLC   SVMQION,=F'3'                                                    
         BNE   WTOLAB1                                                          
         WTO   '*FACPAK* FAMQIO MQ FAIL BEFORE WAIT',MCSFLAG=HRDCPY             
WTOLAB1  EQU   *                                                                
*                                                                               
         L     R3,MQFWAITS         R3 = THE RETRY SHORT WAIT INTERVAL           
         ICM   RF,15,MQFRCVAL                                                   
         CLM   RF,15,MQFRCNTS                                                   
         BL    PMQF100                                                          
         L     R3,MQFWAITM         R3 = THE RETRY MEDIUM WAIT INTERVAL          
         CLM   RF,15,MQFRCNTM                                                   
         BL    PMQF100                                                          
         L     R3,MQFWAITL         R3 = THE RETRY LONG WAIT INTERVAL            
*                                                                               
PMQF100  EQU   *                                                                
         LA    RF,1(RF)                                                         
         STCM  RF,15,MQFRCVAL                                                   
         ST    R3,TIMERFWD                                                      
         STIMERM SET,                                                  +        
               ID=STIMERID,                                            +        
               BINTVL=TIMERFWD,                                        +        
               PARM=ATIMRECB,                                          +        
               EXIT=TIMEOUT                                                     
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING SSBD,RF                                                          
         L     RF,ASSB                                                          
         TM    SSBSTAT1,SSBSEOJ    EXIT IF FACPAK END OF JOB SHUTDOWN           
         BNZ   PMQF300                                                          
         DROP  RF                                                               
*                                                                               
         LA    R2,FECBLST                                                       
*                                                                               
         LA    RF,ATCECB                                                        
         ST    RF,0(R2)                                                         
         LA    R2,4(R2)                                                         
*                                                                               
         L     RF,ATIMRECB                                                      
         ST    RF,0(R2)                                                         
         OI    0(R2),X'80'                                                      
*                                  WAIT ON LIST FOR EVENT COMPLETION            
PMQF200  DS    0H                                                               
*                                  SET FAMQIO TASK ACTIVE FLAG                  
         NI    ATCSTAT1,255-ATCSAACT                                            
         LA    R1,FECBLST                                                       
         WAIT  ECBLIST=(1)                                                      
*                                                                               
         OI    ATCSTAT1,ATCSAACT                                                
*                                                                               
         TM    ATCECB,X'40'        PROCESS FACPAK CONTROL REQUEST               
         BZ    PMQF300                                                          
         MVI   ATCECB,0                                                         
                                                                                
         USING SSBD,RF                                                          
         L     RF,ASSB                                                          
         TM    SSBSTAT1,SSBSEOJ    EXIT IF FACPAK END OF JOB SHUTDOWN           
         BNZ   PMQF300                                                          
         DROP  RF                                                               
         B     PMQF200                                                          
*                                                                               
PMQF300  DS    0H                                                               
         CLC   SVMQION,=F'3'                                                    
         BNE   WTOLAB2                                                          
         WTO   '*FACPAK* FAMQIO MQ FAIL AFTER WAIT',MCSFLAG=HRDCPY              
WTOLAB2  EQU   *                                                                
*                                                                               
PMQFOK   B     YES                                                              
PMQFNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK MQ ERROR RETURN CODES                                         *         
* CHECK FOR MQ MANAGER OR SYSTEM FAIL CONDITION AS OPPOSED TO DATA    *         
* OR APPLICATION SPECIFIC EXCEPTION.                                  *         
***********************************************************************         
CHKMQFL  NTR1  ,                                                                
         LA    R0,MQCC_FAILED                                                   
         C     R0,ATCMQCC                                                       
         BNE   CMQFOK                                                           
         LA    R0,MQRC_CONNECTION_BROKEN                                        
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_Q_MGR_NAME_ERROR                                         
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_Q_MGR_NOT_AVAILABLE                                      
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_Q_MGR_STOPPING                                           
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_Q_MGR_NOT_ACTIVE                                         
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_CONNECTION_STOPPING                                      
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_CONNECTION_NOT_AUTHORIZED                                
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
*                                  ??                                           
         LA    R0,MQRC_Q_MGR_QUIESCING                                          
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_CONNECTION_QUIESCING                                     
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_NOT_AUTHORIZED                                           
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_MAX_CONNS_LIMIT_REACHED                                  
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_SYNCPOINT_LIMIT_REACHED                                  
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_ALREADY_CONNECTED                                        
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_HANDLE_NOT_AVAILABLE                                     
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_HCONN_ERROR                                              
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         LA    R0,MQRC_HOBJ_ERROR                                               
         C     R0,ATCMQRC                                                       
         BE    CMQF010                                                          
         B     CMQFOK                                                           
*                                                                               
CMQF010  DS    0H                                                               
         MVI   QMGRFAIL,C'Y'                                                    
         B     CMQFNO                                                           
*                                                                               
CMQFOK   B     YES                                                              
CMQFNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAR INPUT SIGNAL PENDING FLAGS                                    *         
***********************************************************************         
CLEARISP NTR1  ,                                                                
         ICM   R3,15,ATCAIOPL                                                   
         BZ    CISP030                                                          
         USING MQIOLD,R3                                                        
CISP010  TM    MQIOLFLG,MQIOLISP                                                
         BZ    CISP020                                                          
         NI    MQIOLFLG,255-(MQIOLISP+MQIOLBSY)                                 
*                                                                               
CISP020  ICM   R3,15,MQIOLNXT                                                   
         BZ    CISP030                                                          
         B     CISP010                                                          
*                                                                               
CISP030  B     CISPOK                                                           
*                                                                               
CISPOK   B     YES                                                              
CISPNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GET MESSAGE FROM MQIO INPUT QUEUE                                   *         
***********************************************************************         
GETMSG   NTR1  ,                                                                
         LA    R0,MQIOMSGD                                                      
         L     RE,=A(MSGDESC)                                                   
         ICM   R1,15,=AL4(MSGDESC_LENGTH)                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   MQIOMSGD_ENCODING,=AL4(MQENC_NATIVE)                             
         MVC   MQIOMSGD_CODEDCHARSETID,=AL4(MQCCSI_Q_MGR)                       
         XC    MQIOMSGD_CORRELID,MQIOMSGD_CORRELID                              
         XC    MQIOMSGD_MSGID,MQIOMSGD_MSGID                                    
*                                                                               
         LA    R0,MQIOGOPT                                                      
         L     RE,=A(GETMSGOPTS)                                                
         ICM   R1,15,=AL4(GETMSGOPTS_LENGTH)                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,MQGMO_NO_WAIT                                                 
         LA    RE,MQGMO_SYNCPOINT                                               
         AR    RF,RE                                                            
         LA    RE,MQGMO_ACCEPT_TRUNCATED_MSG                                    
         AR    RF,RE                                                            
         ICM   RE,15,=AL4(MQGMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,MQIOGOPT_OPTIONS                                              
*                                                                               
         ICM   RF,15,=A(MQWI_UNLIMITED)                                         
         ST    RF,MQIOGOPT_WAITINTERVAL                                         
*                                                                               
         ICM   R2,15,MQIOABUF                                                   
         ICM   RF,15,AMQRGET                                                    
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOBJ,                                                   X        
               MQIOMSGD,                                               X        
               MQIOGOPT,                                               X        
               MQIOLBUF,                                               X        
               (R2),                                                   X        
               MQIOLDAT,                                               X        
               MQIOCC,                                                 X        
               MQIORC),                                                X        
               MF=(E,GPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,MQIOCC                                                   
         MVC   ATCMQRC,MQIORC                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,MQIOCC                                                        
         BE    YES                                                              
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,MQIOCC                                                        
         BE    GETMWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,MQIOCC                                                        
         BE    GETMFAIL                                                         
         DC    H'0'                                                             
*                                                                               
GETMWARN DS    0H                                                               
         LA    R0,MQRC_TRUNCATED_MSG_ACCEPTED                                   
         C     R0,MQIORC                                                        
         BE    YES                                                              
         LA    R0,MQRC_NO_MSG_AVAILABLE                                         
         C     R0,MQIORC                                                        
         BE    YES                                                              
         B     GETMFAIL                                                         
*                                                                               
GETMFAIL DS    0H                                                               
         LA    R0,MQRC_NO_MSG_AVAILABLE                                         
         C     R0,MQIORC                                                        
         BE    YES                                                              
         LA    RF,ATCESGEQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'GET'                                          
         BAS   RE,CHKMQFL                                                       
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* SET SIGNAL TO GET MESSAGE FROM MQIO INPUT QUEUE                     *         
***********************************************************************         
SETMSG   NTR1  ,                                                                
         LA    R0,MQIOMSGD                                                      
         L     RE,=A(MSGDESC)                                                   
         ICM   R1,15,=AL4(MSGDESC_LENGTH)                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   MQIOMSGD_ENCODING,=AL4(MQENC_NATIVE)                             
         MVC   MQIOMSGD_CODEDCHARSETID,=AL4(MQCCSI_Q_MGR)                       
         XC    MQIOMSGD_CORRELID,MQIOMSGD_CORRELID                              
         XC    MQIOMSGD_MSGID,MQIOMSGD_MSGID                                    
*                                                                               
         LA    R0,MQIOGOPT                                                      
         L     RE,=A(GETMSGOPTS)                                                
         ICM   R1,15,=AL4(GETMSGOPTS_LENGTH)                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,MQGMO_SET_SIGNAL                                              
         LA    RE,MQGMO_NO_SYNCPOINT                                            
         AR    RF,RE                                                            
         LA    RE,MQGMO_BROWSE_FIRST                                            
         AR    RF,RE                                                            
         LA    RE,MQGMO_ACCEPT_TRUNCATED_MSG                                    
         AR    RF,RE                                                            
         ICM   RE,15,=AL4(MQGMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,MQIOGOPT_OPTIONS                                              
*                                                                               
         ICM   RF,15,=A(MQWI_UNLIMITED)                                         
         ST    RF,MQIOGOPT_WAITINTERVAL                                         
*                                                                               
         XC    MQIOGECB,MQIOGECB                                                
         LA    RF,MQIOGECB                                                      
         ST    RF,MQIOGOPT_SIGNAL1                                              
*                                                                               
         ICM   R2,15,MQIOABUF                                                   
*                                                                               
         ICM   RF,15,AMQRGET                                                    
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOBJ,                                                   X        
               MQIOMSGD,                                               X        
               MQIOGOPT,                                               X        
               MQIOLBUF,                                               X        
               (R2),                                                   X        
               MQIOLDAT,                                               X        
               MQIOCC,                                                 X        
               MQIORC),                                                X        
               MF=(E,SPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,MQIOCC                                                   
         MVC   ATCMQRC,MQIORC                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,MQIOCC                                                        
         BE    SETMFIND                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,MQIOCC                                                        
         BE    SETMFAIL                                                         
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,MQIOCC                                                        
         BE    SETMWARN                                                         
         DC    H'0'                                                             
*                                                                               
SETMWARN LA    R0,MQRC_SIGNAL_REQUEST_ACCEPTED                                  
         C     R0,MQIORC                                                        
         BE    YES                                                              
*                                                                               
         LA    R0,MQRC_TRUNCATED_MSG_ACCEPTED                                   
         C     R0,MQIORC                                                        
         BE    SETMFIND                                                         
         B     SETMFAIL                                                         
*                                                                               
SETMFAIL LA    RF,ATCESSEQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'SET'                                          
         BAS   RE,CHKMQFL                                                       
         B     NO                                                               
*                                                                               
SETMFIND B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* INQUIRE STATUS OF MQ CONNECTION                                     *         
***********************************************************************         
INQUIRY  NTR1  ,                                                                
*                                                                               
         CLC   SVMQION,=F'3'                                                    
         BNE   WTOLAB3                                                          
         WTO   '*FACPAK* FAMQIO TIMED STATUS INQUIRY',MCSFLAG=HRDCPY            
WTOLAB3  EQU   *                                                                
*                                                                               
         LA    RF,MQOT_Q_MGR                OBJECT IS QUEUE MANAGER             
         ST    RF,OBJDESC_OBJECTTYPE        IN OBJECT TYPE FIELD                
         MVI   OBJDESC_OBJECTNAME,0                                             
         LA    RF,MQOO_INQUIRE                                                  
         ST    RF,OPTIONS                                                       
*                                                                               
         ICM   RF,15,AMQROPEN                                                   
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               OBJDESC,                                                X        
               OPTIONS,                                                X        
               HOBJ,                                                   X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    INQU010                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    INQUWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    INQUFAIL                                                         
         DC    H'0'                                                             
*                                                                               
INQU010  DS    0H                                                               
         XC    SELECTCNT,SELECTCNT                                              
         XC    INTATTRCNT,INTATTRCNT                                            
         XC    CHARATTRLN,CHARATTRLN                                            
*                                                                               
         ICM   RF,15,AMQRINQ                                                    
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOBJ,                                                   X        
               SELECTCNT,                                              X        
               SELECTORS,                                              X        
               INTATTRCNT,                                             X        
               INTATTRS,                                               X        
               CHARATTRLN,                                             X        
               CHARATTRS,                                              X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,GCOMPCODE                                                     
         BE    INQU020                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,GCOMPCODE                                                     
         BE    INQUWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,GCOMPCODE                                                     
         BE    INQUFAIL                                                         
         DC    H'0'                                                             
*                                                                               
INQU020  LA    RF,MQCO_NONE                                                     
         ST    RF,OPTIONS                                                       
*                                                                               
         ICM   RF,15,AMQRCLOS                                                   
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOBJ,                                                   X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    YES                                                              
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    INQUWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    INQUFAIL                                                         
         DC    H'0'                                                             
*                                                                               
INQUWARN B     INQUFAIL                                                         
*                                                                               
INQUFAIL LA    RF,ATCESIQQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'INQ'                                          
         MVI   QMGRFAIL,C'Y'                                                    
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* COMMIT LAST MESSAGE PROCESSED                                       *         
***********************************************************************         
MQIOCMT  BAS   RE,GETWRK                                                        
         BNE   PREQNO                                                           
         BAS   RE,COMMOU                                                        
         BNE   PREQNO                                                           
*                                                                               
         ICM   R1,15,AECBRETN      POST ECB IN CALLING TASK                     
         POST  (1)                                                              
         B     PREQOK                                                           
         EJECT                                                                  
***********************************************************************         
* PUT MESSAGE TO WORK QUEUE                                           *         
***********************************************************************         
PUTWRK   NTR1  ,                                                                
         LA    RF,MQPMO_SYNCPOINT                                               
         ICM   RE,15,=AL4(MQPMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,PUTMSGOPTS_OPTIONS                                            
*                                                                               
         MVC   PUTMSGDESC_CORRELID,MQIOMSGD_CORRELID                            
         MVC   PUTMSGDESC_MSGID,MQIOMSGD_MSGID                                  
         ICM   R2,15,MQIOABUF                                                   
*                                                                               
         ICM   RF,15,AMQRPUT                                                    
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HWOBJ,                                                  X        
               PUTMSGDESC,                                             X        
               PUTMSGOPTS,                                             X        
               MQIOLDAT,                                               X        
               (R2),                                                   X        
               PCOMPCODE,                                              X        
               PREASON),                                               X        
               MF=(E,PPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,PCOMPCODE                                                
         MVC   ATCMQRC,PREASON                                                  
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,PCOMPCODE                                                     
         BE    YES                                                              
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,PCOMPCODE                                                     
         BE    PUTWWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,PCOMPCODE                                                     
         BE    PUTWFAIL                                                         
         DC    H'0'                                                             
*                                                                               
PUTWWARN B     PUTWFAIL                                                         
*                                                                               
PUTWFAIL LA    RF,ATCESCMQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'PUT'                                          
         BAS   RE,CHKMQFL                                                       
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* GET MESSAGE FROM WORK QUEUE                                         *         
***********************************************************************         
GETWRK   NTR1  ,                                                                
         LA    RF,MQGMO_NO_WAIT                                                 
         LA    RE,MQGMO_SYNCPOINT                                               
         AR    RF,RE                                                            
         LA    RE,MQGMO_ACCEPT_TRUNCATED_MSG                                    
         AR    RF,RE                                                            
         ICM   RE,15,=AL4(MQGMO_FAIL_IF_QUIESCING)                              
         AR    RF,RE                                                            
         ST    RF,GETMSGOPTS_OPTIONS                                            
*                                                                               
         MVC   GETMSGDESC_CORRELID,MQIOMSGD_CORRELID                            
         XC    GETMSGDESC_MSGID,GETMSGDESC_MSGID                                
*                                                                               
         ICM   R2,15,MQIOABUF                                                   
         LHI   RE,2024                                                          
         STCM  RE,15,MQIOLBUF                                                   
*                                                                               
         ICM   RF,15,=A(MQWI_UNLIMITED)                                         
         ST    RF,GETMSGOPTS_WAITINTERVAL                                       
*                                                                               
         ICM   RF,15,AMQRGET                                                    
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HWOBJ,                                                  X        
               GETMSGDESC,                                             X        
               GETMSGOPTS,                                             X        
               MQIOLBUF,                                               X        
               (R2),                                                   X        
               MQIOLDAT,                                               X        
               GCOMPCODE,                                              X        
               GREASON),                                               X        
               MF=(E,GPARMLIST),VL                                              
*                                                                               
         MVC   ATCMQCC,GCOMPCODE                                                
         MVC   ATCMQRC,GREASON                                                  
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,GCOMPCODE                                                     
         BE    GETWOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,GCOMPCODE                                                     
         BE    GETWWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,GCOMPCODE                                                     
         BE    GETWFAIL                                                         
         DC    H'0'                                                             
*                                                                               
GETWWARN DS    0H                                                               
         LA    R0,MQRC_TRUNCATED_MSG_ACCEPTED                                   
         C     R0,MQIORC                                                        
         BE    GETWOK                                                           
         B     GETWFAIL                                                         
*                                                                               
GETWFAIL DS    0H                                                               
         LA    RF,ATCESCMQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'GET'                                          
         BAS   RE,CHKMQFL                                                       
         B     GETWNO                                                           
*                                                                               
GETWOK   DS    0H                                                               
         B     YES                                                              
*                                                                               
GETWNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* COMMIT MESSAGE GOT FROM INPUT QUEUE                                 *         
***********************************************************************         
COMMIN   NTR1  ,                                                                
         ICM   RF,15,AMQRCOMM                                                   
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CMINOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CMINWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CMINFAIL                                                         
         DC    H'0'                                                             
*                                                                               
CMINWARN DS    0H                                                               
         B     CMINFAIL                                                         
*                                                                               
CMINFAIL DS    0H                                                               
         LA    RF,ATCESCMQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'CMT'                                          
         BAS   RE,CHKMQFL                                                       
         B     CMINNO                                                           
*                                                                               
CMINOK   DS    0H                                                               
         B     YES                                                              
*                                                                               
CMINNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* COMMIT MESSAGE PUT TO OUTPUT QUEUE                                  *         
***********************************************************************         
COMMOU   NTR1                                                                   
*                                                                               
         ICM   RF,15,AMQRCOMM                                                   
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CMOUOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CMOUWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CMOUFAIL                                                         
         DC    H'0'                                                             
*                                                                               
CMOUWARN DS    0H                                                               
         B     CMOUFAIL                                                         
*                                                                               
CMOUFAIL DS    0H                                                               
         LA    RF,ATCESCMQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'CMT'                                          
         BAS   RE,CHKMQFL                                                       
         B     CMOUNO                                                           
*                                                                               
CMOUOK   DS    0H                                                               
         B     YES                                                              
*                                                                               
CMOUNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* CONNECT TO MQ MANAGER                                               *         
***********************************************************************         
CONNECT  NTR1                                                                   
         XC    HCONN,HCONN                  NULL CONNECTION HANDLE              
*                                                                               
         ICM   RF,15,AMQRCONN                                                   
*                                                                               
         CALL  (15),                                                   X        
               (MQMNAME,                                               X        
               HCONN,                                                  X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CONNOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CONNWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CONNFAIL                                                         
         DC    H'0'                                                             
*                                                                               
CONNWARN DS    0H                                                               
         B     CONNFAIL                                                         
*                                                                               
CONNFAIL DS    0H                                                               
         LA    RF,ATCESCOQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'CON'                                          
         MVI   QMGRFAIL,C'Y'                                                    
         B     CONNNO                                                           
*                                                                               
CONNOK   DS    0H                                                               
         LA    RF,ATCCSCOQ                                                      
         STC   RF,ATCSTATI         SET FAMQIO STATUS TO CONNECTED               
         B     YES                                                              
*                                                                               
CONNNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* OPEN QUEUES                                                         *         
***********************************************************************         
         USING SSBD,R7                                                          
OPENQ    NTR1                                                                   
         L     R7,ASSB                                                          
         TM    SSBSTAT4,SSBSAOR    AOR?                                         
         BO    OPEN010             Yes, don't open input queue                  
         LA    RF,MQOT_Q                    OBJECT IS A QUEUE                   
         ST    RF,OBJDESC_OBJECTTYPE        IN OBJECT TYPE FIELD                
         MVC   OBJDESC_OBJECTNAME,MQINNAME  Input queue name                    
         LA    RF,MQOO_BROWSE                                                   
         LA    RE,MQOO_INPUT_SHARED                                             
         AR    RF,RE                                                            
         LA    RE,MQOO_OUTPUT               INDICATE OPEN TO PUT ALSO           
         AR    RF,RE                                                            
         ST    RF,OPTIONS                                                       
*                                                                               
         ICM   RF,15,AMQROPEN                                                   
         CALL  (15),                                                   +        
               (HCONN,                                                 +        
               OBJDESC,                                                +        
               OPTIONS,                                                +        
               HIOBJ,                                                  +        
               COMPCODE,                                               +        
               REASON),                                                +        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    OPEN010                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    OPENWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    OPENFAIL                                                         
         DC    H'0'                                                             
*                                                                               
OPEN010  DS    0H                                                               
         LA    RF,MQOT_Q                    OBJECT IS A QUEUE                   
         ST    RF,OBJDESC_OBJECTTYPE        IN OBJECT TYPE FIELD                
         MVC   OBJDESC_OBJECTNAME,MQOUNAME  Output queue name                   
         LA    RF,MQOO_OUTPUT               INDICATE OPEN IS                    
         ST    RF,OPTIONS                   OUTPUT ONLY                         
*                                                                               
         ICM   RF,15,AMQROPEN                                                   
*                                                                               
         CALL  (15),                                                   +        
               (HCONN,                                                 +        
               OBJDESC,                                                +        
               OPTIONS,                                                +        
               HOOBJ,                                                  +        
               COMPCODE,                                               +        
               REASON),                                                +        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    OPEN020                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    OPENWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    OPENFAIL                                                         
         DC    H'0'                                                             
*                                                                               
OPEN020  DS    0H                                                               
         TM    SSBSTAT4,SSBSAOR    AOR?                                         
         BO    OPEN030             Yes, don't open work queue                   
         LA    RF,MQOT_Q                    OBJECT IS A QUEUE                   
         ST    RF,OBJDESC_OBJECTTYPE        IN OBJECT TYPE FIELD                
         MVC   OBJDESC_OBJECTNAME,MQWKNAME  Work queue name                     
         LA    RF,MQOO_BROWSE                                                   
         LA    RE,MQOO_INPUT_SHARED                                             
         AR    RF,RE                                                            
         LA    RE,MQOO_OUTPUT                                                   
         AR    RF,RE                                                            
         ST    RF,OPTIONS                                                       
*                                                                               
         ICM   RF,15,AMQROPEN                                                   
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               OBJDESC,                                                X        
               OPTIONS,                                                X        
               HWOBJ,                                                  X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    OPEN030                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    OPENWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    OPENFAIL                                                         
         DC    H'0'                                                             
*                                                                               
OPEN030  DS    0H                                                               
         TM    SSBSTAT4,SSBSAOR    AOR?                                         
         BO    OPENOK              Yes, don't open input queue                  
         LA    RF,MQOT_Q                    OBJECT IS A QUEUE                   
         ST    RF,OBJDESC_OBJECTTYPE        IN OBJECT TYPE FIELD                
         MVC   OBJDESC_OBJECTNAME,MQCTNAME  Control queue anme                  
         LA    RF,MQOO_BROWSE                                                   
         LA    RE,MQOO_INPUT_SHARED                                             
         AR    RF,RE                                                            
         LA    RE,MQOO_OUTPUT                                                   
         AR    RF,RE                                                            
         ST    RF,OPTIONS                                                       
*                                                                               
         ICM   RF,15,AMQROPEN                                                   
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               OBJDESC,                                                X        
               OPTIONS,                                                X        
               HCOBJ,                                                  X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         MVC   ATCMQCC,COMPCODE                                                 
         MVC   ATCMQRC,REASON                                                   
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    OPENOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    OPENWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    OPENFAIL                                                         
         DC    H'0'                                                             
*                                                                               
OPENWARN DS    0H                                                               
         B     OPENFAIL                                                         
*                                                                               
OPENFAIL DS    0H                                                               
         LA    RF,ATCESOPQ                                                      
         STC   RF,ATCERRCD                                                      
         MVC   ATCERRCD+1(3),=CL3'OPN'                                          
         MVI   QMGRFAIL,C'Y'                                                    
         B     OPENNO                                                           
*                                                                               
OPENOK   DS    0H                                                               
         LA    RF,ATCCSOPQ                                                      
         STC   RF,ATCSTATI         SET FAMQIO STATUS TO OPENED QUEUES           
         B     YES                                                              
*                                                                               
OPENNO   B     NO                                                               
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
* CLOSE QUEUES                                                        *         
***********************************************************************         
CLOSEQ   NTR1                                                                   
         LA    RF,MQCO_NONE        Normal Close of queue action                 
         ST    RF,OPTIONS                                                       
         ICM   RF,15,AMQRCLOS      Close output queue                           
         OC    HOOBJ,HOOBJ         Output queue handle object set?              
         BZ    CLOS010             No                                           
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HOOBJ,                                                  X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CLOS010                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CLOSWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CLOSFAIL                                                         
         DC    H'0'                                                             
*                                                                               
CLOS010  DS    0H                                                               
         OC    HIOBJ,HIOBJ         Input queue handle object set?               
         BZ    CLOS020             No                                           
         LA    R0,MQCO_NONE        Normal close of queue action                 
         ST    R0,OPTIONS                                                       
*                                                                               
         ICM   RF,15,AMQRCLOS      Close input queue                            
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HIOBJ,                                                  X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CLOS020                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CLOSWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CLOSFAIL                                                         
         DC    H'0'                                                             
*                                                                               
CLOS020  DS    0H                                                               
         LA    R0,MQCO_NONE        Normal close queue action                    
         ST    R0,OPTIONS                                                       
         ICM   RF,15,AMQRCLOS      Close worker queue                           
         OC    HWOBJ,HWOBJ         Worker queue handle object set?              
         BZ    CLOS030             No                                           
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HWOBJ,                                                  X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CLOS030                                                          
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CLOSWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CLOSFAIL                                                         
         DC    H'0'                                                             
*                                                                               
         LA    R0,MQCC_OK                   EXPECTED COMPCODE                   
         C     R0,COMPCODE                  AS EXPECTED?                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLOS030  DS    0H                                                               
         LA    R0,MQCO_NONE        Normal close queue action                    
         ST    R0,OPTIONS                                                       
         ICM   RF,15,AMQRCLOS      Close control queue                          
         OC    HCOBJ,HCOBJ         Control queue handle object set?             
         BZ    CLOSOK              No, so done                                  
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               HCOBJ,                                                  X        
               OPTIONS,                                                X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    CLOSOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    CLOSWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    CLOSFAIL                                                         
         DC    H'0'                                                             
*                                                                               
CLOSWARN DS    0H                                                               
         B     CLOSFAIL                                                         
*                                                                               
CLOSFAIL DS    0H                                                               
*        LA    RF,ATCESCLQ                                                      
*        STC   RF,ATCERRCD                                                      
*        MVC   ATCERRCD+1(3),=CL3'CLS'                                          
         MVI   QMGRFAIL,C'Y'                                                    
         B     CLOSNO                                                           
*                                                                               
CLOSOK   DS    0H                                                               
         LA    RF,ATCCSCLQ                                                      
         STC   RF,ATCSTATI         SET FAMQIO STATUS TO CLOSED QUEUES           
         B     YES                                                              
*                                                                               
CLOSNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* DISCONNECT FROM MQ MANAGER                                          *         
***********************************************************************         
DISCONN  NTR1                                                                   
*                                                                               
         ICM   RF,15,AMQRDISC                                                   
*                                                                               
         CALL  (15),                                                   X        
               (HCONN,                                                 X        
               COMPCODE,                                               X        
               REASON),                                                X        
               MF=(E,PARMLIST),VL                                               
*                                                                               
         LA    R0,MQCC_OK                                                       
         C     R0,COMPCODE                                                      
         BE    DISCOK                                                           
*                                                                               
         LA    R0,MQCC_WARNING                                                  
         C     R0,COMPCODE                                                      
         BE    DISCWARN                                                         
*                                                                               
         LA    R0,MQCC_FAILED                                                   
         C     R0,COMPCODE                                                      
         BE    DISCFAIL                                                         
         DC    H'0'                                                             
*                                                                               
DISCWARN DS    0H                                                               
         B     DISCFAIL                                                         
*                                                                               
DISCFAIL DS    0H                                                               
*        LA    RF,ATCESDIQ                                                      
*        STC   RF,ATCERRCD                                                      
*        MVC   ATCERRCD+1(3),=CL3'DIS'                                          
         MVI   QMGRFAIL,C'Y'                                                    
         B     DISCNO                                                           
*                                                                               
DISCOK   DS    0H                                                               
         LA    RF,ATCCSDIQ                                                      
         STC   RF,ATCSTATI         SET FAMQIO STATUS TO DISCONNECTED            
         B     YES                                                              
*                                                                               
DISCNO   B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* DYNAMICALLY LOAD MQ ROUTINES                                        *         
***********************************************************************         
LOADMQ   NTR1                                                                   
         LOAD  EP=CSQBCONN                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRCONN                                                   
*                                                                               
         LOAD  EP=CSQBOPEN                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQROPEN                                                   
*                                                                               
         LOAD  EP=CSQBGET                                                       
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRGET                                                    
*                                                                               
         LOAD  EP=CSQBPUT                                                       
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRPUT                                                    
*                                                                               
         LOAD  EP=CSQBPUT1                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRPUT1                                                   
*                                                                               
         LOAD  EP=CSQBCLOS                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRCLOS                                                   
*                                                                               
         LOAD  EP=CSQBDISC                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRDISC                                                   
*                                                                               
         LOAD  EP=CSQBCOMM                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRCOMM                                                   
*                                                                               
         LOAD  EP=CSQBBACK                                                      
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRBACK                                                   
*                                                                               
         LOAD  EP=CSQBINQ                                                       
         LTR   RF,RF                                                            
         BNZ   LOADBAD                                                          
         STCM  R0,15,AMQRINQ                                                    
         B     XIT                                                              
*                                                                               
LOADBAD  DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* Delete MQ Routines                                                  *         
***********************************************************************         
DELMQ    NTR1                                                                   
         DELETE EP=CSQBCONN                                                     
         DELETE EP=CSQBOPEN                                                     
         DELETE EP=CSQBGET                                                      
         DELETE EP=CSQBPUT                                                      
         DELETE EP=CSQBCLOS                                                     
         DELETE EP=CSQBDISC                                                     
         DELETE EP=CSQBCOMM                                                     
         DELETE EP=CSQBBACK                                                     
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* Build ECB list entires                                              *         
*       TOR Read/Get ECB for control queue                            *         
*       TOR/AOR TCB ECB to post to wake up FAMQIO                     *         
*       ATC   ECB                                                     *         
*       Timer ECB                                                     *         
***********************************************************************         
BLDECBL  NTR1                                                                   
         LA    R2,ECBLST                                                        
*                                                                               
         USING SSBD,RE                                                          
         L     RE,ASSB                                                          
         TM    SSBSTAT4,SSBSAOR    AOR?                                         
         BO    BECB030             Yes, so don't do for reads (gets)            
         DROP  RE                                                               
*                                                                               
         ICM   R3,15,ATCAIOPL      IO                                           
         BZ    BECB030                                                          
         USING MQIOLD,R3                                                        
*                                                                               
BECB010  TM    MQIOLFLG,MQIOLCTL                                                
         BO    BECB012                                                          
         TM    MQIOLFLG,MQIOLRD    Read                                         
         BZ    BECB020             No, so skip                                  
*                                                                               
BECB012  ICM   R7,15,MQIOLC                                                     
         BZ    BECB020                                                          
         USING MQIOD,R7                                                         
         LA    RF,MQIOGECB                                                      
         ST    RF,0(,R2)                                                        
         LA    R2,4(,R2)                                                        
         DROP  R7                                                               
*                                                                               
BECB020  ICM   R3,15,MQIOLNXT                                                   
         BNZ   BECB010             Keep going                                   
         DROP  R3                                                               
*                                                                               
BECB030  L     R3,ATCB                                                          
         LH    R4,0(,R3)                                                        
         L     R5,2(,R3)                                                        
         LA    R3,6(,R3)                                                        
         USING TCBD,R3                                                          
*                                                                               
BECB040  LA    RF,TCBMQERQ         TCB ECB to wake up MQ                        
         ST    RF,0(,R2)                                                        
         LA    R2,4(,R2)                                                        
*                                                                               
BECB050  BXLE  R3,R4,BECB040                                                    
         DROP  R3                                                               
*                                                                               
         LA    RF,ATCECB           ATC ECB                                      
         ST    RF,0(,R2)                                                        
         LA    R2,4(,R2)                                                        
*                                                                               
         LA    RF,TIMERECB         Timer ECB                                    
         STCM  RF,15,ATIMRECB                                                   
         ST    RF,0(,R2)                                                        
         OI    0(R2),X'80'                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* USEFUL ROUTINES                                                     *         
***********************************************************************         
YES      SR    RC,RC               EXIT SUBROUTINE WITH CC .EQ.                 
NO       LTR   RC,RC               EXIT SUBROUTINE WITH CC .NE.                 
XIT      XIT1  ,                   EXIT SUBROUTINE                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ASYNCHRONOUS TIMER EXIT ROUTINE                                     *         
***********************************************************************         
TIMEOUT  STM   RE,RC,12(RD)                                                     
         LR    RB,RF                                                            
         USING TIMEOUT,RB                                                       
* ??     L     R3,=V(TIMERECB)                                                  
         L     R2,4(R1)                                                         
         POST  (R2)                                                             
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* Constands and Value                                                           
***********************************************************************         
ANAME    DC    C'FAMQIO  '         TASK NAME                                    
SAVEAREA DS    9D                                                               
PARMSAVE DS    F                                                                
BUFFERLENGTH DC F'80'                                                           
MSGCLEAR DC    CL80' '                                                          
ZEROREC  DC    F'0'                                                             
WAITSECS DC    F'30000'            DEFAULT WAIT TIME = 300 SECS                 
MQFWAITS DC    F'01000'            MQ FAIL SHORT WAIT TIME = 10 SECS            
MQFRCNTS DC    F'6'                MQ FAIL RETRY COUNT SHORT                    
MQFWAITM DC    F'06000'            MQ FAIL MEDIUM WAIT TIME = 60 SECS           
MQFRCNTM DC    F'16'               MQ FAIL RETRY COUNT MEDIUM                   
MQFWAITL DC    F'60000'            MQ FAIL LONG WAIT TIME = 600 SECS            
MQFRCVAL DC    F'0'                                                             
TIMERECB DS    F                                                                
TIMERFWD DS    F                                                                
STIMERID DS    XL4                                                              
*                                                                               
OBJDESC    CMQODA  DSECT=NO,LIST=YES        OBJECT DESCRIPTOR                   
MSGDESC    CMQMDA  DSECT=NO,LIST=YES        MESSAGE DESCRIPTOR                  
PUTMSGDESC CMQMDA  DSECT=NO,LIST=YES        PUT MESSAGE DESCRIPTOR              
GETMSGDESC CMQMDA  DSECT=NO,LIST=YES        GET MESSAGE DESCRIPTOR              
PUTMSGOPTS CMQPMOA DSECT=NO,LIST=YES        PUT MESSAGE OPTIONS                 
GETMSGOPTS CMQGMOA DSECT=NO,LIST=YES        GET MESSAGE OPTIONS                 
*                                                                               
***********************************************************************         
*  MQ API CONSTANTS                                                   *         
***********************************************************************         
         CMQA LIST=YES                                                          
*                                                                               
         DC    C'*ECBLST*'                                                      
ECBLST   DC    XL256'00'                                                        
         DC    XL256'00'                                                        
*                                                                               
         DC    C'*FECBLST'                                                      
FECBLST  DC    XL256'00'                                                        
*                                                                               
         SPACE 1                                                                
WORKC    DC    400D'0'             WORK AREA                                    
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
*  MQ API CONSTANTS                                                   *         
***********************************************************************         
         EJECT                                                                  
* DSECT TO COVER WORK STORAGE                                                   
*                                                                               
WORKD      DSECT                                                                
*                                                                               
PARMLIST   CALL ,(0,0,0,0,0,0,0,0,0,0,0),VL,MF=L                                
GPARMLIST  CALL ,(0,0,0,0,0,0,0,0,0,0,0),VL,MF=L                                
PPARMLIST  CALL ,(0,0,0,0,0,0,0,0,0,0,0),VL,MF=L                                
SPARMLIST  CALL ,(0,0,0,0,0,0,0,0,0,0,0),VL,MF=L                                
*                                                                               
DUB        DS    D                                                              
DMCB       DS    6F                                                             
SAVERD     DS    F                                                              
AATC       DS    A                                                              
AECBRETN   DS    A                                                              
*                                  ADDRESSES OF LOADED MQ ROUTINES              
AMQRCONN   DS    A                 A(CSQBCONN)                                  
AMQROPEN   DS    A                 A(CSQBOPEN)                                  
AMQRGET    DS    A                 A(CSQBGET)                                   
AMQRPUT    DS    A                 A(CSQBPUT)                                   
AMQRPUT1   DS    A                 A(CSQBPUT1)                                  
AMQRCLOS   DS    A                 A(CSQBCLOS)                                  
AMQRDISC   DS    A                 A(CSQBDISC)                                  
AMQRCOMM   DS    A                 A(CSQBCOMM)                                  
AMQRBACK   DS    A                 A(CSQBBACK)                                  
AMQRINQ    DS    A                 A(CSQBINQ)                                   
*                                                                               
ASSB       DS    A                                                              
ATCB       DS    A                                                              
*                                                                               
ATIMRECB   DS    A                                                              
LOOPTIME   DS    F                                                              
ACTION     DS    X                                                              
ACTPUT     EQU   X'40'                                                          
ACTGET     EQU   X'20'                                                          
RETCODE    DS    X                 XBASE RETURN CODE                            
ECBFOUND   DS    C                                                              
OPERSTOP   DS    C                                                              
QMGRFAIL   DS    C                                                              
DATALENGTH DS    F                                                              
OPTIONS    DS    F                                                              
COMPCODE   DS    F                                                              
REASON     DS    F                                                              
GCOMPCODE  DS    F                                                              
GREASON    DS    F                                                              
PCOMPCODE  DS    F                                                              
PREASON    DS    F                                                              
SCOMPCODE  DS    F                                                              
SREASON    DS    F                                                              
HCONN      DS    F                 Connect object                               
HOBJ       DS    F                                                              
HIOBJ      DS    F                 Input   queue object                         
HOOBJ      DS    F                 Output  queue object                         
HWOBJ      DS    F                 Worker  queue object                         
HCOBJ      DS    F                 Control queue object                         
OBJECT     DS    F                                                              
EXITCODE   DS    F                                                              
SELECTCNT  DS    F                                                              
SELECTORS  DS    F                                                              
INTATTRCNT DS    F                                                              
INTATTRS   DS    F                                                              
CHARATTRLN DS    F                                                              
CHARATTRS  DS    CL80                                                           
TIMEDATE   DS    0CL6                                                           
DATECONV   DS    0CL6                                                           
           ORG   DATECONV                                                       
           DS    CL1                                                            
DATE_YR    DS    CL2                                                            
DATE_JN    DS    CL3                                                            
           ORG                                                                  
PARMADDR   DS    F                                                              
PARMLEN    DS    H                                                              
*                                                                               
SVMQION    DS    F                                                              
MQMNAME    DS    CL48                                                           
MQINNAME   DS    CL48                                                           
MQOUNAME   DS    CL48                                                           
MQWKNAME   DS    CL48                                                           
MQCTNAME   DS    CL48                                                           
MQMQUEUE   DS    CL48                                                           
*                                                                               
MSGDATA    DS    CL132                                                          
DSPCOMP    DS    CL04                                                           
DSPREAS    DS    CL04                                                           
DSPMSGL    DS    CL08                                                           
CONVAREA   DS    CL8                                                            
*                                                                               
BUFFER     DS    CL80                                                           
*                                                                               
WORKL      EQU   *-WORKD                                                        
         SPACE 1                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         SPACE 1                                                                
* DDMQIOD                                                                       
       ++INCLUDE DDMQIOD                                                        
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039FAMQIO    07/19/12'                                      
         END                                                                    
